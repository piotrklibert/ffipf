import os
import tables
import sequtils
import strutils
import critbits
import sugar
import algorithm
import tables
import re


template debugEcho(args: varargs[string, `$`]) =
  when defined(debugLogging):
    echo args.join(" ")


const MAX_RESULTS* = 100

type
  FFConfig* = object
    dirsBlacklist*: CritBitTree[void]
    extBlacklist*: CritBitTree[void]
    extWeights*: Table[string, float]
    searchRoot*: string


var config: FFConfig
var paths: seq[string] = newSeqOfCap[string](20200)


proc initPaths2*(c: FFConfig, dir: string): var seq[string] =
  ## Traverses the `dir` directory recursively and stores all found files in a
  ## module variable `paths`. Doesn't go into `dirsBlacklist` directories.
  ## Ignores files with `extBlacklist` extensions.
  for (kind, path) in walkDir(dir):
    case kind:
      of pcFile, pcLinkToFile:
        let ext = splitFile(path).ext
        if not c.extBlacklist.contains(ext):
          paths.add(path.replace(c.searchRoot.parentDir, ""))
      of pcDir, pcLinkToDir:
        let name = lastPathPart(path).replace("/","")
        if not c.dirsBlacklist.contains(name):
          discard initPaths2(c, path)
  return paths


proc initFFinder*(c: FFConfig): int =
  paths = @[]
  config = c
  discard initPaths2(c, c.searchRoot)
  return len(paths)



proc reset_paths*() =
  if len(paths) > 0:
    paths = newSeqOfCap[string](20200)


type
  MatchResultObj* = object of RootObj
    score*: float
    res*: string
    missed: bool
  MatchResult* = ref MatchResultObj
  MatchCache = Table[string, MatchResult]
  Matcher = object of RootObj
    re: Regex
    re_raw: string
    parts: seq[string]
    ngroups: int
    empty: bool


proc `$`*(m: MatchResult): string =
  "MatchResult(score: $1, res: $2)".format(m.score, m.res)


proc make_absolute(res: MatchResult): MatchResult =
  result = res
  result.res = config.searchRoot.parentDir & res.res


# Takes the given pattern string "foo" and converts it to a new
# string "(f)([^/]*?)(o)([^/]*?)(o)" that can be used to create
# a regular expression.
proc make_pattern(pattern: string): string =
  pattern.toSeq().mapIt("(" & escapeRe($it) & ")").join(r"([^/]*?)")


# Takes a pattern and creates two Matcher objects, one for the file name part,
# and the other for file path part.
proc make_regexes(pattern: string): (Matcher, Matcher) =
  var path_parts = pattern.split("/")
  let
    file_name_part = path_parts.pop()
    file_regex_raw = r"^(.*?)" & make_pattern(file_name_part) & "(.*)$"
    path_regex_raw =
      if len(path_parts) > 0:
        "^(.*?)" & path_parts.map(make_pattern).join(r"(.*?/.*?)") & "(.*?)$"
      else:
        ""
  let
    path_matcher = Matcher(
      re: path_regex_raw.re,
      re_raw: path_regex_raw,
      empty: len(path_parts) == 0,
      parts: path_parts,
      ngroups: path_regex_raw.toSeq().countIt(it == '(')
    )
    file_matcher = Matcher(
      re: file_regex_raw.re,
      re_raw: file_regex_raw,
      parts: @[file_name_part],
      ngroups: file_regex_raw.toSeq().countIt(it == '(')
    )

  return (path_matcher, file_matcher)


type
  CharGroup = object
    str: string
    inside: bool


proc build_match_result(captures: seq[string], parts: int): MatchResult =
  ## Computes match score for a given sequence of regex captures. Takes a number
  ## of path parts in the original patterns as the second argument. The score is
  ## computed based on the ratio of characters captured as part of the original
  ## pattern to characters outside of such captures, and a ratio of continuous
  ## captured character groups to the number of path parts in the filename.
  var
    total_chars, inside_chars: int
    runs = newSeq[CharGroup]()
  for (i, grp) in captures.pairs():
    if len(grp) == 0:
      continue

    let inside = i mod 2 != 0

    total_chars += len(grp.replace("/", ""))
    if inside:
      inside_chars += len(grp)

    if len(runs) == 0 or runs[^1].inside != inside:
      runs.add( CharGroup(str: grp, inside: inside) )
    else:
      runs[^1].str.add(grp)

  let run_score = parts / runs.filterIt(it.inside == true).len()
  let full_score = inside_chars / total_chars * run_score
  MatchResult(score: full_score, res: captures.join(""), missed: false)


proc match_path(path: string, path_matches: var MatchCache,
                matcher: Matcher): MatchResult =
  ## Matches path portion of the filename, uses `path_matches` as a cache to
  ## store match results of paths already seen.
  if matcher.empty:
    return MatchResult(score: 1 / len(path), res: path, missed: false)
  if path in path_matches:
    return path_matches[path]
  var groups = newSeq[string](matcher.ngroups)
  if path.match(matcher.re, groups):
    result = build_match_result(groups, len(matcher.parts))
    path_matches[path] = result
  else:
    result = MatchResult(score: 0, res: "", missed: true)


proc match_file(file: string, matcher: Matcher,
                path_res: MatchResult): MatchResult =
  ## Matches file name portion of the path. Uses match result from `match_path`
  ## to compute the final result and score.
  var groups = newSeq[string](matcher.ngroups)
  if file.match(matcher.re, groups):
    var res = build_match_result(groups, 1)
    var score = res.score * path_res.score
    let ext = splitFile(file).ext
    if ext in config.extWeights:
      score *= config.extWeights[ext]

    MatchResult(
      score: score,
      res: path_res.res.joinPath(file),
      missed: false
    )
  else:
    MatchResult(
      score: 0,
      res: path_res.res.joinPath(file),
      missed: true
    )


proc search_paths(pattern: string): seq[MatchResult] =
  let (path_matcher, file_matcher) = make_regexes(pattern)
  var path_matches = initTable[string, MatchResult](0)
  collect(newSeq()):
    for path in paths:
      let (dir, file, ext) = splitFile(path)
      let path_match = match_path(dir, path_matches, path_matcher)
      if path_match.missed: continue

      let full_match = match_file(file & ext, file_matcher, path_match)
      if full_match.missed: continue

      full_match


proc search*(pattern: string, count: int = MAX_RESULTS): seq[MatchResult] =
  ## Perform a search for files matching `pattern`. Returns at most `count`
  ## results, but can return less results or even none at all.
  result = @[]
  let matches = search_paths(pattern).sortedByIt(-it.score)
  if len(matches) > 0:
    let count = if high(matches) > count: count - 1 else: high(matches)
    debugEcho matches
    result = matches[0..count].map(make_absolute)


# Simple REPL for testing.
when isMainModule:
  let dirsBlacklist = ["backups", ".git", ".github", ".bzr", ".mypy_cache", ".venv", "elpy", "eln-cache", "auto-saves", "auto-save-list", "node_modules", "undo-history", "var", "url", "flycheck-pycheckers", "semanticdb", ".python-environments", ".cache", ".cask", "test", "terraform-mode", "nimcache"].toCritBitTree
  let extBlacklist = [".elc", ".texi", ".pyc"].toCritBitTree
  let extWeights = {".so": 0.25}.toTable
  let
    args = commandLineParams()
    path = if len(args) > 0: args[0] else: "."
    abs_path = path.expandTilde().expandFilename()

  discard initFFinder(FFConfig(searchRoot: abs_path,
                               dirsBlacklist: dirsBlacklist,
                               extBlacklist: extBlacklist,
                               extWeights: extWeights))
  var line = ""
  while true:
    stdout.write("> ")
    for res in stdin.readLine().search():
      echo res.res
