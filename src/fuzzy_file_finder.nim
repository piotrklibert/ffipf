import os
import sequtils
import strutils
import critbits
import sugar
import algorithm
import tables
import re

const MAX_RESULTS = 20

let dirsBlacklist = [
  "backups", "build", ".git", ".github", ".bzr", ".mypy_cache", ".venv", "elpy",
  "eln-cache", "auto-saves", "auto-save-list", "node_modules", "undo-history",
  "var", "url", "flycheck-pycheckers", "semanticdb", ".python-environments",
  ".cache", ".cask", "test", "terraform-mode",
].toCritBitTree

let extBlacklist = [".elc", ".texi", ".pyc"].toCritBitTree


var paths: seq[string] = newSeqOfCap[string](20200)


# Traverses the `dir` directory recursively and stores all found files in a
# global variable `paths`. Doesn't go into `dirsBlacklist` directories. Ignores
# files with `extBlacklist` extensions.
# TODO: make blacklists configurable when called.
proc init_paths*(ignored_part, dir: string): int =
  for (kind, path) in walkDir(dir):
    if kind == pcFile:
      let (_, _, ext) = splitFile(path)
      if not extBlacklist.contains(ext):
        paths.add(path.replace(ignored_part, ""))
    elif kind == pcDir:
      # echo lastPathPart(path)
      # echo lastPathPart(path) in dirsBlacklist
      if not dirsBlacklist.contains(lastPathPart(path).replace("/","")):
        discard init_paths(ignored_part, path)
  return len(paths)

proc reset_paths*() =
  if len(paths) > 0:
    paths = newSeqOfCap[string](20200)


type
  MatchResult* = object of RootObj
    score*: float
    res*: string
    missed: bool
  MatchCache = Table[string, MatchResult]
  Matcher = object of RootObj
    re: Regex
    parts: seq[string]
    ngroups: int
    empty: bool



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
      empty: len(path_parts) == 0,
      parts: path_parts,
      ngroups: path_regex_raw.toSeq().countIt(it == '(')
    )
    file_matcher = Matcher(
      re: file_regex_raw.re,
      parts: @[file_name_part],
      ngroups: file_regex_raw.toSeq().countIt(it == '(')
    )

  return (path_matcher, file_matcher)


type
  CharGroup = object
    str: string
    inside: bool


# Computes match score for a given sequence of regex captures. Takes a number of
# path parts in the original patterns as the second argument. The score is
# computed based on the ratio of characters captured as part of the original
# pattern to characters outside of such captures, and a ratio of continuous
# captured character groups to the number of path parts in the filename.
proc build_match_result(captures: seq[string], parts: int): MatchResult =
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


# Matches path portion of the filename, uses `path_matches` as a cache to store
# match results of paths already seen.
proc match_path(path: string, path_matches: var MatchCache,
                matcher: Matcher): MatchResult =
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


# Matches file name portion of the path. Uses match result from `match_path` to
# compute the final result and score.
proc match_file(file: string, matcher: Matcher,
                path_res: MatchResult): MatchResult =
  var groups = newSeq[string](matcher.ngroups)
  if file.match(matcher.re, groups):
    var res = build_match_result(groups, 1)
    MatchResult(
      score: res.score * path_res.score,
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
      if path_match.missed:
        continue
      let match = match_file(file & ext, file_matcher, path_match)
      if not match.missed:
        match


# Perform a search for files matching `pattern`. Returns at most
# `MAX_RESULTS - 1` results, but can return less results or none at all.
proc search*(pattern: string): seq[MatchResult] =
  let
    results = search_paths(pattern).sortedByIt(-it.score)
    hi = if high(results) > MAX_RESULTS: MAX_RESULTS else: high(results)
  results[0..<hi]


# Simple REPL for testing.
when isMainModule:
  discard init_paths("/home/cji/", "/home/cji/.emacs.d/")
  var line = ""
  while true:
    stdout.write("> ")
    for res in stdin.readLine().search():
      echo res.res
