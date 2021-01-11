# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest
import os
import sequtils
import strutils

import fuzzy_file_finder


proc getAbsCwd(): string = ".".expandFilename()


test "initialize works":
  let path = getAbsCwd()
  check path.lastPathPart == "ffipf"
  let count = init_paths("", path)
  check count > 0


test "search works":
  let path = getAbsCwd()
  check path.lastPathPart == "ffipf"
  let count = init_paths("", path)
  check count > 0

  let res = search("")
  check len(res) > 0
  check res[0] is MatchResult
  check len(res) <= MAX_RESULTS

  let fnames1 = search(".nim").mapIt(it.res)
  check fnames1.countIt(it.endsWith(".nim")) > 4

  let fnames2 = search(".md").mapIt(it.res)
  check fnames2.countIt(it.endsWith(".md")) >= 1

  let fnames3 = search(".el").mapIt(it.res)
  check fnames3.countIt(it.endsWith(".el")) >= 1

  let fnames4 = search("READ").mapIt(it.res)
  check len(fnames4) >= 1
  check fnames4[0].lastPathPart == "README.md"
