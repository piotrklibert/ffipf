import sequtils
import options
import tables
import critbits


import fuzzy_file_finder

import emacs_types
import emacs_module
import emacs_helpers


module_init("ffipf-backend")


defun(init, max_args=4):
  let
    dirsBlacklist = args[0].fromEmacs(seq[string]).get()
    extBlacklist = args[1].fromEmacs(seq[string]).get()
    extWeights = args[2].fromEmacs(seq[(string, float)]).get().toTable()
    config = FFConfig(dirsBlacklist: dirsBlacklist.toCritBitTree,
                      extBlacklist: extBlacklist.toCritBitTree,
                      extWeights: extWeights,
                      searchRoot: args[3].copy_string())
  let cnt = initFFinder(config)
  return cnt.toEmacs()


defun(search, max_args=1):
  let pattern = copy_string(args[0])
  let emacs_strings = search(pattern).mapIt(it.res).map(toEmacs)
  return apply("list", emacs_strings)


# defun(test_args, max_args=4):
#   let
#     dirsBlacklist = args[0].fromEmacs(seq[string]).get()
#     extBlacklist = args[1].fromEmacs(seq[string]).get()
#     extWeights = args[2].fromEmacs(seq[(string, float)]).get().toTable()
#     config = FFConfig(dirsBlacklist: dirsBlacklist.toCritBitTree,
#                       extBlacklist: extBlacklist.toCritBitTree,
#                       extWeights: extWeights,
#                       searchRoot: args[3].copy_string())
#   echo initFFinder(config)
#   return Qnil

provide()
