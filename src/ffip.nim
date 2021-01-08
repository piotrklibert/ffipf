import sequtils
import fuzzy_file_finder

import emacs_types
import emacs_module


init(emacs)
{. emit: "int plugin_is_GPL_compatible;" .}


proc get_string(env: ptr emacs_env, val: var emacs_value): string =
  result = ""
  var str_length = 0
  if env.copy_string_contents(env, val, nil, addr str_length):
    var cstr = cast[cstring](alloc0(str_length))
    defer:
      dealloc(cstr)
    if env.copy_string_contents(env, val, cstr, addr str_length):
      result = $cstr


proc mk_string(env: ptr emacs_env, str: string): emacs_value =
  var cstr: cstring = str
  env.make_string(env, cstr, len(str))


proc mk_call(env: ptr emacs_env, fun: string, args: var openArray[emacs_value]): emacs_value =
  let Qfn = env.intern(env, fun)
  let nargs = len(args)
  if nargs == 0:
    env.funcall(env, Qfn, nargs, nil)
  else:
    env.funcall(env, Qfn, nargs, addr args[0])


proc mk_num(env: ptr emacs_env, n: int): emacs_value =
  env.make_integer(env, n)


emacs.defun(init, 1):
  let root = get_string(env, args[0])
  reset_paths()
  let paths_count = init_paths("/home/cji/", root)
  return env.mk_num(paths_count)


emacs.defun(search, 1):
  let pat = get_string(env, args[0])
  var re = search(pat).mapIt(env.mk_string(it.res))
  return env.mk_call("list", re)


provide(emacs)
