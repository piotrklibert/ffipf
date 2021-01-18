import sugar
import emacs_types
import emacs_module             # for `environ`, which is `ptr emacs_env`



proc get_string*(val: emacs_value): string {. inline .} =
  result = ""
  var str_length = 0
  if environ.copy_string_contents(environ, val, nil, addr str_length):
    if str_length == 0:
      return result
    var cstr = cast[cstring](alloc0(str_length))
    defer: dealloc(cstr)
    if environ.copy_string_contents(environ, val, cstr, addr str_length):
      result = $cstr


proc mk_string*(str: string): emacs_value =
  var cstr: cstring = str
  environ.make_string(environ, cstr, len(str))


proc mk_sym*(sym: string): emacs_value {. inline .} =
  environ.intern(environ, sym)


proc funcall*(fun: string,
              args: openArray[emacs_value]): emacs_value {. inline .} =
  let env = environ
  let Qfn = mk_sym(fun)
  let nargs = len(args)
  if nargs == 0:
    env.funcall(env, Qfn, nargs, nil)
  else:
    var args_copy = @args
    env.funcall(env, Qfn, nargs, addr args_copy[0])

proc funcall*(fun: string): emacs_value =
  funcall(fun, @[])


proc mk_num*(num: int): emacs_value =
  environ.make_integer(environ, num)

proc mk_num*(num: float): emacs_value =
  environ.make_float(environ, num)


proc get_type*(val: var emacs_value): string {. inline .} =
  var Qtype = environ.type_of(environ, val)
  get_string(funcall("symbol-name", [Qtype]))


proc format*(fmt: string, args: varargs[emacs_value]): string =
  get_string(funcall("format", @[mk_string(fmt)] & @args))

proc `$`*(val: emacs_value): string =
  format("%s", @[val])
