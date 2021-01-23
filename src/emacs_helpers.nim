import sequtils
import emacs_types
import emacs_module             # for `environ`, which is `ptr emacs_env`


template intern*(s: string): emacs_value =
  environ.intern(environ, s)


template raiseNonLocal(): untyped =
  case environ.non_local_exit_check(environ):
    of emacs_funcall_exit_signal, emacs_funcall_exit_throw:
      raise newException(NonLocalExitException, "signal-or-throw")
    else:
      discard

template raiseNonLocal(body: untyped): untyped =
  block:
    let ret = body
    raiseNonLocal()
    ret


proc apply*(fun: string, args: openArray[emacs_value]): emacs_value  =
  let Qfn = intern(fun)
  let nargs = len(args)
  if nargs == 0:
    raiseNonLocal environ.funcall(environ, Qfn, nargs, nil)
  else:
    var args_copy = @args
    raiseNonLocal environ.funcall(environ, Qfn, nargs, addr args_copy[0])


proc funcall*(fun: string, args: varargs[emacs_value]): emacs_value  =
  apply(fun, args)

proc funcall*(fun: string): emacs_value =
  funcall(fun, @[])



proc get_string*(val: emacs_value): string {. inline .} =
  result = ""
  var str_length = 0
  if environ.copy_string_contents(environ, val, nil, addr str_length):
    raiseNonLocal()
    if str_length == 0:
      return result
    var cstr = cast[cstring](alloc0(str_length))
    defer: dealloc(cstr)
    if environ.copy_string_contents(environ, val, cstr, addr str_length):
      raiseNonLocal()
      result = $cstr


proc get_variable*(name: string): emacs_value =
  funcall("symbol-value", environ.intern(environ, name))


proc get_type*(val: var emacs_value): string {. inline .} =
  var Qtype = environ.type_of(environ, val)
  funcall("symbol-name", Qtype).get_string()



proc toEmacs*(str: string): emacs_value =
  var cstr: cstring = str
  raiseNonLocal environ.make_string(environ, cstr, len(str))

proc toEmacs*(num: int): emacs_value {. inline .} =
  raiseNonLocal environ.make_integer(environ, num)

proc toEmacs*(num: float): emacs_value {. inline .} =
  raiseNonLocal environ.make_float(environ, num)

proc toEmacs*[T](args: openArray[T]): seq[emacs_value] =
  args.map(toEmacs)



proc format*(fmt: string, args: varargs[emacs_value]): string {. inline .} =
  get_string(funcall("format", @[fmt.toEmacs()] & @args))

proc `$`*(val: emacs_value): string {. inline .} =
  format("%s", @[val])
