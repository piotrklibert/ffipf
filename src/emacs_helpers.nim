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

template Qnil*(): untyped = intern("nil")


proc emNotNil*(val: emacs_value): bool =
  environ.is_not_nil(environ, val)

proc emIsNil*(val: emacs_value): bool =
  not emNotNil(val)

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



proc copy_string*(val: emacs_value): string {. inline .} =
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
  funcall("symbol-value", intern(name))


proc get_type*(val: emacs_value): string {. inline .} =
  var Qtype = environ.type_of(environ, val)
  funcall("symbol-name", Qtype).copy_string()



proc toEmacs*(str: string): emacs_value =
  var cstr: cstring = str
  raiseNonLocal environ.make_string(environ, cstr, len(str))

proc toEmacs*(num: int): emacs_value {. inline .} =
  raiseNonLocal environ.make_integer(environ, num)

proc toEmacs*(num: float): emacs_value {. inline .} =
  raiseNonLocal environ.make_float(environ, num)

proc toEmacs*[T](args: openArray[T]): seq[emacs_value] =
  args.map(toEmacs)


import options

proc car*(val: emacs_value): emacs_value = funcall("car", val)
proc cdr*(val: emacs_value): emacs_value = funcall("cdr", val)


proc fromEmacs*(val: emacs_value, T: typedesc[bool]): Option[bool] =
  return some(environ.eq(environ, funcall("not", val), Qnil))

proc fromEmacs*(val: emacs_value, T: typedesc[float]): Option[T] =
  let typ = get_type(val)
  if not (typ == "float" or typ == "integer"):
    return none[float]()
  if typ == "float":
    return some( environ.extract_float(environ, val) )
  elif typ == "integer":
    return some( float(environ.extract_integer(environ, val)) )

proc fromEmacs*(val: emacs_value, T: typedesc[int]): Option[T] =
  let typ = get_type(val)
  if not (typ == "float" or typ == "integer"):
    return none[int]()
  if typ == "float":
    return some( int(environ.extract_float(environ, val)) )
  elif typ == "integer":
    return some( int(environ.extract_integer(environ, val)) )

proc length*(val: emacs_value): int =
  funcall("length", val).fromEmacs(int).get()

proc listp*(val: emacs_value): bool =
  get_type(val) == "cons" and get_type(cdr(val)) == "cons"


proc fromEmacs*[U](val: emacs_value, T: typedesc[seq[(string, U)]]): Option[T] =
  ## Convert association list (alist), eg. '(("a" . 4) ("b" . 6))
  if not environ.is_not_nil(environ, val):
    return none[T]()
  if get_type(val) != "cons":
    return none[T]()
  var
    s: seq[(string, U)]
    lst = val
  while true:
    let v = car(lst)
    # skip non-pairs
    if get_type(v) == "cons" and get_type(cdr(v)) != "cons":
      s.add( (car(v).copy_string(), cdr(v).fromEmacs(U).get()) )
    lst = cdr(lst)
    if emIsNil(lst): break
  return some(s)


proc fromEmacs*(val: emacs_value, T: typedesc[seq[string]]): Option[T] =
  if not environ.is_not_nil(environ, val) or not listp(val):
    return none[T]()
  var
    s: seq[string]
    lst = val
  while true:
    s.add( car(lst).copy_string() )
    lst = cdr(lst)
    if emIsNil(lst): break
  return some(s)


proc format*(fmt: string, args: varargs[emacs_value]): string {. inline .} =
  copy_string(funcall("format", @[fmt.toEmacs()] & @args))

proc `$`*(val: emacs_value): string {. inline .} =
  format("%s", @[val])
