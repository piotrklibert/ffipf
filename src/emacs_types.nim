# Copyright (C) 2015 by Yuta Yamada
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

type
  intmax_t* {.importc: "intmax_t", header: "<inttypes.h>".} = clonglong
  ptrdiff_t* {.importc: "ptrdiff_t", header: "<stddef.h>".} = int


type
  emacs_runtime_private {.importc: "struct emacs_runtime_private",
                          header: "<emacs-module.h>".} = object

  emacs_env_private {.importc: "struct emacs_env_private",
                      header: "<emacs-module.h>".} = object


type
  ## Function prototype for the module init function.
  emacs_init_function* = proc (ert: ptr emacs_runtime): cint {.cdecl.}

  ## Function prototype for the module Lisp functions.

  emacs_runtime* {.importc: "struct emacs_runtime",
                   header: "<emacs-module.h>".} = object ## \
    ## Struct passed to a module init function (emacs_module_init).
    size: ptrdiff_t ## Structure size (for version checking).
    private_members: ptr emacs_runtime_private ## \
      ## Private data; users should not touch this.
    get_environment: proc(ert: ptr emacs_runtime): ptr emacs_env {.cdecl.}

  emacs_value* {.importc: "emacs_value", header: "<emacs-module.h>".} = pointer

  # TODO: leave only one of those, as they're identical
  emacs_subr* = proc (env: ptr emacs_env, nargs: ptrdiff_t, args: ptr emacs_value, data: pointer): emacs_value {.cdecl.}
  defun_proc* = proc (env: ptr emacs_env; nargs: ptrdiff_t; args: ptr emacs_value; data: pointer): emacs_value {.cdecl.}

  finalizer_proc* = proc (data: pointer) {.cdecl.}

  emacs_funcall_exit* = enum
    emacs_funcall_exit_return = 0, # Function has returned normally.
    emacs_funcall_exit_signal = 1, # Function has signaled an error using `signal'.
    emacs_funcall_exit_throw  = 2  # Function has exit using `throw'.

  emacs_env* {.importc: "emacs_env", header: "<emacs-module.h>".} = object
    size*: ptrdiff_t

    private_members: ptr emacs_env_private

    make_global_ref*:
      proc (env: ptr emacs_env; any_reference: emacs_value): emacs_value {.cdecl.}
    free_global_ref*:
      proc (env: ptr emacs_env; global_reference: emacs_value) {.cdecl.}


    non_local_exit_check*:
      proc (env: ptr emacs_env): emacs_funcall_exit {.cdecl.}
    non_local_exit_clear*:
      proc (env: ptr emacs_env) {.cdecl.}
    non_local_exit_get*:
      proc (env: ptr emacs_env;
            non_local_exit_symbol_out: ptr emacs_value;
            non_local_exit_data_out: ptr emacs_value): emacs_funcall_exit {.cdecl.}
    non_local_exit_signal*:
      proc (env: ptr emacs_env;
            non_local_exit_symbol: emacs_value;
            non_local_exit_data: emacs_value) {.cdecl.}
    non_local_exit_throw*:
      proc (env: ptr emacs_env; tag: emacs_value; value: emacs_value) {.cdecl.}


    # Defuns
    make_function*:
      proc (env: ptr emacs_env, min_arity: ptrdiff_t; max_arity: ptrdiff_t;
            function: defun_proc, documentation: cstring;
            data: pointer): emacs_value {.cdecl.}
    funcall*:
      proc (env: ptr emacs_env; function: emacs_value; nargs: ptrdiff_t;
            args: ptr emacs_value): emacs_value {.cdecl.}
    intern*:
      proc (env: ptr emacs_env; symbol_name: cstring): emacs_value {.cdecl.}


    type_of*:
      proc (env: ptr emacs_env; value: emacs_value): emacs_value {.cdecl.}
    is_not_nil*:
      proc (env: ptr emacs_env; value: emacs_value): bool {.cdecl.}
    eq*:
      proc (env: ptr emacs_env; a: emacs_value; b: emacs_value): bool {.cdecl.}


    extract_integer*:
      proc (env: ptr emacs_env; value: emacs_value): intmax_t {.cdecl.}
    make_integer*:
      proc (env: ptr emacs_env; value: intmax_t): emacs_value {.cdecl.}

    extract_float*:
      proc (env: ptr emacs_env; value: emacs_value): cdouble {.cdecl.}
    make_float*:
      proc (env: ptr emacs_env; value: cdouble): emacs_value {.cdecl.}


    # String manipulation
    copy_string_contents*:
      proc (env: ptr emacs_env; value: emacs_value; buffer: cstring;
            size_inout: ptr ptrdiff_t): bool {.cdecl.}
    # Create a Lisp string from a utf8 encoded string.
    make_string*:
      proc (env: ptr emacs_env; contents: cstring;
            length: ptrdiff_t): emacs_value {.cdecl.}


    # Embedded pointer type.
    make_user_ptr*:
      proc (env: ptr emacs_env; fin: finalizer_proc, data: pointer): emacs_value {.cdecl.}
    get_user_ptr*:
      proc (env: ptr emacs_env; uptr: emacs_value): pointer {.cdecl.}
    set_user_ptr*:
      proc (env: ptr emacs_env; uptr: emacs_value; data: pointer) {.cdecl.}
    set_user_finalizer*:
      proc (env: ptr emacs_env; uptr: emacs_value; fin: finalizer_proc) {.cdecl.}


    # Vectors
    vec_get*:
      proc (env: ptr emacs_env; vec: emacs_value; i: ptrdiff_t): emacs_value {.cdecl.}
    vec_set*:
      proc (env: ptr emacs_env; vec: emacs_value; i: ptrdiff_t; val: emacs_value) {.cdecl.}
    vec_size*:
      proc (env: ptr emacs_env; vec: emacs_value): ptrdiff_t {.cdecl.}


type EmacsModuleData* = object
  defuns*: string
  name*: string

type NonLocalExitException* = object of Exception
