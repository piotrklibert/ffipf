import macros
import strutils

import emacs_types
import emacs_consts



var module_data* {. compileTime .} = EmacsModuleData()

var envlvl* = 0
var environ*: ptr emacs_env


macro module_init*(mod_name: static[string]) =
  module_data.name = mod_name
  quote do:
    {. emit: "int plugin_is_GPL_compatible;" .}



macro defun*(fsym: untyped, max_args: untyped, body: untyped) =
  let
    NonLocalExitException = newIdentNode("NonLocalExitException")
    env = newIdentNode("env")
    nargs = newIdentNode("nargs")
    args = newIdentNode("args")
    data = newIdentNode("data")
    envlvl = newIdentNode("envlvl")
    fn_name = fsym.strVal
    emacs_func = (module_data.name & "-" & fn_name).replace("_", "-")
    extern_func = "nimEmacs_" & module_data.name.replace("-", "_") & "_" & fn_name
    max_args_str = $max_args.intVal
    env_global = newIdentNode("environ")

  module_data.defuns &= format("""DEFUN ("$1", $2, $3, $4, NULL, NULL);""",
                               emacs_func, extern_func, max_args_str, max_args_str)
  let
    proc_def = quote do:
      proc `fsym`*(`env`: ptr emacs_env,
                   `nargs`: ptrdiff_t,
                   `args`: ptr array[0..`max_args`, emacs_value],
                   `data`: pointer): emacs_value
                    {. exportc, extern: `extern_func` .} =
          block:
            `env_global` = `env`
            `envlvl` += 1
          defer:
            `envlvl` -= 1
            if `envlvl` == 0:
              `env_global` = nil
          try:
            `body`
          except `NonLocalExitException`:
            return nil
          except CatchableError:
            # TODO: convert to Elisp signal
            return nil
  proc_def


macro provide*() =
  let tmp = format_module_init_function(module_data.defuns, module_data.name)
  quote do:
    {. emit: `tmp` .}
