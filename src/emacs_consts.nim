import strutils

const provide_str* = """
/* Provide FEATURE to Emacs.  */
static void
provide(emacs_env *env, const char *feature) {
  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

"""


const bind_function_str* = """
/* Bind NAME to FUN.  */
static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);
  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qfset, 2, args);
}

"""

const module_init_tmpl* = """
/* Module init function.  */
int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment(ert);
  NimMain(); // execute module-global statements

#define DEFUN(lsym, csym, amin, amax, doc, data) \
  bind_function(env, lsym, env->make_function(env, amin, amax, csym, doc, data))

$1

#undef DEFUN

  provide (env, "$2");
  return 0;
}
"""

proc format_module_init_function*(defuns: string, mod_name: string): string  =
  let x = module_init_tmpl.format(defuns, mod_name)
  return provide_str & bind_function_str & x
