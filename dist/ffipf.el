;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'cl-lib)
(require 'project)
;; NOTE: also depends on Ivy for `ffipf-jump-file'


(defvar ffipf-backend-loaded nil)

(defvar ffipf-dirs-blacklist
  (list "backups"
        ".git"
        ".github"
        ".bzr"
        ".mypy_cache"
        ".venv"
        "elpy"
        "eln-cache"
        "auto-saves"
        "auto-save-list"
        "node_modules"
        "undo-history"
        "var"
        "url"
        "flycheck-pycheckers"
        "semanticdb"
        ".python-environments"
        ".cache"
        ".cask"
        "test"
        "terraform-mode"
        "nimcache"))

(defvar ffipf-ext-weights
  '((".so" . 0.25)))

(defvar ffipf-ext-blacklist
  (list ".elc"
        ".eln"
        ".texi"
        ".pyc"
        "#"))

(defvar ffipf-initialized nil
  "Path to current project root or nil")


(defun ffipf-load-backend ()
  (when (not ffipf-backend-loaded)
    (load "ffipf_backend.so")
    (setq ffipf-backend-loaded t)))


(defun ffipf-init (root)
  (ffipf-load-backend)
  (let ((expanded (expand-file-name root)))
    (setq ffipf-initialized expanded)
    ;; (ffipf-backend-init expanded)
    (ffipf-backend-init ffipf-dirs-blacklist
                        ffipf-ext-blacklist
                        ffipf-ext-weights
                        expanded)))


(defun ffipf-search (pat &rest _args)
  (when pat
    (ffipf-backend-search pat)))


(defun ffipf-init-if-needed (project-path)
  (when (or (not ffipf-initialized)
            (not (string= project-path ffipf-initialized)))
    (ffipf-init project-path)))


(defun ffipf ()
  (interactive)
  (let ((project-path (expand-file-name (project-root (project-current t)))))
    (ffipf-init-if-needed project-path))
  (unless (require 'ivy nil t)
    (message "FFIFP: install Ivy to use!"))
  (when (fboundp 'ivy-read)
    (ivy-read "> " 'ffipf-search :dynamic-collection t)))


(defun ffipf-jump-file ()
  (interactive)
  (find-file (ffipf)))


(defun ffipf-test (&optional dir pat)
  ;; (setq dir (or dir (expand-file-name user-emacs-directory)))
  (setq dir (or dir "/home/cji/priv/ffipf/"))
  (setq pat (or pat "ba"))
  ;; (setq pat (or pat ".el"))
  (ffipf-load-backend)
  (cl-assert (and (functionp 'ffipf-backend-init)
                  (functionp 'ffipf-backend-search)))

  (message "Loaded paths: %s"
           (ffipf-backend-init ffipf-dirs-blacklist
                                    ffipf-ext-blacklist
                                    ffipf-ext-weights
                                    dir))
  (let ((res (ffipf-backend-search pat)))
    (cl-loop for path in res
             do (princ (concat path "\n")))
    (message "Results: %d" (length res))))


(provide 'ffipf)
