;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'cl-lib)
(require 'project)

;; NOTE: also depends on Ivy for `ffipf-jump-file'


(defvar ffipf-backend-loaded nil)

(defun ffipf-load-backend ()
  (when (not ffipf-backend-loaded)
    (load "ffipf_backend.so")
    (setq ffipf-backend-loaded t)))


(defvar ffipf-initialized nil
  "Path to current project root or nil")

(defun ffipf-init (root)
  (ffipf-load-backend)
  (let ((expanded (expand-file-name root)))
    (setq ffipf-initialized expanded)
    (ffipf-backend-init expanded)))


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
  (setq dir (or dir (expand-file-name user-emacs-directory)))
  (setq pat (or pat ".el"))
  (ffipf-load-backend)
  (cl-assert (and (functionp 'ffipf-backend-init)
                  (functionp 'ffipf-backend-search)))
  (message "Loaded paths: %s" (ffipf-backend-init dir))
  (let
      ((res (ffipf-backend-search pat)))
    (cl-loop for path in res
             do (princ (concat path "\n")))
    (message "Results: %d" (length res))))


(provide 'ffipf)
