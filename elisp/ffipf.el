;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'cl-lib)


(defun ffipf-test (&optional dir pat)
  (setq dir (or dir "/home/cji/.emacs.d/"))
  ;; (setq dir (or dir "/home/cji/priv/ffip/"))
  (setq pat (or pat ".c"))
  (load "ffipf_backend.so")
  (cl-assert (and (functionp 'ffipf-backend-init)
                  (functionp 'ffipf-backend-search)))
  (message "Loaded paths: %s" (ffipf-backend-init dir))
  (let
      ((res (ffipf-backend-search pat)))
    (cl-loop for path in res do (princ (concat path "\n")))
    (message "Results: %d" (length res))))


(defvar ffipf-initialized nil)


(defun ffipf-search (pat &rest _args)
  (when pat
    (ffipf-backend-search pat)))


(defun ffipf-init (root)
  (let ((expanded (f-expand root)))
    (setq ffipf-initialized expanded)
    (ffipf-backend-init expanded)))


(defun ffipf ()
  (interactive)
  (require 'ivy)
  (require 'project)
  (let ((pr (f-expand (project-root (project-current t)))))
    (when (or (not ffipf-initialized)
              (not (string= pr ffipf-initialized)))
      (ffipf-init pr)))
  (find-file (ivy-read "> " 'ffipf-search :dynamic-collection t)))


(provide 'ffipf)
