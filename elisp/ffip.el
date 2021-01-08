;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'cl-lib)


(defun ffip-test (&optional dir pat)
  (setq dir (or dir "/home/cji/.emacs.d/"))
  (setq pat (or pat "co/path"))
  (load "ffip.so")
  (cl-assert (and (functionp 'ffip-init)
                  (functionp 'ffip-search)))
  (message "Loaded paths: %s" (ffip-init dir))
  (let
      ((res (ffip-search pat)))
    (cl-loop for path in res do (princ (concat path "\n")))
    (message "Results: %d" (length res))))


(defvar ffip-initialized nil)


(defun ffip-search-1 (pat &rest args)
  (when pat
    (tst-search pat)))


(defun ffip-init-1 (root)
  (let ((expanded (f-expand root)))
    (setq ffip-initialized expanded)
    (ffip-init expanded)))


(defun ffip ()
  (require 'ivy)
  (require 'project)
  (let ((pr (project-root (project-current t))))
    (when (or (not ffip-initialized)
              (not (string= pr ffip-initialized)))
      (ffip-init-1 pr)))
  (ivy-read "> " 'ffip-search-1 :dynamic-collection t))


(provide 'ffip)
