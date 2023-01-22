;;; no-load-path.el --- don't use load-path  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  wangchunye

;; Author: wangchunye <wcy123@gmail.com>
;; Keywords: extensions
;; Version: 0.0
;; Package-Version: 0.0
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; usage
;;
;;
;;; Code:
;;;;
;; NOTE: `no-load-path.el` is only needed at compile time. at runtime, it is not
;;loaded at all

(require 'bytecomp)
(defconst no-load-path--prelude
  `((defvar no-load-path-features-mapping
      (make-hash-table))
    (defvar no-load-path--profile t)
    (defun no-load-path-log (format-string &rest args)
      (when no-load-path--profile
        (let ((timestamp (format "[%5.2f] "
				 (* 1000 (float-time
					  (time-subtract
					   (current-time)
					   before-init-time))))))
          (message "%s" (concat timestamp (apply #'format format-string args))))))
    (defvar require-level 0)
    (defun my-require-advice (origin-require feature &optional filename noerror)
      (if (featurep feature)
          feature
        (let ((feature-file
               (gethash feature no-load-path-features-mapping filename))
              (indent (make-string (* require-level 8) ?\ )))
          (no-load-path-log "%s LOADING %s by %s at %s"  indent
                            feature load-file-name feature-file)
          (let* ((require-level (1+ require-level))
                 (start (current-time))
                 (ret (apply origin-require feature feature-file
                             noerror '()))
                 (end (current-time)))
            (no-load-path-log "%s LOADED %s in %5.2f ms"
                              indent
                              feature
                              (* 1000 (float-time
                                       (time-subtract end start))))
            ret))))
    ;; `personal-keybindings` is defined in `bind-key.el` along with
    ;; use-package.el. We actually is not '(require use-package)' at
    ;; runtime, so this variable is not loaded at runtime, we must defined
    ;; it here to suppress compilation warnings
    (defvar personal-keybindings '())
    (advice-add 'require :around #'my-require-advice))
  "the initialization source code at runtime.")
(mapc #'eval no-load-path--prelude)


;; this function hijack origin handler and cache the search path in
;; the compiled elisp code.
(defun my-byte-compile-file-form-require (form)
  (cl-destructuring-bind (_ feature &optional filename noerror) form
    (no-load-path-log "DEBUG COMPILE FOR REQUIRE: %S" form)
    (let* ((located-file (no-load-path--find-file (eval feature))))
      (byte-compile-file-form-require `(require ,feature
                                                ,(or located-file (eval filename))
                                                ,noerror)))))

(defconst no-load-path-system-path
  (expand-file-name ".." data-directory))

(defun no-load-path--normalize-path (file)
  (when file
    (cond
     ((string-match
       (concat "\\`" (regexp-quote no-load-path-system-path))
       file)
      file (file-name-base file))
     (:else
      (replace-regexp-in-string
             (concat "\\`" (regexp-quote (getenv "HOME")))
             "~" file)))))

(defun no-load-path--find-file (file)
  (let ((ret (locate-file
              (let ((file1 (format "%s" file)))
                (if (string= (file-name-extension file1) "el")
                    (file-name-sans-extension file1)
                  file1))
              load-path (get-load-suffixes) nil)))
    (no-load-path--normalize-path ret)))

(defun no-load-path-replace-autoload (autoload)
  (cond
   ((null autoload) autoload)
   ((and (listp autoload)
         (eq (car autoload) 'autoload)
         (>= (length autoload) 3))
    (let* ((file (format "%s" (nth 2 autoload)))
           (located-file (if (file-name-absolute-p file)
                             file
                           (no-load-path--find-file file))))
      (if located-file
          `(autoload ,(nth 1 autoload) ,located-file ,@(nthcdr 3 autoload))
        (warn "no-load-path: cannot locate file %S in path %S" file load-path))))
   ((consp autoload) (cons (no-load-path-replace-autoload (car autoload))
                           (no-load-path-replace-autoload (cdr autoload))))
   (t autoload)))

;;
(defun no-load-path-straigh.el-snippet()
  (no-load-path-log "boostraping straight.el")
  (defvar straight-vc-git-default-clone-depth)
  (defvar straight-vc-git-default-protocol)
  (defvar straight-use-symlinks)
  (setq straight-vc-git-default-clone-depth 1)
  (setq straight-use-symlinks nil)
  (setq use-package-verbose nil)
  (declare-function straight--build-file "")
  ;; bootstraping straight.el is relatively slow, ~200ms. so only do it at compile time
  (defvar bootstrap-version)
  ;; the following lines are copied from
  ;; https://github.com/raxod502/straight.el#conceptual-overview but
  ;; it is only loaded at compile time. Actually at runtime,
  ;; straight.el is not loaded at all.
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (no-load-path-log "straight.el is loaded")
  ;; Again, we only load use-package at compile time. And use-package
  ;; will be lazily loaded runtime because of `bind-key.el` is reqired
  ;; for key binding.
  ;; suppress compilation warning
  (straight-use-package
   '(use-package
        :type git :host github :repo "jwiegley/use-package"
        :fork (:host github
                     :repo "wcy123/use-package")))
  (require 'use-package))

;; dump all autoload objects discovered by straight.el then we don't
;; need straight.el at runtime.
(defun no-load-path-generate-auto-load (autoloads-cache)
  (let ((autoload-file (expand-file-name ".autoloads.el" user-emacs-directory)))
    (no-load-path-log "write to file %S" autoload-file)
    (with-temp-file autoload-file
      (mapc #'(lambda(c) (print c (current-buffer)))
            no-load-path--prelude)
      (maphash
       #'(lambda (package autoloads)
           (princ (format ";; autoloads for package %s\n" package) (current-buffer))
           (let ()
             (mapc #'(lambda (feature)
                       (let* ((package-feature-file (straight--build-file package (format "%s" feature)))
                              (feature-file (no-load-path--normalize-path (locate-file
                                                                           (file-name-base package-feature-file)
                                                                           (list (file-name-directory package-feature-file))
                                                                           '(".elc" ".el") nil))))
                         (princ (format ";; package %s provides %s\n" package feature) (current-buffer))
                         (princ (format "(puthash '%s %S no-load-path-features-mapping)\n"
                                        feature feature-file)
                                (current-buffer)))
                       )
                   (car autoloads))
             (mapc #'(lambda (autoload)
                       (print (no-load-path-replace-autoload autoload) (current-buffer)))
                   (cdr autoloads))))
       autoloads-cache))
    (no-load-path-log "compile %s" autoload-file)
    (byte-compile-file autoload-file nil)))
(defun plist-remove (plist key)
  (cond
   ((null plist) nil)
   ((null key) plist)
   ((eq (car plist) key) (cddr plist))
   (t `(,(car plist) ,(cadr plist) ,@(plist-remove (cddr plist) key)))))

(defmacro profile-form (name form)
  `(let ((t1 nil)
         (ret nil))
     (setq t1 (current-time))
     (setq ret ,form)
     (no-load-path-log "PROFILE %s takes %5.2f ms %5.2f acc" ,name
                       (* 1000.0 (float-time
                                  (time-subtract
                                   (current-time)
                                   t1)))
                       (* 1000.0 (float-time
                                  (time-subtract
                                   (current-time)
                                   before-init-time))))
     ret))
(defun no-load-path-use-package (origin name &rest args)
  `(progn
     ;; avoid compilation warning: `use-package-autoload-keymap` might
     ;; not be defined at runtime.
     (declare-function use-package-autoload-keymap nil)
     (profile-form ',name
                   ,(progn
                      ;; hacking straight-use-package integration with use-package.
                      ;; prevent straight.el from loading packages at runtime, but
                      ;; compile time instead
                      (straight-use-package (or (plist-get args :straight) name))
                      (let ((ret (apply origin name :no-require t
                                        (plist-remove
                                         (plist-remove args :straight)
                                         :ensure))))
                        (no-load-path-replace-autoload ret))))))


(defmacro no-load-path-init ()
  '(progn
     (declare-function no-load-path-log nil)
     (load (expand-file-name ".autoloads" user-emacs-directory) nil nil nil)))
(defmacro no-load-path-done ()
  `(progn
     (eval-when-compile
       (no-load-path-generate-auto-load straight--autoloads-cache))
     (setq no-load-path--profile (getenv "EMACS_PROFILE"))))

(defun no-load-path-initialize ()
  (no-load-path-log "NO-LOAD-PATH STARTED")
  (put 'require 'byte-hunk-handler 'my-byte-compile-file-form-require)
  (advice-add 'use-package :around #'no-load-path-use-package)
  (no-load-path-straigh.el-snippet)
  (no-load-path-log "NO-LOAD-PATH LOADED"))
(unless (featurep 'no-load-path)
  (no-load-path-initialize))
(provide 'no-load-path)
