(message "PROFILE START INIT.EL %5.2f ms acc" (* 1000.0 (float-time
                                                 (time-subtract
                     (current-time)
                     before-init-time))))
;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)
(eval-and-compile
  ;; many package use common macros like if-let* in substitute
  (require 'subr-x)) ;; (string-strim) etc
(eval-when-compile
  ;; no-load-path.el must be found in `load-path`. Fortunately this is
  ;; only needed at compile time.
  (require 'no-load-path))
(profile-form
 'no-load-path-init
 (no-load-path-init))

(profile-form
 'add-hook
 (progn
   (setq package-enable-at-startup nil)
   (setq inhibit-default-init t)
   ;; (setq initial-buffer-choice nil)
   (setq inhibit-startup-echo-area-message (user-login-name))
   (setq inhibit-startup-screen t)
   (setq initial-major-mode 'text-mode)
   (add-hook 'after-init-hook
             #'(lambda ()
                 (setq initial-scratch-message
                       (format "%5.2f ms for emacs to startup "
                               (* 1000.0 (float-time
                                          (time-subtract
                                           after-init-time
                                           before-init-time)))))))))

;; == basic configurations
(eval-after-load 'simple
  '(progn
    (add-hook 'before-save-hook 'delete-trailing-whitespace t nil)
    (set-default 'tab-width 4)
    (set-default 'indent-tabs-mode nil)
    (add-to-list 'minor-mode-alist '(mark-active " Mark"))
    (pending-delete-mode 1)
    (defalias 'yes-or-no-p #'y-or-n-p)))

(eval-when-compile
  (when (locate-library "xref")
    (use-package xref
      :straight (xref :type built-in))))

(use-package xt-mouse
  :straight (xt-mouse :type built-in)
  :when (not (display-graphic-p))
  :defer 2
  :config (xterm-mouse-mode 1)
  (require 'mwheel))

(setq
 ;; I prefer split horizontally.
 split-width-threshold 160
 ;; keyboard scroll one line at a time
 scroll-step 1)
(menu-bar-mode -1)
;; scroll one line at a time (less "jumpy" than defaults)
(use-package mwheel
  :straight (mwheel :type built-in)
  :functions (mouse-wheel-mode)
  :defer t
  :defines (mouse-wheel-scroll-amount mouse-wheel-progressive-speed  mouse-wheel-follow-mouse)
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (mouse-wheel-mode 1)
  )
(use-package mouse
  :straight (mouse :type built-in)
  :defer t
  :commands (mouse-yank-at-click)
  :defines (mouse-drag-copy-region)
  :config
  (when (not (display-graphic-p))
    (global-set-key (kbd "<mouse-2>") 'mouse-yank-at-click))
  (setq mouse-drag-copy-region t))

;; == savehist
(use-package savehist
  :straight (savehist :type built-in)
  ;; it is so important, so we pay 2-3ms to  initialize it, it worth.
  ;; :defer 2
  :config
  (savehist-mode 1))

;; == exec-path-from-shell
(use-package exec-path-from-shell
  :when (or (memq window-system '(mac ns x))
            (memq system-type '(darwin)))
  :disabled t
  :after (simple)
  :functions (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-initialize))
;; == eshell
(use-package eshell
  :straight (eshell :type built-in)
  :defines (eshell-prompt-regexp eshell-directory-name
                                 eshell-prompt-function)
  :bind (("M-1" . eshell)
         ("<f1>" . eshell))
  :config
  (progn
    (defvar zlua-env_LUAEXE
      (or (getenv "ZLUA_LUAEXE") "/usr/bin/lua"))
    (defvar zlua-env_SCRIPT
      (or (getenv "ZLUA_SCRIPT") "/usr/local/z.lua/z.lua"))
    (defun maybe-zlua (pwd)
      (when (and (file-readable-p zlua-env_LUAEXE)
                 (file-readable-p zlua-env_SCRIPT))
        (call-process zlua-env_LUAEXE nil nil nil zlua-env_SCRIPT "--add" pwd)))
    (defun with-face (str &rest face-plist)
      (propertize str 'face face-plist))

    (setq eshell-prompt-regexp "^[^#$\n]*[#$] "
          eshell-directory-name (if (directory-name-p
                                     "/home.on.host/eshell")
                                    "/home.on.host/eshell"
                                  "~/eshell")
          eshell-prompt-function
          (lambda nil
            (let ((pwd (eshell/pwd))
                  (header-bg "#fff"))
              (maybe-zlua pwd)
              (concat
               (with-face (concat pwd " ") :background header-bg)
               (with-face (format-time-string "(%Y-%m-%d %H:%M) "
                                              (current-time)) :background header-bg :foreground "#888")
               (with-face
                (or (ignore-errors
                      (let ((backend (vc-responsible-backend
                                      default-directory)))
                        (concat
                         (format "(%s)" backend)
                         (or (if (string= "Git" backend)
                                 (concat
                                  " \u16D8 "
                                  (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
                             ""))))
                    "")
                :background header-bg)
               (with-face "\n" :background header-bg)
	           (if (string= pwd (getenv "HOME"))
	               "~" (eshell/basename pwd))
               " "
               (user-login-name)
               "@[-]"
	           (if (= (user-uid) 0) "# " "$ ")))))

    (defun eshell/zh (&rest args)
      (if (not (and (file-readable-p zlua-env_LUAEXE)
                    (file-readable-p zlua-env_SCRIPT)))
          (error "cannot find zlua %s %s" zlua-env_LUAEXE zlua-env_SCRIPT)
        (let* ((cmd (apply 'concat zlua-env_LUAEXE  " " zlua-env_SCRIPT
                           " -t -l " args))
               (output (shell-command-to-string cmd))
               (list (split-string output "[\n]" t nil)))
          (if (eq list nil)
              (error "no match to %s" args)
            (let ((dirs (seq-reduce #'(lambda (r a)
                                        (cons (replace-regexp-in-string "^-?[0-9]+ +" "" a) r))
                                    list nil)))
              (my/eshell-z dirs))))))
    (defun my/eshell-z (dirs)
      (if (eq (cdr dirs) nil)
          ;; single match
          (eshell/cd (car dirs))
        (let ((dir (ivy-read "cd to dir: " dirs)))
          (eshell/cd dir))))

    (defun eshell/zb (&rest args)
      (let* ((list (split-string output "[\n]" t nil)))
        (if (eq list nil)
            (error "no match to %s" args)
          (let ((dirs (cl-remove-duplicates
                       (remove nil
                               (mapcar
                                #'(lambda (buffer)
                                    (with-current-buffer buffer
                                      default-directory))
                                (buffer-list)))
                       :test #'string=)))
            (my/eshell-z dirs)))))))

;; == configuration for win32
(when (eq window-system 'w32)
  (defvar putty-directory)
  (defvar tramp-default-method)
  (setq putty-directory "C:/Program Files/PuTTY/")
  (setq tramp-default-method "plink")
  (when (and (not (string-match putty-directory (getenv "PATH")))
	     (file-directory-p putty-directory))
    (setenv "PATH" (concat putty-directory ";" (getenv "PATH")))
    (add-to-list 'exec-path putty-directory)))

;; == diminish
(use-package diminish)

;; == substitute
;; (use-package substitute
;;   :straight
;;   (substitute :type git
;;               :host github
;; 		      :branch "fix-compilation-warning"
;;               :repo "wcy123/substitute")
;;   :bind (
;;          ("M-# s" . substitute-target-below-point)
;;          ("M-# r" . substitute-target-above-point)
;;          ("M-# d" . substitute-target-in-defun)
;;          ("M-# b" . substitute-target-in-buffer)
;;          )
;;   :functions (substitute-report-operation)
;;   :hook (substitute-post-replace . substitute-report-operation)
;;   :config
;;   (setq substitute-highlight t))

;;
(use-package expand-region
  :commands (er/expand-region)
  :bind (("M-=" . er/expand-region)))

;; == ivy mode
(use-package ivy
  :defer 2
  :diminish ivy-mode
  :defines (ivy-use-virtual-buffers
            ivy-initial-inputs-alist
            ivy-display-style ivy-count-format
            ivy-minibuffer-map)
  :functions (ivy-mode)
  :hook (after-init-idle .  ivy-mode)
  :bind (:map ivy-minibuffer-map
              (("C-w" . ivy-yank-word)))
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-display-style 'fancy)
  (setq ivy-count-format "%d/%d ")
  )

(use-package ivy-hydra)
(use-package counsel
  :defer t
  :defines (ivy-minibuffer-map)
  :bind (("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line)))

(use-package swiper
  :bind (;; ("C-s" . swiper-isearch)
         ;; ("C-r" . swiper-isearch)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

;;== dump jump, jump to definition
(use-package dumb-jump
  :defines (dumb-jump-selector)
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g l" . dumb-jump-quick-look)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy)
  ;; (setq dumb-jump-selector 'helm)
  :init
  ;; (dumb-jump-mode)
  )


;; == avy
(use-package avy
  :disabled nil
  :after (isearch-mode-map)
  :bind (:map isearch-mode-map
              (("C-l" . avy-isearch))))
;; === rg
(use-package rg
  :commands (rg)
  :bind (("C-c s"  . rg)))

;; == projectile
(use-package projectile
  :defines (projectile-completion-system projectile-command-map)
  :functions (projectile-discover-projects-in-directory)
  :bind-keymap ("C-x p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy))
(use-package ag)

;; == magit
(use-package magit
  :bind ("C-x g" . 'magit-status))
;; == compile
(use-package compile
  :straight (compile :type built-in)
  :bind (("M-7" . compile)
         ("<f7>" . compile))
  :defines (compilation-scroll-output compilation-read-command)
  :config
       (setq compilation-scroll-output t
             compilation-read-command nil
             compilation-ask-about-save nil))

;; == ffap
(use-package ffap
  :defines (ffap-c-path)
  :config
  (if (getenv "INCLUDE")
      (setq ffap-c-path
            (append (split-string (getenv "INCLUDE") ":" nil)
                    ffap-c-path))))

;; == company-mode
(defvar company-backends)
(use-package company
  :after (prog-mode)
  :diminish (company-mode . "C")
  :defines (
            company-active-map
            company-idle-delay
            company-minimum-prefix-length
            company-show-numbers
            company-tooltip-limit
            company-dabbrev-downcase)
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              (("C-n" . company-select-next)
               ("C-p" . company-select-previous)))
  :config
  (setq company-idle-delay              nil)
  (setq company-minimum-prefix-length   2)
  (setq company-show-numbers            t)
  (setq company-tooltip-limit           20)
  (setq company-dabbrev-downcase        nil)
  :bind (:map prog-mode-map
              ("C-r" . company-complete)))
(use-package company-irony
  :disabled t
  :after company)
(use-package company-c-headers
  :after company
  :defines (company-backends)
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; (use-package eglot
;;   :defines (eglot-mode-map eglot-server-programs)
;;   :hook (((c-mode c++-mode) . eglot-ensure))
;;   :bind (:map eglot-mode-map
;;               ("C-c h" . eglot-help-at-point)
;;               ("C-c f r" . xref-find-references)
;;               ("C-c f d" . eglot-find-declaration ;; xref-find-definitions
;;                )
;;               ("C-c f D" . xref-find-definitions-other-window)
;;               ("C-c f t" . eglot-find-typeDefinition)
;;               ("C-c f i" . eglot-find-implementation)
;;               ("C-c =" . eglot-format-buffer)
;;               ("C-c c" . eglot-completion-at-point)
;;               ("C-c r" . eglot-rename)
;;               ("C-c a" . eglot-code-actions))
;;   :config
;;   (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;;   )
(use-package lsp-mode
  :defines (lsp-keymap-prefix lsp-diagnostic-package lsp-diagnostics-provider)
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "M-l"
              ;; https://github.com/emacs-lsp/lsp-mode/issues/1413
              ;; lsp-diagnostic-package :none
              lsp-diagnostics-provider :none)
  :hook (((rust-mode c-mode c++-mode)
          . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)))
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp
  :after (company)
  :commands company-lsp
  :config
  ;; no need to do it, lsp already did it
  ;; (push 'company-lsp company-backends)
  )
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode)
(use-package which-key
  :defer 1
  :config
  (which-key-mode))

(use-package hippie-exp
  :straight (hippie-exp :type built-in)
  :bind ("M-?" . hippie-expand))

;; == skeleton-snippet
(use-package skeleton-snippet
  :straight
  (skeleton-snippet :type git
                    :host github
                    :repo "wcy123/skeleton-snippet")
  :bind (("C-]" . skeleton-snippet))
  )
;; == shared-clipboard
(use-package shared-clipboard
  :straight
  (shared-clipboard :type git
		    :branch "main"
                    :host github
                    :repo "wcy123/shared-clipboard")
  :defer 2
  :init
  ;; (shared-clipboard-enable)
  )
(use-package skeleton-snippet-store
  :straight
  (skeleton-snippet-store :type git
                          :host github
                          :files ("*.el" "snippets")
                          :repo "wcy123/skeleton-snippet-store")
  :after (skeleton-snippet)
  :config
  (skeleton-snippet-store-initialize))

;; (use-package yasnippet
;;   :after (prog-mode)
;;   :defines (yas-minor-mode-map yas-maybe-expand)
;;   :hook (prog-mode . yas-minor-mode)
;;   :hook (minibuffer-setup . yas-minor-mode)
;;   :bind (:map yas-minor-mode-map
;;               ("<tab>" . nil)
;;               ("TAB" . nil))
;;   :config
;;   (define-key yas-minor-mode-map (kbd "M-?") yas-maybe-expand)
;;   (eval-after-load 'hippie-exp
;;     '(progn
;;       (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))))

;; (use-package yasnippet-classic-snippets
;;   ;; it takes ~300ms to load snippets
;;   )
;; (use-package yasnippet-snippets
;;   ;; it takes even longger
;;   :straight (yasnippet-snippets
;;              :type git
;;              :host github
;;              :files ("*.el" "snippets")
;;              :repo "AndreaCrotti/yasnippet-snippets"))
;; (use-package wcy123-snippets
;;   :straight (wcy123-snippets
;;              :type git
;;              :host github
;;              :files ("*.el" "snippets")
;;              :repo "wcy123/wcy123-emacs-snippets")
;;   :after (yasnippet)
;;   :config
;;   (wcy123-snippets-initialize))


;; == flycheck ==
(use-package flycheck
  ;; :disabled nil
  :hook (( ;; c++-mode c-mode
                   python-mode) . flycheck-mode))

;; == ace-jump-mode
(use-package ace-jump-mode)

;; == recentf
(use-package recentf
     :defines (recentf-max-saved-items)
     :config
     (setq recentf-max-saved-items 200))

;; == leader-key-mode
(use-package leader-key-mode
  :straight
  (leader-key-mode :type git
                   :host github
                   :repo "wcy123/leader-key-mode")
  :functions (leader-key-mode)
  :defer 1
  :config
  (leader-key-mode))

;; == tmux-cc
(use-package tmux-cc
  :straight
  (tmux-cc :type git
           :host github
           :repo "wcy123/tmux-cc")
  :commands
  (tmux-cc-send-current-line tmux-cc-select-block
                             tmux-cc-send-region))
;; == shell
(use-package sh-script
  :defer t
  :defines (sh-mode-map)
  :bind (:map sh-mode-map
              ("C-z" . tmux-cc-send-current-line)
              ("M-z" . tmux-cc-send-region)))
;; == markdown
(use-package markdown-mode
  :defines (markdown-mode-map)
  :mode "\\.md\\'"
  :mode "\\.markdown\\'"
  ;; :hook (markdown-mode . yas-minor-mode-on)
  :bind (:map markdown-mode-map
              ("ESC <up>"  . markdown-move-up)
              ("ESC <down>" . markdown-move-down)
              ("ESC <left>" . markdown-promote)
              ("ESC  <right>" . markdown-demote)
              ("C-z" . tmux-cc-send-current-line)
              ("M-z" . tmux-cc-send-region)
              ("<M-RET>" . markdown-insert-list-item)))

;; ;; == c/c++
(use-package cc-mode
  :after (cc-mode)
  :defines (c-default-style)
  :hook (c-mode-common . subword-mode)
  :config
  ;; 设置缩进风格. 用 M-x c-set-style ,然后用 TAB 查看补全结果,可以看到所有风格名称.
  (setq c-default-style
        '((java-mode . "java")
          ;;(c-mode . "k&r")
          (c-mode . "linux")
          ;;(c++-mode . "ellemtel")
          (c++-mode . "stroustrup")
          (other . "gnu")
          )))

;; == clang-format
(use-package clang-format
  :defines (clang-format-fallback-style)
  :after (cc-mode)
  :config
  (set-default 'clang-format-fallback-style "Google")
  (add-hook 'c-mode-common-hook #'(lambda()
                                    (add-hook 'before-save-hook
                                              'clang-format-buffer t t))))
;; == gud
(use-package gud
  :defines (gud-chdir-before-run)
  :config
  (setq gud-chdir-before-run nil))
;; == xcscope
(use-package xcscope
  :after cc-mode
  :hook (c-mode . cscope-minor-mode)
  :hook (c++-mode . cscope-minor-mode)
  :hook (dired-mode-hook . cscope-minor-mode)
  :defines (cscope-list-entry-keymap cscope-minor-mode-keymap)
  :config
  (define-key cscope-list-entry-keymap
    [mouse-2]  nil)
  (define-key cscope-list-entry-keymap [S-mouse-2] nil)
  (define-key cscope-minor-mode-keymap [mouse-3]   nil)
  (define-key cscope-minor-mode-keymap [S-mouse-3] nil))


;; == cmake
(use-package cmake-mode
  :defines (company-backends)
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'"
  :hook (cmake-mode . my-cmake-mode-hook)
  :config
  (defun my-cmake-mode-hook ()
    (set (make-local-variable 'company-backends) '(company-files
                                                   company-cmake))))
(use-package cmake-format
  :after (cmake-mode)
  :disabled t ;; it seems buggy
  :when (locate-file "cmake-format" exec-path)
  :straight (cmake-format
             :type git
             :host github
             :repo "simonfxr/cmake-format.el"
             :fork (:host github
                          :repo "wcy123/cmake-format.el"))
  ;; optional: enable automatic formatting on save
  :hook ((cmake-mode . cmake-format-mode))
  :config
  ;; optional:
  ;; (setq cmake-format-command "/path/to/cmake-format"
  ;;       cmake-format-args '("list" "of" "flags"))
  )

;; -------------------- Python --------------------------------
;; format on save
(use-package elpy
  :after (python)
  :defines (elpy-modules
            elpy-rpc-pythonpath
            python-shell-interpreter
            python-shell-interpreter-args
            elpy-rpc-virtualenv-path
            python-mode-map
            elpy-formatter)
  :functions (elpy-enable)
  :init
  (setq elpy-rpc-virtualenv-path 'current)
  ;; must set elpy-rpc-pythonpath eariler otherwise,
  ;; elpy-rpc-pythonpath will be initialize when loading elpy,
  ;; elpy-rpc and initialized by locate-library, locate-library does
  ;; work with no-load-path.el
  (setq elpy-rpc-pythonpath (expand-file-name "straight/build/elpy"
                                                   user-emacs-directory))
  (eval-after-load 'elpy
    '(progn
       (when (require 'flycheck nil t)
         (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
         (add-hook 'elpy-mode-hook 'flycheck-mode))
       (setq elpy-modules (delq 'elpy-module-yasnippet elpy-modules))
       (add-hook 'elpy-mode-hook (lambda ()
                                   (add-hook 'before-save-hook 'elpy-format-code nil t)))
       (setq python-shell-interpreter "ipython"
             python-shell-interpreter-args "-i --simple-prompt"
             elpy-formatter 'yapf
             )
       ))
  (elpy-enable)
  :bind (:map python-mode-map
              ("M-." . elpy-goto-definition)
              ))
;; https://realpython.com/python-pep8/#autoformatters
;; Run autopep8 on save
;; (use-package py-autopep8)
;; (use-package python :straight (python :type built-in)
;;   :hook (python-mode . py-autopep8-enable-on-save))

;; (use-package blacken) ;; black is not stable.

;; -------------------- ELISP --------------------------------
(use-package elisp-mode
  :straight (elisp-mode :type built-in)
  :hook (emacs-lisp-mode . auto-fill-mode)
  :hook (emacs-lisp-mode . show-paren-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-l" . eval-buffer)
              ("C-c C-c" . eval-defun))
  :config (require 'pp))

(use-package elisp-slime-nav
  :after (elisp-mode)
  :functions (elisp-slime-nav-mode)
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

(use-package pp
  :straight (pp :type built-in)
  :after (pp)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-m" . pp-macroexpand-expression)
              ("C-c C-e" . pp-macroexpand-last-sexp))
  :bind (("M-:" . pp-eval-expression)
         ("C-x C-e" . pp-eval-last-sexp)))

;; ------------------- protobuf ------------------------
(use-package protobuf-mode
  :mode "\\.proto\\'")
;;; --- prototxt ----
(use-package prototxt-mode
  :straight
  (substitute :type git
              :host github
		      :branch "master"
              :repo "drdv/prototxt-mode")
  :config
  (setq prototxt-mode-indentation-level t))

;; == adoc
(use-package adoc-mode
  :mode "\\.adoc\\'")

;;; -------------------- haskell ---------------
(use-package haskell-mode
  :mode "\\.hs\\'")

;;; ------------------ for rust ----------------------------
(use-package rust-mode
  :mode "\\.rs\\'"
  :defines (rust-format-on-save flycheck-checker)
  :functions (cargo-minor-mode company-indent-or-complete-common)
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook #'(lambda ()
                                (setq flycheck-checker 'rust-clippy)))
  (add-hook 'rust-mode-hook 'flycheck-mode))
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;;; ------------------ for mzscheme ----------------------------
(use-package geiser-chez
  :config
  (setq geiser-chez-binary "chezscheme"))

(use-package geiser-guile
  :config
  (setq geiser-impl--implementation 'guile))

(use-package paredit
  :straight
  (paredit :type git
           :host nil
		   :branch "master"
           :repo "https://github.com/emacsmirror/paredit")
  :config
  (add-hook 'scheme-mode-hook
            #'enable-paredit-mode)

  (add-hook 'emacs-lisp-mode-hook
            #'enable-paredit-mode))
;;; ------------------ for copilot ----------------------------
(use-package copilot
  :straight
  (copilot :type git
           :host github
           :repo "copilot-emacs/copilot.el"
           :branch "main")
  :bind (:map copilot-completion-map
              ("C-n" . copilot-next-completion)
              ("C-p" . copilot-previous-completion)
              ("C-a" . copilot-accept-completion)
              ("C-w" . copilot-accept-completion-by-word)
              ("C-l" . copilot-accept-completion-by-line))
  :hook (prog-mode . copilot-mode)
  :config
  (setq copilot-idle-delay 0.2)
  (setq copilot-completion-display-function #'copilot-completion-ui))
;;; -------- for copilot chat ----------------------------
;; cannot clone org mode.
;; (use-package copilot-chat
;;   :straight
;;   (copilot-chat :type git
;;                 :host github
;;                 :repo "chep/copilot-chat.el"
;;                 :branch "master")
;;   :bind (("C-c c" . copilot-chat))
;;   :config
;;   (setq copilot-chat-idle-delay 0.2)
;;   (setq copilot-chat-completion-display-function
;;   #'copilot-chat-completion-ui))
;;; ---------- for powershell --------
(use-package powershell
  :straight (powershell :type git
                        :host github
                        :repo "jschaf/powershell.el")
  :mode "\\.ps1\\'"
  :defines (powershell-mode-map)
  :bind (:map powershell-mode-map
              ("C-c C-c" . powershell-send-buffer)
              ("C-c C-l" . powershell-send-region))
  :config
  (setq powershell-indent-offset 4))
;; --- gcmh
(use-package gcmh
  :diminish gcmh-mode
  :config
  (gcmh-mode 1))
;; END
(no-load-path-done)
