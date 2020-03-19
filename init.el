(message "PROFILE START INIT.EL %5.2f ms acc" (* 1000.0 (float-time
                                                 (time-subtract
                     (current-time)
                     before-init-time))))
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
    (defalias 'yes-or-no-p #'y-or-n-p)))

(use-package xt-mouse
  :straight (xt-mouse :type built-in)
  :when (not (display-graphic-p))
  :defer 2
  :config (xterm-mouse-mode 1))

(setq
 ;; I prefer split horizontally.
 split-width-threshold 160
 ;; keyboard scroll one line at a time
 scroll-step 1)
;; scroll one line at a time (less "jumpy" than defaults)
(use-package mwheel
  :after (mwheel)
  :straight (mwheel :type built-in)
  :defines (mouse-wheel-scroll-amount mouse-wheel-progressive-speed  mouse-wheel-follow-mouse)
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  )

(use-package savehist
  :straight (savehist :type built-in)
  :defer 2
  :config
  (savehist-mode 1))
(menu-bar-mode -1)

;; == exec-path-from-shell
(use-package exec-path-from-shell
  :when (or (memq window-system '(mac ns x))
	       (memq system-type '(darwin)))
  :functions (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-initialize))

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

;; == ivy mode
(use-package ivy
     :diminish ivy-mode
     :defines (ivy-use-virtual-buffers)
     :functions (ivy-mode)
     :hook (after-init-idle .  ivy-mode)
     :config
     (setq ivy-use-virtual-buffers t))

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
  (projectile-discover-projects-in-directory (getenv "PWD"))
  (setq projectile-completion-system 'ivy))

;; == magit
(use-package magit
  :bind ("C-x g" . 'magit-status))
;; == compile
(use-package compile
  :straight (compile :type built-in)
  :bind (("M-7" . compile))
  :defines (compilation-scroll-output compilation-read-command)
  :config
       (setq compilation-scroll-output t
           compilation-read-command nil))

;; == ffap
(use-package ffap
  :defines (ffap-c-path)
  :config
  (if (getenv "INCLUDE")
      (setq ffap-c-path
            (append (split-string (getenv "INCLUDE") ":" nil)
                    ffap-c-path))))

;; == company-mode
(use-package company
  :after (prog-mode)
  :defines (company-active-map
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
              ("M-RET" . company-complete-common)))
(use-package company-irony
  :disabled t
  :after company)
(use-package company-c-headers
  :after company
  :defines (company-backends)
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; == flycheck ==
(use-package flycheck
  :disabled t
  :hook ((c++-mode c-mode) . flycheck-mode))

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
  :config
  (leader-key-mode))

;; == tmux-cc
(use-package tmux-cc
  :straight
  (tmux-cc :type git
           :host github
           :repo "wcy123/tmux-cc")
  :commands
  (tmux-cc-send-current-line tmux-cc-select-block tmux-cc-send-region))
;; == markdown
(use-package markdown-mode
  :defines (markdown-mode-map)
  :mode "\\.md\\'"
  :mode "\\.markdown\\'"
  :bind (:map markdown-mode-map
              ("ESC <up>"  . markdown-move-up)
              ("ESC <down>" . markdown-move-down)
              ("ESC <left>" . markdown-promote)
              ("ESC  <right>" . markdown-demote)
              ("C-j" . tmux-cc-send-current-line)
              ("C-M-j" . tmux-cc-select-block)
              ("C-c <RET>" . tmux-cc-send-region)
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
  :hook (dired-mode-hook . cscope-minor-mode))

;; == cmake
(use-package cmake-mode
  :defines (company-backends)
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'"
  :hook (cmake-mode . my-cmake-mode-hook)
  :config
  (defun my-cmake-mode-hook ()
    (set (make-local-variable 'company-backends) '(company-files company-cmake))))

;; -------------------- ELISP --------------------------------
(use-package elisp-mode
  :straight (elisp-mode :type built-in)
  :hook (emacs-lisp-mode-hook . auto-fill-mode)
  :hook (emacs-lisp-mode-hook . show-paren-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-l" . eval-buffer)
              ("C-c C-c" . eval-defun)))
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

;; == adoc
(use-package adoc-mode
  :mode "\\.adoc\\'")

;;; -------------------- haskell ---------------
(use-package haskell-mode
  :mode "\\.hs\\'")

;;; ------------------ for rust ----------------------------
(use-package rust-mode
  :mode "\\.rs\\'"
  :defines (rust-format-on-save)
  :functions (cargo-minor-mode company-indent-or-complete-common)
  :config
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (setq rust-format-on-save t))
(use-package racer
  :defines (rust-mode-map company-tooltip-align-annotations)
  :functions (company-indent-or-complete-common )
  :after (rust-mode)
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))


;; END
(no-load-path-done)
