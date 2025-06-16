;; suppress prompt on windows,
(set-language-environment "UTF-8")
;; initialize straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; do not use package.el
(setq package-enable-at-startup nil)
(straight-use-package 'use-package)
(use-package straight
  :custom
  (straight-use-package-by-default t))
;; packages
(use-package leader-key-mode
  :straight
  (leader-key-mode :type git
                   :host github
                   :repo "wcy123/leader-key-mode")
  :functions (leader-key-mode)
  :defer 1
  :config
  (leader-key-mode))
(eval-after-load 'simple
  '(progn
    (add-hook 'before-save-hook 'delete-trailing-whitespace t nil)
    (set-default 'tab-width 4)
    (set-default 'indent-tabs-mode nil)
    (add-to-list 'minor-mode-alist '(mark-active " Mark"))
    (pending-delete-mode 1)
    (defalias 'yes-or-no-p #'y-or-n-p)))
(use-package savehist
  :straight (savehist :type built-in)
  ;; it is so important, so we pay 2-3ms to  initialize it, it worth.
  ;; :defer 2
  :config
  (savehist-mode 1))
(use-package recentf
  :defines (recentf-max-saved-items)
  :config
  (recentf-mode)
  (setq recentf-max-saved-items 200))

;; (use-package ivy
;;   :defer 2
;;   :diminish ivy-mode
;;   :defines (ivy-use-virtual-buffers
;;             ivy-initial-inputs-alist
;;             ivy-display-style ivy-count-format
;;             ivy-minibuffer-map)
;;   :functions (ivy-mode)
;;   :hook (after-init-idle .  ivy-mode)
;;   :bind (:map ivy-minibuffer-map
;;               (("C-w" . ivy-yank-word)))
;;   :config
;;   (ivy-mode 1)
;;   ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
;;   (setq ivy-use-virtual-buffers t)
;;   ;; no regexp by default
;;   (setq ivy-initial-inputs-alist nil)
;;   (setq ivy-display-style 'fancy)
;;   (setq ivy-count-format "%d/%d ")
;;   )
;; Enable Vertico.
(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))
;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))
;; Optionally use the `orderless' completion style.
;;; Orderless completion style (and prot-orderless.el)
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic
                                                 partial-completion)))))

(use-package consult
  :straight
  (consul :type git
		  :branch "main"
          :host github
          :repo "minad/consult")
  :bind (("C-x C-r" . consult-recentf)
         ("C-x b" . consult-buffer)
         ("C-x x l" . consult-line)
         )
  :config
  (setq consult-narrow-key "<"))
(use-package expand-region
  :commands (er/expand-region)
  :bind (("M-=" . er/expand-region)))
(use-package rg
  :commands (rg)
  :bind (("C-c s"  . rg)))
(use-package ag)
(use-package projectile
  :defines (projectile-completion-system projectile-command-map)
  :functions (projectile-discover-projects-in-directory)
  :bind-keymap ("C-x p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq
        projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-sort-order 'recentf)
  ;; (setq projectile-completion-system 'ivy)
  )
(use-package magit
  :bind ("C-x g" . 'magit-status))
(use-package compile
  :straight (compile :type built-in)
  :bind (("M-7" . compile)
         ("<f7>" . compile))
  :defines (compilation-scroll-output compilation-read-command)
  :config
       (setq compilation-scroll-output t
             compilation-read-command nil
             compilation-ask-about-save nil))
(use-package ffap
  :defines (ffap-c-path)
  :config
  (if (getenv "INCLUDE")
      (setq ffap-c-path
            (append (split-string (getenv "INCLUDE") ":" nil)
                    ffap-c-path))))
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
(use-package company-lsp
  :after (company)
  :commands company-lsp
  :config
  ;; no need to do it, lsp already did it
  ;; (push 'company-lsp company-backends)
  )
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package which-key
  :defer 1
  :config
  (which-key-mode))
(use-package hippie-exp
  :straight (hippie-exp :type built-in)
  :bind ("M-?" . hippie-expand))
(use-package skeleton-snippet
  :straight
  (skeleton-snippet :type git
                    :host github
                    :repo "wcy123/skeleton-snippet")
  :bind (("C-]" . skeleton-snippet))
  )
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
(use-package tmux-cc
  :straight
  (tmux-cc :type git
           :host github
           :repo "wcy123/tmux-cc")
  :commands
  (tmux-cc-send-current-line tmux-cc-select-block
                             tmux-cc-send-region))

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
(use-package clang-format
  :defines (clang-format-fallback-style)
  :after (cc-mode)
  :config
  (set-default 'clang-format-fallback-style "Google")
  (add-hook 'c-mode-common-hook #'(lambda()
                                    (add-hook 'before-save-hook
                                              'clang-format-buffer t t))))
(use-package cmake-mode
  :defines (company-backends)
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'"
  :hook (cmake-mode . my-cmake-mode-hook)
  :config
  (defun my-cmake-mode-hook ()
    (set (make-local-variable 'company-backends) '(company-files
                                                   company-cmake))))

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
(use-package protobuf-mode
  :mode "\\.proto\\'")
(use-package prototxt-mode
  :straight
  (substitute :type git
              :host github
		      :branch "master"
              :repo "drdv/prototxt-mode")
  :config
  (setq prototxt-mode-indentation-level t))
(use-package paredit
  :straight
  (paredit :type git
           :host nil
		   :branch "master"
           :repo "https://github.com/emacsmirror/paredit")
  :bind (:map paredit-mode-map
              ("M-<right>" . paredit-forward-slurp-sexp)
              ("M-<left>" . paredit-forward-barf-sexp)
              ("C-M-<right>" . paredit-backward-slurp-sexp)
              ("C-M-<left>" . paredit-backward-barf-sexp))
  :config
  (add-hook 'scheme-mode-hook
            #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook
            #'enable-paredit-mode))

(use-package copilot
  :straight
  (copilot :type git
           :host github
           :repo "copilot-emacs/copilot.el"
           :branch "main")
  :bind (:map copilot-completion-map
              ("M-n" . copilot-next-completion)
              ("M-p" . copilot-previous-completion)
              ("<tab>" . copilot-accept-completion)
              ("C-i" . copilot-accept-completion)
              ("M-f" . copilot-accept-completion-by-word)
              ("C-e" . copilot-accept-completion-by-line))
  :bind (("M-]" . copilot-complete))
  :hook (prog-mode . copilot-mode)
  :config
  (setq copilot-idle-delay nil)
  (setq copilot-indent-offset-warning-disable t)
  (setq copilot-max-char-warning-disable t)
  (setq copilot-completion-display-function #'copilot-completion-ui))

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode))

(use-package powershell
  :straight (powershell :type git
                        :host github
                        :repo "jschaf/powershell.el")
  :defines (powershell-mode-map)
  :bind (:map powershell-mode-map
              ("C-c C-c" . powershell-send-buffer)
              ("C-c C-l" . powershell-send-region))
  :config
  (setq powershell-indent-offset 4))


;; Local Variables:
;; mode: emacs-lisp
;; End:
