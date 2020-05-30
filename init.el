;;; -*- lexical-binding: t -*-

(require 'cl)

(lexical-let ((old-gc-treshold gc-cons-threshold))
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold old-gc-treshold))))

(require 'package)
(package-initialize)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/"))
      packages-archives-priorities
      '(("melpa-stable" . 10)
        ("gnu" . 5)
        ("melpa . 0")))

(let ((package-buildins nil)
      (packages '(f
                  company
                  doom-themes
                  magit
                  multiple-cursors
                  projectile
                  which-key
                  smartparens
                  default-text-scale
                  openwith
                  flycheck-package
                  package-lint-flymake
                  package-lint
                  seq
                  projectile-ripgrep
                  amx
                  clang-format
                  diff-hl
                  counsel
                  swiper
                  ivy
                  string-inflection
                  magit-lfs
                  haskell-mode
                  undo-tree)))
  (let ((packages (remove-if 'package-installed-p packages)))
    (when packages
      (package-refresh-contents)
      (mapc 'package-install packages))))

(add-to-list 'load-path (f-join user-emacs-directory "site-lisp/")
(require 'my-compile)

(setq initial-scratch-message nil
      inhibit-startup-screen t
      echo-keystrokes 0.1
      ring-bell-function 'quiet
      custom-file (make-temp-file ""))

(fringe-mode 32)

(setq
      compilation-auto-jump-to-first-error t
      compilation-skip-threshold 2
      compilation-scroll-output t)

(setq scroll-step 1
      mouse-wheel-scroll-amount '(1 ((shift) 3)))

(setq-default tab-width 4
              indent-tabs-mode nil
              tab-always-indent 'complete)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'dired-find-alternate-file 'disabled nil)

(dolist (mode '(tool-bar-mode
                menu-bar-mode
                scroll-bar-mode
                blink-cursor-mode))
  (funcall mode 0))

(dolist (mode '(show-paren-mode
                column-number-mode
                which-key-mode
                projectile-mode
                global-company-mode
                desktop-save-mode
                ivy-mode
                amx-mode
                counsel-mode
                global-undo-tree-mode
                global-diff-hl-mode
                smartparens-mode
                global-hl-line-mode))
  (funcall mode 1))

(load-theme 'doom-spacegrey t)

(cond ((member "Fira Code" (font-family-list))
       (set-face-attribute 'default nil :font "Fira Code Retina-13")))

(require 'f)
(defvar emacs-autosave-directory
  (f-join user-emacs-directory "autosaves/")
  "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")

(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

(setq company-idle-delay 0
      company-echo-delay 0
      company-minimum-prefix-length 2)

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(setq ispell-list-command "--list")

(add-hook 'prog-mode-hook 'flycheck-mode)

(setq projectile-completion-system 'ivy)

(setq isearch-wrap-function (lambda ()))

(setq ivy-use-virtual-buffers t
      enable-recursive-minibuffers t
      ivy-count-format "(%d/%d) ")

(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-filter-by-visiting-file nil)))

(setq undo-tree-visualizer-diff t
      undo-tree-enable-undo-in-region t)

(add-hook 'csharp-mode-hook 'omnisharp-mode)

(eval-after-load 'company
  '(add-to-list 'company-backend 'company-omnisharp))

(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

(define-key custom-bindings-map (kbd "C-?") 'help-command)

(define-key custom-bindings-map (kbd "C-=") 'undo-tree-redo)

(define-key custom-bindings-map (kbd "C-c t") 'string-inflection-toggle)

(define-key custom-bindings-map (kbd "C-x C-f") 'counsel-find-file)

(define-key custom-bindings-map (kbd "C-x C-b") 'ibuffer)

(define-key ivy-occur-mode-map (kbd "n") 'next-line)
(define-key ivy-occur-mode-map (kbd "p") 'previous-line)

(define-key custom-bindings-map (kbd "M-s S") 'counsel-rg)

(define-key custom-bindings-map (kbd "C-s") 'swiper-isearch)
(define-key custom-bindings-map (kbd "C-r") 'swiper-isearch-backward)

(define-key custom-bindings-map (kbd "C-h") 'delete-backward-char)
(define-key custom-bindings-map (kbd "M-h") 'backward-kill-word)

(define-key custom-bindings-map [C-wheel-up] 'default-text-scale-increase)
(define-key custom-bindings-map [C-wheel-down] 'default-text-scale-decrease)

(define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
(define-key custom-bindings-map (kbd "M-s s") 'isearch-forward)
(define-key custom-bindings-map (kbd "M-s r") 'isearch-backward)

(define-key custom-bindings-map (kbd "C-c e") 'mc/edit-lines)
(define-key custom-bindings-map (kbd "C-c a") 'mc/mark-all-like-this)
(define-key custom-bindings-map (kbd "C-c n") 'mc/mark-next-like-this)
(define-key custom-bindings-map (kbd "C-x g") 'magit-status)

(define-key c++-mode-map (kbd "C-c c") 'my/compile)

(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") #'company-select-next)
(define-key company-active-map (kbd "C-p") #'company-select-previous)
(define-key company-active-map (kbd "C-h") #'delete-backward-char)
(define-key company-active-map (kbd "C-s") #'company-filter-candidates)

(define-key company-search-map (kbd "C-n") #'company-select-next)
(define-key company-search-map (kbd "C-p") #'company-select-previous)
(define-key company-search-map (kbd "C-h") #'company-search-delete-char)

(define-key custom-bindings-map (kbd "C-c p") 'projectile-command-map)

(define-minor-mode custom-bindings-mode
  "A mode that activates custom bindings."
  t nil custom-bindings-map)

(custom-bindings-mode t)
  
