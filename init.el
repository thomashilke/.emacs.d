(package-initialize)

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/") ("elpa" . "http://elpa.gnu.org/packages/")))
(setq required-packages-list '(company flycheck eglot magit projectile amx ivy counsel which-key yasnippet multiple-cursors ace-window delight))

(setq inhibit-startup-screen t)
(setq scroll-step 1
      mouse-wheel-scroll-amount '(2 ((shift) 3)))

(fset 'yes-or-no-p 'y-or-no-p)
(setq-default tab-always-indent 'complete)

(dolist (mode '(scroll-bar-mode menu-bar-mode tool-bar-mode))
  (funcall mode 0))

(dolist (mode '(global-company-mode global-undo-tree-mode counsel-mode ivy-mode amx-mode projectile-mode yas-global-mode which-key-mode show-paren-mode which-key-mode xterm-mouse-mode))
  (funcall mode 1))

(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

(put 'dired-find-alternate-file 'disabled nil)

(require 'delight)
(delight '((which-key-mode nil which-key)
	   (company-mode nil company)
	   (undo-tree-mode nil undo-tree)
	   (counsel-mode nil counsel)
	   (eldoc-mode nil eldoc)
	   (yas-minor-mode nil yasnippet)
	   (ivy-mode nil ivy)))

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)

(require 'isearch)
(global-set-key (kbd "M-s s") 'isearch-forward)
(global-set-key (kbd "M-s r") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)
(setq isearch-wrap-function (lambda ()))

(require 'ivy)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) ")
(define-key ivy-minibuffer-map (kbd "C-c n") #'ivy-restrict-to-matches)

(require 'counsel)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(require 'swiper)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)

(require 'multiple-cursors)
(global-set-key (kbd "C-c e") 'mc/edit-lines)
(global-set-key (kbd "C-c e") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c e") 'mc/mark-previous-like-this)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'amx)
(setq amx-backend 'ivy)

(require 'undo-tree)
(global-set-key (kbd "C-=") 'undo-tree-redo)

(require 'company)
(setq company-idle-delay 0
      company-echo-delay 0
      company-minimum-prefix-length 2)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-h") #'delete-backward-char)
  (define-key company-active-map (kbd "C-s") #'company-filter-candidates)

  (define-key company-search-map (kbd "C-n") #'company-select-next)
  (define-key company-search-map (kbd "C-p") #'company-select-previous)
  (define-key company-search-map (kbd "C-h") #'company-search-delete-char))

(require 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

(require 'eglot)
(add-hook 'c++-mode-hook
	  (lambda ()
	    (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
	    (eglot-ensure)
	    (flycheck-mode 1)
	    (setq flycheck-clang-language-standard "c++17")))
