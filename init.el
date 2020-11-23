(package-initialize)

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("elpa" . "http://elpa.gnu.org/packages/")))

(setq required-packages-list '(company flycheck eglot magit projectile amx ivy counsel which-key yasnippet multiple-cursors ace-window delight fsharp-mode yasnippet-snippets))

(dolist (package required-packages-list)
  (when (not (package-installed-p package))
    (package-install package)))

(setq inhibit-startup-screen t
      scroll-step 1
      mouse-wheel-scroll-amount '(2 ((shift) 3)))

(setq custom-file (make-temp-file ""))

(fset 'yes-or-no-p 'y-or-n-p)
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
(global-set-key (kbd "M-s S") #'counsel-rg)
(setq counsel-rg-base-command
      "/usr/bin/rg -M 240 --with-filename --no-heading --line-number --color never %s --path-separator / .")

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
	    (flymake-mode 1)
	    (setq flycheck-clang-language-standard "c++17")))

(require 'flymake)

(require 'cc-mode)
(define-key c++-mode-map (kbd "C-c ! n") #'flymake-goto-next-error)
(define-key c++-mode-map (kbd "C-c ! p") #'flymake-goto-prev-error)
(define-key c++-mode-map (kbd "C-c ! l") #'flymake-show-diagnostics-buffer)
(define-key c++-mode-map (kbd "C-c c") #'compile)

(require 'fsharp-mode)
(define-key fsharp-mode-map (kbd "C-c ! n") #'flymake-goto-next-error)
(define-key fsharp-mode-map (kbd "C-c ! p") #'flymake-goto-prev-error)
(define-key fsharp-mode-map (kbd "C-c ! l") #'flymake-show-diagnostics-buffer)

(require 'eglot-fsharp)
(add-hook 'fsharp-mode-hook #'eglot-ensure)
(setq inferior-fsharp-program "/usr/bin/fsharpi --readline-")

;;; toggling between horizontal and vertical windows split
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  ((if this-win-2nd (other-window 1)))))))

(global-set-key (kbd "C-x +") #'toggle-window-split)


;;; configure backups files
(require 'f)
(defvar emacs-autosave-directory
  (f-join user-emacs-directory "autosaves/")
  "")

(setq backup-directory-alist (list (cons ".*" emacs-autosave-directory))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;;; configure C++ indentation style
(c-add-style "ThH"
	     '("gnu"
	       (c-basic-offset . 2)	; Guessed value
	       (c-offsets-alist
		(access-label . -)	; Guessed value
		(block-close . 0)	; Guessed value
		(class-close . 0)	; Guessed value
		(class-open . 0)	; Guessed value
		(defun-block-intro . +)	; Guessed value
		(inclass . +)		; Guessed value
		(inline-close . 0)	; Guessed value
		(innamespace . +)	; Guessed value
		(namespace-close . 0)	; Guessed value
		(namespace-open . 0)	; Guessed value
		(statement . 0)		    ; Guessed value
		(statement-block-intro . +) ; Guessed value
		(substatement-open . 0)	    ; Guessed value
		(topmost-intro . 0)	    ; Guessed value
		(annotation-top-cont . 0)
		(annotation-var-cont . +)
		(arglist-close . c-lineup-close-paren)
		(arglist-cont c-lineup-gcc-asm-reg 0)
		(arglist-cont-nonempty . c-lineup-arglist)
		(arglist-intro . +)
		(block-open . 0)
		(brace-entry-open . 0)
		(brace-list-close . 0)
		(brace-list-entry . c-lineup-under-anchor)
		(brace-list-intro . +)
		(brace-list-open . 0)
		(c . c-lineup-C-comments)
		(case-label . 0)
		(catch-clause . 0)
		(comment-intro . c-lineup-comment)
		(composition-close . 0)
		(composition-open . 0)
		(cpp-define-intro c-lineup-cpp-define +)
		(cpp-macro . -1000)
		(cpp-macro-cont . +)
		(defun-close . 0)
		(defun-open . 0)
		(do-while-closure . 0)
		(else-clause . 0)
		(extern-lang-close . 0)
		(extern-lang-open . 0)
		(friend . 0)
		(func-decl-cont . +)
		(incomposition . +)
		(inexpr-class . +)
		(inexpr-statement . +)
		(inextern-lang . +)
		(inher-cont . c-lineup-multi-inher)
		(inher-intro . +)
		(inlambda . c-lineup-inexpr-block)
		(inline-open . 0)
		(inmodule . +)
		(knr-argdecl . 0)
		(knr-argdecl-intro . +)
		(label . 2)
		(lambda-intro-cont . +)
		(member-init-cont . c-lineup-multi-inher)
		(member-init-intro . +)
		(module-close . 0)
		(module-open . 0)
		(objc-method-args-cont . c-lineup-ObjC-method-args)
		(objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
		(objc-method-intro .
				   [0])
		(statement-case-intro . +)
		(statement-case-open . 0)
		(statement-cont . +)
		(stream-op . c-lineup-streamop)
		(string . -1000)
		(substatement . +)
		(substatement-label . 2)
		(template-args-cont c-lineup-template-args +)
		(topmost-intro-cont . c-lineup-topmost-intro-cont))))

(add-hook 'c++-mode-hook (lambda () (c-set-style "ThH")))
