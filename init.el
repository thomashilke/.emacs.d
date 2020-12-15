;;; my-dot-emacs.el --- Thomas Hilke's Emacs configuration at Rollomatic -*- lexical-binding: t -*-

;; Copyright (C) 2020-2020 Thomas Hilke

;; Author: Thomas Hilke <t.hilke@rollomatic.ch>
;; URL: http://github.com/thomashilke/dot-emacs
;; Version: 0
;; Keywords: lisp configuration emacs
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:
;;  This is my Emacs configuration at Rollomatic.

;;; Code:

;;; add custom code load path

(require 'package)
(package-initialize)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(require 'tls)
(when (not (eq window-system 'w32))
  (let ((trustfile "c:/GitHOME/cacert.pem"))
    (setq tls-program
          (list
           (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                   (if (eq window-system 'w32) ".exe" "") trustfile)))
    (setq gnutls-verify-error t)
    (setq gnutls-trustfiles (list trustfile))))

(require 'f)
(add-to-list 'load-path (f-join user-emacs-directory "site-lisp/"))

(cond ((member "Fira Code" (font-family-list))
       (set-face-attribute 'default nil :font "Fira Code Retina-13"))
      ((member "Consolas" (font-family-list))
       (set-face-attribute 'default nil :font "Consolas" :height 110)))

(setq inhibit-startup-screen t
      initial-scratch-message ""
      inhibit-splash-screen t
      ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil
              tab-width 4)

(fset 'yes-or-no-p 'y-or-n-p)
(fringe-mode 16)

(dolist (mode '(scroll-bar-mode
                menu-bar-mode
                tool-bar-mode
                blink-cursor-mode))
  (funcall mode 0))

(dolist (mode '(desktop-save-mode
                show-paren-mode
                openwith-mode
                projectile-mode
                amx-mode
                ivy-mode
                counsel-mode
                global-undo-tree-mode
                global-company-mode
                global-diff-hl-mode
                global-hl-line-mode
                global-flycheck-mode
                ;smartparens-global-mode
                yas-global-mode
                semantic-mode
                global-semantic-idle-summary-mode))
  (funcall mode 1))

(require 'delight)
(delight '((which-key-mode nil which-key)
           (company-mode nil company)
           (undo-tree-mode nil undo-tree)
           (counsel-mode nil counsel)
           (projectile-mode nil projectile)
           (omnisharp-mode nil omnisharp)
           (smartparens-mode nil smartparens)
           (eldoc-mode nil eldoc)
           (ivy-mode nil ivy)
           (auto-revert-mode nil autorevert)
           (yas-minor-mode nil yasnippet)))


(require 'rainbow-delimiters)

(require 'openwith)
(setq openwith-associations '(("\\.xlsx\\'" "C:/Program Files/Microsoft Office/root/Office16/EXCEL.EXE" (file))))

;;; configure PATH to point to some useful unix tools
(require 'grep)
(setenv "PATH"
  (concat
   "c:/Program Files/Git/usr/bin/"
   (getenv "PATH")))
(setq grep-program (w32-short-file-name "c:/Program Files/Git/usr/bin/grep.exe"))
(setq find-program (w32-short-file-name "c:/Program Files/Git/usr/bin/find.exe"))

(require 'diff)
(setq diff-command (w32-short-file-name "c:/Program Files/Git/usr/bin/diff.exe"))
(setq ediff-diff-program (w32-short-file-name "c:/Program Files/Git/usr/bin/diff.exe"))

(require 'gnuplot)
(setq gnuplot-program (w32-short-file-name "c:/Program Files/gnuplot/bin/gnuplot.exe"))


;;; configure omnisharp for eglot
(require 'eglot)
(add-to-list 'eglot-server-programs
             '(csharp-mode . ("C:\\GitHOME\\.emacs.d\\.cache\\omnisharp\\server\\v1.37.3\\OmniSharp.exe" "-lsp")))

;;; associate .csproj with nxml-mode
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.vcxproj\\'" . nxml-mode))


;;; configure global keybinding and behaviour
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key [C-wheel-up] 'default-text-scale-increase)
(global-set-key [C-wheel-down] 'default-text-scale-decrease)


;;; pressing 'a' in dired open the folder in the same buffer
(put 'dired-find-alternate-file 'disabled nil)

(require 'autorevert)
(setq auto-revert-use-notify nil)

;;; resolve the trailing whitespaces issue for ever
;;; the whitespaces in c++ code is completely fucked up, and it causes tons of ws modifications
;;; => only add a hook in C# mode? or never in C++ mode?
;;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defun buffer-mode (buffer-or-string)
  "Return the major mode associated with a buffer identified by BUFFER-OR-STRING."
  (with-current-buffer buffer-or-string
    major-mode))

(add-hook 'before-save-hook (lambda ()
                              (if (not (eq (buffer-mode (current-buffer)) 'c++-mode))
                                  (delete-trailing-whitespace))))

;;; configure compilation-mode
(setq compilation-auto-jump-to-first-error t
      compilation-skip-threshold 2
      compilation-scroll-output t)

;;; configure motion
(setq scroll-step 1
      mouse-wheel-scroll-amount '(1 ((shift) 3)))

;;; configure backup files
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


(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)



(require 'isearch)
;;; "M-s ." search for symbol at point.
;;; "M-s w" search for word.
;;; "M-s o" open buffer with occurences of the search string.
(global-set-key (kbd "M-s s") 'isearch-forward)
(global-set-key (kbd "M-s r") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)
(setq isearch-wrap-function (lambda ()))


(require 'ivy)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(add-hook 'ivy-occur-mode-hook
          (lambda ()
            (local-set-key (kbd "n") 'next-line)
            (local-set-key (kbd "p") 'previous-line)))

(define-key ivy-minibuffer-map (kbd "C-c n") #'ivy-restrict-to-matches)

(require 'counsel)
(global-set-key (kbd "M-s S") #'counsel-rg)
(setq ripgrep-executable "c:/PROGRA~1/Git/usr/bin/rg.exe")
(setq counsel-rg-base-command
   "c:/PROGRA~1/Git/usr/bin/rg.exe -M 240 --with-filename --no-heading --line-number --color never %s --path-separator / .")


(require 'swiper)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)


(require 'multiple-cursors)
(global-set-key (kbd "C-c e") 'mc/edit-lines)
(global-set-key (kbd "C-c >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c <") 'mc/mark-previous-like-this)


(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(if (eq system-type 'windows-nt)
    (progn
      (setq exec-path (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin"))
      (setenv "PATH" (concat "C:\\Program Files (x86)\\Git\\bin;" (getenv "PATH")))))


(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'ibuffer-mode-hook
          (lambda () (ibuffer-filter-by-visiting-file nil)))

(setq ibuffer-formats '((mark modified read-only locked
                              " " (name 30 30 :left :elide)
                              " " (size 9 -1 :right)
                              " " (mode 16 16 :left :elide) " " filename-and-process)
                        (mark " " (name 30 30 :left :elide) " " filename-and-process)))

(require 'string-inflection)
(global-set-key (kbd "C-c t") 'string-inflection-toggle)

(setq amx-backend 'ivy)

(require 'undo-tree)
(setq undo-tree-visualizer-diff t)
(setq undo-tree-enable-undo-in-region t)
(global-set-key (kbd "C-=") 'undo-tree-redo)



(setq-default tab-always-indent 'complete)

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



;;; org-mode configuration
(require 'org)
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (R . t)
   (emacs-lisp . t)))


(require 'csharp-mode)
(add-hook 'csharp-mode-hook 'omnisharp-mode)

(require 'omnisharp)
(setq omnisharp-server-executable-path "C:\\GitHOME\\.emacs.d\\.cache\\omnisharp\\server\\v1.37.3\\OmniSharp.exe")

(require 'my-omnisharp)

(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))

(defconst rollomatic-csharp-style
  '("bsd"
    (c-basic-offset . 4)
    (c-offsets-alist
     (innamespace . [4])
     (arglist-cont-nonempty . +)
     (arglist-intro . +))))

(defconst rollomatic-c++-style
  '("bsd"
    (c-basic-offset . 4)
    (c-offsets-alist
     (innamespace . [4])
     (arglist-cont-nonempty . +)
     (arglist-intro . +))))

(c-add-style "rollomatic-c++-style" rollomatic-c++-style)
(c-add-style "rollomatic-csharp-style" rollomatic-csharp-style)

(defun setup-csharp-mode ()
  "Customize the environment when in csharp-mode."
  (c-set-style "rollomatic-csharp-style")
  (local-set-key (kbd "C-c C-g d") #'omnisharp-go-to-definition)
  (local-set-key (kbd "C-c C-g u") #'omnisharp-find-usages-with-ivy)
  (local-set-key (kbd "C-c C-g i") #'omnisharp-find-implementations-with-ivy)
  (local-set-key (kbd "C-c C-g r") #'omnisharp-rename)
  (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
  (local-set-key (kbd "C-c c") #'compile)
  (local-set-key (kbd "C-,") #'pop-tag-mark))

(global-set-key (kbd "C-c b p") #'my/msbuild-build-current-project)
(global-set-key (kbd "C-c b s") #'my/msbuild-build-current-solution)
(global-set-key (kbd "C-c b r") #'my/msbuild-rebuild-current-solution)
(global-set-key (kbd "C-c b c") #'my/msbuild-clean-current-solution)

(require 'my-msbuild)
(setq msbuild-program (w32-short-file-name "c:/Program Files (x86)/Microsoft Visual Studio/2019/Professional/MSBuild/Current/Bin/MSBuild.exe"))
(setq nuget-program (w32-short-file-name (concat user-emacs-directory ".cache/nuget/nuget.exe")))

(add-hook 'csharp-mode-hook #'setup-csharp-mode)

(define-key emacs-lisp-mode-map (kbd "<f5>") 'find-function-at-point)
(define-key emacs-lisp-mode-map (kbd "<f6>") 'find-variable-at-point)

(setq azure-devops-emacs-integration-pat-token "f5cd7q6y2jztg35cwjdqykamdygf7ndogkezwuxxvtqlyas46fcq")
(setq azure-devops-emacs-integration-pat-token-base64
      (base64-encode-string (concat ":" azure-devops-emacs-integration-pat-token) t))

;;; c++-mode configuration
(defun setup-c++-mode ()
  "Customize the environment when in c++-mode."
  (c-set-style "rollomatic-c++-style"))

(add-hook 'c++-mode-hook #'setup-csharp-mode)

(define-key c++-mode-map (kbd "C-c C-g d") 'semantic-ia-fast-jump)
(define-key c++-mode-map (kbd "C-,") #'pop-tag-mark)
(require 'my-utilities)
(define-key c++-mode-map (kbd "C-c w") #'wrap-if-case-with-curly-brackets)

(semantic-reset-system-include 'c++-mode)
(semantic-add-system-include "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Professional\\VC\\Tools\\MSVC\\14.25.28610\\include\\" 'c++-mode)
(require 'etags)
;;; fix found at: https://tsengf.blogspot.com/2011/06/semantic-ia-fast-jump-doesnt-push-tag.html
(unless (fboundp 'push-tag-mark)
  (defun push-tag-mark ()
    "Push the current position to the ring of markers so that
    \\[pop-tag-mark] can be used to come back to current position."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))))

(add-hook 'c++-common-hook #'setup-c++-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; lisp-mode-configuration
(require 'package-lint)
(require 'flycheck-package)
(eval-after-load 'flycheck
  '(flycheck-package-setup))
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(defun flycheck-display-error-messages-unless-error-buffer (errors)
  (unless (get-buffer-window flycheck-error-list-buffer)
    (flycheck-display-error-messages errors)))

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-buffer)

(add-to-list 'load-path (concat user-emacs-directory "lsp-mode/"))
(require 'lsp-mode)
(require 'lsp-clients)
(setq lsp-clients-csharp-language-server-path (concat user-emacs-directory ".omnisharp/OmniSharp.exe"))


;;; clang-format configuration
(require 'clang-format)
(global-set-key (kbd "C-c C-g f") 'clang-format-region)
(global-set-key (kbd "C-c C-g F") 'clang-format-buffer)
(setq clang-format-style "file")
(setq clang-format-style-option "Microsoft")

;;; Configuring ace-window
(global-set-key (kbd "C-x o") 'ace-window)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(eglot powershell gnuplot gnuplot-mode yasnippet-snippets srefactor ace-window delight smartparens default-text-scale lsp-treemacs treemacs which-key openwith rainbow-delimiters flycheck-package package-lint-flymake package-lint seq projectile-ripgrep amx forge restclient clang-format clang-format+ buffer-move diff-hl company-lsp projectile counsel swiper ivy-purpose ivy window-purpose logview string-inflection browse-kill-ring company-ghc magit-lfs transient magit company omnisharp haskell-mode multiple-cursors yasnippet phi-search undo-tree)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
