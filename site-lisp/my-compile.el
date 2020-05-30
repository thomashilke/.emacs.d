(require 'f)
(require 'cl)

(defun my/compile--makefile-p (filename)
  (not (null (string-match-p "^[Mm]akefile$" filename))))

(defun my/compile--find-topmost-makefile (start-directory)
  ""
  (let (topmost-makefile)
    (f-traverse-upwards (lambda (path)
                          (-when-let* ((makefiles
                                        (remove-if-not #'my/compile--makefile-p
                                                       (directory-files path))))
                            (setq topmost-makefile
                                  (mapcar (lambda (filename)
                                            (f-join path filename))
                                          makefiles)))
                          nil)
                        start-directory)
    (if topmost-makefile
        (car topmost-makefile)
      (message "No Makefile found.")
      nil)))

(defun my/compile ()
  ""
  (interactive)
  (-when-let* ((makefile (my/compile--find-topmost-makefile default-directory))
               (default-directory (f-dirname makefile)))
    (call-interactively #'compile)))


(provide 'my-compile)
