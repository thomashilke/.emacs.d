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


(defun my/compile--tests-directory-p (path)
  (and (f-dir-p path)
       (not (null (string-match-p "tests"
                                  (f-filename path))))))

(defun my/compile--find-tests-directory (path)
  (f-traverse-upwards (lambda (path)
                        (not (null (remove-if-not #'my/compile--tests-directory-p
                                                  (directory-files path t)))))
                      path))

(defun my/compile--get-test-src-path (src-filename)
  (let* ((src-name (f-filename src-filename))
         (src-ext (f-ext src-name))
         (src-basename (f-no-ext src-name))
         (test-src-name (concat src-basename "_test.cpp"))
         (tests-directory (my/compile--find-tests-directory default-directory))
         (tests-directory-depth (length (f-split tests-directory)))
         (relative-src-filename (reduce #'f-join
                                        (seq-drop (f-split (f-dirname src-filename))
                                                  tests-directory-depth))))
    
    (f-join tests-directory "tests" relative-src-filename test-src-name)))

(defun test-src-p (path)
  (not (null (string-match-p "_test" (f-no-ext path)))))

(defun get-implementation-name (path)
  (let* ((filename (f-filename path))
         (basename (f-no-ext filename)))
    (concat (seq-take basename (- (length basename) 5)))))

(defun visited-p (filename)
  (not (null (remove-if-not (lambda (buffer)
                              (string-equal filename
                                            (buffer-file-name buffer)))
                            (buffer-list)))))

(defun my/compile--get-implementation-src-path (test-filename)
  (when (test-src-p test-filename)
    (let* ((test-directory-components (f-split (f-dirname test-filename)))
           (tests-directory-components (seq-take-while (lambda (name) (not (string-equal name "tests")))
                                                       test-directory-components))
           (tests-directory-depth (length tests-directory-components))

           (src-directory-components (append (seq-take test-directory-components tests-directory-depth)
                                             (seq-drop test-directory-components (1+ tests-directory-depth))
                                             (list (get-implementation-name test-filename))))
           (src-candidate-filename (reduce #'f-join src-directory-components)))
      (cond ((visited-p (concat src-candidate-filename ".cpp"))
             (concat src-candidate-filename ".cpp"))
            ((visited-p (concat src-candidate-filename ".hpp"))
             (concat src-candidate-filename ".hpp"))
            ((f-exists-p (concat src-candidate-filename ".hpp"))
             (concat src-candidate-filename ".hpp"))))))

(defun goto-file (filename)
  (if (f-exists-p filename)
      (find-file-other-window filename)))

(defun find-or-create-test-file (filename)
  (find-file-other-window filename)
  (if (not (f-exists-p filename))
      (progn
        (make-directory (f-dirname filename) t)
        (insert (format "#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_SUITE(%s);

BOOST_AUTO_TEST_CASE(a_first_test_case) {
  BOOST_TEST(false);
}

BOOST_AUTO_TEST_SUITE_END();
" (f-no-ext (f-filename filename)))))))

(defun toggle-test-implementation ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (cond ((test-src-p filename)
           (goto-file (my/compile--get-implementation-src-path filename)))
          (t
           (find-or-create-test-file (my/compile--get-test-src-path filename))))))

(provide 'my-compile)
