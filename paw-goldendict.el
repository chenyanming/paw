;;; paw-goldendict.el -*- lexical-binding: t; -*-

(defcustom paw-goldendict-program "goldendict"
  "Executable used to access the goldendict."
  :type 'file)

;;;###autoload
(defun paw-goldendict-search-details (&optional word en)
  "Search word with goldendict."
  (interactive)
  (let ((word (or word (paw-get-word))))
    ;; (message word)
    (start-process "goldendict" "*goldendict*" "goldendict" word)))

(defun paw-goldendict-process-filter (proc string)
  "Accumulates the strings received from the goldendict process."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (insert string))))


(provide 'paw-goldendict)
