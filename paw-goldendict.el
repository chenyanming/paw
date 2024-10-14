;;; paw-goldendict.el -*- lexical-binding: t; -*-

(defcustom paw-goldendict-program "goldendict"
  "Executable used to access the goldendict."
  :type 'file)

;;;###autoload
(defun paw-goldendict-search-details (&optional word en)
  "Search word with goldendict."
  (interactive)
  (let ((word (or word
                  (pcase major-mode
                    ('pdf-view-mode
                     (car (pdf-view-active-region-text)))
                    ('eaf-mode
                     (eaf-execute-app-cmd 'eaf-py-proxy-copy_text)
                     (sleep-for 0.01) ;; TODO small delay to wait for the clipboard
                     (eaf-call-sync "execute_function" eaf--buffer-id "get_clipboard_text"))
                    (_ (if (use-region-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))
                         (thing-at-point 'word t)))) )))
    ;; (message word)
    (start-process "goldendict" "*goldendict*" "goldendict" word)))

(defun paw-goldendict-process-filter (proc string)
  "Accumulates the strings received from the goldendict process."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (insert string))))


(provide 'paw-goldendict)
