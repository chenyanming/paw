;;; paw-goldendict.el -*- lexical-binding: t; -*-

;;;###autoload
(defun paw-goldendict-search-details (&optional word)
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
    (message word)
    (start-process "goldendict" nil "goldendict" word)))


(provide 'paw-goldendict)
