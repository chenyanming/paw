;;; paw/paw-mac.el -*- lexical-binding: t; -*-

;;; paw-mac.el -*- lexical-binding: t; -*-

(defcustom paw-mac-dictionary-program "mac"
  "Executable used to access the mac."
  :type 'file)

;;;###autoload
(defun paw-mac-dictionary-search-details (&optional word en)
  "Search word with mac."
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
    (shell-command-to-string
     (format "osascript -e 'tell application \"Eudic\" to activate show dic with word \"%s\"'" word))))


(provide 'paw-mac)
