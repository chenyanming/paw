;;; paw/paw-mac.el -*- lexical-binding: t; -*-

;;; paw-mac.el -*- lexical-binding: t; -*-

(defcustom paw-mac-dictionary-program "mac"
  "Executable used to access the mac.
mac: the Mac dictionary.
eudic: the eudic dictionary.
"
  :type 'file)

;;;###autoload
(defun paw-mac-dictionary-search-details (&optional word en)
  "Search word with mac."
  (interactive)
  (let ((word (or word (paw-get-word))))
    (if (string= paw-mac-dictionary-program "mac")
        (shell-command-to-string
         (format "open dict://%s" (url-hexify-string word)))
      (shell-command-to-string
       (format "osascript -e 'tell application \"Eudic\" to activate show dic with word \"%s\"'" word)))))


(provide 'paw-mac)
