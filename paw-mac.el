;;; paw/paw-mac.el -*- lexical-binding: t; -*-

;;; paw-mac.el -*- lexical-binding: t; -*-

(defcustom paw-mac-dictionary-program "mac"
  "Executable used to access the mac."
  :type 'file)

;;;###autoload
(defun paw-mac-dictionary-search-details (&optional word en)
  "Search word with mac."
  (interactive)
  (let ((word (or word (paw-get-word))))
    (shell-command-to-string
     (format "osascript -e 'tell application \"Eudic\" to activate show dic with word \"%s\"'" word))))


(provide 'paw-mac)
