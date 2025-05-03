;;; paw/paw-mac.el -*- lexical-binding: t; -*-

;;; paw-mac.el -*- lexical-binding: t; -*-

(defcustom paw-mac-dictionary-program "mac"
  "Executable used to access the mac.
mac: the Mac dictionary.
eudic: the eudic dictionary.
"
  :type 'file)


(defvar paw-mac-chatgpt-script (concat (file-name-directory load-file-name) "chatgpt.scpt")
  "Path to emacs paw command line program.")

(defcustom paw-mac-chatgpt-browser-program "Google Chrome"
  "Browser used to open the chatgpt result."
  :type 'String
  :group 'paw-mac)

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


(defun paw-mac-chatgpt-search-details (&optional word en)
  "Search word with mac."
  (interactive)
  (let* ((word (cond ((stringp word) word)
                     ((use-region-p)
                      (replace-regexp-in-string "[ \n]+" " " (replace-regexp-in-string "^[ \n]+" "" (buffer-substring-no-properties (region-beginning) (region-end)))))
                     (t (current-word t t))))
         (context paw-note-context)
         (buffer (if (buffer-live-p paw-note-target-buffer)
                     paw-note-target-buffer
                   (current-buffer)))
         (prompt (if paw-ask-ai-p paw-ask-ai-defualt-prompt
                   (assoc-default
                    (completing-read (format "[Ask Chatgpt in %s] %s: " paw-mac-chatgpt-browser-program word) paw-ask-ai-prompt nil t)
                    paw-ask-ai-prompt)))
         (prompt (replace-regexp-in-string "{content}" word prompt))
         (prompt (replace-regexp-in-string "{context}" (with-current-buffer buffer
                                                         (pcase major-mode
                                                           ('nov-mode
                                                            (format " in this book, author: %s, title: %s, published at %s, context: %s"
                                                                    (alist-get 'creator nov-metadata)
                                                                    (alist-get 'title nov-metadata)
                                                                    (alist-get 'date nov-metadata)
                                                                    context))
                                                           ;; TODO support other modes
                                                           (_ (paw-get-note)))) prompt))
         ;; removing problematic characters when passing to osascript
         ;; (prompt (replace-regexp-in-string "\n" " " prompt))
         (prompt (replace-regexp-in-string "'" "\\\\'" prompt)))
    (start-process-shell-command
     "paw-mac-chatgpt"
     "*paw-mac-chatgpt*"
     (format "osascript %s %s %s"
             paw-mac-chatgpt-script
             (shell-quote-argument paw-mac-chatgpt-browser-program)
             (shell-quote-argument prompt)))))


(provide 'paw-mac)
