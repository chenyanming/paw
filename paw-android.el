;;; paw-android.el -*- lexical-binding: t; -*-
(require 'paw-vars)

(defcustom paw-eudic-android-program "com.eusoft.eudic"
  "The Eudic android program."
  :type 'string
  :group 'paw)


(defcustom paw-moji-android-program "com.mojitec.mojidict"
  "The Moji android program."
  :type 'string
  :group 'paw)

(defcustom paw-chatgpt-android-program "com.openai.chatgpt"
  "The chatgpt android program."
  :type 'string
  :group 'paw)

(defun paw-eudic-search-details (&optional word en)
  "Call `paw-eudic-android-program' in termux to search for WORD."
  (interactive)
  (call-process-shell-command
   (format "termux-am start -a android.intent.action.SEND --es android.intent.extra.TEXT \"%s\" -t text/plain %s"
           (cond ((stringp word) word)
                 ((use-region-p)
                  (replace-regexp-in-string "[ \n]+" " " (replace-regexp-in-string "^[ \n]+" "" (buffer-substring-no-properties (region-beginning) (region-end)))))
                 (t (current-word t t)))
           paw-eudic-android-program)))



(defun paw-moji-search-details (&optional word)
  "Call `paw-moji-android-program' in termux to search for WORD."
  (interactive)
  (call-process-shell-command
   (format "termux-am start -a android.intent.action.SEND --es android.intent.extra.TEXT \"%s\" -t text/* %s"
           (cond ((stringp word) word)
                 ((use-region-p)
                  (replace-regexp-in-string "[ \n]+" " " (replace-regexp-in-string "^[ \n]+" "" (buffer-substring-no-properties (region-beginning) (region-end)))))
                 (t (current-word t t)))
           paw-moji-android-program)))

(defun paw-android-search-details (&optional word)
  "Call share menu in termux to search for WORD."
  (interactive)
  (call-process-shell-command
   (format "termux-am start -a android.intent.action.SEND --es android.intent.extra.TEXT \"%s\" -t text/*"
           (cond ((stringp word) word)
                 ((use-region-p)
                  (replace-regexp-in-string "[ \n]+" " " (replace-regexp-in-string "^[ \n]+" "" (buffer-substring-no-properties (region-beginning) (region-end)))))
                 (t (current-word t t)))
           )))

(defun paw-chatgpt-search-details (&optional word)
  "Call `paw-chatgpt-android-program' in termux to search for WORD."
  (interactive)
  (let* ((word (cond ((stringp word) word)
                     ((use-region-p)
                      (replace-regexp-in-string "[ \n]+" " " (replace-regexp-in-string "^[ \n]+" "" (buffer-substring-no-properties (region-beginning) (region-end)))))
                     (t (current-word t t))))
         (buffer (if (buffer-live-p paw-note-target-buffer)
                     paw-note-target-buffer
                   (current-buffer)))
         (prompt (if paw-ask-ai-p paw-ask-ai-defualt-prompt
                   (assoc-default
                    (completing-read (format "%s: " word) paw-ask-ai-prompt nil t)
                    paw-ask-ai-prompt)))
         (prompt (replace-regexp-in-string "{content}" word prompt))
         (prompt (replace-regexp-in-string "{context}" (with-current-buffer buffer
                                                         (pcase major-mode
                                                           ('nov-mode
                                                            (format "in this book, author: %s, title: %s, published at %s"
                                                                    (alist-get 'creator nov-metadata)
                                                                    (alist-get 'title nov-metadata)
                                                                    (alist-get 'date nov-metadata)))
                                                           ;; TODO support other modes
                                                           (_ (paw-get-note)))) prompt)))
    (call-process-shell-command
     (format "termux-am start -a android.intent.action.SEND --es android.intent.extra.TEXT \"%s\" -t text/plain -f 0x18000000 %s"
             prompt
             paw-chatgpt-android-program)) ))

(defun paw-android-browse-url (url)
  "Open given URL in Termux."
  (interactive "sEnter the URL: ")
  (if (string-match-p "^http\\(s\\)?://[^ \n]*$" url)
      (call-process-shell-command (format "termux-open %s" url))
    (message "Invalid URL. Please enter a URL that begins with http://")))

(defun paw-android-language-convert(lang)
  "TODO Convert the detected langauge to android google tts recognized language."
  (pcase lang
    ("ja" "jpn-jpn")
    ("en" "eng-usa")
    (_ lang)))

(defun paw-android-say-word (word &optional lang)
  "Say word with Tasker"
  (call-process-shell-command (format "am broadcast --user 0 -a net.dinglish.tasker.emacs -e word \"%s\" -e lang \"%s\"" word (paw-android-language-convert (if lang lang (paw-check-language word))))))



(defun paw-android-mpv-start(file)


  (call-process-shell-command (format "am start -n is.xyz.mpv/.MPVActivity -a android.intent.action.VIEW -d %s" file))
  )

(provide 'paw-android)
