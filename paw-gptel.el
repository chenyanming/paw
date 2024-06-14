;;; paw-gptel.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'gptel)
(require 'paw-db)

(defcustom paw-gptel-language "chinese"
  "The language to use in gptel."
  :type 'string
  :group 'paw)

(defun paw-gptel-update-note (id word type &optional callback)
  (gptel-request
      (pcase type
        ('word (format "You are an English Expert, you carefully research the word, give me the English explanations/sentences from Famous Dictionaries, and the related Chinese explanation: %s, return your answer in Emacs Org mode format, heading starts from **, no need to tell me 'Sure'"
                       word))
        (_ (format "Explain: %s directly, return your answer in Emacs Org Mode format, heading starts from **, no need to tell me 'Sure'"
                   word )))
    :callback
    (lambda (response info)
      (if (not response)
          (message "gptel-quick failed with message: %s" (plist-get info :status))
        (paw-update-note id response)
        (message "%s" response))
      (if callback
          (funcall callback)))))

(defun paw-gptel-translate (word &optional prompt callback)
  (let* ((word (replace-regexp-in-string "^[ \n]+" "" word))
         (prompt (if (stringp prompt)
                     (format "I'm reading, I have a question about the following highlighted text: %s, %s, answer using %s" word prompt paw-gptel-language)
                   (format "Translate: %s, to %s" word paw-gptel-language)))
         (paw-view-note-buffer (get-buffer paw-view-note-buffer-name)))
    (message "%s" prompt)
    (gptel-request prompt
    :callback
    (lambda (response info)
      (if (not response)
          (message "paw-gptel-translate failed with message: %s" (plist-get info :status))
        (message "%s" response))
      (if callback
          (funcall callback)
        (with-current-buffer
            ;; workaround, because sometimes it may jump to other buffers
            (if (buffer-live-p paw-view-note-buffer)
                paw-view-note-buffer
              (generate-new-buffer paw-view-note-buffer-name))
          (save-excursion
            (let* ((buffer-read-only nil)
                   (translation response))
              ;; (pp translation)
              (goto-char (point-min))
              (search-forward "** Translation" nil t)
              (org-mark-subtree)
              (forward-line)
              (delete-region (region-beginning) (region-end))
              (insert "#+BEGIN_SRC org-mode\n\n")
              (insert translation)
              (insert "\n\n")
              (insert "#+END_SRC\n")
              (message "Translation completed")
              ;; (message "Translation completed %s" translation)
              ) )
          (deactivate-mark))))) ))

(provide 'paw-gptel)
