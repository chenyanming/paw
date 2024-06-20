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
         (buffer (current-buffer))) ;; the button is pressed on current-buffer
    (message "%s" prompt)
    (gptel-request prompt
    :callback
    (lambda (response info)
      (if (not response)
          (message "paw-gptel-translate failed with message: %s" (plist-get info :status))
        (message "%s" response))
      (if callback
          (funcall callback)
        (with-current-buffer buffer
          (save-excursion
            (let* ((buffer-read-only nil)
                   (translation response))
              ;; (pp translation)
              (goto-char (point-min))
              (search-forward "** Translation" nil t)
              (org-mark-subtree)
              (forward-line)
              (delete-region (region-beginning) (region-end))
              (paw-insert-and-make-overlay (concat translation "\n" ) 'face 'org-block)
              (goto-char (point-min))
              (search-forward "** Dictionaries" nil t)
              (beginning-of-line)
              (message "Translation completed")
              ;; (message "Translation completed %s" translation)
              ) )
          (deactivate-mark))))) ))

(provide 'paw-gptel)
