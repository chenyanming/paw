;;; paw-gptel.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'gptel)
(require 'paw-db)

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

(defun paw-gptel-update-exp (id word type &optional callback)
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
        (paw-update-exp id response)
        (message "%s" response))
      (if callback
          (funcall callback)))))


(defun paw-gptel-translate (word &optional prompt callback buffer section)
  (let ((buffer (or buffer (current-buffer)))) ;; the button is pressed on current-buffer
    (message "%s" prompt)
    (gptel-request prompt
    :callback
    (lambda (response info)
      (if (not response)
          (message "paw-gptel-translate failed with message: %s" (plist-get info :status))
        ;; (message "%s" response)
        )
      (if callback
          (funcall callback)
        (with-current-buffer buffer
          (save-excursion
            (let* ((buffer-read-only nil)
                   (translation response))
              ;; (pp translation)
              (unless (string-match-p section (org-no-properties (org-get-heading t t t t)))
                (goto-char (point-min))
                (search-forward (format "** %s" section) nil t))
              (org-mark-subtree)
              (goto-char (mark-marker))
              ;; (forward-line)
              ;; (delete-region (region-beginning) (region-end))
              (let ((bg-color paw-view-note-background-color))
                (paw-insert-and-make-overlay
                 translation
                 'face `(:background ,bg-color :extend t))
                (insert "\n"))
              (if (or paw-ask-ai-p paw-ai-translate-p paw-ai-translate-context-p)
                  (insert "\n"))
              (goto-char (point-min))
              (search-forward "** Dictionaries" nil t)
              (beginning-of-line)
              ;; (message "Translation completed")
              ;; (message "Translation completed %s" translation)
              ) )
          (deactivate-mark))))) ))

(provide 'paw-gptel)
