;;; paw-eww.el -*- lexical-binding: t; -*-

(defun paw-eww-search-with-dictionary-render (status url &optional point word buffer)
  (let* ((eww-buffer (generate-new-buffer "*paw-eww*"))
         (result (progn
                   (eww-render status url nil eww-buffer)
                   (substring-no-properties
                    (with-current-buffer eww-buffer
                      (buffer-string))))))
    (kill-buffer eww-buffer)
    (with-current-buffer buffer
      (let ((buffer-read-only nil))
        (save-excursion
          (goto-char (point-min))
          (if (string= sdcv-fail-notify-string result) ;; if no result, goto Translation
              (search-forward "** Translation" nil t)
            (search-forward "** Meaning" nil t)
            (org-mark-subtree)
            (forward-line)
            (delete-region (region-beginning) (region-end))
            (if (string= paw-view-note-meaning-src-lang "org")
                (paw-insert-and-make-overlay (format "%s" result) 'face `(:background ,paw-view-note-background-color :extend t))
              (progn
                ;; (paw-insert-and-make-overlay "#+BEGIN_SRC org\n" 'invisible t)
                ;; (insert "#+BEGIN_SRC org\n")
                (if word
                    (insert (replace-regexp-in-string (concat "\\b" word "\\b") (concat "*" word "*") result))
                  (insert result))

                ;; (paw-insert-and-make-overlay "#+END_SRC" 'invisible t)
                ;; (insert "#+END_SRC")

                ))
            (insert "\n"))) )
      (deactivate-mark))))

(defun paw-eww-search-with-dictionary (&optional word buffer)
  (interactive)
  (require 'eww)
  (let ((url (car (assoc-default "TIO" (paw-provider-english-urls)))))
    (url-retrieve url #'paw-eww-search-with-dictionary-render (list url (point) word (or buffer (current-buffer))))))

(defun paw-eww-browse-url (url)
  (require 'eww)
  (url-retrieve url #'paw-eww-search-with-dictionary-render (list url (point) nil (current-buffer))))

(provide 'paw-eww)
