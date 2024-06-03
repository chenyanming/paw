;;; pen/pen-go-translate.el -*- lexical-binding: t; -*-
(require 'go-translate)

(defclass pen-gt-translate-render (gt-render) ()
  :documentation "Used to save the translate result into kill ring.")

(cl-defmethod gt-output ((render pen-gt-translate-render) translator)
  (deactivate-mark)
  (when (= (oref translator state) 3)
    (let* ((ret (gt-extract render translator))
           (pen-view-note-buffer (get-buffer "*pen-view-note*"))
           (pen-sub-note-buffer (get-buffer "*pen-sub-note*")))
      (when-let (err (cl-find-if (lambda (r) (<= (plist-get r :state) 1)) ret))
        (error "%s" (plist-get err :result)))
      ;; workaround, because the result is not in the buffer at first
      (with-current-buffer
          (if (buffer-live-p pen-view-note-buffer)
              pen-view-note-buffer
            (if (buffer-live-p pen-sub-note-buffer)
                pen-sub-note-buffer
              (generate-new-buffer "*pen-view-note*")))
        (save-excursion
          (let* ((buffer-read-only nil)
                 (translation (mapconcat (lambda (r) (string-join (plist-get r :result) "\n")) ret "\n\n")))
            (goto-char (point-min))
            (search-forward "** Translation" nil t)
            (org-mark-subtree)
            (forward-line)
            (delete-region (region-beginning) (region-end))
            (pen-insert-and-make-overlay "#+BEGIN_SRC fundamental-mode\n" 'invisible t)
            (insert translation)
            (pen-insert-and-make-overlay "#+END_SRC" 'invisible t)
            (insert "\n")
            ;; (message "Translation completed")
            ;; (message "Translation completed %s" translation)
            ) )
        (deactivate-mark)))))


(defun pen-translate()
  (interactive)
  (require 'go-translate)
  (gt-do-translate))

(defun pen-nov-translate()
  (interactive)
  (require 'go-translate)
  (gt-start
   (gt-translator
    :taker (gt-taker :langs '(en zh) :text
                     (lambda()
                       (cond ((use-region-p)
                              (replace-regexp-in-string "[ \n]+" " " (replace-regexp-in-string "^[ \n]+" "" (buffer-substring-no-properties (region-beginning) (region-end)))))
                             (t (current-word t t)))))
    :engines (list (gt-bing-engine)
                   (gt-google-engine)
                   (gt-google-rpc-engine)
                   (gt-youdao-dict-engine)
                   (gt-youdao-suggest-engine))
    :render (gt-buffer-render)) ))

(defun pen-go-translate-insert(&optional word)
  (interactive)
  (require 'go-translate)
  (gt-start
   (gt-translator
    :taker (gt-taker :langs '(en zh ja) :text
                     (lambda()
                       (let ((word (if word (replace-regexp-in-string "[ \n]+" " " (replace-regexp-in-string "^[ \n]+" "" word)) nil)))
                         (cond ((use-region-p)
                                (buffer-substring-no-properties (region-beginning) (region-end)))
                               (t (if word word (current-word t t)))))))
    :engines (list (gt-bing-engine))
    :render (pen-gt-translate-render))))

(provide 'pen-go-translate)
