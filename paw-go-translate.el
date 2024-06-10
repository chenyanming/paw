;;; paw-go-translate.el -*- lexical-binding: t; -*-
(require 'go-translate)

(defcustom paw-go-transalte-langs '(en zh ja)
  "The languages to translate."
  :type 'list
  :group 'paw)

(defclass paw-gt-translate-render (gt-render) ()
  :documentation "Used to save the translate result into kill ring.")

(cl-defmethod gt-output ((render paw-gt-translate-render) translator)
  (deactivate-mark)
  (when (= (oref translator state) 3)
    (let* ((ret (gt-extract render translator))
           (paw-view-note-buffer (get-buffer "*paw-view-note*")))
      (when-let (err (cl-find-if (lambda (r) (<= (plist-get r :state) 1)) ret))
        (error "%s" (plist-get err :result)))
      ;; workaround, because the result is not in the buffer at first
      (with-current-buffer
          (if (buffer-live-p paw-view-note-buffer)
              paw-view-note-buffer
            (generate-new-buffer "*paw-view-note*"))
        (save-excursion
          (let* ((buffer-read-only nil)
                 (translation (mapconcat (lambda (r) (string-join (plist-get r :result) "\n")) ret "\n\n")))
            (goto-char (point-min))
            (search-forward "** Translation" nil t)
            (org-mark-subtree)
            (forward-line)
            (delete-region (region-beginning) (region-end))
            (paw-insert-and-make-overlay "#+BEGIN_SRC fundamental-mode\n" 'invisible t)
            (insert translation)
            (paw-insert-and-make-overlay "#+END_SRC" 'invisible t)
            (insert "\n")
            ;; (message "Translation completed")
            ;; (message "Translation completed %s" translation)
            ) )
        (deactivate-mark)))))


(defun paw-translate()
  (interactive)
  (require 'go-translate)
  (gt-do-translate))

(defun paw-nov-translate()
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

(defun paw-go-translate-insert(&optional word)
  (interactive)
  (require 'go-translate)
  (gt-start
   (gt-translator
    :taker (gt-taker :langs paw-go-transalte-langs :text
                     (lambda()
                       (let ((word (if word (replace-regexp-in-string "[ \n]+" " " (replace-regexp-in-string "^[ \n]+" "" word)) nil)))
                         (cond ((use-region-p)
                                (buffer-substring-no-properties (region-beginning) (region-end)))
                               (t (if word word (current-word t t)))))))
    :engines (list (gt-bing-engine))
    :render (paw-gt-translate-render))))

(provide 'paw-go-translate)
