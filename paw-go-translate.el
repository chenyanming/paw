;;; paw-go-translate.el -*- lexical-binding: t; -*-
(require 'paw-vars)
(require 'go-translate)

(defcustom paw-go-transalte-langs '(en zh ja)
  "The languages to translate. If `paw-detect-language-p' is t, then will
detect the language first, and append it to
`paw-go-transalte-langs' to translate."
  :type 'list
  :group 'paw)

(defclass paw-gt-translate-render (gt-render)
  ((buffer
    :initarg :buffer
    :initform nil
    :type (or buffer null)
    :documentation "If this is not nil, then will override the default output logic."))
  :documentation "Used to save the translate result into BUFFER.")

(cl-defmethod gt-output ((render paw-gt-translate-render) translator)
  (deactivate-mark)
  (when (= (oref translator state) 3)
    (let* ((ret (gt-extract render translator))
           (buffer
            (if (buffer-live-p (oref render buffer))
                (oref render buffer)
              ;; FIXME: sometimes, the buffer was killed, we have to get it
              ;; again, this may have issues if both view note and sub view note
              ;; are presented. Here we simply show the translation in the sub
              ;; note buffer first.
              (if (buffer-live-p (get-buffer paw-view-note-sub-buffer-name) )
                  (get-buffer paw-view-note-sub-buffer-name)
                (get-buffer paw-view-note-buffer-name)))))
      (when-let (err (cl-find-if (lambda (r) (<= (plist-get r :state) 1)) ret))
        (setq paw-go-translate-running-p nil)
        (error "%s" (plist-get err :result)))
      (with-current-buffer buffer
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
          )
        (deactivate-mark))))
  (setq paw-go-translate-running-p nil))


(defun paw-translate()
  (interactive)
  (gt-do-translate))

(defun paw-nov-translate()
  (interactive)
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

(defun paw-go-translate-detect-language-convert(lang)
  "TODO Convert the detected langauge to go-translate recognized language."
  (pcase lang
    ("zh-Hant" "zh")
    (_ lang)))

(defun paw-go-translate-insert(&optional word buffer)
  "Translate the WORD and insert the result into BUFFER.
if `paw-detect-language-p' is t, then will detect the language of WORD
first, and append it to `paw-go-transalte-langs' to translate."
  (interactive)
  (setq paw-go-translate-running-p t)
  (let* ((detected-lang (paw-go-translate-detect-language-convert (paw-check-language word) ))
         (langs (-union `(,(intern detected-lang)) paw-go-transalte-langs)))
    (gt-start
     (gt-translator
      :taker (gt-taker :langs langs :text
                       (lambda()
                         (let ((word (if word (replace-regexp-in-string "[ \n]+" " " (replace-regexp-in-string "^[ \n]+" "" word)) nil)))
                           (cond ((use-region-p)
                                  (buffer-substring-no-properties (region-beginning) (region-end)))
                                 (t (if word word (current-word t t)))))))
      :engines (list (gt-bing-engine))
      :render (paw-gt-translate-render :buffer buffer))) ))

(provide 'paw-go-translate)
