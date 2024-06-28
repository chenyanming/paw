;;; paw-go-translate.el -*- lexical-binding: t; -*-
(require 'paw-vars)
(require 'go-translate)
(require 'dash)

(defcustom paw-go-transalte-langs '(en zh ja)
  "The languages to translate. If `paw-detect-language-p' is t, then will
detect the language first, and append it to
`paw-go-transalte-langs' to translate."
  :type 'list
  :group 'paw)

(defclass paw-gt-translate-render (gt-render)
  ((buffer-name     :initarg :buffer-name     :initform nil))
  :documentation "Used to save the translate result into BUFFER.")

;; override the original gt-init, to remove the Processing message
(cl-defmethod gt-init :around ((render gt-render) translator)
  (gt-log-funcall "init (%s %s)" render translator)
  (condition-case err
      (progn (cl-call-next-method render translator)
             (gt-update-state translator))
    (error (gt-log 'render (format "%s initialize failed, abort" (eieio-object-class render)))
           (user-error (format "[output init error] %s" err)))))

(cl-defmethod gt-output ((render paw-gt-translate-render) translator)
  (deactivate-mark)
  (when (= (oref translator state) 3)
    (let* ((ret (gt-extract render translator))
           (buffer (get-buffer (oref render buffer-name))))
      (when-let (err (cl-find-if (lambda (r) (<= (plist-get r :state) 1)) ret))
        (setq paw-go-translate-running-p nil)
        (error "%s" (plist-get err :result)))
      (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (let* ((buffer-read-only nil)
                 (translation (mapconcat (lambda (r) (string-join (plist-get r :result) "\n")) ret "\n\n")))
            (goto-char (point-min))
            (search-forward "** Translation" nil t)
            (org-mark-subtree)
            (forward-line)
            (delete-region (region-beginning) (region-end))
            (paw-insert-and-make-overlay (concat translation "\n" ) 'face 'org-block)
            (goto-char (point-min))
            (search-forward "** Dictionaries" nil t)
            (beginning-of-line)
            ;; (message "Translation completed")
            ;; (message "Translation completed %s" translation)
            )
          (deactivate-mark)))))
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

(defun paw-go-translate-insert(&optional word lang buffer)
  "Translate the WORD and insert the result into BUFFER.
if `paw-detect-language-p' is t, then will detect the language of WORD
first, and append it to `paw-go-transalte-langs' to translate."
  (interactive)
  (setq paw-go-translate-running-p t)
  (let* ((detected-lang (paw-go-translate-detect-language-convert (if lang lang (paw-check-language word))))
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
      :render (paw-gt-translate-render :buffer-name (buffer-name buffer)))) ))

(provide 'paw-go-translate)
