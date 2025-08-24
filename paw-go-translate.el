;;; paw-go-translate.el -*- lexical-binding: t; -*-
(require 'paw-vars)
(require 'gt)
(require 'immersive-translate)
(require 'dash)

(defcustom paw-go-translate-langs '(en zh ja)
  "The languages to translate. If `paw-detect-language-p' is t, then will
detect the language first, and append it to
`paw-go-translate-langs' to translate."
  :type 'list
  :group 'paw)

(define-obsolete-variable-alias 'paw-go-transalte-langs
  'paw-go-translate-langs "paw 1.1.1")

(defclass paw-gt-translate-render (gt-render)
  ((buffer-name     :initarg :buffer-name     :initform nil)
   (section     :initarg :section     :initform nil))
  :documentation "Used to save the translate result into BUFFER.")

;; override the original gt-init, to remove the Processing message
(cl-defmethod gt-init :around ((render gt-render) translator)
  (gt-log-funcall "init (%s %s)" render translator)
  (condition-case err
      (progn (cl-call-next-method render translator)
             (gt-update translator))
    (error (gt-log 'render (format "%s initialize failed, abort" (eieio-object-class render)))
           (user-error (format "[output init error] %s" err)))))

(cl-defmethod gt-output ((render paw-gt-translate-render) translator)
  (deactivate-mark)
  (when (= (oref translator state) 3)
    (let* ((ret (gt-extract-data render translator))
           (buffer (get-buffer (oref render buffer-name)))
           (section (oref render section)))
      (if-let (err (cl-find-if (lambda (r) (<= (plist-get r :state) 1)) ret))
          (setq paw-go-translate-running-p nil)
        ;; (error "%s" (plist-get err :result))
        ;; (error "Translation Error")
        (if (buffer-live-p buffer)
            (with-current-buffer buffer
              (save-excursion
                (let* ((buffer-read-only nil)
                       (translation (mapconcat (lambda (r) (string-join (plist-get r :result) "\n")) ret "\n\n")))
                  (unless (string-match-p section (org-no-properties (org-get-heading t t t t)))
                    (goto-char (point-min))
                    (search-forward (format "** %s" section) nil t))
                  (org-end-of-subtree t t)
                  ;; (forward-line)
                  ;; (delete-region (region-beginning) (region-end))
                  (let ((bg-color paw-view-note-background-color))
                    (paw-insert-and-make-overlay
                     translation
                     'face `(:background ,bg-color :extend t))
                    (insert "\n"))
                  (if (or paw-ask-ai-p paw-ai-translate-p paw-ai-translate-context-p)
                      (insert "\n"))
                  (beginning-of-line)
                  ;; (message "Translation completed")
                  ;; (message "Translation completed %s" translation)
                  ) )
              (deactivate-mark))))))
  (setq paw-go-translate-running-p nil))


(defun paw-translate()
  "HACK: Override the original immersive-translate functions.
Because the original functions don't work well on many cases,
especially on nov-mode and org-mode. So hack them here, it may
not need if immersive-translate improve in the future."
  (interactive)
  ;; put advice here, if user don't call this function, it will not override the original functions
  (advice-add #'immersive-translate--get-paragraph :override 'paw-immersive-translate--get-paragraph)
  (advice-add #'immersive-translate-end-of-paragraph :override 'paw-immersive-translate-end-of-paragraph)
  (advice-add #'immersive-translate-region :override 'paw-immersive-translate-region)
  (advice-add #'immersive-translate--format-translation :override 'paw-immersive-translate--format-translation)
  (advice-add #'immersive-translate--add-ov :override 'paw-immersive-translate--add-ov)
  (advice-add #'immersive-translate--translation-exist-p :override 'paw-immersive-translate--translation-exist-p)

  (immersive-translate-paragraph))

(defun paw-translate-clear()
  "Clear the translation overlay."
  (interactive)
  (when (featurep 'immersive-translate)
    (dolist (ov immersive-translate--translation-overlays)
      (delete-overlay ov))
    (setq immersive-translate--translation-overlays nil)))



(defun paw-immersive-translate()
  "HACK: Override the original immersive-translate functions.
Because the original functions don't work well on many cases,
especially on nov-mode and org-mode. So hack them here, it may
not need if immersive-translate improve in the future."
  (interactive)
  ;; put advice here, if user don't call this function, it will not override the original functions
  (advice-add #'immersive-translate--get-paragraph :override 'paw-immersive-translate--get-paragraph)
  (advice-add #'immersive-translate-end-of-paragraph :override 'paw-immersive-translate-end-of-paragraph)
  (advice-add #'immersive-translate-region :override 'paw-immersive-translate-region)
  (advice-add #'immersive-translate--format-translation :override 'paw-immersive-translate--format-translation)
  (advice-add #'immersive-translate--add-ov :override 'paw-immersive-translate--add-ov)
  (advice-add #'immersive-translate--translation-exist-p :override 'paw-immersive-translate--translation-exist-p)


  (if immersive-translate--translation-overlays
      (paw-translate-clear)
    (immersive-translate-buffer)))

(defun paw-immersive-translate--get-paragraph ()
  "TODO Return the paragraph or line at point."
  (pcase major-mode
    ('Info-mode
     (immersive-translate--info-get-paragraph))
    ('helpful-mode
     (immersive-translate--helpful-get-paragraph))
    ((pred immersive-translate--elfeed-tube-p)
     (immersive-translate--elfeed-tube-get-paragraph))
    ((or 'elfeed-show-mode 'mu4e-view-mode)
     (immersive-translate--elfeed-get-paragraph))
    ('org-mode
     (let ((paragraph (thing-at-point 'paragraph t)))
       ;; HACK for org-media-note
       (replace-regexp-in-string "[0-2]?[0-9]:[0-5][0-9]:[0-5][0-9]"
                                 ""
                                 (replace-regexp-in-string "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]"
                                                           "\\1"
                                                           (if (save-excursion (re-search-forward "[0-2]?[0-9]:[0-5][0-9]:[0-5][0-9]" nil t) )
                                                               (thing-at-point 'line t)
                                                             (if paragraph
                                                                 paragraph
                                                               (thing-at-point 'line t)))))))
    (_ (thing-at-point 'paragraph t))))


(defun paw-immersive-translate--format-translation (str marker)
  "Function which produces the string to insert as a translation.

STR is the original translation. MARKER is the position where the
translation should be inserted."
  (with-temp-buffer
    (insert str)
    ;; (fill-region-as-paragraph (point-min) (point-max))
    (concat
     "\n"
     (replace-regexp-in-string
      "^"
      (immersive-translate--get-indent marker)
      (let ((string (buffer-substring-no-properties (point-min) (point-max))))
        (eval
         `(thread-last
            ,string
            ,@immersive-translate-translation-filter-functions))))
     "\n")))


(defun paw-immersive-translate-end-of-paragraph ()
  "TODO: Move to the end of the current paragraph or line."
  (pcase major-mode
    ((and (or 'elfeed-show-mode 'mu4e-view-mode)
          (pred (not immersive-translate--elfeed-tube-p)))
     (unless (get-text-property (point) 'immersive-translate--beg)
       (text-property-search-backward 'immersive-translate--beg))
     (text-property-search-forward 'immersive-translate--end)
     (end-of-line))
    ;; HACK for org-media-note
    ('org-mode (if (save-excursion (re-search-forward "[0-2]?[0-9]:[0-5][0-9]:[0-5][0-9]" nil t))
                   (end-of-line)
                 (if (thing-at-point 'paragraph t)
                     (end-of-paragraph-text)
                   (end-of-line))))
    (_ (end-of-paragraph-text))))


(defun paw-immersive-translate-region (start end)
  "TODO Translate the text between START and END."
  (save-excursion
    (goto-char start)
    (pcase major-mode
      ((and (or 'elfeed-show-mode 'mu4e-view-mode)
            (pred (not immersive-translate--elfeed-tube-p)))
       (while (and (text-property-search-forward 'immersive-translate--end)
                   (< (point) end))
         (immersive-translate-paragraph)))
      (_ (while (and
                 (< (point) end)
                 ;; (re-search-forward "^\\s-*$" end 'noerror)
                 (not (eobp)))
           (forward-line)
           (immersive-translate-paragraph))))))


(defun paw-immersive-translate--add-ov (response)
  "TODO Add an after-string overlay after point.

The value of after-string is RESPONSE."
  (let ((ovs (overlays-in (1- (point)) (point)))
        (new-ov (make-overlay (point) (1+ (point)))))
    (mapc (lambda (ov)
            (when (overlay-get ov 'after-string)
              (delete-overlay ov)))
          ovs)
    (overlay-put new-ov
                 'after-string
                 (propertize response 'face 'paw-translate-face))
    (push new-ov immersive-translate--translation-overlays)))


(defun paw-immersive-translate--translation-exist-p ()
  "TODO Return non-nil if the current paragraph has been translated."
  (save-excursion
    (immersive-translate-end-of-paragraph)
    (when-let ((overlays (overlays-in (1- (point)) (1+ (point)))))
      (cl-some (lambda (ov)
                 (and (overlay-get ov 'after-string)
                      (not (overlay-get ov 'paw-inline-note-word))))
               overlays))))

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

(defun paw-go-translate-insert(&optional word lang buffer section)
  "Translate the WORD and insert the result into BUFFER on SECTION.
if `paw-detect-language-p' is t, then will detect the language of WORD
first, and append it to `paw-go-translate-langs' to translate."
  (interactive)
  (setq paw-go-translate-running-p t)
  (let* ((detected-lang (paw-go-translate-detect-language-convert (if lang lang (paw-check-language word))))
         (langs (-union `(,(intern detected-lang)) paw-go-translate-langs)))
    (gt-start
     (gt-translator
      :taker (gt-taker :langs langs :text
                       (lambda()
                         (let ((word (if word (replace-regexp-in-string "[ \n]+" " " (replace-regexp-in-string "^[ \n]+" "" word)) nil)))
                           (cond ((use-region-p)
                                  (buffer-substring-no-properties (region-beginning) (region-end)))
                                 (t (if word word (current-word t t)))))))
      :engines (list (gt-bing-engine))
      :render (paw-gt-translate-render :buffer-name (buffer-name buffer)
                                       :section (or section "Translation")))) ))

(provide 'paw-go-translate)
