;;; paw/paw-svg.el -*- lexical-binding: t; -*-

(require 'svg-lib nil t)

(defconst paw-svg-enable nil)
(defconst paw-pbm-enable t)

(defun paw-star-face-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "star-face" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :radius 0 ))
         ('dark
          (svg-lib-icon "star-face" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :radius 0 :foreground "yellow" :background (face-attribute 'default :background)))))
   ""))



(defun paw-word-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "star" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :radius 0 ))
         ('dark
          (svg-lib-icon "star" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :radius 0 :foreground "yellow" :background (face-attribute 'default :background)))))
   ""))


(defun paw-question-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "help" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "red"))
         ('dark
          (svg-lib-icon "help" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "red" :background (face-attribute 'default :background)))))
   "?"))


(defun paw-todo-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "checkbox-blank-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "checkbox-blank-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "□"))


(defun paw-done-icon ()
    (or
     (if paw-svg-enable
         (pcase (frame-parameter nil 'background-mode)
           ('light
            (svg-lib-icon "checkbox-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
           ('dark
            (svg-lib-icon "checkbox-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
     "✓"))

(defun paw-cancel-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "close-box-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "close-box-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "✗"))

(defun paw-bookmark-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "bookmark" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "bookmark-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "⟦"))

(defun paw-file-link-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "file" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "file-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "⟷"))

(defun paw-url-link-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "link" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "link" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "➔"))

(defun paw-annotation-link-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "open-in-new" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "open-in-new" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "❰"))

(defun paw-attachment-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "paperclip" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "paperclip" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "❱"))

(defun paw-image-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "image" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "image-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "⟨"))


(defun paw-play-youdao-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[play]" (or callback 'paw-play-youdao-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "modules/paw/images/play.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-play-youdao-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-play-youdao-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (buttonize (format  "[▶]") (lambda (arg) (funcall (or callback 'paw-play-youdao-button-function)))))))

(defun paw-play-youdao-button-function (&optional arg)
  (interactive)
  (funcall paw-read-function-1 (paw-get-real-word paw-note-word)))

(defun paw-play-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[play]" (or callback 'paw-play-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "modules/paw/images/play.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-play-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-play-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (buttonize (format  "[▶]") (lambda (arg) (funcall (or callback 'paw-play-button-function)))))))

(defun paw-play-button-function (&optional arg)
  (interactive)
  (funcall paw-read-function-2 (paw-get-real-word paw-note-word)))

(defun paw-add-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[plus]" (or callback 'paw-add-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "modules/paw/images/plus.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-add-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-add-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (buttonize (format  "[+]") (lambda (arg) (funcall (or callback 'paw-add-button-function)))))))

(defun paw-add-button-function (&optional arg)
  (interactive)
  (funcall-interactively 'paw-add-online-word paw-note-word))

(defun paw-edit-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[pencil]" (or callback 'paw-edit-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "modules/paw/images/file-edit-outline.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-edit-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-edit-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (buttonize (format  "[E]") (lambda (arg) (funcall (or callback 'paw-edit-button-function)))))))

(defun paw-edit-button-function(&optional arg)
  (interactive)
  (funcall 'paw-find-note))

(defun paw-delete-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[delete]" (or callback 'paw-delete-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "modules/paw/images/delete-outline.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-delete-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-delete-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (buttonize (format  "[-]") (lambda (arg) (funcall (or callback 'paw-delete-button-function)))))))

(defun paw-delete-button-function(&optional arg)
  (interactive)
  (funcall 'paw-delete-word)
  (when (get-buffer "*paw-view-note*")
    (paw-view-note-quit)))

(defun paw-stardict-button ()
  (if paw-svg-enable
      (svg-lib-button "[text-search] Sdcv" 'paw-stardict-button-function)
    (format "%s" (buttonize "<Sdcv>" 'paw-stardict-button-function) )))

(defun paw-stardict-button-function (&optional arg)
  (interactive)
  (funcall paw-stardict-function paw-note-word))

(defun paw-goldendict-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[text-search] Eudic" (or callback 'paw-goldendict-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "modules/paw/images/open-in-new.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-goldendict-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-goldendict-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (format "%s" (buttonize "<Eudic>" (lambda (arg) (funcall (or callback 'paw-goldendict-button-function))))))))

(defun paw-goldendict-button-function (&optional arg)
  (interactive)
  (funcall paw-external-dictionary-function paw-note-word))


(defun paw-mdict-button ()
  (if paw-svg-enable
      (svg-lib-button "[text-search] Mdict" 'paw-mdict-button-function)
    (format "%s" (buttonize "<M>" 'paw-mdict-button-function) )))

(defun paw-mdict-button-function (&optional arg max-attempts)
  (interactive)
  (require 'mdx-dictionary)

  (let ((attempts 0)
        (check-interval 2) ; sets time interval between checks
        (max-attempts (or max-attempts 3))) ; set max attempts, default to 3

    ;; Start server if not already running
    (unless (process-live-p mdx-dictionary-server-process)
      (mdx-dictionary-start-server))
    ;; Check if server is running
    (while (and (< attempts max-attempts)
                (not (process-live-p mdx-dictionary-server-process)))

      ;; If not, wait a bit and try again
      (message "Server not running, attempt %d of %d..."
               (1+ attempts) max-attempts)
      (sit-for check-interval)

      ;; Increment attempts
      (setq attempts (1+ attempts)))

    ;; After max attempts, if still not running, message error
    (if (process-live-p mdx-dictionary-server-process)
        (funcall paw-mdict-dictionary-function
                 (format "http://localhost:8000/%s" paw-note-word))
      (error "Failed to start server after %d attempts" max-attempts))))

(defun paw-translate-button ()
  (cond (paw-svg-enable (svg-lib-button "[ideogram-cjk-variant] 译" 'paw-translate-button-function))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "modules/paw/images/translate.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") 'paw-translate-button-function)
                          (define-key map (kbd "<return>") 'paw-translate-button-function)
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (format "<%s>" (buttonize "译" 'paw-translate-button-function) ))))

(defun paw-translate-button-function (&optional arg)
  (interactive)
  (funcall paw-translate-function paw-note-word))

(defun paw-ai-translate-button ()
  (cond (paw-svg-enable (svg-lib-button "[ideogram-cjk-variant] AI译" 'paw-ai-translate-button-function))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "modules/paw/images/translate-variant.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") 'paw-ai-translate-button-function)
                          (define-key map (kbd "<return>") 'paw-ai-translate-button-function)
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (format "<%s>" (buttonize "AI译" 'paw-ai-translate-button-function) ))))

(defun paw-ai-translate-button-function (&optional arg)
  (interactive)
  (funcall paw-ai-translate-function paw-note-word))

(defun paw-ask-ai-button ()
  (cond (paw-svg-enable (svg-lib-button "[chat-question] Ask AI" 'paw-ask-ai-button-function))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "modules/paw/images/chat-question-outline.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") 'paw-ask-ai-button-function)
                          (define-key map (kbd "<return>") 'paw-ask-ai-button-function)
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (format "<%s>" (buttonize "Ask AI" 'paw-ask-ai-button-function) ))))

(defun paw-ask-ai-button-function (&optional arg)
  (interactive)
  (funcall paw-ai-translate-function paw-note-word (read-string "Ask AI: ")))



(defvar paw-provider-english-urls nil)
(defun paw-provider-english-urls()
  (setq paw-provider-english-urls
        (cl-loop for paw-provider in paw-provider-english-url-alist collect
                 (let* ((name (car paw-provider))
                        (url (paw-provider-lookup paw-note-word (car paw-provider))))
                   (list name url) )) ))

(defun paw-english-web-buttons ()
  (cl-loop for url in paw-provider-english-url-alist collect
           (if paw-svg-enable
               (svg-lib-button
                (format "[web] %s" (car url))
                'paw-english-web-buttons-function)
             (format "[%s]" (buttonize (format "%s" (car url)) 'paw-english-web-buttons-function) ))))

(defun paw-english-web-buttons-function (&optional arg)
  (interactive)
  (funcall paw-dictionary-browse-function
           (if paw-svg-enable
               (car (assoc-default (let* ((mouse-point (save-excursion
                                                         (mouse-set-point last-input-event)
                                                         (point)))
                                          (props (cdr (get-text-property mouse-point 'display))))
                                     (when (eq (plist-get props :type) 'svg)
                                       (let* ((data (plist-get props :data))
                                              (buf (with-temp-buffer
                                                     (insert data)
                                                     (xml-parse-region (point-min) (point-max))))
                                              (text-node (car (xml-get-children (car buf) 'text))))
                                         (s-trim (car (last text-node)) )))) paw-provider-english-urls))
             (car (assoc-default (button-label (button-at (point))) paw-provider-english-urls)))))


(defvar paw-provider-japanese-urls nil)
(defun paw-provider-japanese-urls()
  (setq paw-provider-japanese-urls
        (cl-loop for paw-provider in paw-provider-japanese-url-alist collect
                 (let* ((name (car paw-provider))
                        (url (paw-provider-lookup paw-note-word (car paw-provider))))
                   (list name url) )) ))

(defun paw-japanese-web-buttons ()
  (cl-loop for url in paw-provider-japanese-url-alist collect
           (if paw-svg-enable
               (svg-lib-button
                (format "[web] %s" (car url))
                'paw-japanese-web-buttons-function)
             (format "[%s]" (buttonize (format "%s" (car url)) 'paw-japanese-web-buttons-function) ))))

(defun paw-japanese-web-buttons-function (&optional arg)
  (interactive)
  (funcall paw-dictionary-browse-function
           (if paw-svg-enable
               (car (assoc-default (let* ((mouse-point (save-excursion
                                                     (mouse-set-point last-input-event)
                                                     (point)))
                                      (props (cdr (get-text-property mouse-point 'display)))
                                      svg-data)
                                 (when (eq (plist-get props :type) 'svg)
                                   (setq svg-data (plist-get props :data))
                                   (with-temp-buffer
                                     (insert svg-data)
                                     (goto-char (point-min))
                                     (if (re-search-forward "<text.*?>\\s-*\\(.*?\\)</text>" nil t)
                                         (match-string-no-properties 1)
                                       "No text found in SVG data")))) paw-provider-japanese-urls))
             (car (assoc-default (button-label (button-at (point))) paw-provider-japanese-urls)))))

(defvar paw-provider-general-urls nil)
(defun paw-provider-general-urls()
  (setq paw-provider-general-urls
        (cl-loop for paw-provider in paw-provider-general-url-alist collect
                 (let* ((name (car paw-provider))
                        (url (paw-provider-lookup paw-note-word (car paw-provider))))
                   (list name url) )) ))

(defun paw-general-web-buttons ()
  (cl-loop for url in paw-provider-general-url-alist collect
           (if paw-svg-enable
               (svg-lib-button
                (format "[web] %s" (car url))
                'paw-general-web-buttons-function)
             (format "[%s]" (buttonize (format "%s" (car url)) 'paw-general-web-buttons-function) ))))

(defun paw-general-web-buttons-function (&optional arg)
  (interactive)
  (funcall paw-dictionary-browse-function
           (if paw-svg-enable
               (car (assoc-default (let* ((mouse-point (save-excursion
                                                         (mouse-set-point last-input-event)
                                                         (point)))
                                          (props (cdr (get-text-property mouse-point 'display)))
                                          svg-data)
                                     (when (eq (plist-get props :type) 'svg)
                                       (setq svg-data (plist-get props :data))
                                       (with-temp-buffer
                                         (insert svg-data)
                                         (goto-char (point-min))
                                         (if (re-search-forward "<text.*?>\\s-*\\(.*?\\)</text>" nil t)
                                             (match-string-no-properties 1)
                                           "No text found in SVG data")))) paw-provider-general-urls))
             (car (assoc-default (button-label (button-at (point))) paw-provider-general-urls)))))




(defvar paw-star-face-icon (paw-star-face-icon))
(defvar paw-word-icon (paw-word-icon))
(defvar paw-question-icon (paw-question-icon))
(defvar paw-todo-icon (paw-todo-icon))
(defvar paw-done-icon (paw-done-icon))
(defvar paw-cancel-icon (paw-cancel-icon))
(defvar paw-bookmark-icon (paw-bookmark-icon))
(defvar paw-file-link-icon (paw-file-link-icon))
(defvar paw-url-link-icon (paw-url-link-icon))
(defvar paw-annotation-link-icon (paw-annotation-link-icon))
(defvar paw-org-link-icon nil)
(defvar paw-attachment-icon (paw-attachment-icon))
(defvar paw-image-icon (paw-image-icon))

(defvar paw-play-youdao-button (paw-play-youdao-button))
(defvar paw-play-button (paw-play-button))
(defvar paw-add-button (paw-add-button))
(defvar paw-edit-button (paw-edit-button))
(defvar paw-delete-button (paw-delete-button))
(defvar paw-stardict-button (paw-stardict-button))
(defvar paw-goldendict-button (paw-goldendict-button))
(defvar paw-mdict-button (paw-mdict-button))
(defvar paw-translate-button (paw-translate-button))
(defvar paw-ai-translate-button (paw-ai-translate-button))
(defvar paw-ask-ai-button (paw-ask-ai-button))
(defvar paw-english-web-buttons (paw-english-web-buttons))
(defvar paw-japanese-web-buttons (paw-japanese-web-buttons))
(defvar paw-general-web-buttons (paw-general-web-buttons))


;;;###autoload
(defun paw-get-icons ()
  (interactive)
  (setq paw-star-face-icon (paw-star-face-icon))
  (setq paw-word-icon (paw-word-icon))
  (setq paw-question-icon (paw-question-icon))
  (setq paw-todo-icon (paw-todo-icon))
  (setq paw-done-icon (paw-done-icon))
  (setq paw-cancel-icon (paw-cancel-icon))
  (setq paw-bookmark-icon (paw-bookmark-icon))
  (setq paw-file-link-icon (paw-file-link-icon))
  (setq paw-url-link-icon (paw-url-link-icon))
  (setq paw-annotation-link-icon (paw-annotation-link-icon))
  (setq paw-org-link-icon nil)
  (setq paw-attachment-icon (paw-attachment-icon))
  (setq paw-image-icon (paw-image-icon)))

(defvar paw-get-buttons-p nil)

;;;###autoload
(defun paw-get-buttons ()
  (interactive)
  (unless paw-get-buttons-p
    (setq paw-play-youdao-button (paw-play-youdao-button))
    (setq paw-play-button (paw-play-button))
    (setq paw-add-button (paw-add-button))
    (setq paw-edit-button (paw-edit-button))
    (setq paw-delete-button (paw-delete-button))
    (setq paw-stardict-button (paw-stardict-button))
    (setq paw-goldendict-button (paw-goldendict-button))
    (setq paw-mdict-button (paw-mdict-button))
    (setq paw-translate-button (paw-translate-button))
    (setq paw-ai-translate-button (paw-ai-translate-button))
    (setq paw-ask-ai-button (paw-ask-ai-button))
    (setq paw-english-web-buttons (paw-english-web-buttons))
    (setq paw-japanese-web-buttons (paw-japanese-web-buttons))
    (setq paw-general-web-buttons (paw-general-web-buttons))
    (setq paw-get-buttons-p t)))

(provide 'paw-svg)