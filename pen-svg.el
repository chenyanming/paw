;;; pen/pen-svg.el -*- lexical-binding: t; -*-

(require 'svg-lib)

(defconst pen-svg-enable nil)
(defconst pen-pbm-enable t)

(defun pen-star-face-icon ()
  (or
   (if pen-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "star-face" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :radius 0 ))
         ('dark
          (svg-lib-icon "star-face" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :radius 0 :foreground "yellow" :background (face-attribute 'default :background)))))
   ""))



(defun pen-word-icon ()
  (or
   (if pen-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "star" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :radius 0 ))
         ('dark
          (svg-lib-icon "star" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :radius 0 :foreground "yellow" :background (face-attribute 'default :background)))))
   ""))


(defun pen-question-icon ()
  (or
   (if pen-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "help" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "red"))
         ('dark
          (svg-lib-icon "help" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "red" :background (face-attribute 'default :background)))))
   "?"))


(defun pen-todo-icon ()
  (or
   (if pen-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "checkbox-blank-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "checkbox-blank-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "□"))


(defun pen-done-icon ()
    (or
     (if pen-svg-enable
         (pcase (frame-parameter nil 'background-mode)
           ('light
            (svg-lib-icon "checkbox-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
           ('dark
            (svg-lib-icon "checkbox-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
     "✓"))

(defun pen-cancel-icon ()
  (or
   (if pen-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "close-box-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "close-box-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "✗"))

(defun pen-bookmark-icon ()
  (or
   (if pen-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "bookmark" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "bookmark-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "⟦"))

(defun pen-file-link-icon ()
  (or
   (if pen-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "file" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "file-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "⟷"))

(defun pen-url-link-icon ()
  (or
   (if pen-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "link" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "link" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "➔"))

(defun pen-annotation-link-icon ()
  (or
   (if pen-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "open-in-new" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "open-in-new" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "❰"))

(defun pen-attachment-icon ()
  (or
   (if pen-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "paperclip" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "paperclip" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "❱"))

(defun pen-image-icon ()
  (or
   (if pen-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "image" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "image-outline" nil :scale 1 :height (if IS-WINDOWS 0.5 0.9) :margin (if IS-WINDOWS -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   "⟨"))


(defun pen-play-youdao-button (&optional callback)
  (cond (pen-svg-enable (svg-lib-button "[play]" (or callback 'pen-play-youdao-button-function)))
        (pen-pbm-enable (let* ((image (create-image (expand-file-name "modules/pen/images/play.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'pen-play-youdao-button-function))
                          (define-key map (kbd "<return>") (or callback 'pen-play-youdao-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (buttonize (format  "[▶]") (lambda (arg) (funcall (or callback 'pen-play-youdao-button-function)))))))

(defun pen-play-youdao-button-function (&optional arg)
  (interactive)
  (funcall pen-read-function-1 (pen-get-real-word pen-note-word)))

(defun pen-play-button (&optional callback)
  (cond (pen-svg-enable (svg-lib-button "[play]" (or callback 'pen-play-button-function)))
        (pen-pbm-enable (let* ((image (create-image (expand-file-name "modules/pen/images/play.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'pen-play-button-function))
                          (define-key map (kbd "<return>") (or callback 'pen-play-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (buttonize (format  "[▶]") (lambda (arg) (funcall (or callback 'pen-play-button-function)))))))

(defun pen-play-button-function (&optional arg)
  (interactive)
  (funcall pen-read-function-2 (pen-get-real-word pen-note-word)))

(defun pen-add-button (&optional callback)
  (cond (pen-svg-enable (svg-lib-button "[plus]" (or callback 'pen-add-button-function)))
        (pen-pbm-enable (let* ((image (create-image (expand-file-name "modules/pen/images/plus.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'pen-add-button-function))
                          (define-key map (kbd "<return>") (or callback 'pen-add-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (buttonize (format  "[+]") (lambda (arg) (funcall (or callback 'pen-add-button-function)))))))

(defun pen-add-button-function (&optional arg)
  (interactive)
  (funcall-interactively 'pen-add-online-word pen-note-word))

(defun pen-edit-button (&optional callback)
  (cond (pen-svg-enable (svg-lib-button "[pencil]" (or callback 'pen-edit-button-function)))
        (pen-pbm-enable (let* ((image (create-image (expand-file-name "modules/pen/images/file-edit-outline.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'pen-edit-button-function))
                          (define-key map (kbd "<return>") (or callback 'pen-edit-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (buttonize (format  "[E]") (lambda (arg) (funcall (or callback 'pen-edit-button-function)))))))

(defun pen-edit-button-function(&optional arg)
  (interactive)
  (funcall 'pen-find-note))

(defun pen-delete-button (&optional callback)
  (cond (pen-svg-enable (svg-lib-button "[delete]" (or callback 'pen-delete-button-function)))
        (pen-pbm-enable (let* ((image (create-image (expand-file-name "modules/pen/images/delete-outline.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'pen-delete-button-function))
                          (define-key map (kbd "<return>") (or callback 'pen-delete-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (buttonize (format  "[-]") (lambda (arg) (funcall (or callback 'pen-delete-button-function)))))))

(defun pen-delete-button-function(&optional arg)
  (interactive)
  (funcall 'pen-delete-word)
  (when (get-buffer "*pen-view-note*")
    (pen-view-note-quit)))

(defun pen-stardict-button ()
  (if pen-svg-enable
      (svg-lib-button "[text-search] Sdcv" 'pen-stardict-button-function)
    (format "%s" (buttonize "<Sdcv>" 'pen-stardict-button-function) )))

(defun pen-stardict-button-function (&optional arg)
  (interactive)
  (funcall pen-stardict-function pen-note-word))

(defun pen-goldendict-button (&optional callback)
  (cond (pen-svg-enable (svg-lib-button "[text-search] Eudic" (or callback 'pen-goldendict-button-function)))
        (pen-pbm-enable (let* ((image (create-image (expand-file-name "modules/pen/images/open-in-new.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'pen-goldendict-button-function))
                          (define-key map (kbd "<return>") (or callback 'pen-goldendict-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (format "%s" (buttonize "<Eudic>" (lambda (arg) (funcall (or callback 'pen-goldendict-button-function))))))))

(defun pen-goldendict-button-function (&optional arg)
  (interactive)
  (funcall pen-external-dictionary-function pen-note-word))


(defun pen-mdict-button ()
  (if pen-svg-enable
      (svg-lib-button "[text-search] Mdict" 'pen-mdict-button-function)
    (format "%s" (buttonize "<M>" 'pen-mdict-button-function) )))

(defun pen-mdict-button-function (&optional arg max-attempts)
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
        (funcall pen-mdict-dictionary-function
                 (format "http://localhost:8000/%s" pen-note-word))
      (error "Failed to start server after %d attempts" max-attempts))))

(defun pen-translate-button ()
  (cond (pen-svg-enable (svg-lib-button "[ideogram-cjk-variant] 译" 'pen-translate-button-function))
        (pen-pbm-enable (let* ((image (create-image (expand-file-name "modules/pen/images/translate.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") 'pen-translate-button-function)
                          (define-key map (kbd "<return>") 'pen-translate-button-function)
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (format "<%s>" (buttonize "译" 'pen-translate-button-function) ))))

(defun pen-translate-button-function (&optional arg)
  (interactive)
  (funcall pen-translate-function pen-note-word))

(defun pen-ai-translate-button ()
  (cond (pen-svg-enable (svg-lib-button "[ideogram-cjk-variant] AI译" 'pen-ai-translate-button-function))
        (pen-pbm-enable (let* ((image (create-image (expand-file-name "modules/pen/images/translate-variant.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") 'pen-ai-translate-button-function)
                          (define-key map (kbd "<return>") 'pen-ai-translate-button-function)
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (format "<%s>" (buttonize "AI译" 'pen-ai-translate-button-function) ))))

(defun pen-ai-translate-button-function (&optional arg)
  (interactive)
  (funcall pen-ai-translate-function pen-note-word))

(defun pen-ask-ai-button ()
  (cond (pen-svg-enable (svg-lib-button "[chat-question] Ask AI" 'pen-ask-ai-button-function))
        (pen-pbm-enable (let* ((image (create-image (expand-file-name "modules/pen/images/chat-question-outline.pbm" doom-private-dir)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") 'pen-ask-ai-button-function)
                          (define-key map (kbd "<return>") 'pen-ask-ai-button-function)
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (format "<%s>" (buttonize "Ask AI" 'pen-ask-ai-button-function) ))))

(defun pen-ask-ai-button-function (&optional arg)
  (interactive)
  (funcall pen-ai-translate-function pen-note-word (read-string "Ask AI: ")))



(defvar pen-provider-english-urls nil)
(defun pen-provider-english-urls()
  (setq pen-provider-english-urls
        (cl-loop for pen-provider in pen-provider-english-url-alist collect
                 (let* ((name (car pen-provider))
                        (url (pen-provider-lookup pen-note-word (car pen-provider))))
                   (list name url) )) ))

(defun pen-english-web-buttons ()
  (cl-loop for url in pen-provider-english-url-alist collect
           (if pen-svg-enable
               (svg-lib-button
                (format "[web] %s" (car url))
                'pen-english-web-buttons-function)
             (format "[%s]" (buttonize (format "%s" (car url)) 'pen-english-web-buttons-function) ))))

(defun pen-english-web-buttons-function (&optional arg)
  (interactive)
  (funcall pen-dictionary-browse-function
           (if pen-svg-enable
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
                                         (s-trim (car (last text-node)) )))) pen-provider-english-urls))
             (car (assoc-default (button-label (button-at (point))) pen-provider-english-urls)))))


(defvar pen-provider-japanese-urls nil)
(defun pen-provider-japanese-urls()
  (setq pen-provider-japanese-urls
        (cl-loop for pen-provider in pen-provider-japanese-url-alist collect
                 (let* ((name (car pen-provider))
                        (url (pen-provider-lookup pen-note-word (car pen-provider))))
                   (list name url) )) ))

(defun pen-japanese-web-buttons ()
  (cl-loop for url in pen-provider-japanese-url-alist collect
           (if pen-svg-enable
               (svg-lib-button
                (format "[web] %s" (car url))
                'pen-japanese-web-buttons-function)
             (format "[%s]" (buttonize (format "%s" (car url)) 'pen-japanese-web-buttons-function) ))))

(defun pen-japanese-web-buttons-function (&optional arg)
  (interactive)
  (funcall pen-dictionary-browse-function
           (if pen-svg-enable
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
                                       "No text found in SVG data")))) pen-provider-japanese-urls))
             (car (assoc-default (button-label (button-at (point))) pen-provider-japanese-urls)))))

(defvar pen-provider-general-urls nil)
(defun pen-provider-general-urls()
  (setq pen-provider-general-urls
        (cl-loop for pen-provider in pen-provider-general-url-alist collect
                 (let* ((name (car pen-provider))
                        (url (pen-provider-lookup pen-note-word (car pen-provider))))
                   (list name url) )) ))

(defun pen-general-web-buttons ()
  (cl-loop for url in pen-provider-general-url-alist collect
           (if pen-svg-enable
               (svg-lib-button
                (format "[web] %s" (car url))
                'pen-general-web-buttons-function)
             (format "[%s]" (buttonize (format "%s" (car url)) 'pen-general-web-buttons-function) ))))

(defun pen-general-web-buttons-function (&optional arg)
  (interactive)
  (funcall pen-dictionary-browse-function
           (if pen-svg-enable
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
                                           "No text found in SVG data")))) pen-provider-general-urls))
             (car (assoc-default (button-label (button-at (point))) pen-provider-general-urls)))))




(defvar pen-star-face-icon (pen-star-face-icon))
(defvar pen-word-icon (pen-word-icon))
(defvar pen-question-icon (pen-question-icon))
(defvar pen-todo-icon (pen-todo-icon))
(defvar pen-done-icon (pen-done-icon))
(defvar pen-cancel-icon (pen-cancel-icon))
(defvar pen-bookmark-icon (pen-bookmark-icon))
(defvar pen-file-link-icon (pen-file-link-icon))
(defvar pen-url-link-icon (pen-url-link-icon))
(defvar pen-annotation-link-icon (pen-annotation-link-icon))
(defvar pen-org-link-icon nil)
(defvar pen-attachment-icon (pen-attachment-icon))
(defvar pen-image-icon (pen-image-icon))

(defvar pen-play-youdao-button (pen-play-youdao-button))
(defvar pen-play-button (pen-play-button))
(defvar pen-add-button (pen-add-button))
(defvar pen-edit-button (pen-edit-button))
(defvar pen-delete-button (pen-delete-button))
(defvar pen-stardict-button (pen-stardict-button))
(defvar pen-goldendict-button (pen-goldendict-button))
(defvar pen-mdict-button (pen-mdict-button))
(defvar pen-translate-button (pen-translate-button))
(defvar pen-ai-translate-button (pen-ai-translate-button))
(defvar pen-ask-ai-button (pen-ask-ai-button))
(defvar pen-english-web-buttons (pen-english-web-buttons))
(defvar pen-japanese-web-buttons (pen-japanese-web-buttons))
(defvar pen-general-web-buttons (pen-general-web-buttons))


;;;###autoload
(defun pen-get-icons ()
  (interactive)
  (setq pen-star-face-icon (pen-star-face-icon))
  (setq pen-word-icon (pen-word-icon))
  (setq pen-question-icon (pen-question-icon))
  (setq pen-todo-icon (pen-todo-icon))
  (setq pen-done-icon (pen-done-icon))
  (setq pen-cancel-icon (pen-cancel-icon))
  (setq pen-bookmark-icon (pen-bookmark-icon))
  (setq pen-file-link-icon (pen-file-link-icon))
  (setq pen-url-link-icon (pen-url-link-icon))
  (setq pen-annotation-link-icon (pen-annotation-link-icon))
  (setq pen-org-link-icon nil)
  (setq pen-attachment-icon (pen-attachment-icon))
  (setq pen-image-icon (pen-image-icon)))

(defvar pen-get-buttons-p nil)

;;;###autoload
(defun pen-get-buttons ()
  (interactive)
  (unless pen-get-buttons-p
    (setq pen-play-youdao-button (pen-play-youdao-button))
    (setq pen-play-button (pen-play-button))
    (setq pen-add-button (pen-add-button))
    (setq pen-edit-button (pen-edit-button))
    (setq pen-delete-button (pen-delete-button))
    (setq pen-stardict-button (pen-stardict-button))
    (setq pen-goldendict-button (pen-goldendict-button))
    (setq pen-mdict-button (pen-mdict-button))
    (setq pen-translate-button (pen-translate-button))
    (setq pen-ai-translate-button (pen-ai-translate-button))
    (setq pen-ask-ai-button (pen-ask-ai-button))
    (setq pen-english-web-buttons (pen-english-web-buttons))
    (setq pen-japanese-web-buttons (pen-japanese-web-buttons))
    (setq pen-general-web-buttons (pen-general-web-buttons))
    (setq pen-get-buttons-p t)))

(provide 'pen-svg)
