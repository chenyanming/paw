;;; pen/pen-org.el -*- lexical-binding: t; -*-

(require 'json)
(require 'pen-db)
(require 'pen-util)
(require 'svg-lib nil t)
(require 'posframe)

(defcustom pen-note-dir org-directory
  "pen note dir"
  :group 'pen
  :type 'string)


(defvar pen-file-property-doc-file "FILE_PATH")
(defvar pen-file-property-note-location "FILE_LOCATION")

;;; pen-note-mode

(defvar pen-note-target-buffer nil)
(defvar pen-note-word nil)
(defvar pen-note-entry nil)
(defvar pen-note-origin-type nil)
(defvar pen-note-origin-path nil)
(defvar pen-note-header-function #'pen-note-header
  "Function that returns the string to be used for the Calibredb edit note header.")

(defvar pen-current-entry nil)
(defvar pen-entries-history nil)
(defvar pen-entries-history-max 5)

(defvar pen-note-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'pen-send-edited-note)
    (define-key map "\C-c\C-k" 'pen-note-quit)
    map)
  "Keymap for `pen-note-mode'.")

(evil-define-key 'normal pen-note-mode-map
  (kbd "&") 'pen-find-origin-in-note
  (kbd "q") 'pen-note-quit)

(define-derived-mode pen-note-mode org-mode "pen-note"
  "Major mode for display word lists.
\\{pen-note-mode-map}"
  (setq header-line-format '(:eval (funcall pen-note-header-function)))
  (if (boundp org-download-image-dir)
      (setq-local org-download-image-dir (expand-file-name (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) pen-note-dir))))

(defun pen-note-header ()
  "TODO: Return the string to be used as the Calibredb edit note header."
  (format "%s -> Edit Notes. %s %s"
          (propertize pen-note-word 'face 'pen-note-header-title-face)
          "Finish 'C-c C-c',"
          "abort 'C-c C-k'."))

(defun pen-insert-note (entry &optional no-note-header find-note export no-dictionary)
  "Format ENTRY and output the org file content."
  (let* ((word (pen-get-real-word entry))
         (exp (alist-get 'exp entry))
         (content (alist-get 'content entry))
         (content-json (condition-case nil
                           (let ((output (json-read-from-string content)))
                             (if (and (not (eq output nil))
                                      (not (arrayp output))
                                      (not (numberp output)))
                                 output
                               nil))
                         (error nil)))
         (content-filename (or (alist-get 'filename content-json) ""))
         (content-path (or (alist-get 'path content-json) ""))
         (note (alist-get 'note entry))
         (note-type (alist-get 'note_type entry))
         (serverp (alist-get 'serverp entry))
         (origin-type (alist-get 'origin_type entry))
         (origin-id (alist-get 'origin_id entry))
         (origin-path (alist-get 'origin_path entry))
         (origin-point (alist-get 'origin_point entry))
         (created-at (alist-get 'created_at entry))
         (kagome (alist-get 'kagome entry))
         beg)

    ;; workaround: avoid org-modern clear my display
    (if (bound-and-true-p org-modern-mode)
        (setq-local font-lock-unfontify-region-function 'pen-note--unfontify))
    (insert "* ")
    (insert (s-collapse-whitespace word) " ")
    (insert "\n")
    (pcase origin-type
      ('nov-mode
       (org-entry-put nil pen-file-property-doc-file origin-path))
      ('pdf-view-mode
       (org-entry-put nil pen-file-property-doc-file origin-path))
      ('wallabag-entry-mode
       (require 'wallabag)
       (org-entry-put nil pen-file-property-doc-file (number-to-string (if (numberp origin-id) origin-id 0))))
      (_
       (if origin-path
           (org-entry-put nil pen-file-property-doc-file origin-path) )))
    (pcase origin-type
      ('nov-mode
       (org-entry-put nil pen-file-property-note-location (replace-regexp-in-string "\n" "" (pp-to-string origin-point))))
      ('wallabag-entry-mode
       (org-entry-put nil pen-file-property-note-location (replace-regexp-in-string "\n" "" (pp-to-string origin-point))))
      ('pdf-view-mode
       (org-entry-put nil pen-file-property-note-location (replace-regexp-in-string "\n" "" (pp-to-string origin-point))))
      (_
       (if origin-point
           (org-entry-put nil pen-file-property-note-location (replace-regexp-in-string "\n" "" (pp-to-string origin-point))))))
    (if created-at
        (org-entry-put nil "CREATED_AT" created-at))
    (pcase (car note-type)
      ('image
       (insert "#+attr_org: :width 600px\n")
       (insert (format "[[file:%s][file:%s]]" (expand-file-name content-path pen-note-dir) (expand-file-name content-path pen-note-dir)))
       (insert "\n"))
      ('attachment
       (pcase (downcase (file-name-extension content-filename) )
         ((or "jpg" "jpeg" "png" "gif")
          (insert "#+attr_org: :width 600px\n")
          (insert (format "[[file:%s][file:%s]]" (expand-file-name content-path pen-note-dir) (expand-file-name content-path pen-note-dir))))
         (_ (insert (format "%s [[file:%s][%s]]" (pen-attach-icon-for (expand-file-name content-path pen-note-dir))
                            (expand-file-name content-path pen-note-dir) content-filename))))
       (insert "\n"))
      ;; (_ (insert "#+begin_quote\n" (pcase (car note-type)
      ;;                                ((or 'word 'question 'bookmark) (format "%s" (cdr note-type)))
      ;;                                ('stamp (if (stringp (cdr note-type))
      ;;                                            (cdr note-type) ""))
      ;;                                ('todo "- [ ] ")
      ;;                                ('done "- [X] ")
      ;;                                ('cancel "- [X] ")
      ;;                                (_ "")) word "\n#+end_quote\n\n"))
      (_ nil)

      )


    (unless find-note
      (pcase (car note-type)
        ((or 'image 'attachment) nil)
        (_
         (insert "** Websites ")
         (if (string= (pen-check-language word) "en")
             ;; insert all english buttons
             (progn
               (pen-provider-english-urls)
               (cl-loop for button in pen-english-web-buttons do
                        (insert button " ")) )
           ;; insert all japanese buttons
           (pen-provider-japanese-urls)
           (cl-loop for button in pen-japanese-web-buttons do
                    (insert button " ")))

         (insert "\n")
         (insert "** Search ")
         (pen-provider-general-urls)
         (cl-loop for button in pen-general-web-buttons do
                  (insert button " "))

         (insert pen-stardict-button " ")
         (insert pen-mdict-button " ")
         (insert "\n")

         )

        ) )

    (unless find-note
      (pcase (car note-type)
        ((or 'image 'attachment) nil)
        (_
         (insert "** Translation ")
         (insert pen-translate-button " ")
         (insert pen-ai-translate-button " ")
         (insert pen-ask-ai-button " ")
         (insert "\n")


         (when (and (stringp exp) (not (string= exp "")))
           (insert "** Saved Meanings ")
           (insert pen-play-youdao-button " ")
           (insert pen-play-button " ")
           (if (eq serverp 3)
               (insert pen-add-button " ")
             (insert pen-edit-button " ")
             (insert pen-delete-button " "))
           (insert pen-goldendict-button " ")
           (insert "\n")
           (pen-insert-and-make-overlay "#+BEGIN_SRC org\n" 'invisible t export)
           (insert (substring-no-properties exp))
           (pen-insert-and-make-overlay "\n#+END_SRC" 'invisible t export)
           (insert "\n"))

         ;; TODO use unique overlay instead of search string
         (unless no-dictionary
           (pen-insert-and-make-overlay "** Dictionary " 'pen-dictionary-word word)
           (insert pen-play-youdao-button " ")
           (insert pen-play-button " ")
           (if (eq serverp 3)
               (insert pen-add-button " ")
             (insert pen-edit-button " ")
             (insert pen-delete-button " "))
           (insert pen-goldendict-button " ")
           (insert "\n")
           (if kagome
               (insert kagome)
             (pne-sdcv-search-with-dictionary-async word sdcv-dictionary-simple-list)
             (pen-insert-and-make-overlay "#+BEGIN_SRC sdcv\n" 'invisible t export)
             (setq sdcv-current-translate-object word)
             ;; (insert (replace-regexp-in-string "^\\*" "-" (sdcv-search-with-dictionary word sdcv-dictionary-simple-list)) "\n")
             (insert sdcv-fail-notify-string "\n")
             (pen-insert-and-make-overlay "#+END_SRC" 'invisible t export)
             (insert "\n")) ))))

    (when find-note
      (insert "** Saved Meanings\n")
      (if (stringp exp)
          (insert (substring-no-properties exp))
        (insert "\n\n")))


    (unless no-note-header
      (insert "** Notes\n"))

    ;; highlight the word part in note
    ;; it has bug during view-notes or find-notes
    ;; (if (or (eq origin-type 'nov-mode) (eq origin-type 'wallabag-entry-mode))
    ;;     (progn
    ;;       (setq beg (point))
    ;;       (insert (substring-no-properties note))
    ;;       (goto-char beg)
    ;;       (unless (string-match-p "\n" word)
    ;;         (when (re-search-forward word (point-max) t)
    ;;           (insert "~")
    ;;           (goto-char (match-beginning 0))
    ;;           (insert "~"))))
    ;;   (insert (substring-no-properties note)))
    (pen-insert-and-make-overlay "#+BEGIN_SRC org\n" 'invisible t export)
    (if (stringp note)
        (insert (substring-no-properties note))
      (insert "\n"))
    (insert "\n")
    (pen-insert-and-make-overlay "#+END_SRC" 'invisible t export)


    ))

;;;###autoload
(defun pen-find-note (&optional entry no-widen)
  (interactive)
  (let* ((entry (or entry
                    (get-char-property (point) 'pen-entry)
                    pen-note-entry))
         (word (alist-get 'word entry))
         (note (alist-get 'note entry))
         (origin-path (alist-get 'origin_path entry))
         (origin-point (alist-get 'origin_point entry))
         ;; save to temporary file directory, so taht we do not need to delete it
         ;; this org file is really useless, since we already have database
         (file (expand-file-name (concat (if (string-match ":id:\\(.*\\)" word)
                                             (match-string 1 word)
                                           word) ".org") temporary-file-directory))
         (hide-frame (if pen-posframe-p
                         (posframe-hide (or (get-buffer "*pen-view-note*")
                                            (get-buffer "*pen-sub-note*")))))
         (target-buffer (current-buffer))
         (default-directory pen-note-dir))
    (if (file-exists-p file)
        (let ((buffer (find-buffer-visiting file)))
          (if buffer
              (let ((window (get-buffer-window buffer)))
                (if window
                    (select-window window)
                  (split-window-below)
                  (windmove-down)
                  (switch-to-buffer buffer)
                  (find-file file)))
            ;; (split-window-below)
            ;; (windmove-down)
            (find-file-other-window file)))
      (with-temp-file file
        (org-mode)
        (cond ((stringp origin-path) (insert "#+TITLE: " (file-name-nondirectory origin-path) "\n") )
              ((stringp origin-point) (insert "#+TITLE: studylist - " origin-point "\n"))
              (t (insert "#+TITLE: NO TITLE\n")))
        (insert "#+STARTUP: showall\n\n")
        (pen-insert-note entry nil t t))
      (let ((buffer (find-buffer-visiting file)))
        (if buffer
            (let ((window (get-buffer-window buffer)))
              (if window
                  (select-window window)
                (split-window-below)
                (windmove-down)
                (switch-to-buffer buffer)
                (find-file file)))
          ;; (split-window-below)
          ;; (windmove-down)
          (find-file-other-window file))))
    (pen-note-mode)
    (add-hook 'after-save-hook 'pen-send-edited-note nil t)
    (setq-local pen-note-word word)
    (setq-local pen-note-target-buffer target-buffer)
    (setq-local pen-note-origin-type major-mode)
    (setq-local pen-note-origin-path (or origin-path ""))
    (setq-local pen-note-note note)
    (goto-char (point-min))
    ;; jump to * Notes and narrow
    (search-forward-regexp "** Saved Meanings\n")
    ;; (unless no-widen
    ;;   (org-narrow-to-subtree))
    ))

;;;###autoload
(defun pen-send-edited-note ()
  "Use buffer contents as note for an ebook.
Lines beginning with `#' are ignored.
Bound to \\<C-cC-c> in `pen-note-mode'."
  (interactive)
  (unless (derived-mode-p 'pen-note-mode)
    (error "Not in mode derived from `pen-note-mode'"))
  (save-buffer)
  (let* ((buffer (current-buffer))
         (exp (save-excursion
                 (with-current-buffer buffer
                   (goto-char (point-min))
                   (re-search-forward "#** Saved Meanings")
                   (substring-no-properties (org-get-entry)))))
         (note (save-excursion
                 (with-current-buffer buffer
                   (goto-char (point-min))
                   (re-search-forward "#** Notes")
                   (substring-no-properties (org-get-entry))))))
    (with-current-buffer buffer
      (let* ((word pen-note-word)
             (target-buffer pen-note-target-buffer))
        (when word
          (pen-update-exp pen-note-word exp)
          (pen-update-note pen-note-word note))
        (when (buffer-live-p target-buffer)
          (with-current-buffer target-buffer
            (setf (cdr (assoc 'exp (overlay-get (cl-find-if
                                                 (lambda (o)
                                                   (equal (alist-get 'word (overlay-get o 'pen-entry)) word))
                                                 (overlays-in (point-min) (point-max))) 'pen-entry) ) ) exp)
            (setf (cdr (assoc 'note (overlay-get (cl-find-if
                                                  (lambda (o)
                                                    (equal (alist-get 'word (overlay-get o 'pen-entry)) word))
                                                  (overlays-in (point-min) (point-max))) 'pen-entry) ) ) note))) ))
    ;; update pen-view-note
    (when (buffer-live-p (get-buffer "*pen-view-note*"))
        (with-current-buffer (get-buffer "*pen-view-note*")
          (when pen-note-entry
            (setf (cdr (assoc 'exp pen-note-entry)) exp)
            (setf (cdr (assoc 'note pen-note-entry)) note)
            (unless pen-posframe-p (pen-view-note pen-note-entry t t) )))
        (unless pen-posframe-p (pop-to-buffer buffer) ))

    ;; update buffer, so that we do not need to run pen to make the dashboard refresh
    (if (buffer-live-p (get-buffer "*pen*"))
        (pen t))

    (message "Note saved.")))

;;;###autoload
(defun pen-note-quit ()
  "Quit *pen-edit-annatation*.
Bound to \\<C-cC-k> in `pen-note-mode'."
  (interactive)
  (when (eq major-mode 'pen-note-mode)
   (let ((base-buffer (current-buffer)))
     ;; delete the file if no contents.
     ;; (with-current-buffer base-buffer
     ;;   ;; (goto-char (point-min))
     ;;   ;; (re-search-forward "#* Notes")
     ;;   ;; (if (eq 1 (count-lines (point) (point-max)))
     ;;   ;;     (delete-file (buffer-file-name base-buffer)))
     ;;   ;; delete the file, since it is useless
     ;;   (delete-file (buffer-file-name base-buffer)))
     (kill-buffer-and-window)
     ;; (if (< (length (window-prev-buffers)) 2)
     ;;    (progn
     ;;      (quit-window)
     ;;      (kill-buffer base-buffer))
     ;;  (kill-buffer))

     )))


;;; pen-view-note mode

(defvar pen-view-note-header-function #'pen-view-note-header
  "Function that returns the string to be used for the Calibredb edit note header.")

(defvar pen-view-note-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "q" #'pen-view-note-quit)
    map)
  "Keymap for `pen-view-note-mode'.")

(evil-define-key '(normal visual insert) pen-view-note-mode-map
  (kbd "&") 'pen-find-origin-in-note
  (kbd "s") 'pen-view-note
  (kbd "r") 'pen-view-note-play
  (kbd "n") 'pen-view-next-note
  (kbd "p") 'pen-view-prev-note
  ;; (kbd "q") 'pen-view-note-quit
  (kbd "a") 'pen-add-online-word
  )


(define-derived-mode pen-view-note-mode org-mode "pen-view-note"
  "Major mode for display note.
\\{pen-note-mode-map}"
  (setq buffer-read-only t)
  (if (featurep 'svg-lib) (svg-lib-button-mode 1)) ;; this is necessary on org-mode even for emacs buttons
  (pen-get-buttons)
  ;; (setq header-line-format '(:eval (funcall pen-view-note-header-function)))
        )

(defun pen-view-note-header ()
  "Return the string to be used as the Calibredb edit note header."
  (let* ((entry pen-current-entry)
         (origin-word (alist-get 'word entry))
         (word (pen-get-real-word origin-word))
         ;; (exp (alist-get 'exp entry))
         ;; (serverp (alist-get 'serverp entry))
         (origin-path (alist-get 'origin_path entry))
         (origin-point (alist-get 'origin_point entry))
         )
    (format " %s%s"
            (propertize word 'face 'pen-note-header-title-face)
            (propertize (cond ((stringp origin-point) (format " > %s" origin-point))
                  ((stringp origin-path) (format " > %s" (file-name-nondirectory origin-path)) )
                  (t ("NO TITLE"))) 'face 'pen-note-header-title-path-face)
            )
    )
  )

;;;###autoload
(defun pen-view-note-quit ()
  "Quit *pen-edit-annatation*."
  (interactive)
  (let ((pen-view-note-buffer (get-buffer "*pen-view-note*"))
        (pen-sub-note-buffer (get-buffer "*pen-sub-note*")) )

    (posframe-hide (if (buffer-live-p pen-view-note-buffer)
                       pen-view-note-buffer
                     (if (buffer-live-p pen-sub-note-buffer)
                         pen-sub-note-buffer
                       )))
    (evil-force-normal-state)
    )

  ;; (when (eq major-mode 'pen-view-note-mode)
  ;;   ;; (kill-buffer-and-window)

  ;;   ;; (if (< (length (window-prev-buffers)) 2)
  ;;   ;;     (progn
  ;;   ;;       (quit-window)
  ;;   ;;       (kill-buffer "*pen-view-note*"))
  ;;   ;;   (kill-buffer))
  ;;   )
  )



;;;###autoload
(defun pen-view-note (&optional entry no-pushp no-sub-note)
  "View note on anything!"
  (interactive)
  (let* ((entry (pen-view-note-get-entry entry)) ;; shit!!! property word is not pure! eaf has error!
         (origin-word (alist-get 'word entry))
         ;; (entry (cl-find-if (lambda (x) (equal origin-word (alist-get 'word x))) pen-full-entries)) ; search back the pen-search-entries
         (word (let ((real-word (pen-get-real-word origin-word)))
                 (if (s-blank-str? real-word)
                     ;; (error "Please select a word or sentence.")
                     (error "") ;; compress the error
                   real-word)))
         ;; (content (alist-get 'content entry))
         ;; (content-json (condition-case nil
         ;;                   (let ((output (json-read-from-string content)))
         ;;                     (if (and (not (eq output nil))
         ;;                              (not (arrayp output))
         ;;                              (not (numberp output)))
         ;;                         output
         ;;                       nil))
         ;;                 (error nil)))
         ;; (content-filename (or (alist-get 'filename content-json) ""))
         ;; (content-path (or (alist-get 'path content-json) ""))
         (note (alist-get 'note entry))
         (note-type (alist-get 'note_type entry))
         (origin-type (alist-get 'origin_type entry))
         ;; (origin-id (alist-get 'origin_id entry))
         (origin-path (alist-get 'origin_path entry))
         (origin-point (alist-get 'origin_point entry))
         ;; (created-at (alist-get 'created_at entry))
         ;; (kagome (alist-get 'kagome entry))
         (default-directory pen-note-dir)
         (buffer (get-buffer "*pen-view-note*"))
         (sub-buffer (if (and (not no-sub-note) (not pen-posframe-p))
                         (if (eq major-mode 'pen-view-note-mode)
                             (get-buffer-create "*pen-sub-note*")) ))) ; it is important for `org-display-inline-images'
    (setq pen-current-entry entry)
    (when (and (not no-pushp) (not sub-buffer))
        (if (> (length pen-entries-history) pen-entries-history-max)
            (setq pen-entries-history (butlast pen-entries-history)))
        (push entry pen-entries-history))
    (with-current-buffer (if sub-buffer sub-buffer (if buffer buffer (setq buffer (get-buffer-create "*pen-view-note*")) ) )
      (let ((inhibit-read-only t))
        (org-mode)
        (goto-char (point-min))
        (erase-buffer)
        ;; (unless (search-forward "#+TITLE" nil t)
        ;;   (cond ((stringp origin-point) (insert "#+TITLE: studylist - " origin-point "\n"))
        ;;         ((stringp origin-path) (insert "#+TITLE: " (file-name-nondirectory origin-path) "\n") )
        ;;         (t (insert "#+TITLE: NO TITLE\n")))
        ;;   ;; (insert "#+STARTUP: showall\n")
        ;;   )
        ;; (goto-char (point-max))
        (pen-view-note-mode)
        ;; must set local variables before insert note, so that pen-insert-note can access those values
        (setq-local pen-note-word origin-word)
        (setq-local pen-note-note note)
        (setq-local pen-note-entry entry)
        (setq-local pen-note-origin-type (or origin-type major-mode))
        (setq-local pen-note-origin-path (or origin-path (pen-get-origin-path)))

        ;; prounce
        (pcase (car note-type)
          ((or 'image 'attachment) nil)
          (_
           (if pen-say-word-p
               (funcall pen-default-say-word-function word))

           ;; (if pen-transalte-p
           ;;     (funcall pen-translate-function pen-note-word)
           ;;     )
           (if (featurep 'svg-lib) (svg-lib-button-mode 1))))

        (pen-insert-note entry)

        (goto-char (point-min))


        (if (string-equal system-type "android") (face-remap-add-relative 'default :height 0.85) )
        (face-remap-add-relative 'org-document-title :height 0.5)
        (face-remap-add-relative 'org-document-info-keyword :height 0.5)
        (face-remap-add-relative 'org-meta-line :height 0.5)
        (face-remap-add-relative 'org-drawer :height 0.5)
        ;; (face-remap-add-relative 'org-block :family "Bookerly" :height 0.8)
        ;; (when (eq (car note-type) 'attachment)
        ;;   (search-forward-regexp "* Notes\n")
        ;;   (org-narrow-to-subtree))
        (setq-local header-line-format '(:eval (funcall pen-view-note-header-function)))

        ;; find the origin-word in database, if it exist, just add overlays for it
        (pcase (car note-type)
          ('word (let ((entry (pen-candidate-by-word origin-word)))
                   (when entry
                     (pen-show-all-annotations entry))))))

      (pcase (car note-type)
        ((or 'image 'attachment) nil)
        (_
         (if pen-transalte-p
             (funcall pen-translate-function word)))))

    ;; pop to pen-view-note or pen-sub-note and find the correct position
    (if (not pen-posframe-p)
        (pop-to-buffer (if sub-buffer sub-buffer buffer))
      (unless (eq major-mode 'pen-view-note-mode)
        (posframe-show (if sub-buffer sub-buffer buffer)
                       :poshandler 'posframe-poshandler-point-window-center
                       :width (min 100 (round (* 0.95 (window-width))) )
                       :height (min 100 (round (* 0.5 (window-height))) )
                       :respect-header-line t
                       :cursor 'box
                       :internal-border-width 2
                       :accept-focus t
                       ;; :refposhandler nil
                       :hidehandler (lambda(_)
                                      (or (eq last-command 'keyboard-quit)
                                          (eq this-command 'keyboard-quit)))
                       :internal-border-color (if (eq (frame-parameter nil 'background-mode) 'light)
                                                  "#888888"
                                                "#F4F4F4")))
      (select-frame-set-input-focus (posframe--find-existing-posframe (if sub-buffer sub-buffer buffer))))

    ;; (display-buffer-other-frame (if sub-buffer sub-buffer buffer))
    (unless (search-forward "** Saved Meanings" nil t)
      (search-forward "** Translation" nil t))
    (beginning-of-line)
    (recenter 0)

    ;; (pen-annotation-mode 1)
    ;; (sleep-for 0.0001) ;; small delay to avoid error
    ;; (select-window (previous-window))

    ;; (if (string-equal system-type "android")
    ;;     (message (s-truncate 30 word))
    ;;   (message "%s" word))

    ;; (sleep-for 0.001) ;; small delay to avoid error
    ;; (unless sub-buffer
    ;;   (other-window 1))
    )
  ;; back to *pen*
  ;; (let ((window (get-buffer-window (pen-buffer))))
  ;;   (if (window-live-p window)
  ;;       (select-window window)))
  )


(defun pen-view-note-get-entry(&optional entry)
  "Get the entry from the point or the entry"
  (or entry
      (get-char-property (point) 'pen-entry)
      (let ((thing (cond ((eq major-mode 'eaf-mode)
                          (pcase eaf--buffer-app-name
                            ("browser"
                             (eaf-execute-app-cmd 'eaf-py-proxy-copy_text)
                             (sleep-for 0.01) ;; TODO small delay to wait for the clipboard
                             (eaf-call-sync "execute_function" eaf--buffer-id "get_clipboard_text"))
                            ("pdf-viewer"
                             (eaf-execute-app-cmd 'eaf-py-proxy-copy_select)
                             (sleep-for 0.01) ;; TODO small delay to wait for the clipboard
                             (eaf-call-sync "execute_function" eaf--buffer-id "get_clipboard_text"))))
                         (t (if mark-active
                                (buffer-substring-no-properties (region-beginning) (region-end))
                              (thing-at-point 'symbol t))))))
        (if (not (s-blank-str? thing) )
            (pen-view-note-get-thing thing)
          nil))))

(defun pen-view-note-get-thing(thing)
  "get new entry or not"
  (let* ((lan (pen-check-language thing))
         (len (length thing)))
    (pcase lan
      ("ja" (if (> len 5)
                (progn
                  (call-interactively 'pen-view-note-current-thing )
                  nil)
              (pen-new-entry thing)))
      (_ (pen-new-entry thing)))))

;;;###autoload
(defun pen-view-note-query()
  (interactive)
  (let* ((entry (get-text-property (point) 'pen-entry))
         (real-word (pen-get-real-word entry))
         (word (if (string= real-word "") (thing-at-point 'word t) real-word))
         (default (if word (format " (default %s)" word) ""))
         (final-word (read-string (format "Query%s: " default) nil nil word)))
    (pen-view-note (if word entry (pen-new-entry final-word) ) nil t)))

;;;###autoload
(defun pen-view-note-play()
  "play the word in the note or play the word after getting the entry."
  (interactive)
  (cond ((eq major-mode 'pen-view-note-mode)
         (funcall pen-default-say-word-function pen-note-word))
        (mark-active
         (funcall pen-default-say-word-function (buffer-substring-no-properties (region-beginning) (region-end))))
        ((bound-and-true-p focus-mode)
         (funcall pen-default-say-word-function (buffer-substring-no-properties (car (focus-bounds)) (cdr (focus-bounds)))))
        (t
         (funcall pen-default-say-word-function (alist-get 'word (pen-view-note-get-entry))))))

;;;###autoload
(defun pen-view-next-note ()
  "locate the current entry position in history, and view next note."
  (interactive)
  (let ((index (cl-position pen-current-entry pen-entries-history)))
    (if (< (1- index) 0)
        (message "No more next note.")
      (setq pen-current-entry (nth (1- index) pen-entries-history))
      (pen-view-note pen-current-entry t t)
      )))

;;;###autoload
(defun pen-view-prev-note()
  "locate the current entry position in history, and view previous note."
  (interactive)
  ;; return the item number of the current entry
  (let ((index (cl-position pen-current-entry pen-entries-history)))
    (if (>= (1+ index) (length pen-entries-history))
        (message "No more previous note.")
      (setq pen-current-entry (nth (1+ index) pen-entries-history))
      (pen-view-note pen-current-entry t t)

      )))

;;;###autoload
(defun pen-view-notes (&optional path)
  (interactive)
  (let* ((entry-at-point (get-char-property (point) 'pen-entry))
         (origin-path-at-point (or path (alist-get 'origin_path (get-char-property (point) 'pen-entry)) ))
         (marked-entries (if (eq major-mode 'pen-search-mode)
                             (pen-find-marked-candidates)
                           (let* ((overlays (overlays-in (point-min) (point-max)))
                                  (candidates (cl-remove-duplicates
                                               (mapcar (lambda (overlay)
                                                         (overlay-get overlay 'pen-entry))
                                                       overlays) )))
                             candidates)))
         (entries (or marked-entries (-sort (lambda (ex ey)
                                              (let ((x (alist-get 'origin_point ex))
                                                    (y (alist-get 'origin_point ey)))
                                               (cond ((and (numberp x) (numberp y))
                                                      (< x y))
                                                     ((and (consp x) (consp y))
                                                      (< (car x) (car y)))
                                                     ((consp x)
                                                      (< (car x) y))
                                                     ((consp y)
                                                      (< x (car y)))))) (pen-candidates-by-origin-path origin-path-at-point))))
         (default-directory pen-note-dir)
         (pen-say-word-p nil)) ; it is important for `org-display-inline-images'
    (if entries
        (progn
          (when (get-buffer "*pen-view-note*")
            (kill-buffer "*pen-view-note*"))
          (with-current-buffer (get-buffer-create "*pen-view-note*")
            (org-mode)
            (if (stringp origin-path-at-point)
                (insert "#+TITLE: " (file-name-nondirectory origin-path-at-point) "\n")
              (insert "#+TITLE: NO TITLE\n"))
            (insert "#+STARTUP: showall\n")
            (insert "* Table of Contents :TOC::\n")
            (dolist (entry entries)
              (when entry
                (pen-insert-note entry nil nil t t)
                (insert "\n")))
            ;; goto the word in the *pen-view-note*
            (let* (;; (origin-type (alist-get 'origin_type entry))
                   ;; (origin-id (alist-get 'origin_id entry))
                   (entry (or (car marked-entries) entry-at-point))
                   (note-type (car (alist-get 'note_type entry)))
                   (word (unless path
                           (pen-get-real-word entry)))
                   (content (alist-get 'content entry))
                   (origin-path (or path (alist-get 'origin_path entry)))
                   (content-json (condition-case nil
                                     (let ((output (json-read-from-string content)))
                                       (if (and (not (eq output nil))
                                                (not (arrayp output))
                                                (not (numberp output)))
                                           output
                                         nil))
                                   (error nil)))
                   ;; (content-filename (or (alist-get 'filename content-json) ""))
                   (content-path (or (alist-get 'path content-json) "")))
              (goto-char (point-min))
              (unless path
                (pcase note-type
                  ('image (search-forward content-path))
                  ('attachment (search-forward content-path))
                  (_ (search-forward word))) )
              (goto-char (line-beginning-position))
              (require 'toc-org)
              (toc-org-mode)
              (toc-org-insert-toc)
              (pen-view-note-mode)
              ;; (setq-local pen-note-word (file-name-nondirectory origin-path))
              ))

          ;; clear marks
          (pen-clear-marks)
          (switch-to-buffer "*pen-view-note*")
          ;; (display-buffer-other-frame "*pen-view-note*")
          (recenter))
      (sdcv-search-detail
       (pen-get-real-word (alist-get 'word (get-text-property (point) 'pen-entry)) )))))

;;;###autoload
(defun pen-find-notes (&optional entry)
  "TODO"
  (interactive)
  (let* ((entry-at-point (or entry (get-char-property (point) 'pen-entry)))
         (word (alist-get 'word entry-at-point))
         (origin-path (alist-get 'origin_path entry-at-point))
         (origin-path-at-point (alist-get 'origin_path (get-char-property (point) 'pen-entry)))
         (marked-entries (pen-find-marked-candidates))
         (entries (or marked-entries (-sort (lambda (ex ey)
                                              (let ((x (alist-get 'origin_point ex))
                                                    (y (alist-get 'origin_point ey)))
                                                (cond ((and (numberp x) (numberp y))
                                                       (< x y))
                                                      ((and (consp x) (consp y))
                                                       (< (car x) (car y)))
                                                      ((consp x)
                                                       (< (car x) y))
                                                      ((consp y)
                                                       (< x (car y)))))) (pen-candidates-by-origin-path origin-path-at-point))))
         (file (expand-file-name (concat (md5 origin-path) ".org") temporary-file-directory))
         (default-directory pen-note-dir))

    ;; delete the fil
    (if (file-exists-p file)
        (delete-file file))

    ;; insert the contents
    (with-temp-file file
      (if origin-path
          (insert "#+TITLE: " (file-name-nondirectory origin-path) "\n")
        (insert "#+TITLE: NO TITLE\n"))
      (insert "#+STARTUP: showall\n\n")
      (insert "* Table of Contents :TOC::\n")
      (dolist (entry entries)
        (pen-insert-note entry t t t)
        (insert "\n")))
    (let ((buffer (find-buffer-visiting file)))
      (if buffer
          (let ((window (get-buffer-window buffer)))
            (if window
                (select-window window)
              (split-window-below)
              (windmove-down)
              (switch-to-buffer buffer)
              (find-file file)))
        (split-window-below)
        (windmove-down)
        (find-file file)))

    ;; goto the word
    (let* ((entry (or (car marked-entries) entry-at-point))
           (note-type (car (alist-get 'note_type entry)))
           (word (pen-get-real-word entry))
           (content (alist-get 'content entry))
           (origin-path (alist-get 'origin_path entry))
           (content-json (condition-case nil
                             (let ((output (json-read-from-string content)))
                               (if (and (not (eq output nil))
                                        (not (arrayp output))
                                        (not (numberp output)))
                                   output
                                 nil))
                           (error nil)))
           ;; (content-filename (or (alist-get 'filename content-json) ""))
           (content-path (or (alist-get 'path content-json) "")))
      (goto-char (point-min))
      (pcase note-type
        ('image (search-forward content-path))
        ('attachment (search-forward content-path))
        (_ (search-forward word)))
      (goto-char (line-beginning-position))
      (require 'toc-org)
      (toc-org-mode)
      (toc-org-insert-toc)
      (pen-note-mode)
      ;; (setq-local pen-note-word (file-name-nondirectory origin-path))
      )

    ;; (when (eq (car note-type) 'attachment)
    ;;   (search-forward-regexp "* Notes\n")
    ;;   (org-narrow-to-subtree))
    ))


;;;###autoload
(defun pen-find-origin-in-note (arg)
  (interactive "P")
  (let ((entry (pen-candidate-by-id (if (string-match ":id:\\(.*\\)" pen-note-word)
                                        (match-string 1 pen-note-word)
                                      pen-note-word))))
    (if entry
        (if arg
            (pen-goto-dashboard (car entry))
          (pen-find-origin (car entry) t))

      (message "No this entry."))))

;;;###autoload
(defun pen-view-notes-outline ()
  "View notes outline."
  (interactive)
  (when (get-buffer "*pen-view-note-outline*")
    (kill-buffer "*pen-view-note-outline*"))
  (with-current-buffer (get-buffer-create "*pen-view-note-outline*")
    (insert "#+TITLE: Annotations Outline\n")
    ;; get all origin_type
    (-map (lambda (type)
            (insert "* " (symbol-name (car type)))
            (insert "\n")
            (-map (lambda (path)
                    (insert "** " (format "[[pen-path:%s][%s (%d)]]\n"
                                         (car path )
                                         (file-name-nondirectory (car path))
                                         (length (pen-candidates-by-origin-path (car path))))))
                  (pen-db-sql `[:select :distinct origin_path
                               :from status
                               :where (= origin_type ',(car type))])

                  )
            )
          (pen-db-sql `[:select :distinct origin_type
                        :from status
                        :where (not origin_type nil)])
           )
            ;; (insert "#+STARTUP: showall\n")
            ;; (insert "* Table of Contents :TOC::\n")

            ;; (require 'toc-org)
            ;; (toc-org-mode)
            ;; (toc-org-insert-toc)
            (pen-view-note-mode))
  (switch-to-buffer "*pen-view-note-outline*")
  (goto-char (point-min)))

(defcustom pen-csv-file
  (expand-file-name (concat org-directory "pen.csv"))
  "pen csv file for all entries."
  :group 'pen
  :type 'file)

;;;###autoload
(defun pen-export-notes-to-csv(&optional select-path only-words current-path)
  (interactive)
  (let* ((result (cond (select-path (pen-candidates-by-origin-path (consult--read (pen-get-all-origin-path)
                                                                           :prompt "Select the note group: ")))
                       (only-words (pen-candidates-only-online-words))
                       (current-path (pen-candidates-by-origin-path))
                       (t (pen-all-candidates))))
         (csv-string (mapconcat (lambda (entry)
                                  (let* ((word (pen-get-real-word entry))
                                         (exp (alist-get 'exp entry))
                                         (content (alist-get 'content entry))
                                         (content-json (condition-case nil
                                                           (let ((output (json-read-from-string content)))
                                                             (if (and (not (eq output nil))
                                                                      (not (arrayp output))
                                                                      (not (numberp output)))
                                                                 output
                                                               nil))
                                                         (error nil)))
                                         (content-filename (or (alist-get 'filename content-json) ""))
                                         (content-path (or (alist-get 'path content-json) ""))
                                         (note (alist-get 'note entry))
                                         (note-type (alist-get 'note_type entry))
                                         (serverp (alist-get 'serverp entry))
                                         (origin-type (alist-get 'origin_type entry))
                                         (origin-id (alist-get 'origin_id entry))
                                         (origin-path (alist-get 'origin_path entry))
                                         (origin-point (alist-get 'origin_point entry))
                                         (created-at (alist-get 'created_at entry)))
                                    ;; concat all fields
                                    (format "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\""
                                            (if (stringp origin-path)
                                                (file-name-nondirectory origin-path)
                                              origin-point)
                                            word
                                            (if exp exp "")
                                            (replace-regexp-in-string "\"" "\"\"" note)
                                            (replace-regexp-in-string "\"" "\"\"" (if (or (eq content 0) (eq content nil)) "" content))
                                            serverp
                                            created-at)))
                                result "\n")))
    (with-temp-file pen-csv-file
      (insert "origin-path,word,exp,note,content,serverp,created-at\n")
      (insert csv-string))))


(defun pen-view-note-buffer-p ()
  "Return t if the current buffer is a pen-view-note buffer."
  (or (string-equal "*pen-view-note*" (buffer-name))
      (string-equal "*pen-sub-note*" (buffer-name)))
  )

(defun pen-note--unfontify (beg end &optional _loud)
  "Unfontify prettified elements between BEG and END."
  (let ((font-lock-extra-managed-props
         (append
          ;; Only remove line/wrap-prefix if block fringes are used
          (if (and org-modern-block-fringe (not (bound-and-true-p org-indent-mode)))
              '(wrap-prefix line-prefix invisible)
            '(invisible))
          font-lock-extra-managed-props)))
    (org-unfontify-region beg end)))

(provide 'pen-note)
