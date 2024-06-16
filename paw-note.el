;;; paw-org.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'paw-db)
(require 'paw-util)
(require 'paw-faces)
(require 'paw-svg)

(require 'evil-core nil t)
(require 'posframe)
(require 's)
(require 'json)

(defcustom paw-note-dir org-directory
  "paw note dir for image and attachment"
  :group 'paw
  :type 'string)


(defvar paw-file-property-id "ID")
(defvar paw-file-property-doc-file "FILE_PATH")
(defvar paw-file-property-note-location "FILE_LOCATION")

;;; paw-note-mode

(defvar paw-note-target-buffer nil)
(defvar paw-note-word nil)
(defvar paw-note-entry nil)
(defvar paw-note-origin-type nil)
(defvar paw-note-origin-path nil)
(defvar paw-note-header-function #'paw-note-header
  "Function that returns the string to be used for the Calibredb edit note header.")

(defvar paw-current-entry nil
  "the current entry of the `paw-view-note-mode' buffer, invoked by `paw-view-note'.")

(defvar paw-view-note-entries nil
  "the current entry of the `paw-view-note-mode' buffer, invoked by `paw-view-notes'")

(defvar paw-entries-history nil
  "the history of entries, used for `paw-view-next-note' and
`paw-view-prev-note' inside a `paw-view-note-mode' buffer.")

(defvar paw-entries-history-max 5
  "the max number of entries in the `paw-entries-history'")

(defvar paw-note-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'paw-send-edited-note)
    (define-key map "\C-c\C-k" 'paw-note-quit)
    map)
  "Keymap for `paw-note-mode'.")

(if (fboundp 'evil-define-key)
    (evil-define-key 'normal paw-note-mode-map
      (kbd "&") 'paw-find-origin-in-note
      (kbd "q") 'paw-note-quit) )

(define-derived-mode paw-note-mode org-mode "paw-note"
  "Major mode for display word lists.
\\{paw-note-mode-map}"
  (setq header-line-format '(:eval (funcall paw-note-header-function)))
  (if (boundp org-download-image-dir)
      (setq-local org-download-image-dir (expand-file-name (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) paw-note-dir))))

(defun paw-note-header ()
  "TODO: Return the string to be used as the Calibredb edit note header."
  (format "%s -> Edit Notes. %s %s"
          (propertize paw-note-word 'face 'paw-note-header-title-face)
          "Finish 'C-c C-c',"
          "abort 'C-c C-k'."))

(defun paw-insert-note (entry &optional no-note-header find-note export multiple-notes)
  "Format ENTRY and output the org file content."
  (let* ((word (paw-get-real-word entry))
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
         (lang (alist-get 'lang entry))
         beg)

    ;; workaround: avoid org-modern clear my display
    (if (bound-and-true-p org-modern-mode)
        (setq-local font-lock-unfontify-region-function 'paw-note--unfontify))
    (insert "* ")
    (if multiple-notes
        (insert (format "[[paw:%s][%s]]" (alist-get 'word entry) (s-collapse-whitespace word))  " ")
      (insert (s-collapse-whitespace word)  " "))
    (insert "\n")
    (org-entry-put nil paw-file-property-id (alist-get 'word entry))
    (pcase origin-type
      ('nov-mode
       (org-entry-put nil paw-file-property-doc-file origin-path))
      ('pdf-view-mode
       (org-entry-put nil paw-file-property-doc-file origin-path))
      ('wallabag-entry-mode
       (require 'wallabag)
       (org-entry-put nil paw-file-property-doc-file (number-to-string (if (numberp origin-id) origin-id 0))))
      (_
       (if origin-path
           (org-entry-put nil paw-file-property-doc-file origin-path) )))
    (pcase origin-type
      ('nov-mode
       (org-entry-put nil paw-file-property-note-location (replace-regexp-in-string "\n" "" (pp-to-string origin-point))))
      ('wallabag-entry-mode
       (org-entry-put nil paw-file-property-note-location (replace-regexp-in-string "\n" "" (pp-to-string origin-point))))
      ('pdf-view-mode
       (org-entry-put nil paw-file-property-note-location (replace-regexp-in-string "\n" "" (pp-to-string origin-point))))
      (_
       (if origin-point
           (org-entry-put nil paw-file-property-note-location (replace-regexp-in-string "\n" "" (pp-to-string origin-point))))))
    (if created-at
        (org-entry-put nil "CREATED_AT" created-at))
    (pcase (car note-type)
      ('image
       (insert "#+attr_org: :width 600px\n")
       (insert (format "[[file:%s][file:%s]]" (expand-file-name content-path paw-note-dir) (expand-file-name content-path paw-note-dir)))
       (insert "\n"))
      ('attachment
       (pcase (downcase (file-name-extension content-filename) )
         ((or "jpg" "jpeg" "png" "gif")
          (insert "#+attr_org: :width 600px\n")
          (insert (format "[[file:%s][file:%s]]" (expand-file-name content-path paw-note-dir) (expand-file-name content-path paw-note-dir))))
         (_ (insert (format "%s [[file:%s][%s]]" (paw-attach-icon-for (expand-file-name content-path paw-note-dir))
                            (expand-file-name content-path paw-note-dir) content-filename))))
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
         (unless multiple-notes
           (insert "** Websites ")
           (if (string= lang "ja")
               ;; insert all english buttons
               (progn
                 ;; insert all japanese buttons
                 (paw-provider-japanese-urls)
                 (cl-loop for button in paw-japanese-web-buttons do
                          (insert button " ")))
             (paw-provider-english-urls)
             (cl-loop for button in paw-english-web-buttons do
                      (insert button " ")))

           (insert "\n")
           (insert "** Search ")
           (paw-provider-general-urls)
           (cl-loop for button in paw-general-web-buttons do
                    (insert button " "))

           (insert paw-stardict-button " ")
           (insert paw-mdict-button " ")
           (insert "\n")

           )
         )

        ) )

    (unless find-note
      (pcase (car note-type)
        ((or 'image 'attachment) nil)
        (_
         ;; TODO show detected language is a little bit annoying
         ;; (if paw-detect-language-p
         ;;     (insert "** Translation (" lang "->"
         ;;         (mapconcat #'symbol-name (-remove-item `,(intern lang) paw-go-transalte-langs) ",")
         ;;         ") ")
         ;;   (insert "** Translation "))
         (unless multiple-notes
          (insert "** Translation ")
          (insert paw-translate-button " ")
          (insert paw-ai-translate-button " ")
          (insert paw-ask-ai-button " ")
          (insert "\n"))


         (when (or
                multiple-notes ;; we need the buttons on paw-view-notes
                (and (stringp exp) (not (string= exp ""))) ) ;; dont show it if empty
           (insert "** Saved Meanings ")
           (insert paw-default-play-button " ")
           (if (eq serverp 3)
               (insert paw-add-button " ")
             (insert paw-edit-button " ")
             (insert paw-delete-button " "))
           (insert paw-goldendict-button " ")
           (insert "\n")
           (paw-insert-and-make-overlay "#+BEGIN_SRC org\n" 'invisible t export)
           (insert (substring-no-properties (if exp exp "")))
           (paw-insert-and-make-overlay "\n#+END_SRC" 'invisible t export)
           (insert "\n"))

         ;; TODO use unique overlay instead of search string
         (if kagome
             (unless multiple-notes
               (paw-insert-and-make-overlay "** Dictionary " 'paw-dictionary-word word)
               (insert paw-default-play-button " ")
               (if (eq serverp 3)
                   (insert paw-add-button " ")
                 (insert paw-edit-button " ")
                 (insert paw-delete-button " "))
               (insert paw-goldendict-button " ")
               (insert paw-next-button " ")
               (insert paw-prev-button " ")
               (insert "\n")
               (insert kagome))
           (unless multiple-notes
             (paw-insert-and-make-overlay "** Dictionary " 'paw-dictionary-word word)
             (insert paw-default-play-button " ")
             (if (eq serverp 3)
                 (insert paw-add-button " ")
               (insert paw-edit-button " ")
               (insert paw-delete-button " "))
             (insert paw-goldendict-button " ")
             (insert "\n")
             (paw-insert-and-make-overlay "#+BEGIN_SRC sdcv\n" 'invisible t export)
             (setq sdcv-current-translate-object word)
             ;; (insert (replace-regexp-in-string "^\\*" "-" (sdcv-search-with-dictionary word sdcv-dictionary-simple-list)) "\n")
             (insert sdcv-fail-notify-string "\n")
             (paw-insert-and-make-overlay "#+END_SRC" 'invisible t export)
             (insert "\n")))
         )))

    (when find-note
      (insert "** Saved Meanings\n")
      (if (stringp exp)
          (insert (substring-no-properties exp) "\n")
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
    (unless find-note (paw-insert-and-make-overlay "#+BEGIN_SRC org\n" 'invisible t export))
    (if (stringp note)
        ;; bold the word in note
        (insert (replace-regexp-in-string word (concat "*" word "*") (substring-no-properties note)) )
      (insert "\n"))
    (insert "\n")
    (unless find-note (paw-insert-and-make-overlay "#+END_SRC" 'invisible t export) )


    ))

;;;###autoload
(defun paw-find-note (&optional entry no-widen)
  (interactive)
  (let* ((entry (or entry
                    (get-char-property (point) 'paw-entry)
                    paw-note-entry
                    (car (paw-candidate-by-word (paw-note-word)))))
         (word (alist-get 'word entry))
         (note (alist-get 'note entry))
         (origin-path (alist-get 'origin_path entry))
         (origin-point (alist-get 'origin_point entry))
         ;; save to temporary file directory, so taht we do not need to delete it
         ;; this org file is really useless, since we already have database
         (file (expand-file-name (concat (if (string-match ":id:\\(.*\\)" word)
                                             (match-string 1 word)
                                           word) ".org") temporary-file-directory))
         (hide-frame (if paw-posframe-p
                         (posframe-hide (get-buffer paw-view-note-buffer-name))))
         (target-buffer (if paw-note-target-buffer
                            paw-note-target-buffer
                          (current-buffer)))
         (default-directory paw-note-dir))
    (if (file-exists-p file)
        (let ((buffer (find-buffer-visiting file)))
          (if buffer
              (let ((window (get-buffer-window buffer)))
                (if window
                    (select-window window)
                  (pop-to-buffer buffer)
                  (find-file file)))
            (find-file-other-window file)))
      (with-temp-file file
        (org-mode)
        (cond ((stringp origin-path) (insert "#+TITLE: " (file-name-nondirectory origin-path) "\n") )
              ((stringp origin-point) (insert "#+TITLE: studylist - " origin-point "\n"))
              (t (insert "#+TITLE: NO TITLE\n")))
        (insert "#+STARTUP: showall\n\n")
        (paw-insert-note entry nil t t))
      (let ((buffer (find-buffer-visiting file)))
        (if buffer
            (let ((window (get-buffer-window buffer)))
              (if window
                  (select-window window)
                (pop-to-buffer buffer)
                (find-file file)))
          (find-file-other-window file))))
    (paw-note-mode)
    (add-hook 'after-save-hook 'paw-send-edited-note nil t)
    (setq-local paw-note-word word)
    (setq-local paw-note-target-buffer target-buffer)
    (setq-local paw-note-origin-type major-mode)
    (setq-local paw-note-origin-path (or origin-path ""))
    (setq-local paw-note-note note)
    (goto-char (point-min))
    ;; jump to * Notes and narrow
    (search-forward-regexp "** Saved Meanings\n")
    ;; (unless no-widen
    ;;   (org-narrow-to-subtree))
    ))

(defcustom paw-view-note-after-editting-note nil
  "Whether to view note after editting the note."
  :group 'paw
  :type 'boolean)

;;;###autoload
(defun paw-send-edited-note ()
  "Use buffer contents as note for an ebook.
Lines beginning with `#' are ignored.
Bound to \\<C-cC-c> in `paw-note-mode'."
  (interactive)
  (unless (derived-mode-p 'paw-note-mode)
    (error "Not in mode derived from `paw-note-mode'"))
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
      (let* ((word paw-note-word)
             (target-buffer paw-note-target-buffer))
        (when word
          (paw-update-exp paw-note-word exp)
          (paw-update-note paw-note-word note))

        ;; update the overlays on target-buffer
        (when (buffer-live-p target-buffer)
          (with-current-buffer target-buffer
            (setf (cdr (assoc 'exp (overlay-get (cl-find-if
                                                 (lambda (o)
                                                   (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                                                 (overlays-in (point-min) (point-max))) 'paw-entry) ) ) exp)
            (setf (cdr (assoc 'note (overlay-get (cl-find-if
                                                  (lambda (o)
                                                    (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                                                  (overlays-in (point-min) (point-max))) 'paw-entry) ) ) note)))

        ;; query back the entry
        (setq paw-note-entry (car (paw-candidate-by-word word) ))))

    ;; show the word again
    (if paw-view-note-after-editting-note
        (paw-view-note paw-note-entry t))

    ;; update buffer, so that we do not need to run paw to make the dashboard refresh
    (if (buffer-live-p (get-buffer "*paw*"))
        (paw t))

    (message "Note saved.")))

;;;###autoload
(defun paw-note-quit ()
  "Quit editting annotation.
Bound to \\<C-cC-k> in `paw-note-mode'."
  (interactive)
  (when (eq major-mode 'paw-note-mode)
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


;;; paw-view-note mode

(defvar paw-view-note-header-function #'paw-view-note-header
  "Function that returns the string to be used for the paw-view-note header.")

(defvar paw-view-notes-header-function #'paw-view-notes-header
  "Function that returns the string to be used for the paw-view-notes header.")

(defvar paw-view-note-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'paw-view-note)
    (define-key map "r" #'paw-view-note-play)
    (define-key map "n" #'paw-view-next-note)
    (define-key map "p" #'paw-view-prev-note)
    (define-key map "g r" #'paw-view-note-refresh)
    (define-key map "C-n" #'paw-view-note-next-thing)
    (define-key map "C-p" #'paw-view-note-prev-thing)
    ;; (define-key map "q" #'paw-view-note-quit)
    (define-key map "a" #'paw-add-online-word)
    map)
  "Keymap for `paw-view-note-mode'.")

(if (fboundp 'evil-define-key)
    (evil-define-key '(normal visual insert) paw-view-note-mode-map
      (kbd "&") 'paw-find-origin-in-note
      (kbd "s") 'paw-view-note
      (kbd "r") 'paw-view-note-play
      (kbd "n") 'paw-view-next-note
      (kbd "p") 'paw-view-prev-note
      (kbd "g r") 'paw-view-note-refresh
      (kbd "C-n") 'paw-view-note-next-thing
      (kbd "C-p") 'paw-view-note-prev-thing
      ;; (kbd "q") 'paw-view-note-quit
      (kbd "a") 'paw-add-online-word
      ) )


(define-derived-mode paw-view-note-mode org-mode "paw-view-note"
  "Major mode for display note.
\\{paw-note-mode-map}"
  (setq buffer-read-only t)
  (if (featurep 'svg-lib) (svg-lib-button-mode 1)) ;; this is necessary on org-mode even for emacs buttons
  (paw-get-buttons)
  ;; (setq header-line-format '(:eval (funcall paw-view-note-header-function)))
        )

(defun paw-view-note-header ()
  "Return the string to be used as the paw-view-note header."
  (let* ((entry paw-current-entry)
         (origin-word paw-note-word)
         (kagome (alist-get 'kagome entry))
         (word (paw-get-real-word origin-word))
         ;; (exp (alist-get 'exp entry))
         ;; (serverp (alist-get 'serverp entry))
         (origin-type (alist-get 'origin_type entry))
         (origin-path (alist-get 'origin_path entry))
         (origin-point (alist-get 'origin_point entry))
         )
    (format " %s%s"
            (propertize word 'face 'paw-note-header-title-face)
            (propertize (cond ((stringp origin-point) (format " > %s" origin-point))
                              ((stringp origin-path) (format " > %s" (pcase origin-type
                                                                       ('eww-mode
                                                                        origin-path)
                                                                       (_ (if kagome
                                                                              (buffer-name paw-note-target-buffer)
                                                                            (file-name-nondirectory origin-path))))) )
                  (t ("NO TITLE"))) 'face 'paw-note-header-title-path-face)
            )
    )
  )

(defun paw-view-notes-header ()
  "Return the string to be used as the paw-view-notes header."
  (let* ((len (length paw-view-note-entries)))
    (format " %s%s"
            (propertize (format "%s notes" len) 'face 'paw-note-header-title-face)
            (propertize (format " > %s" paw-note-origin-path ) 'face 'paw-note-header-title-path-face))
    )
  )


;;;###autoload
(defun paw-view-note-quit ()
  "TODO: Quit *paw-view-note*."
  (interactive)
  (let ((paw-view-note-buffer (get-buffer paw-view-note-buffer-name)))
    (posframe-hide (if (buffer-live-p paw-view-note-buffer)
                       paw-view-note-buffer))
    (evil-force-normal-state)
    )

  (when (eq major-mode 'paw-view-note-mode)
    (kill-buffer-and-window)

    ;; (if (< (length (window-prev-buffers)) 2)
    ;;     (progn
    ;;       (quit-window)
    ;;       (kill-buffer paw-view-note-buffer-name))
    ;;   (kill-buffer))
    )
  )

;;;###autoload
(defun paw-view-note (&optional entry no-pushp buffer-name)
  "View note on anything!
- if `entry', show `entry', it should be a valid paw-entry, check `paw-new-entry'.
- if no-pushp, do not push the entry to `paw-entries-history'.
- Show on the `buffer-name' or `paw-view-note-buffer-name' buffer."
  (interactive)
  (let* ((entry (paw-view-note-get-entry entry)) ;; !!! property word is not pure! eaf has error!
         (origin-word (alist-get 'word entry))
         ;; (entry (cl-find-if (lambda (x) (equal origin-word (alist-get 'word x))) paw-full-entries)) ; search back the paw-search-entries
         (word (let ((real-word (paw-get-real-word origin-word)))
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
         (serverp (alist-get 'serverp entry))
         (kagome (alist-get 'kagome entry))
         (default-directory paw-note-dir)
         (target-buffer (if paw-note-target-buffer
                            paw-note-target-buffer
                          (current-buffer)))
         (buffer (if buffer-name ;; if BUFFER-NAME provided, use it
                     (get-buffer-create buffer-name)
                   (if (eq major-mode 'paw-view-note-mode) ;; if view note in *paw-view-note* buffer, use `paw-view-note-sub-buffer-name'
                       (progn
                         (setq no-pushp t) ;; in case it push to history
                         (get-buffer-create paw-view-note-sub-buffer-name) )
                     (get-buffer paw-view-note-buffer-name) ))) ;; otherwise, use `paw-view-note-buffer-name'
         (buffer (if (buffer-live-p buffer)
                     buffer
                   (get-buffer-create paw-view-note-buffer-name))))
    (when (not no-pushp)
      (setq paw-current-entry entry)
      (if (> (length paw-entries-history) paw-entries-history-max)
          (setq paw-entries-history (butlast paw-entries-history)))
      (push entry paw-entries-history))

    ;; pronounce process
    ;; Android: This must run outside of the buffer, otherwise, it will cause error
    (pcase (car note-type)
      ((or 'image 'attachment) nil)
      (_
       (if paw-say-word-p
           (funcall paw-default-say-word-function word))))

    (with-current-buffer buffer
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
        (paw-view-note-mode)
        ;; must set local variables before insert note, so that paw-insert-note can access those values
        (setq-local paw-note-target-buffer target-buffer)
        (setq-local paw-note-word origin-word)
        (setq-local paw-note-note note)
        (setq-local paw-note-entry entry)
        (setq-local paw-note-origin-type (or origin-type major-mode))
        (setq-local paw-note-origin-path (or origin-path (paw-get-origin-path)))

        ;; svg-lib
        (pcase (car note-type)
          ((or 'image 'attachment) nil)
          (_
           (if (featurep 'svg-lib) (svg-lib-button-mode 1))))

        (paw-insert-note entry)

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
        (setq-local header-line-format '(:eval (funcall paw-view-note-header-function)))

        ;; find the origin-word in database, if it exist, add overlays inside `paw-view-note-buffer-name' buffer
        (pcase (car note-type)
          ('word (if (eq serverp 1) ;; only online words
                     (let ((entry (paw-candidate-by-word origin-word)))
                       (when entry
                         (paw-show-all-annotations entry))) ))))

      ;; async search the word with sdcv
      (unless kagome
        (paw-sdcv-search-with-dictionary-async word sdcv-dictionary-simple-list buffer))

      ;; Android TBC: The translate process seems need to run inside of the buffer, otherwise, it will cause error
      ;; async translate the word
      (pcase (car note-type)
        ((or 'image 'attachment) nil)
        (_
         (if paw-transalte-p
             (funcall paw-translate-function word buffer))))

      )

    ;; pop to paw-view-note find the correct position
    (if (not paw-posframe-p)
        (pop-to-buffer buffer)
      (unless (eq major-mode 'paw-view-note-mode)
        (posframe-show buffer
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
      (select-frame-set-input-focus (posframe--find-existing-posframe buffer)))

    ;; (display-buffer-other-frame buffer)
    (unless (search-forward "** Saved Meanings" nil t)
      (search-forward "** Translation" nil t))
    (beginning-of-line)
    (recenter 0)


    ;; (paw-annotation-mode 1)
    ;; (sleep-for 0.0001) ;; small delay to avoid error
    ;; (select-window (previous-window))

    ;; (if (string-equal system-type "android")
    ;;     (message (s-truncate 30 word))
    ;;   (message "%s" word))

    )
  ;; back to *paw*
  ;; (let ((window (get-buffer-window (paw-buffer))))
  ;;   (if (window-live-p window)
  ;;       (select-window window)))
  )

(defun paw-view-note-refresh()
  "Query the word in database and view note again."
  (interactive)
  (unless (eq major-mode 'paw-view-note-mode)
    (error "Not in mode `paw-view-note-mode'"))
  (let* ((origin-word paw-note-word)
         (current-entry paw-current-entry)
         (current-entry-word (alist-get 'word current-entry))
         (entry (car (paw-candidate-by-word origin-word))))
    (if entry
        (paw-view-note entry t (current-buffer))
      (if (eq origin-word current-entry-word)
          (paw-view-note current-entry t (current-buffer))
        (paw-view-note (paw-new-entry origin-word) t (current-buffer)) ))))


(defun paw-view-note-get-entry(&optional entry)
  "Get the entry from the point or the entry"
  (or entry
      (let* ((entry (get-char-property (point) 'paw-entry)))
        (when entry
          (unless (eq major-mode 'paw-search-mode)
              (let* ((overlay (cl-find-if
                           (lambda (o)
                             (overlay-get o 'paw-entry))
                           (overlays-at (point))))
                 (beg (overlay-start overlay))
                 (end (overlay-end overlay)))
            (paw-click-show beg end 'paw-click-face)))
          entry))
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
                                (let ((beg (region-beginning))
                                      (end (region-end)))
                                  (paw-click-show beg end 'paw-click-face)
                                  (buffer-substring-no-properties beg end))
                              (-let (((beg . end) (bounds-of-thing-at-point 'symbol)))
                                (paw-click-show beg end 'paw-click-face))
                              (thing-at-point 'symbol t))))))
        (if (not (s-blank-str? thing) )
            (paw-view-note-get-thing thing)
          nil))))

(defun paw-view-note-get-thing(thing)
  "get new entry or not"
  (let* ((lan (paw-check-language thing))
         (len (length thing)))
    (pcase lan
      ("ja" (if (> len 5) ; TODO, for ja, len > 5, consider as a sentence
                (progn
                  (funcall-interactively 'paw-view-note-current-thing thing)
                  nil)
              (paw-new-entry thing nil lan)))
      ("en" (if (> len 30) ; TODO, for en, len > 30, consider as a sentence
                (progn
                  (funcall-interactively 'paw-view-note-current-thing thing)
                  nil)
              (paw-new-entry thing nil lan)))
      (_ (paw-new-entry thing nil lan)))))

;;;###autoload
(defun paw-view-note-query()
  (interactive)
  (let* ((entry (get-text-property (point) 'paw-entry))
         (real-word (paw-get-real-word entry))
         (word (if (string= real-word "") (thing-at-point 'word t) real-word))
         (default (if word (format " (default %s)" word) ""))
         (final-word (read-string (format "Query%s: " default) nil nil word)))
    (paw-view-note (if word entry (paw-new-entry final-word) ) nil)))

;;;###autoload
(defun paw-view-note-play()
  "play the word in the note or play the word after getting the entry."
  (interactive)
  (cond ((eq major-mode 'paw-view-note-mode)
         (funcall paw-default-say-word-function paw-note-word))
        (mark-active
         (funcall paw-default-say-word-function (buffer-substring-no-properties (region-beginning) (region-end))))
        ((bound-and-true-p focus-mode)
         (funcall paw-default-say-word-function (buffer-substring-no-properties (car (focus-bounds)) (cdr (focus-bounds)))))
        (t
         (funcall paw-default-say-word-function (alist-get 'word (paw-view-note-get-entry))))))

;;;###autoload
(defun paw-view-next-note ()
  "locate the current entry position in history, and view next note."
  (interactive)
  (let ((index (cl-position paw-current-entry paw-entries-history)))
    (if (< (1- index) 0)
        (message "No more next note.")
      (setq paw-current-entry (nth (1- index) paw-entries-history))
      (paw-view-note paw-current-entry t paw-view-note-buffer-name)
      )))

;;;###autoload
(defun paw-view-prev-note()
  "locate the current entry position in history, and view previous note."
  (interactive)
  ;; return the item number of the current entry
  (let ((index (cl-position paw-current-entry paw-entries-history)))
    (if index
        (if (>= (1+ index) (length paw-entries-history))
            (message "No more previous note.")
          (setq paw-current-entry (nth (1+ index) paw-entries-history))
          (paw-view-note paw-current-entry t paw-view-note-buffer-name))
      (setq paw-current-entry (first paw-entries-history))
      (paw-view-note paw-current-entry t paw-view-note-buffer-name))))

;;;###autoload
(defun paw-view-notes (&optional path)
  "View all notes under the same path of the current note. If PATH
is provided, use PATH instead."
  (interactive)
  (let* ((entry-at-point (get-char-property (point) 'paw-entry))
         (origin-path-at-point (or path (alist-get 'origin_path (get-char-property (point) 'paw-entry)) ))
         (marked-entries (if (eq major-mode 'paw-search-mode)
                             (paw-find-marked-candidates)))
         (entries (or marked-entries (-sort (lambda (ex ey)
                                              (let ((x (alist-get 'origin_point ex))
                                                    (y (alist-get 'origin_point ey)))
                                                (cond ((and (numberp x) (numberp y))
                                                       (< x y))
                                                      ((and (consp x) (consp y))
                                                       (< (car x) (car y)))
                                                      ((stringp x)
                                                       t)
                                                      ((stringp y)
                                                       t)
                                                      ((consp x)
                                                       (< (car x) y))
                                                      ((consp y)
                                                       (< x (car y)))
                                                      ))) (paw-candidates-by-origin-path origin-path-at-point))))
         (overlays (paw-get-all-entries-from-overlays))
         (entries (-union entries overlays)) ;; union the overlays from current buffer (online words but not on the same path)
         (default-directory paw-note-dir)
         (paw-say-word-p nil)) ; it is important for `org-display-inline-images'
    (when entries
      ;; clear marks
      (paw-clear-marks)
      (pop-to-buffer (get-buffer-create paw-view-note-buffer-name))
      (with-current-buffer (get-buffer-create paw-view-note-buffer-name)
        (let ((inhibit-read-only t))
          (org-mode)
          (goto-char (point-min))
          (erase-buffer)
          (paw-view-note-mode)
          ;; (if (stringp origin-path-at-point)
          ;;     (insert "#+TITLE: " (file-name-nondirectory origin-path-at-point) "\n")
          ;;   (insert "#+TITLE: NO TITLE\n"))
          (insert "#+STARTUP: showall\n")
          ;; TODO toc can not work with paw-org-link
          ;; (insert "* Table of Contents :TOC_1::\n")
          (dolist (entry entries)
            (when entry
              (paw-insert-note entry nil nil nil t)
              (insert "\n")))

          (setq-local paw-note-origin-path origin-path-at-point)
          (setq-local paw-view-note-entries entries)
          (setq-local header-line-format '(:eval (funcall paw-view-notes-header-function)))

          ;; goto the word in the *paw-view-note*
          (let* (;; (origin-type (alist-get 'origin_type entry))
                 ;; (origin-id (alist-get 'origin_id entry))
                 (entry entry-at-point)
                 (note-type (car (alist-get 'note_type entry)))
                 (word (unless path
                         (paw-get-real-word entry)))
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
                (_ (search-forward word nil t))) )
            ;; (goto-char (line-beginning-position))
            ;; (require 'toc-org)
            ;; (toc-org-mode)
            ;; (toc-org-insert-toc)
            ;; (setq-local paw-note-word (file-name-nondirectory origin-path))
            ) ))
      ;; (display-buffer-other-frame paw-view-note-buffer-name)
      (recenter)
      )))

;;;###autoload
(defun paw-find-notes (&optional entry)
  "TODO"
  (interactive)
  (let* ((entry-at-point (or entry (get-char-property (point) 'paw-entry)))
         (word (alist-get 'word entry-at-point))
         (origin-path (alist-get 'origin_path entry-at-point))
         (origin-path-at-point (alist-get 'origin_path (get-char-property (point) 'paw-entry)))
         (marked-entries (paw-find-marked-candidates))
         (entries (or marked-entries (-sort (lambda (ex ey)
                                              (let ((x (alist-get 'origin_point ex))
                                                    (y (alist-get 'origin_point ey)))
                                               (cond ((and (numberp x) (numberp y))
                                                      (< x y))
                                                     ((and (consp x) (consp y))
                                                      (< (car x) (car y)))
                                                     ((stringp x)
                                                      t)
                                                     ((stringp y)
                                                      t)
                                                     ((consp x)
                                                      (< (car x) y))
                                                     ((consp y)
                                                      (< x (car y)))
                                                     ))) (paw-candidates-by-origin-path origin-path-at-point))))
         (file (expand-file-name (concat (md5 origin-path) ".org") temporary-file-directory))
         (default-directory paw-note-dir))

    ;; delete the fil
    (if (file-exists-p file)
        (delete-file file))

    ;; insert the contents
    (with-temp-file file
      (org-mode)
      (if (stringp origin-path-at-point)
          (insert "#+TITLE: " (file-name-nondirectory origin-path-at-point) "\n")
        (insert "#+TITLE: NO TITLE\n"))
      (insert "#+STARTUP: showall\n\n")
      ;; (insert "* Table of Contents :TOC::\n")
      (dolist (entry entries)
        (when entry
          (paw-insert-note entry nil t t t)
          (insert "\n") )))

    (find-file file)

    ;; goto the word
    (let* ((entry (or (car marked-entries) entry-at-point))
           (note-type (car (alist-get 'note_type entry)))
           (word (paw-get-real-word entry))
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
      ;; (require 'toc-org)
      ;; (toc-org-mode)
      ;; (toc-org-insert-toc)
      ;; (setq-local paw-note-word (file-name-nondirectory origin-path))
      )

    ;; (when (eq (car note-type) 'attachment)
    ;;   (search-forward-regexp "* Notes\n")
    ;;   (org-narrow-to-subtree))
    ))


;;;###autoload
(defun paw-find-origin-in-note (arg)
  (interactive "P")
  (let ((entry (paw-candidate-by-id (if (string-match ":id:\\(.*\\)" paw-note-word)
                                        (match-string 1 paw-note-word)
                                      paw-note-word))))
    (if entry
        (if arg
            (paw-goto-dashboard (car entry))
          (paw-find-origin (car entry) t))

      (message "No this entry."))))

;;;###autoload
(defun paw-view-notes-outline ()
  "View notes outline."
  (interactive)
  (when (get-buffer "*paw-view-note-outline*")
    (kill-buffer "*paw-view-note-outline*"))
  (with-current-buffer (get-buffer-create "*paw-view-note-outline*")
    (insert "#+TITLE: Annotations Outline\n")
    ;; get all origin_type
    (-map (lambda (type)
            (insert "* " (symbol-name (car type)))
            (insert "\n")
            (-map (lambda (path)
                    (insert "** " (format "[[paw-path:%s][%s (%d)]]\n"
                                         (car path )
                                         (file-name-nondirectory (car path))
                                         (length (paw-candidates-by-origin-path (car path))))))
                  (paw-db-sql `[:select :distinct origin_path
                               :from status
                               :where (= origin_type ',(car type))])

                  )
            )
          (paw-db-sql `[:select :distinct origin_type
                        :from status
                        :where (not origin_type nil)])
           )
            ;; (insert "#+STARTUP: showall\n")
            ;; (insert "* Table of Contents :TOC::\n")

            ;; (require 'toc-org)
            ;; (toc-org-mode)
            ;; (toc-org-insert-toc)
            (paw-view-note-mode))
  (switch-to-buffer "*paw-view-note-outline*")
  (goto-char (point-min)))

(defcustom paw-csv-file
  (expand-file-name (concat org-directory "paw.csv"))
  "paw csv file for all entries."
  :group 'paw
  :type 'file)

;;;###autoload
(defun paw-export-notes-to-csv(&optional select-path only-words current-path)
  (interactive)
  (let* ((result (cond (select-path (paw-candidates-by-origin-path (consult--read (paw-get-all-origin-path)
                                                                           :prompt "Select the note group: ")))
                       (only-words (paw-candidates-only-online-words))
                       (current-path (paw-candidates-by-origin-path))
                       (t (paw-all-candidates))))
         (csv-string (mapconcat (lambda (entry)
                                  (let* ((word (paw-get-real-word entry))
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
    (with-temp-file paw-csv-file
      (insert "origin-path,word,exp,note,content,serverp,created-at\n")
      (insert csv-string))))


(defun paw-view-note-buffer-p ()
  "Return t if the current buffer is a paw-view-note buffer."
  (string-equal paw-view-note-buffer-name (buffer-name))
  )

(defun paw-note--unfontify (beg end &optional _loud)
  "Unfontify prettified elements between BEG and END."
  (let ((font-lock-extra-managed-props
         (append
          ;; Only remove line/wrap-prefix if block fringes are used
          (if (and org-modern-block-fringe (not (bound-and-true-p org-indent-mode)))
              '(wrap-prefix line-prefix invisible)
            '(invisible))
          font-lock-extra-managed-props)))
    (org-unfontify-region beg end)))

(provide 'paw-note)
