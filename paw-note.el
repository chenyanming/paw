;;; paw-org.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'paw-db)
(require 'paw-util)
(require 'paw-faces)
(require 'paw-svg)
(require 'paw-sdcv)
(require 'paw-anki)

(require 'evil-core nil t)
(require 'posframe nil t)
(require 's)
(require 'thingatpt)
(require 'pcase)
(require 'dash)
(require 'json)
(require 'org)
(require 'transient)


(declare-function evil-define-key* "ext:evil-core.el" t t)

(defcustom paw-note-dir org-directory
  "paw note dir for image and attachment"
  :group 'paw
  :type 'string)


(defvar paw-file-property-id "ID")
(defvar paw-file-property-doc-file "FILE_PATH")
(defvar paw-file-property-note-location "FILE_LOCATION")
(defvar paw-file-property-current-location "CURRENT_LOCATION")

;;; paw-note-mode

(defvar paw-note-target-buffer nil)
(defvar paw-note-return-buffer nil)
(defvar paw-note-word nil)
(defvar paw-note-entry nil)
(defvar paw-note-origin-type nil)
(defvar paw-note-origin-path nil)
(defvar paw-note-context nil)
(defvar paw-note-note nil)
(defvar paw-note-lang nil)
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
    (define-key map "\M-s" 'paw-find-origin)
    (define-key map "\C-c\C-i" 'paw-insert-annotation-link)
    (define-key map "\C-c\C-k" 'paw-note-quit)
    map)
  "Keymap for `paw-note-mode'.")

(if (bound-and-true-p evil-mode)
    (evil-define-key* 'normal paw-note-mode-map
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
  (format "%s %s %s %s %s"
          "Insert 'C-c C-i',"
          "Sync 'M-s',"
          "Finish 'C-c C-c',"
          "Abort 'C-c C-k'."
          (propertize (paw-get-real-word paw-note-word) 'face 'paw-note-header-title-face)))


(defcustom paw-insert-note-sections-hook
  '()
  "TODO Hook run to insert sections into a view note buffer."
  :group 'paw
  :type 'hook)

(defcustom paw-view-note-background-color 'unspecified
  "The background color of each block inside paw-view-note"
  :group 'paw
  :type "string")

(defcustom paw-view-note-sections '("Translation" "Context" "Saved Meanings" "Notes" "Meaning" "Dictionaries" "Search" "Anki")
  "Sections to be used in `paw-view-note-mode'.
The order of the sections is the order of the list.
Supported values are:
- \"Dictionaries\"
- \"Search\"
- \"Context\"
- \"Translation\"
- \"Saved Meanings\"
- \"Meaning\"
- \"Notes\"
- \"Anki\"
"
  :group 'paw
  :type '(repeat string))

(defun paw-insert-note (entry &rest properties)
  "Format ENTRY and output the org file content."
  (let* ((ori-word (alist-get 'word entry))
         (word (paw-get-real-word entry))
         (id (paw-get-id ori-word))
         (clean-word (paw-clean-word word))
         (clean-word-with-id (paw-clean-word ori-word))
         (no-note-header (plist-get properties :no-note-header))
         (find-note (plist-get properties :find-note))
         (export (plist-get properties :export))
         (anki-editor (plist-get properties :anki-editor))
         (multiple-notes (plist-get properties :multiple-notes))
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
         (anki-note-id (alist-get 'anki-note-id content-json))
         (note (alist-get 'note entry))
         (context (alist-get 'context entry))
         (note-type (alist-get 'note_type entry))
         (serverp (alist-get 'serverp entry))
         (origin-type (alist-get 'origin_type entry))
         (origin-id (alist-get 'origin_id entry))
         (origin-path (alist-get 'origin_path entry))
         (origin-point (alist-get 'origin_point entry))
         (current-point (alist-get 'current_point entry))
         (created-at (alist-get 'created_at entry))
         (kagome (or (plist-get properties :kagome) (alist-get 'kagome entry)))
         (lang (alist-get 'lang entry))
         (sound (alist-get 'sound entry))
         beg)

    ;; workaround: avoid org-modern clear my display
    (if (bound-and-true-p org-modern-mode)
        (setq-local font-lock-unfontify-region-function 'paw-note--unfontify))
    ;; workaround: avoid pangu-spacing-separator-face is different than `paw-view-note-background-color'
    (if (facep 'pangu-spacing-separator-face)
        (face-remap-add-relative 'pangu-spacing-separator-face `(:background ,paw-view-note-background-color :extend t)))
    (insert "* ")
    (if multiple-notes
        (progn (insert (format "[[paw-view-note:%s][%s]]" id (s-collapse-whitespace clean-word)))
               (unless (or anki-editor find-note)
                 (insert " " paw-return-button " " )
                 (insert paw-default-play-button " ")
                 (insert paw-play-source-button " ")
                 (if (eq serverp 3)
                     (insert paw-add-button " "))
                 (insert paw-delete-button " ")
                 (insert paw-goldendict-button " ")
                 (pcase serverp
                   (1 (insert paw-level-1-button))
                   (4 (insert paw-level-2-button))
                   (5 (insert paw-level-3-button))
                   (6 (insert paw-level-4-button))
                   (7 (insert paw-level-5-button))
                   (8 (insert paw-level-1-button))
                   (9 (insert paw-level-2-button))
                   (10 (insert paw-level-3-button))
                   (11 (insert paw-level-4-button))
                   (12 (insert paw-level-5-button))
                   (_ nil))))
      (insert (s-collapse-whitespace clean-word)  " "))
    (insert "\n")
    (unless (or anki-editor find-note)
      (insert ":PROPERTIES:\n")
      (insert ":" paw-file-property-id ": " id "\n")
      (pcase origin-type
        ('nov-mode
         (insert ":" paw-file-property-doc-file ": " origin-path "\n"))
        ('pdf-view-mode
         (insert ":" paw-file-property-doc-file ": " origin-path "\n"))
        ('wallabag-entry-mode
         (require 'wallabag)
         (insert ":" paw-file-property-doc-file ": " (number-to-string (if (numberp origin-id) origin-id 0)) "\n"))
        (_
         (if origin-path
             (insert ":" paw-file-property-doc-file ": " origin-path "\n") )))
      (pcase origin-type
        ('nov-mode
         (insert ":" paw-file-property-note-location ": " (replace-regexp-in-string "\n" "" (pp-to-string origin-point)) "\n"))
        ('wallabag-entry-mode
         (insert ":" paw-file-property-note-location ": " (replace-regexp-in-string "\n" "" (pp-to-string origin-point)) "\n"))
        ('pdf-view-mode
         (insert ":" paw-file-property-note-location ": " (replace-regexp-in-string "\n" "" (pp-to-string origin-point)) "\n"))
        (_
         (if origin-point
             (insert ":" paw-file-property-note-location ": " (replace-regexp-in-string "\n" "" (pp-to-string origin-point)) "\n"))))
      (if current-point (insert ":" paw-file-property-current-location ": " (replace-regexp-in-string "\n" "" (pp-to-string current-point)) "\n") )
      (if lang (insert ":LANGUAGE: " lang "\n") )
      (if created-at
          (insert ":CREATED_AT: " created-at "\n"))

      (insert ":END:\n"))

    (when anki-editor
      (insert ":PROPERTIES:\n")
      (insert ":" paw-anki-property-deck ": " paw-anki-deck "\n")
      (insert ":" paw-anki-property-notetype ": " paw-anki-note-type "\n")
      (if anki-note-id
          (insert ":" paw-anki-property-note-id ": " anki-note-id "\n"))
      ;; (org-entry-put nil paw-anki-property-deck paw-anki-deck)
      ;; (org-entry-put nil paw-anki-property-notetype paw-anki-note-type)
      (insert ":END:\n")
      )


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

    (cl-loop for item in paw-view-note-sections
             do (pcase item
                  ("Dictionaries"
                   (unless find-note
                     (pcase (car note-type)
                       ((or 'image 'attachment) nil)
                       (_
                        (unless multiple-notes
                          (insert "** ")
                          ;; FIXME wordaround to add org face
                          (paw-insert-and-make-overlay "Dictionaries " 'face 'org-level-2)
                          ;; (if (string= lang "ja")
                          ;;     ;; insert all english buttons
                          ;;     (paw-insert-note-japanese-dictionaries)
                          ;;   (paw-insert-note-english-dictionaries))
           ;;; Change from if to cond
                          (cond ((string= lang "ja")
                                 (paw-insert-note-japanese-dictionaries))
                                ((string= lang "zh")
                                 (paw-insert-note-chinese-dictionaries))
                                ;; insert all english buttons
                                ((string= lang "en")
                                 (paw-insert-note-english-dictionaries)))
                          (insert "\n"))))))
                  ("Search"
                   (unless find-note
                     (pcase (car note-type)
                       ((or 'image 'attachment) nil)
                       (_
                        (unless multiple-notes
                          (if paw-provide-general-urls-p
                              (progn
                                (insert "** ")
                                ;; FIXME wordaround to add org face
                                (paw-insert-and-make-overlay "Search " 'face 'org-level-2)

                                (paw-insert-note-general-dictionaries)

                                ;; (insert paw-stardict-button " ")
                                ;; (insert paw-mdict-button " ")
                                (insert "\n"))))))))
                  ("Context"
                   (unless (s-blank-str? context)
                     (insert "** ")
                     (paw-insert-and-make-overlay "Context " 'face 'org-level-2)
                     (insert paw-translate-button " ")
                     (insert paw-ai-translate-button " ")
                     (insert "\n")
                     ;; bold the word in Context
                     (let ((bg-color paw-view-note-background-color))
                       (paw-insert-and-make-overlay
                        (replace-regexp-in-string word (concat "*" word "*") (substring-no-properties context))
                        'face `(:background ,bg-color :extend t))
                       (insert "\n"))))
                  ("Translation"
                   (unless (or find-note multiple-notes)
                     (pcase (car note-type)
                       ((or 'image 'attachment) nil)
                       (_
                        ;; TODO show detected language is a little bit annoying
                        ;; (if paw-detect-language-p
                        ;;     (insert "** Translation (" lang "->"
                        ;;         (mapconcat #'symbol-name (-remove-item `,(intern lang) paw-go-translate-langs) ",")
                        ;;         ") ")
                        ;;   (insert "** Translation "))
                        (insert "** ")
                        (paw-insert-and-make-overlay "Translation " 'face 'org-level-2)
                        (insert paw-translate-button " ")
                        (insert paw-ai-translate-button " ")
                        (insert paw-ask-ai-button " ")
                        (insert paw-goldendict-button " ")
                        (insert paw-share-button " ")
                        (insert "\n")))))
                  ("Saved Meanings"
                   (cond
                    ;; paw-find-notes/paw-view-notes
                    (multiple-notes
                     (if (stringp exp) (insert exp "\n")))
                    (t
                     (unless find-note
                     (pcase (car note-type)
                       ((or 'image 'attachment) nil)
                       (_
                        (when (and (or multiple-notes (and (stringp exp))) (not anki-editor))
                          (insert "** ")
                          (paw-insert-and-make-overlay "Saved Meanings " 'face 'org-level-2)
                          ;; unknown words could have Saved Meanings but shouldn't be able to edit
                          ;; because the Saved Meanings are from Internal Dictionaries
                          (unless (eq serverp 3)
                            (insert paw-edit-button))
                          (insert "\n")
                          (let ((bg-color paw-view-note-background-color))
                            (paw-insert-and-make-overlay
                             (format "%s" (or exp ""))
                             'face `(:background ,bg-color :extend t))
                            (insert "\n"))))))
                   (unless anki-editor
                     (when find-note
                       (insert "** Saved Meanings\n")
                       (if (stringp exp)
                           (insert (substring-no-properties exp) "\n")
                         (insert "\n\n")))))))
                  ("Meaning"
                   (pcase (car note-type)
                       ((or 'image 'attachment) nil)
                       (_
                        ;; TODO use unique overlay instead of search string
                        (if kagome
                            (unless multiple-notes
                              (insert "** ")
                              (paw-insert-and-make-overlay "Meaning " 'face 'org-level-2)
                              (insert paw-default-play-button " ")
                              (insert paw-play-source-button " ")
                              (if (eq serverp 3)
                                  (insert paw-add-button " ")
                                (if (or (paw-online-p serverp)
                                        (paw-offline-p serverp))
                                    (insert paw-edit-button " ")))
                              (insert paw-delete-button " ")
                              (insert paw-goldendict-button " ")
                              (insert paw-next-button " ")
                              (insert paw-prev-button " ")
                              (insert "\n"))
                          (unless multiple-notes
                            (insert "** ")
                            (paw-insert-and-make-overlay "Meaning " 'face 'org-level-2)
                            (insert paw-default-play-button " ")
                            (insert paw-play-source-button " ")
                            (if (eq serverp 3)
                                (insert paw-add-button " ")
                              (if (or (paw-online-p serverp)
                                      (paw-offline-p serverp))
                                  (insert paw-edit-button " ")))
                            (insert paw-delete-button " ")
                            (insert paw-goldendict-button " ")
                            (pcase serverp
                              (1 (insert paw-level-1-button))
                              (4 (insert paw-level-2-button))
                              (5 (insert paw-level-3-button))
                              (6 (insert paw-level-4-button))
                              (7 (insert paw-level-5-button))
                              (8 (insert paw-level-1-button))
                              (9 (insert paw-level-2-button))
                              (10 (insert paw-level-3-button))
                              (11 (insert paw-level-4-button))
                              (12 (insert paw-level-5-button))
                              (_ nil))
                            (insert "\n")
                            (if (boundp 'sdcv-current-translate-object)
                                (setq sdcv-current-translate-object word))

                            ;; (insert (replace-regexp-in-string "^\\*" "-" (sdcv-search-with-dictionary word sdcv-dictionary-simple-list)) "\n")
                            (let ((bg-color paw-view-note-background-color))
                              (paw-insert-and-make-overlay
                               (if (boundp 'sdcv-fail-notify-string) sdcv-fail-notify-string "")
                               'face `(:background ,bg-color :extend t))
                              (insert "\n")))))))
                  ("Notes"
                   (cond
                    ;; paw-view-notes/paw-find-notes
                    (multiple-notes
                     (if (and (stringp note) (not (string-empty-p note) ))
                         (insert (replace-regexp-in-string word (concat "*" word "*") (substring-no-properties note)) "\n")))
                    (t
                     (unless anki-editor
                       (unless no-note-header
                         (insert "** ")
                         (paw-insert-and-make-overlay "Notes " 'face 'org-level-2)
                         (unless find-note
                           (insert paw-translate-button " ")
                           (insert paw-ai-translate-button " ")
                           (unless (eq serverp 3)
                             (insert paw-edit-button)))
                         (insert "\n"))
                       (if (stringp note)
                           ;; bold the word in note
                           (let ((bg-color paw-view-note-background-color))
                             (paw-insert-and-make-overlay
                              (replace-regexp-in-string word (concat "*" word "*") (substring-no-properties note))
                              'face `(:background ,bg-color :extend t))
                             (insert "\n"))
                         (insert "\n"))))))
                  ("Anki"
                   (when anki-editor
                     (if (file-exists-p paw-anki-media-dir)
                         (if (and paw-anki-deck paw-anki-note-type paw-anki-field-names)
                             (if (= (length paw-anki-field-names) (length paw-anki-field-values))
                                 (cl-loop for field-name in paw-anki-field-names and i from 0 do
                                          (insert "** " field-name "\n")
                                          (let ((field-value (nth i paw-anki-field-values)))
                                            (pcase field-value
                                              ('word
                                               (insert word "\n"))
                                              ('exp
                                               (insert (or exp "") "\n"))
                                              ('sound
                                               (if (and sound
                                                        (file-exists-p sound)
                                                        ;; sometimes edge-tts created the file but no sound in it
                                                        (> (file-attribute-size (file-attributes sound)) 0))
                                                   (if (eq system-type 'android)
                                                       ;; old copy way, works on android
                                                       (progn
                                                         (insert "[sound:" (file-name-nondirectory sound) "]\n")
                                                         (copy-file sound paw-anki-media-dir t) )
                                                     ;; anki editor way, anki connect will download it
                                                     (insert "[[file:" sound "]]\n"))
                                                 (insert "\n")))
                                              ('note
                                               (insert (replace-regexp-in-string word (concat "*" word "*") (substring-no-properties note)) "\n"))
                                              ('cloze_note
                                               (insert (replace-regexp-in-string word (concat "{{c1::" word "}}") (substring-no-properties note)) "\n"))
                                              ('cloze_note_exp_hint
                                               (insert (replace-regexp-in-string word (concat "{{c1::" word "::" exp "}}") (substring-no-properties note)) "\n"))
                                              ('file
                                               (insert (if origin-path
                                                           (pcase origin-type
                                                             ((or 'wallabag-entry-mode 'eaf-mode "browser" 'eww-mode)
                                                              origin-path)
                                                             (_ (file-name-nondirectory origin-path )))
                                                         (if (and origin-point (stringp origin-point))
                                                             origin-point)) "\n"))
                                              ('choices
                                               (insert (mapconcat (lambda(entry)
                                                                    (alist-get 'word entry))
                                                                  (paw-candidates-by-origin-path-serverp t) "|") "\n" ))
                                              ('nil (insert ""))
                                              (x
                                               (insert x)))))
                               (error "Field names and values are not matched."))
                           (paw-anki-configure-card-format))
                       (error "paw-anki-media-dir was not configured, otherwise we can not add sound file."))))
                  (_ nil)))

    ;; hide all drawers at the end
    (org-fold-hide-drawer-all)))

(defun paw-insert-note-japanese-dictionaries ()
  ;; insert all japanese buttons
  ;; (cl-loop for button in paw-japanese-web-buttons do
  ;;          (insert button " "))
  (paw-japanese-web-buttons-sections)
  (insert paw-japanese-web-left-button " ")
  (setq paw-japanese-web-buttons-sections-beg (point))
  (cl-loop for button in (nth paw-japanese-web-section-index paw-japanese-web-buttons-sections) do
           (insert button " "))
  (setq paw-japanese-web-buttons-sections-end (point))
  (insert paw-japanese-web-right-button " "))

(defun paw-insert-note-chinese-dictionaries ()
  ;; insert all chinese buttons
  ;; (cl-loop for button in paw-chinese-web-buttons do
  ;;          (insert button " "))
  (paw-chinese-web-buttons-sections)
  (insert paw-chinese-web-left-button " ")
  (setq paw-chinese-web-buttons-sections-beg (point))
  (cl-loop for button in (nth paw-chinese-web-section-index paw-chinese-web-buttons-sections) do
           (insert button " "))
  (setq paw-chinese-web-buttons-sections-end (point))
  (insert paw-chinese-web-right-button " "))

(defun paw-insert-note-english-dictionaries ()
  ;; (cl-loop for button in paw-english-web-buttons do
  ;;          (insert button " "))
  (paw-english-web-buttons-sections)
  (insert paw-english-web-left-button " ")
  (setq paw-english-web-buttons-sections-beg (point))
  (cl-loop for button in (nth paw-english-web-section-index paw-english-web-buttons-sections) do
           (insert button " "))
  (setq paw-english-web-buttons-sections-end (point))
  (insert paw-english-web-right-button " "))

(defun paw-insert-note-general-dictionaries()
  ;; (cl-loop for button in paw-general-web-buttons do
  ;;          (insert button " "))
  (paw-general-web-buttons-sections)
  (insert paw-general-web-left-button " ")
  (setq paw-general-web-buttons-sections-beg (point))
  (cl-loop for button in (nth paw-general-web-section-index paw-general-web-buttons-sections) do
           (insert button " "))
  (setq paw-general-web-buttons-sections-end (point))
  (insert paw-general-web-right-button " "))

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
                          (if origin-path
                              (let ((buf (find-buffer-visiting origin-path)))
                                (if buf buf (current-buffer)))
                            (current-buffer))))
         (default-directory paw-note-dir))
    (with-temp-file file
      (org-mode)
      ;; (cond ((stringp origin-path) (insert "#+TITLE: " (file-name-nondirectory origin-path) "\n") )
      ;;       ((stringp origin-point) (insert "#+TITLE: studylist - " origin-point "\n"))
      ;;       (t (insert "#+TITLE: NO TITLE\n")))
      ;; (insert "#+STARTUP: showall\n\n")
      (insert (alist-get 'note entry)))
    (let ((buffer (find-buffer-visiting file)))
      (if buffer
          (let ((window (get-buffer-window buffer)))
            (if window
                (select-window window)
              (pop-to-buffer buffer)
              (find-file file)))
        (find-file-other-window file)))
    (paw-note-mode)
    (setq header-line-format '(:eval (funcall paw-note-header-function)))
    (add-hook 'after-save-hook 'paw-send-edited-note nil t)
    (setq-local paw-note-entry entry)
    (setq-local paw-note-word word)
    (setq-local paw-note-target-buffer target-buffer)
    (setq-local paw-note-origin-type major-mode)
    (setq-local paw-note-origin-path (or origin-path ""))
    (setq-local paw-note-note note)
    (rename-buffer (format "[note] %s <-->" (paw-get-real-word word)))
    (goto-char (point-min))
    ;; jump to * Notes and narrow
    ;; (search-forward-regexp "** Saved Meanings\n")
    ;; (unless no-widen
    ;;   (org-narrow-to-subtree))
    ))

(defcustom paw-view-note-after-editting-note nil
  "Whether to view note after editting the note."
  :group 'paw
  :type 'boolean)

(defun paw-find-saved-meanings (&optional entry)
  "Use minibuffer contents as Saved Meanings."
  (interactive)
  (let* ((entry (or entry
                    (get-char-property (point) 'paw-entry)
                    paw-note-entry
                    (car (paw-candidate-by-word (paw-note-word)))))
         (word (alist-get 'word entry))
         (exp (alist-get 'exp entry))
         (origin-path (alist-get 'origin_path entry))
         (origin-point (alist-get 'origin_point entry))
         (target-buffer (if paw-note-target-buffer
                            paw-note-target-buffer
                          (current-buffer)))
         (new-exp (read-string (format "Saved Meanings (%s): " word) exp)))
    (paw-update-exp paw-note-word new-exp)

    ;; update the overlays on target-buffer
    (when (buffer-live-p target-buffer)
      (with-current-buffer target-buffer
        (unless (eq major-mode 'paw-search-mode)
          (when-let ((overlay (cl-find-if
                               (lambda (o)
                                 (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                               (overlays-in (point-min) (point-max))))
                     (paw-entry (overlay-get overlay 'paw-entry)))
            (setf (cdr (assoc 'exp paw-entry)) new-exp)
            (overlay-put overlay 'help-echo new-exp)))))

    ;; query back the entry
    (setq paw-note-entry (car (paw-candidate-by-word word) ))

    ;; show the word again
    (paw-view-note-refresh)

    ;; update buffer, so that we do not need to run paw to make the dashboard refresh
    (if (buffer-live-p (get-buffer "*paw*"))
        (paw t))

    (message "Saved Meanings saved."))

  )

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
         (note (with-current-buffer buffer
                 (buffer-string))))
    (with-current-buffer buffer
      (let* ((word paw-note-word)
             (target-buffer paw-note-target-buffer))
        (when word
          (paw-update-note paw-note-word note))

        ;; update the overlays on target-buffer
        (when (buffer-live-p target-buffer)
          (with-current-buffer target-buffer
            (unless (eq major-mode 'paw-search-mode)
              (setf (cdr (assoc 'note (overlay-get (cl-find-if
                                                    (lambda (o)
                                                      (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                                                    (overlays-in (point-min) (point-max))) 'paw-entry) ) ) note)
              (let ((ov (cl-find-if
                         (lambda (o)
                           (equal (alist-get 'word (overlay-get o 'paw-entry))
                                  word))
                         (overlays-in (point-min) (point-max)))))
                (when ov
                  (if (s-blank-str? note)
                      (overlay-put ov 'after-string nil)
                    (overlay-put ov 'after-string nil)
                    (overlay-put ov 'after-string paw-comment-button))
                  (save-excursion (paw-add-inline-annotation ov)))))))

        ;; query back the entry
        (setq paw-note-entry (car (paw-candidate-by-word word) ))))

    ;; show the word again
    (if paw-view-note-after-editting-note
        (if-let ((window (get-buffer-window paw-view-note-buffer-name)))
            (with-selected-window (select-window window)
              (paw-view-note-refresh))
          (paw-view-note paw-note-entry :no-pushp t)))
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
    (let ((base-buffer (current-buffer))
          (note-content (buffer-string)))
      (with-current-buffer base-buffer
	;; if note is empty and it is a comment, delete the note
	(if (and (s-blank-str? note-content)
                 (eq 'comment (car (alist-get 'note_type paw-note-entry))))
            (paw-delete-word paw-note-entry t)))
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
    (define-key map "ss" #'paw-view-note)
    (define-key map "si" #'paw-ask-ai-button-function)
    (define-key map "sg" #'paw-goldendict-search-details)
    (define-key map "se" #'paw-view-note-in-dictionary)
    (define-key map "sf" #'paw-yomitan-search-details-firefox)
    (define-key map "sC" #'paw-yomitan-search-details-chrome)
    (define-key map "sc" #'paw-view-note-in-current-thing)
    (define-key map "r" #'paw-view-note-play)
    (define-key map "R" #'paw-view-note-replay)
    (define-key map "n" #'paw-next-annotation)
    (define-key map "p" #'paw-previous-annotation)
    (define-key map "M-n" #'paw-view-next-note)
    (define-key map "M-p" #'paw-view-prev-note)
    (define-key map "M-s" #'paw-find-origin)
    (define-key map "gr" #'paw-view-note-refresh)
    (define-key map "C-n" #'paw-view-note-next-thing)
    (define-key map "C-p" #'paw-view-note-prev-thing)
    (define-key map "q" #'paw-view-note-quit)
    (define-key map "x" #'paw-view-note-quit)
    (define-key map "a" #'paw-add-online-word)
    (define-key map "A" #'paw-add-offline-word)
    (define-key map "d" #'paw-delete-button-function)
    (define-key map "`" #'paw-view-note-under-mouse)
    (define-key map "i" #'paw-edit-button-function)
    (define-key map "?" #'paw-view-note-transient)
    map)
  "Keymap for `paw-view-note-mode'.")

(if (bound-and-true-p evil-mode)
    (evil-define-key* '(normal visual insert) paw-view-note-mode-map
      (kbd "&") 'paw-find-origin-in-note
      (kbd "`") 'paw-view-note-under-mouse
      (kbd "i") 'paw-edit-button-function
      (kbd "s s") 'paw-view-note
      (kbd "s c") 'paw-view-note-current-thing
      (kbd "s e") 'paw-view-note-in-dictionary
      (kbd "s i") 'paw-ask-ai-button-function
      (kbd "s a") 'paw-eudic-search-details
      (kbd "s f") 'paw-yomitan-search-details-firefox
      (kbd "s g") 'paw-goldendict-search-details
      (kbd "s m") 'paw-mac-dictionary-search-details
      (kbd "s C") 'paw-yomitan-search-details-chrome
      (kbd "r") 'paw-view-note-play
      (kbd "R") 'paw-view-note-replay
      (kbd "n") 'paw-next-annotation
      (kbd "p") 'paw-previous-annotation
      (kbd "N") 'paw-previous-annotation
      (kbd "M-n") 'paw-view-next-note
      (kbd "M-p") 'paw-view-prev-note
      (kbd "M-s") 'paw-find-origin
      (kbd "g r") 'paw-view-note-refresh
      (kbd "C-n") 'paw-view-note-next-thing
      (kbd "C-p") 'paw-view-note-prev-thing
      (kbd "x") 'paw-view-note-quit
      (kbd "q") 'paw-view-note-quit
      (kbd "a") 'paw-add-online-word
      (kbd "A") 'paw-add-offline-word
      (kbd "d") 'paw-delete-button-function
      (kbd "D") 'paw-delete-button-function
      (kbd "?") 'paw-view-note-transient))

(transient-define-prefix paw-view-note-transient ()
  "Transient menu for `paw-view-note-mode'."
  [["Navigation"
    ("n" "Next annotation" paw-next-annotation)
    ("p" "Previous annotation" paw-previous-annotation)
    ("N" "Previous annotation" paw-previous-annotation)
    ("M-n" "Next note" paw-view-next-note)
    ("M-p" "Previous note" paw-view-prev-note)
    ("M-s" "Sync note" paw-find-origin)]
   ["Actions"
    ("i" "Edit note" paw-edit-button-function)
    ("r" "Play note" paw-view-note-play)
    ("R" "Replay note" paw-view-note-replay)
    ("g r" "Refresh note" paw-view-note-refresh)
    ("a" "Add online word" paw-add-online-word)
    ("A" "Add offline word" paw-add-offline-word)
    ("d" "Delete" paw-delete-button-function)
    ("D" "Delete (alt)" paw-delete-button-function)]
   ["Search"
    ("s s" "View note" paw-view-note)
    ("s c" "View current thing" paw-view-note-current-thing)
    ("s i" "Ask AI" paw-ask-ai-button-function)
    ("s e" "Search in dictionary" paw-view-note-in-dictionary)
    ("s f" "Search in Yomitan Firefox" paw-yomitan-search-details-firefox)
    ("s g" "Search in GoldenDict" paw-goldendict-search-details)
    ("s m" "Search in Mac Dictionary" paw-mac-dictionary-search-details)
    ("s a" "Search in Eudic Dictionary" paw-eudic-search-details)
    ("s A" "Search in Chatgpt" paw-chatgpt-search-details)
    ("s C" "Search in Yomitan Chrome" paw-yomitan-search-details-chrome)]
   ["Miscellaneous"
    ("&" "Find origin" paw-find-origin-in-note)
    ("`" "View under mouse" paw-view-note-under-mouse)
    ("C-n" "Next thing" paw-view-note-next-thing)
    ("C-p" "Previous thing" paw-view-note-prev-thing)
    ("x" "Quit paw view note" paw-view-note-quit)
    ("q" "Quit" transient-quit-one)]])

(define-derived-mode paw-view-note-mode org-mode "paw-view-note"
  "Major mode for display note.
\\{paw-note-mode-map}"
  (setq buffer-read-only t)
  (if (featurep 'svg-lib) (svg-lib-button-mode 1)) ;; this is necessary on org-mode even for emacs buttons
  (paw-get-buttons)
  ;; (setq header-line-format '(:eval (funcall paw-view-note-header-function)))
        )


(defcustom paw-view-note-after-render-hook nil
  "A hook called after paw-view-note has finished rendering the buffer."
  :group 'paw
  :type 'boolean)


(defun paw-view-note-header ()
  "Return the string to be used as the paw-view-note header."
  (let* ((word (paw-note-word))
         (word (paw-clean-word word)) ;; clean the word
         (entry (car (paw-candidate-by-word word) ))
         (kagome (alist-get 'kagome entry)) ;; FIXME no kagome entry anymore
         (word (paw-get-real-word word))
         ;; (exp (alist-get 'exp entry))
         (serverp (alist-get 'serverp entry))
         (origin-type (alist-get 'origin_type entry))
         (origin-path (alist-get 'origin_path entry))
         (origin-point (alist-get 'origin_point entry))
         )
    (format "%s %s%s"
            (pcase serverp
              (1 paw-level-1-button)
              (4 paw-level-2-button)
              (5 paw-level-3-button)
              (6 paw-level-4-button)
              (7 paw-level-5-button)
              (8 paw-level-1-button)
              (9 paw-level-2-button)
              (10 paw-level-3-button)
              (11 paw-level-4-button)
              (12 paw-level-5-button)
              (_ ""))
            (propertize word 'face 'paw-note-header-title-face)
            (propertize (cond ((stringp origin-point) (format " > %s" origin-point))
                              ((stringp origin-path) (format " > %s" (pcase origin-type
                                                                       ('eww-mode
                                                                        origin-path)
                                                                       (_ (if kagome
                                                                              (buffer-name paw-note-target-buffer)
                                                                            (file-name-nondirectory origin-path))))) )
                              (t "")) 'face 'paw-note-header-title-path-face)
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
    (evil-force-normal-state))

  (when (eq major-mode 'paw-view-note-mode)
    (paw-sdcv-kill-process)
    (when (process-live-p paw-say-word-download-running-process)
      (kill-process paw-say-word-download-running-process)
      (setq paw-say-word-download-running-process nil))
    (when (process-live-p paw-say-word-running-process)
      (kill-process paw-say-word-running-process)
      (setq paw-say-word-running-process nil))
    (quit-window)))

(defcustom paw-view-note-show-type 'buffer
  "The method of the view note."
  :group 'paw
  :type '(choice (const :tag "minibuffer" minibuffer)
                (const :tag "buffer" buffer)
                (const :tag "all" all)))


(defcustom paw-view-note-window-width 0.35
  "The width of the window for *paw-view-note* window when it is show on the `paw-view-note-horizontal-position'."
  :type 'number
  :group 'paw)

(defcustom paw-view-note-window-height 0.44
  "The height of the window for *paw-view-note* window when it is show on the `paw-view-note-vertical-position'."
  :type 'number
  :group 'paw)

(defcustom paw-view-note-window-auto-adjust t
  "Whether to auto adjust the window size for *paw-view-note* window.
If the height of the window is larger than the width, show on the
`paw-view-note-vertical-position', otherwise show on the
`paw-view-note-horizontal-position'."
  :type 'boolean
  :group 'paw)

(defcustom paw-view-note-back-to-original-buffer t
  "Whether to back to the original buffer after view note."
  :type 'boolean
  :group 'paw)

(defcustom paw-view-note-back-to-original-buffer-supported-modes
  '("pdf-viewer" paw-search-mode)
  "The modes that support back to the original buffer after view note.
For eaf mode, you can also use \"pdf-viewer\" or \"browser\" or other
`eaf--buffer-app-name'."
  :group 'paw
  :type 'list)

(defcustom paw-view-note-horizontal-position 'right
  "The horizontal position of the window for *paw-view-note* window."
  :type '(choice
          (const :tag "top" top)
          (const :tag "bottom" bottom)
          (const :tag "right" right)
          (const :tag "left" left))
  :group 'paw)

(defcustom paw-view-note-vertical-position 'bottom
  "The vertical position of the window for *paw-view-note* window."
  :type '(choice
          (const :tag "top" top)
          (const :tag "bottom" bottom)
          (const :tag "right" right)
          (const :tag "left" left))
  :group 'paw)

(defcustom paw-view-notes-order 'asc
  "The order of showing all notes on created date.
It will affect `paw-view-notes' and `paw-find-notes'."
  :type '(choice
          (const :tag "asc" asc)
          (const :tag "desc" desc))
  :group 'paw)

(defun paw-view-note-window-setup ()
  "Setup the window for *paw-view-note*."
  (let* ((height (window-pixel-height (selected-window)))
         (width (window-pixel-width (selected-window)))
         (buffer "^\\*paw-view-note*")
         (new-rule (if (> height width)
                       `(,buffer
                         (display-buffer-reuse-window display-buffer-in-side-window)
                         (side . ,paw-view-note-vertical-position)
                         (window-height . ,paw-view-note-window-height)
                         (no-other-window . t))
                     `(,buffer
                       (display-buffer-reuse-window display-buffer-in-side-window)
                       (side . ,paw-view-note-horizontal-position)
                       (window-width . ,paw-view-note-window-width)
                       (no-other-window . t)))))
    (if (bound-and-true-p +popup-mode) ;; for Doom Emacs
        (progn
          (setq +popup--display-buffer-alist
                (seq-remove (lambda (rule)
                              (and (stringp (car rule))
                                   (string-equal (car rule) buffer)))
                            +popup--display-buffer-alist))
          (if (> height width)
              (set-popup-rule! buffer :size paw-view-note-window-height :side paw-view-note-vertical-position :quit t :modeline nil :select nil :ttl nil :vslot 2 :slot 1)
            (set-popup-rule! buffer :size paw-view-note-window-width :side paw-view-note-horizontal-position :quit t :modeline t :select nil :ttl nil :vslot 2 :slot 1)))
      (setq display-buffer-alist
            (seq-remove (lambda (rule)
                          (and (stringp (car rule))
                               (string-equal (car rule) buffer)))
                        display-buffer-alist))
      (add-to-list 'display-buffer-alist new-rule))))

;;;###autoload
(defun paw-view-note (&optional entry &rest properties)
  "View note on anything!
1. Search with dictionary.
3. Capture the context.
2. Transalat it.
3. Pronounce it.
4. Other operations.
- if `entry', show `entry', it should be a valid paw-entry, check `paw-new-entry'.
- If run on *paw-view-note* buffer, create a new buffer `paw-view-note-sub-buffer-name' and show on there.
- If run on *paw* buffer, show on `paw-view-note-buffer-name' with the current item under point.
- If run on other buffers, show on `paw-view-note-buffer-name' with the thing under point or the selected area.
- If run on the annnations overlay, show on `paw-view-note-buffer-name' with that annotation."
  (interactive)
  ;; auto adjust the window size
  (if paw-view-note-window-auto-adjust
      (paw-view-note-window-setup))

  ;; show the note
  (let* ((entry (paw-view-note-get-entry entry)) ;; !!! property word is not pure! eaf has error!
         (no-pushp (plist-get properties :no-pushp))
         (buffer-name (plist-get properties :buffer-name))
         (display-func (plist-get properties :display-func))
         (origin-word (alist-get 'word entry))
         (word (let ((real-word (paw-get-real-word origin-word)))
                 (if (s-blank-str? real-word)
                     ;; (error "Please select a word or sentence.")
                     (error "") ;; compress the error
                   real-word)))
         (word (paw-clean-word word)) ;; clean the word
         (exp (alist-get 'exp entry))
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
         (serverp (alist-get 'serverp entry))
         (note (alist-get 'note entry))
         ;; get the context
         (context (or (alist-get 'context entry)
                      (setf (alist-get 'context entry) (paw-get-note))))
         (note-type (alist-get 'note_type entry))
         (origin-type (alist-get 'origin_type entry))
         ;; (origin-id (alist-get 'origin_id entry))
         (origin-path (alist-get 'origin_path entry))
         (origin-point (alist-get 'origin_point entry))
         (created-at (if (eq serverp 3) ;; for UNKNOWN word, we get created_at during view note
                         (setf (alist-get 'created_at entry) (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
                       (alist-get 'created_at entry)))
         (serverp (alist-get 'serverp entry))
         (kagome (or (plist-get properties :kagome) (alist-get 'kagome entry)))
         (lang (or (alist-get 'lang entry) (paw-check-language word)))
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
                   (get-buffer-create paw-view-note-buffer-name)))
         (org-mode-hook nil))

    ;; deactivate mark before showing *paw-view-note*
    (if mark-active
        (deactivate-mark))

    (when (not no-pushp)
      (setq paw-current-entry entry)
      (if (> (length paw-entries-history) paw-entries-history-max)
          (setq paw-entries-history (butlast paw-entries-history)))
      (push entry paw-entries-history))

    ;; push the language into entry
    (push `(lang . ,lang) entry)

    ;; pronounce process
    ;; Android: This must run outside of the buffer, otherwise, it will cause error
    (if (and (memq (car note-type) paw-say-word-supported-note-types)
             paw-say-word-p)
        (setf (alist-get 'sound entry) (funcall paw-default-say-word-function (paw-remove-spaces word lang) :lang lang)))

    (when (or (eq paw-view-note-show-type 'minibuffer)
            (eq paw-view-note-show-type 'all))
      ;; show exp in minibuffer if exist,
      ;; but this is redundant, since user can still view it on help-echo
      ;; (if (and (stringp exp) (not (string= exp "")))
      ;;     (message (paw-remove-spaces exp lang))
      ;;   (funcall paw-dictionary-function word))

      (funcall paw-dictionary-function word lang)
      )

    (when (or (eq paw-view-note-show-type 'buffer)
            (eq paw-view-note-show-type 'all))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            ;; (org-mode)
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
            (setq-local paw-note-context context)
            (setq-local paw-note-note note)
            (setq-local paw-note-lang lang)
            (setq-local paw-note-entry entry)
            (setq-local paw-note-origin-type (or origin-type (with-current-buffer target-buffer
                                                               major-mode)))
            (setq-local paw-note-origin-path (or origin-path (paw-get-origin-path)))

            ;; svg-lib
            (pcase (car note-type)
              ((or 'image 'attachment) nil)
              (_
               (if (featurep 'svg-lib) (svg-lib-button-mode 1))))

            (paw-insert-note entry :kagome kagome)

            (goto-char (point-min))


            (if (string-equal system-type "android") (face-remap-add-relative 'default :height 0.85) )
            (face-remap-add-relative 'org-document-title :height 0.5)
            (face-remap-add-relative 'org-document-info-keyword :height 0.5)
            (face-remap-add-relative 'org-meta-line :height 0.5)
            (face-remap-add-relative 'org-drawer :height 0.5)
            ;; hack the sdcv background
            (face-remap-add-relative 'org-block :background paw-view-note-background-color)
            ;; (when (eq (car note-type) 'attachment)
            ;;   (search-forward-regexp "* Notes\n")
            ;;   (org-narrow-to-subtree))
            (setq-local header-line-format '(:eval (funcall paw-view-note-header-function)))

            ;; find the origin-word in database, if it exist, add overlays inside `paw-view-note-buffer-name' buffer
            ;; (pcase (car note-type)
            ;;   ('word (if (paw-online-p serverp) ;; only online words
            ;;              (let ((entry (paw-candidate-by-word origin-word)))
            ;;                (when entry
            ;;                  (paw-show-all-annotations entry))) )))
            )

          ;; Android TBC: The translate process seems need to run inside of the buffer, otherwise, it will cause error
          ;; async translate the word
          (pcase (car note-type)
            ((or 'image 'attachment) nil)
            (_
             (if kagome
                 (funcall kagome word buffer)
               (funcall paw-search-function word buffer))

             (if paw-translate-p
                 (funcall paw-translate-function word lang buffer "Translation"))

             (if paw-ai-translate-p
                 (funcall paw-ai-translate-function
                          word
                          (format "Translate this word/sentence/phrase into %s: %s. It is used in: %s"
                                  paw-gptel-language
                                  word
                                  paw-note-note)
                          nil
                          nil
                          "Translation"))

             (if paw-ai-translate-context-p
                 (funcall paw-ai-translate-function
                          context
                          (format "Translate this word/sentence/phrase into %s: %s. It is used in: %s"
                                  paw-gptel-language
                                  context
                                  paw-note-note)
                          nil
                          nil
                          "Context"))

             (if paw-translate-context-p
                 (funcall paw-translate-function context lang buffer "Context"))


             (if paw-ask-ai-p
                 (funcall 'paw-ask-ai-button-function))))

          )
      ;; pop to paw-view-note find the correct position
      (if (not paw-posframe-p)
          (funcall (or display-func 'pop-to-buffer) buffer)
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


      (with-current-buffer buffer
        ;; (display-buffer-other-frame buffer)
        (unless (search-forward (format "** %s" (car paw-view-note-sections)) nil t)
          (goto-char (point-min)))
        (recenter 0)
        (forward-line 1))

      (run-hooks 'paw-view-note-after-render-hook)

      ;; back to the original buffer
      (with-current-buffer target-buffer
        (when (or (and (memq major-mode paw-view-note-back-to-original-buffer-supported-modes)
                       paw-view-note-back-to-original-buffer)
                  (and (derived-mode-p 'eaf-mode)
                       (member eaf--buffer-app-name paw-view-note-back-to-original-buffer-supported-modes)))
          (select-window (get-buffer-window target-buffer))))

      ;; (paw-annotation-mode 1)
      ;; (sleep-for 0.0001) ;; small delay to avoid error
      ;; (select-window (previous-window))

      ;; (if (string-equal system-type "android")
      ;;     (message (s-truncate 30 word))
      ;;   (message "%s" word))
      )






    )
  ;; back to *paw*
  ;; (let ((window (get-buffer-window (paw-buffer))))
  ;;   (if (window-live-p window)
  ;;       (select-window window)))
  )

(define-obsolete-function-alias 'paw-view-note-in-minibuffer
  'paw-view-note-in-dictionary "paw 1.1.1")

;;;###autoload
(defun paw-view-note-in-dictionary (&optional entry &rest properties)
  "Search the word in using `paw-dictionary-function'."
  (interactive)
  (let ((paw-view-note-show-type 'minibuffer))
    (apply #'paw-view-note entry properties)))

(defun paw-view-note-in-buffer (&optional entry &rest properties)
  (interactive)
  (let ((paw-view-note-show-type 'buffer))
    (apply #'paw-view-note entry properties)))

(defun paw-view-note-under-mouse ()
  "View paw note under the mouse pointer.
Similar as `paw-view-note-click-function', but it can be bond to any
input."
  (interactive)
  (when-let* ((mouse-pos (mouse-pixel-position))
              (frame (car mouse-pos))
              (x (cadr mouse-pos))
              (y (cddr mouse-pos))
              (posn (posn-at-x-y x y frame))
              (pos (posn-point posn)))
    (save-excursion
      (with-current-buffer (window-buffer (posn-window posn))
        (if mark-active
            (paw-view-note)
          (let ((word))
            (goto-char pos)
            (setq word (substring-no-properties (or (thing-at-point 'symbol t) "")))
            (unless (and (string= (if (get-buffer paw-view-note-buffer-name)
                                      (with-current-buffer paw-view-note-buffer-name
                                        paw-note-word)
                                    "")
                                  word)
                         (not word))
              (paw-view-note))))))))

(defun paw-view-note-refresh()
  "Query the word in database and view note again."
  (interactive)
  (if (eq major-mode 'paw-view-note-mode)
      (let* ((origin-word (paw-note-word))
             (current-entry paw-current-entry)
             (current-entry-word (alist-get 'word current-entry))
             (entry (car (paw-candidate-by-word origin-word)))
             (window-pos (window-start))
             (paw-say-word-p nil)) ;; do not pronounce again when refresh
        (if paw-view-note-entries
            (progn
              (paw-view-notes nil paw-note-origin-path)
              (search-forward (concat "[" (paw-get-real-word origin-word) "]") nil t)
              (beginning-of-line)
              (set-window-start (selected-window) window-pos))
          (if entry
              (paw-view-note entry :no-pushp t :buffer-name (current-buffer))
            (if (eq origin-word current-entry-word)
                (paw-view-note current-entry :no-pushp t :buffer-name (current-buffer))
              (paw-view-note (paw-new-entry origin-word) :no-pushp t :buffer-name (current-buffer))))))))

(defun paw-view-note-get-entry(&optional entry)
  "Get the entry from the point or the entry"
  (or entry
      (paw-view-note-get-entry--has-overlay)
      (paw-view-note-get-entry--no-overlay)))

(defun paw-view-note-get-entry--has-overlay()
  "Get the entry from the point that has overlay."
  (when-let* ((entry (get-char-property (point) 'paw-entry)))
    (unless (eq major-mode 'paw-search-mode)
      (let* ((overlay (cl-find-if
                       (lambda (o)
                         (equal (overlay-get o 'paw-entry) entry))
                       (overlays-at (point))))
             (beg (overlay-start overlay))
             (end (overlay-end overlay)))
        (paw-view-note-show-click-overlay beg end)))
    entry))

(defun paw-view-note-get-entry--no-overlay()
  "Get the entry from the point that does not have overlay."
  (let ((thing (paw-get-word)))
    (if (not (s-blank-str? thing) )
	(paw-view-note-get-thing thing)
      nil)))

(defun paw-view-note-show-click-overlay (&optional beg end)
  (if (and beg end)
      (paw-click-show beg end 'paw-click-face)
    (if mark-active
        (let ((beg (region-beginning))
              (end (region-end)))
          (paw-click-show beg end 'paw-click-face))
      (-let (((beg . end) (bounds-of-thing-at-point 'symbol)))
        (if (and beg end) (paw-click-show beg end 'paw-click-face))))))

(defun paw-view-note-get-thing(thing)
  "Get thing and return an paw entry by `paw-new-entry'.
For Japanese, split the thing using kagome into list of strings, and get
the current word at point. kagome is blocking command and slow but works
well.

For example:
Origin thing: 私は大学卒業後
Splited strings: 私 は 大学 卒業 後
The current substring based on point: 大学
Return 大学"
  (let* ((lan (paw-check-language thing))
	 (len (length thing))
         (origin-point (paw-get-location)))
    (pcase lan
      ("ja" (if mark-active
                (progn
                  (funcall-interactively 'paw-view-note-current-thing thing)
                  nil)
              (let* ((thing (thing-at-point 'symbol t))
                     (bound (bounds-of-thing-at-point 'symbol))
                     (beg (car bound))
                     (end (cdr bound))
                     (len (- end beg))
                     (cur (point))
                     (pos (- cur beg))
                     (strs (split-string
                            (string-trim-right (shell-command-to-string (format "%s ja_segment \"%s\""
                                                                                (if (executable-find "paw")
                                                                                    paw-cli-program
                                                                                  (concat paw-python-program " " paw-cli-program))
                                                                                thing)))
                            ;; (paw-kagome-command-blocking thing) ;; kagome is too slow
                            " "))
                     (current-str (catch 'found
                                    (let ((start 0))
                                      (dolist (str strs)
                                        (let ((str-len (length str)))
                                          (when (and (<= start pos) (< pos (+ start str-len)))
                                            (throw 'found (list str start (+ start str-len))))
                                          (setq start (+ start str-len))))))))
                (when current-str
                  (let ((str (nth 0 current-str))
                        (str-start (nth 1 current-str))
                        (str-end (nth 2 current-str)))
                    (message "%s" str)
                    (paw-view-note-show-click-overlay (+ beg str-start) (+ beg str-end))
                    (paw-new-entry str :lang lan :origin_point origin-point)))) ))
      ("zh" (if mark-active
		(progn
		  (funcall-interactively 'paw-view-note-current-thing thing)
		  nil)
	      (let* ((thing (thing-at-point 'symbol t))
		     (bound (bounds-of-thing-at-point 'symbol))
		     (beg (car bound))
		     (end (cdr bound))
		     (len (- end beg))
		     (cur (point))
		     (pos (- cur beg))
		     (strs (jieba-cut thing))
		     (current-str (catch 'found
				    (let ((start 0))
				      (seq-do (lambda (str)
						(let ((str-len (length str)))
						  (when (and (<= start pos) (< pos (+ start str-len)))
						    (throw 'found (list str start (+ start str-len))))
						  (setq start (+ start str-len))))
					      strs)
				      )))
		     )
		(when current-str
		  (let ((str (nth 0 current-str))
			(str-start (nth 1 current-str))
			(str-end (nth 2 current-str)))
		    (message "%s" str)
		    (paw-view-note-show-click-overlay (+ beg str-start) (+ beg str-end))
		    (paw-new-entry str :lang lan :origin_point origin-point)
		    ))
		)))
      ("en"
       (paw-view-note-show-click-overlay)
       (if (> len 30) ; TODO, for en, len > 30, consider as a sentence
           (progn
             (funcall-interactively 'paw-view-note-current-thing thing)
             nil)
         (paw-new-entry thing :lang lan :origin_point origin-point)))
      (_
       (paw-view-note-show-click-overlay)
       (paw-new-entry thing :lang lan :origin_point origin-point)))))

;;;###autoload
(defun paw-view-note-query()
  "Query a word and view note."
  (interactive)
  (consult--read (paw-candidates-format :only-words t :print-full-content t)
                 :prompt "Query a word: "
                 :sort nil
                 :history nil
                 :lookup (lambda(cand candidates input-string _)
                           (let* ((selected (cl-find-if
                                             (lambda (input)
                                               (string= input cand)) candidates))
                                  (entry (if selected
                                             (get-text-property 0 'paw-entry selected)
                                           (paw-new-entry cand))))
                             (paw-view-note entry)))))

;;;###autoload
(defun paw-view-note-play (arg)
  "play the word in the note or play the word after getting the entry.
When ARG, ask you to select a audio source."
  (interactive "P")
  (cond ((eq major-mode 'paw-view-note-mode)
         (if arg
             (funcall paw-default-say-word-function (paw-get-real-word (paw-note-word)) :source t)
           (funcall paw-default-say-word-function (paw-get-real-word (paw-note-word)))))
        (mark-active
         (if arg
             (funcall paw-default-say-word-function (buffer-substring-no-properties (region-beginning) (region-end)) :source t)
             (funcall paw-default-say-word-function (buffer-substring-no-properties (region-beginning) (region-end))) ))
        ((bound-and-true-p focus-mode)
         (if arg
             (funcall paw-default-say-word-function (buffer-substring-no-properties (car (focus-bounds)) (cdr (focus-bounds))) :source t)
             (funcall paw-default-say-word-function (buffer-substring-no-properties (car (focus-bounds)) (cdr (focus-bounds)))) ))
        (t
         (if arg
             (funcall paw-default-say-word-function (alist-get 'word (paw-view-note-get-entry)) :source t)
             (funcall paw-default-say-word-function (alist-get 'word (paw-view-note-get-entry))) ))))


(defun paw-view-note-replay (arg)
  "play the word in the note or play the word after getting the entry.
When ARG, ask you to select a audio source.
Always re-download the audio."
  (interactive "P")
  (cond ((eq major-mode 'paw-view-note-mode)
         (if arg
             (funcall paw-default-say-word-function (paw-get-real-word (paw-note-word)) :refresh t :source t)
           (funcall paw-default-say-word-function (paw-get-real-word (paw-note-word)) :refresh t)))
        (mark-active
         (if arg
             (funcall paw-default-say-word-function (buffer-substring-no-properties (region-beginning) (region-end)) :refresh t :source t)
           (funcall paw-default-say-word-function (buffer-substring-no-properties (region-beginning) (region-end))  :refresh t )))
        ((bound-and-true-p focus-mode)
         (if arg
             (funcall paw-default-say-word-function (buffer-substring-no-properties (car (focus-bounds)) (cdr (focus-bounds))) :refresh t :source t)
           (funcall paw-default-say-word-function (buffer-substring-no-properties (car (focus-bounds)) (cdr (focus-bounds))) :refresh t ) ))
        (t
         (if arg
             (funcall paw-default-say-word-function (alist-get 'word (paw-view-note-get-entry)) :refresh t :source t)
           (funcall paw-default-say-word-function (alist-get 'word (paw-view-note-get-entry)) :refresh t ) ))))

;;;###autoload
(defun paw-view-next-note ()
  "locate the current entry position in history, and view next note."
  (interactive)
  (let ((index (cl-position paw-current-entry paw-entries-history)))
    (if (< (1- index) 0)
        (message "No more next note.")
      (setq paw-current-entry (nth (1- index) paw-entries-history))
      (paw-view-note paw-current-entry :no-pushp t :buffer-name paw-view-note-buffer-name)
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
          (paw-view-note paw-current-entry :no-pushp t :buffer-name paw-view-note-buffer-name))
      (setq paw-current-entry (first paw-entries-history))
      (paw-view-note paw-current-entry :no-pushp t :buffer-name paw-view-note-buffer-name))))

;;;###autoload
(defun paw-view-notes (arg &optional path)
  "View all notes under the same path of the current note (e.g. overview all notes made on the current epub).
If ARG, view all paw-entry overlays in current buffer (e.g. overview all notes appeared the current buffer).
If PATH is provided, use PATH instead."
  (interactive "P")
  ;; auto adjust the window size
  (if paw-view-note-window-auto-adjust
      (paw-view-note-window-setup))
  (let* ((entry-at-point (get-char-property (point) 'paw-entry))
         (origin-path-at-point (if path
                                   path
                                 (if arg
                                     buffer-file-truename
                                   (alist-get 'origin_path (get-char-property (point) 'paw-entry)))))
         (marked-entries (if (eq major-mode 'paw-search-mode)
                             (paw-find-marked-candidates)))
         (entries (or marked-entries (paw-candidates-by-origin-path origin-path-at-point)))
         (paw-get-note-during-paw-view-notes nil) ;; BE CAREFUL: for UNKNOWN word, we get note during view note, it may very slow
         (overlays (if arg (paw-get-all-entries-from-overlays) nil))
         (entries (if arg overlays entries))
         (entries (if arg entries
                    (-sort (lambda (ex ey)
                             (let ((x (alist-get 'created_at ex))
                                   (y (alist-get 'created_at ey)))
                               (if (and x y)
                                   (pcase paw-view-notes-order
                                     ('asc (time-less-p (date-to-time x) (date-to-time y)))
                                     ('desc (time-less-p (date-to-time y) (date-to-time x))))
                                 t))) ;; sort by created date
                           entries) ))
         (return-buffer (if paw-note-return-buffer
                            paw-note-return-buffer
                          (current-buffer)))
         (default-directory paw-note-dir)
         (paw-say-word-p nil)
         (org-mode-hook nil)) ; it is important for `org-display-inline-images'
    (when entries
      ;; clear marks
      (paw-clear-marks)
      (pop-to-buffer (get-buffer-create paw-view-note-buffer-name))
      (with-current-buffer (get-buffer-create paw-view-note-buffer-name)
        (let ((inhibit-read-only t))
          ;; (org-mode)
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
              (paw-insert-note entry :multiple-notes t)
              (insert "\n")))

          (setq-local paw-note-origin-path origin-path-at-point)
          (setq-local paw-note-return-buffer return-buffer)
          (setq-local paw-view-note-entries (delq nil entries))
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
                (_ (search-forward (concat "[" word "]") nil t)))
              (goto-char (line-beginning-position)))
            ;; (require 'toc-org)
            ;; (toc-org-mode)
            ;; (toc-org-insert-toc)
            ;; (setq-local paw-note-word (file-name-nondirectory origin-path))
            ) ))
      ;; (display-buffer-other-frame paw-view-note-buffer-name)
      (recenter)
      )))

;;;###autoload
(defun paw-find-notes (arg &optional entry)
  "Find all notes in the same origin-path and save it into an org file under `paw-note-dir.'
If ARG, view all paw-entry overlays in the current buffer."
  (interactive "P")
  (let* ((entry-at-point (or entry (get-char-property (point) 'paw-entry)))
         (word (alist-get 'word entry-at-point))
         (origin-path (if arg
                          buffer-file-truename
                        (or (alist-get 'origin_path entry-at-point) buffer-file-truename)))
         (origin-path-at-point (alist-get 'origin_path (get-char-property (point) 'paw-entry)))
         (marked-entries (if (eq major-mode 'paw-search-mode)
                             (paw-find-marked-candidates)))
         (entries (or marked-entries (paw-candidates-by-origin-path origin-path-at-point)))
         (paw-get-note-during-paw-view-notes nil) ;; BE CAREFUL: for UNKNOWN word, we get note during view note, it may very slow
         (overlays (if arg (paw-get-all-entries-from-overlays) nil))
         (entries (if arg overlays entries))
         (entries (if arg entries
                    (-sort (lambda (ex ey)
                             (let ((x (alist-get 'created_at ex))
                                   (y (alist-get 'created_at ey)))
                               (if (and x y)
                                   (pcase paw-view-notes-order
                                     ('asc (time-less-p (date-to-time x) (date-to-time y)))
                                     ('desc (time-less-p (date-to-time y) (date-to-time x))))
                                 t))) ;; sort by created date
                           entries) ))
         (file (file-name-concat paw-note-dir (concat (file-name-base origin-path) ".org")))
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
      (paw-find-notes-latex-headers)
      (insert "#+STARTUP: showall\n\n")
      ;; (insert "* Table of Contents :TOC::\n")
      (dolist (entry entries)
        (when entry
          (paw-insert-note entry :find-note t :export t :multiple-notes t)
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
        (_ (search-forward (concat "[" word "]") nil t)))
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

(defun paw-find-notes-latex-headers ()
  (let ((lines '("#+LATEX_CLASS: ctexart"
                 "#+OPTIONS: toc:nil num:t"
                 "#+LATEX_HEADER: \\usepackage[a4paper,margin=2.5cm]{geometry}"
                 "#+LATEX_HEADER: \\usepackage{xcolor}"
                 "#+LATEX_HEADER: \\usepackage{titlesec}"
                 ;; 标题格式，字体大小
                 "#+LATEX_HEADER: \\titleformat{\\section}{\\normalfont\\normalsize\\bfseries}{\\thesection}{1em}{}"
                 "#+LATEX_HEADER: \\titleformat{\\subsection}{\\normalfont\\small\\bfseries}{\\thesubsection}{1em}{}"
                 "#+LATEX_HEADER: \\titleformat{\\subsubsection}{\\normalfont\\footnotesize\\bfseries}{\\thesubsubsection}{1em}{}"
                 ;; 缩小标题与上下内容的间距：分别是 上 左 下
                 "#+LATEX_HEADER: \\titlespacing{\\section}{0pt}{*0.2}{*0.2}"
                 "#+LATEX_HEADER: \\titlespacing{\\subsection}{0pt}{*0.1}{*0.1}"
                 "#+LATEX_HEADER: \\titlespacing{\\subsubsection}{0pt}{*0.1}{*0.1}"
                 ;; 段落间距和首行缩进
                 "#+LATEX_HEADER: \\setlength{\\parskip}{0.1em}"
                 "#+LATEX_HEADER: \\setlength{\\parindent}{2em}"
                 ;; 超链接设置
                 "#+LATEX_HEADER: \\hypersetup{"
                 "#+LATEX_HEADER:   colorlinks=true,"
                 "#+LATEX_HEADER:   linkcolor=blue!50!black,"
                 "#+LATEX_HEADER:   urlcolor=black,"
                 "#+LATEX_HEADER:   citecolor=green!50!black,"
                 "#+LATEX_HEADER:   filecolor=black,"
                 "#+LATEX_HEADER:   pdftitle={你的文档标题},"
                 "#+LATEX_HEADER:   pdfauthor={你的名字},"
                 "#+LATEX_HEADER:   pdfsubject={文档主题},"
                 "#+LATEX_HEADER:   pdfkeywords={关键词1, 关键词2},"
                 "#+LATEX_HEADER:   pdfborder={0 0 0},"
                 "#+LATEX_HEADER:   breaklinks=true,"
                 "#+LATEX_HEADER: }"
                 "#+LATEX_HEADER: \\pagestyle{plain}"
                 "#+LATEX_HEADER: \\setmainfont{Palatino}"
                 "#+LATEX_HEADER: \\setCJKmainfont{KaiGen Gothic HW TW}")))
    (dolist (line lines)
      (insert line "\n"))))


(provide 'paw-note)
