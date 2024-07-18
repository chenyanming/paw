;;; paw-anki.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'anki-editor)

(defvar paw-anki-property-deck anki-editor-prop-deck)
(defvar paw-anki-property-notetype anki-editor-prop-note-type)
(defconst paw-anki-property-note-id anki-editor-prop-note-id)

(defcustom paw-anki-deck "English"
  "The default Anki deck to use."
  :type 'string
  :group 'paw-anki)

(defcustom paw-anki-download-sound t
  "Whether to download sound file when pushing note to Anki."
  :type 'boolean
  :group 'paw-anki)

(defvar paw-anki-download-word-english-functions '(paw-say-word-cambridge paw-say-word-oxford paw-youdao-say-word))
(defvar paw-anki-download-word-japanese-functions '(paw-say-word-jpod101-alternate))

(defcustom paw-anki-download-sound-functions '(paw-say-word-cambridge paw-say-word-oxford paw-say-word-jpod101-alternate paw-edge-tts-say-word paw-youdao-say-word)
  "The functions to download sound file when pushing note to Anki, one by one. If any one success, it will break."
  :type 'list
  :group 'paw-anki)

(defcustom paw-anki-templates '(("Memrise (Lτ) Preset [Translation+Listenting | Typing+MultipleChoice] v3.32"
                                 ("Learnable" "Definition" "Audio" "Mems" "Attributes" "Extra" "Extra 2" "Choices")
                                 (word exp sound nil nil note file choices))
                                ("Memrise Cloze"
                                 ("Text" "Audio" "Extra" "Extra 2")
                                 (cloze_note_exp_hint sound note file))
                                ("Basic"
                                 ("Front" "Back")
                                 (exp word))
                                ("Cloze"
                                 ("Text" "Back Extra")
                                 (cloze_note_exp_hint file)))
  "The default Anki templates to use when using `paw-anki-configure-card-format'."
  :type 'alist
  :group 'paw-anki)

(defcustom paw-anki-note-type "Memrise (Lτ) Preset [Translation+Listenting | Typing+MultipleChoice] v3.32"
  "The default Anki note type to use."
  :type 'string
  :group 'paw-anki
  )

(defcustom paw-anki-field-names '("Learnable" "Definition" "Audio" "Mems" "Attributes" "Extra" "Extra 2" "Choices")
  "The default Anki field names to use."
  :type 'list
  :group 'paw-anki)

(defcustom paw-anki-field-values '(word exp sound nil nil note file choices)
  "The default Anki field values to use.
Currently Support:
- word: the word to learn
- exp: the explanation of the word
- sound: the sound file of the word
- file: the file name where the word is generated
- note: the note of the word
- cloze_note: the note of the word, word is clozed
- cloze_note_exp_hint: the note of the word, word is clozed, use exp as hint
- choices: the choices of the word
- nil: empty field
- Other values: the value of the field, it must be a string"
  :type 'list
  :group 'paw-anki)

(defvar paw-anki-supported-filed-values '(word exp sound nil note cloze_note cloze_note_exp_hint file choices))


(defcustom paw-anki-dir (cond ((eq system-type 'darwin)
                               (file-name-concat (getenv "HOME") "Library/Application Support/Anki2/User 1"))
                              ((eq system-type 'android)
                               "/storage/emulated/0/AnkiDroid")
                              (t (file-name-concat (getenv "HOME") ".local/share/Anki2/User 1")))
  "The directory where Anki files are stored."
  :type 'string
  :group 'paw-anki)

(defcustom paw-anki-media-dir (file-name-concat paw-anki-dir "collection.media/")
  "The directory where Anki media files are stored."
  :type 'string
  :group 'paw-anki)


(defun paw-anki-configure-card-format ()
  "Configure the Anki card format."
  (interactive)
  (if (and paw-anki-field-names (yes-or-no-p "Do you want to select templates from the predefined templates?"))
      (progn
        (setq paw-anki-deck (completing-read "Deck: " (anki-editor-deck-names)))
        (let* ((template (completing-read "Template: " (mapcar #'car paw-anki-templates)))
             (fields (cdr (assoc template paw-anki-templates))))
        (setq paw-anki-note-type template)
        (setq paw-anki-field-names (car fields))
        (setq paw-anki-field-values (cadr fields))))
    (setq paw-anki-deck (completing-read "Deck: " (anki-editor-deck-names)))
    (setq paw-anki-note-type (completing-read "Note Type: " (anki-editor-note-types)) )
    (setq paw-anki-field-names (anki-editor-api-call-result 'modelFieldNames
                                                            :modelName paw-anki-note-type))
    (setq paw-anki-field-values nil)
    (unwind-protect
        (progn
          (cl-loop for field-name in paw-anki-field-names and i from 0 do
                   (let ((field-value (completing-read (format "Field %d (%s): " i field-name)
                                                       paw-anki-supported-filed-values)))
                     (push (intern field-value) paw-anki-field-values)))
          (setq paw-anki-field-values (nreverse paw-anki-field-values)))
      (unless (and (= (length paw-anki-field-names) (length paw-anki-field-values)))
        (setq paw-anki-field-values nil)))))

(defvar paw-anki-editor-push-notes-total 1)
(defvar paw-anki-editor-push-notes-remaining 1)

(defun paw-anki-editor-push-notes ()
  "Push notes of marked-entries in dashboard to anki, or push all
anki notes in the same origin path (same file or same buffer).
Same file name under `paw-annotation-search-paths' is also
considerred same origin path."
  (interactive)
  (let* ((entry-at-point (get-char-property (point) 'paw-entry))
         (word (alist-get 'word entry-at-point))
         (origin-path-at-point (alist-get 'origin_path (get-char-property (point) 'paw-entry)))
         (marked-entries (if (eq major-mode 'paw-search-mode)
                             (paw-find-marked-candidates)))
         (entries (or marked-entries (paw-candidates-by-origin-path origin-path-at-point))))
    (if marked-entries
        (progn
          (setq paw-anki-editor-push-notes-total (length entries))
          (setq paw-anki-editor-push-notes-remaining (length entries))
          (dolist (entry entries)
            (paw-anki-editor-push-note entry))
          (paw-search-update-buffer-and-resume))
      (when (yes-or-no-p (format "Push all notes under %s to anki? " origin-path-at-point))
        (setq paw-anki-editor-push-notes-total (length entries))
        (setq paw-anki-editor-push-notes-remaining (length entries))
        (dolist (entry entries)
          (paw-anki-editor-push-note entry))))))


(defun paw-anki-editor-delete-notes ()
  "Delete anki notes of marked-entries in dashboard, or delete all
anki notes in the same origin path (same file or same buffer),
Same file name under `paw-annotation-search-paths' is also
considerred same origin path."
  (interactive)
  (let* ((entry-at-point (get-char-property (point) 'paw-entry))
         (word (alist-get 'word entry-at-point))
         (origin-path-at-point (alist-get 'origin_path (get-char-property (point) 'paw-entry)))
         (marked-entries (if (eq major-mode 'paw-search-mode)
                             (paw-find-marked-candidates)))
         (entries (or marked-entries (paw-candidates-by-origin-path origin-path-at-point))))
    (if marked-entries
        (progn
          (dolist (entry entries)
            (paw-anki-editor-delete-note entry t))
          (paw-search-update-buffer-and-resume))
      (when (yes-or-no-p (format "Delete all notes under %s from anki? " origin-path-at-point))
        (dolist (entry entries)
          (paw-anki-editor-delete-note entry t))
        ))))


(defun paw-anki-editor-push-note (&optional entry)
  "Push note at point to Anki."
  (interactive)
  (let* ((entry (or entry (get-char-property (point) 'paw-entry) ))
         (word (alist-get 'word entry))
         (sound (alist-get 'sound entry))
         (org-mode-hook nil)
         (content))
    ;; WORKAROUND, if push note in dashboard, we find the mp3 first, and update
    ;; entry, but would not generate mp3 at this stage, because it is very slow
    ;; if too many notes to push
    (let ((sound (concat (expand-file-name (md5 word) paw-tts-cache-dir) ".mp3"))
          (download-function)
          (fns-list paw-anki-download-sound-functions))
        (if (and (file-exists-p sound) (> (file-attribute-size (file-attributes sound)) 0))
            (progn
              (setf (alist-get 'sound entry) sound)
              (setq content (paw-anki-editor--push-note entry word))
              (setq paw-anki-editor-push-notes-remaining (1- paw-anki-editor-push-notes-remaining))
              (message "Progress: %.2f%%" (- 100 (* 100 (/ (float paw-anki-editor-push-notes-remaining) paw-anki-editor-push-notes-total)) ))
              (when (<= paw-anki-editor-push-notes-remaining 0)
                (setq paw-anki-editor-push-notes-remaining 1)
                (paw-search-update-buffer-and-resume)))
          (paw-anki-download-sound fns-list word
                                   (lambda(file)
                                     (copy-file file sound t)
                                     (setf (alist-get 'sound entry) sound)
                                     (setq content (paw-anki-editor--push-note entry word))
                                     (setq paw-anki-editor-push-notes-remaining (1- paw-anki-editor-push-notes-remaining))
                                     (message "%.2f%%" (- 100 (* 100 (/ (float paw-anki-editor-push-notes-remaining) paw-anki-editor-push-notes-total)) ))
                                     (when (<= paw-anki-editor-push-notes-remaining 0)
                                       (setq paw-anki-editor-push-notes-remaining 1)
                                       (paw-search-update-buffer-and-resume))))))
    content))

(defun paw-anki-download-sound (download-fns-list word finished)
  (pcase (paw-check-language word)
    ("en" (dolist (fn paw-anki-download-word-japanese-functions)
            (setq download-fns-list (cl-remove fn download-fns-list))))
    (_ (dolist (fn paw-anki-download-word-english-functions)
         (setq download-fns-list (cl-remove fn download-fns-list)))))
  (when download-fns-list
    (let ((download-function (car download-fns-list))
          (remaining-functions (cdr download-fns-list))) ;; always choose first sound when auto download if possible
      (funcall download-function word
               :always-first t
               :lambda (lambda (file)
                         (if (and file (file-exists-p file) (> (file-attribute-size (file-attributes file)) 0))
                             (funcall finished file)
                           (paw-anki-download-sound remaining-functions word finished)))
               :download-only t))))





(defun paw-anki-editor--push-note(entry word)
  (if (buffer-live-p (get-buffer "*anki*"))
      (kill-buffer "*anki*") )
  (let ((content))
    (with-current-buffer (get-buffer-create "*anki*" )
    ;; (with-temp-buffer
    (org-mode)
    (anki-editor-mode +1)
    (paw-insert-note entry :find-note t :export t :multiple-notes t :anki-editor t)
    (goto-char (point-min))
    (anki-editor-push-note-at-point)
    (setq content (json-encode `((anki-note-id . ,(org-entry-get nil anki-editor-prop-note-id)))) )
    (paw-db-update-content word content))
    content))

(defun paw-anki-editor-delete-note (&optional entry no-update)
  "Delete note at point to Anki."
  (interactive)
  (let* ((entry (or entry (get-char-property (point) 'paw-entry) ))
         (word (alist-get 'word entry))
         (content (alist-get 'content entry))
         (content-json (condition-case nil
                           (let ((output (json-read-from-string content)))
                             (if (and (not (eq output nil))
                                      (not (arrayp output))
                                      (not (numberp output)))
                                 output
                               nil))
                         (error nil)))
         (anki-note-id (alist-get 'anki-note-id content-json))
         (org-mode-hook nil))
    (if (buffer-live-p (get-buffer "*anki*"))
        (kill-buffer "*anki*") )
    (if anki-note-id
        (with-current-buffer (get-buffer-create "*anki*" )
          (org-mode)
          (anki-editor-mode +1)
          (paw-insert-note entry :find-note t :export t :multiple-notes t :anki-editor t)
          (goto-char (point-min))
          (paw-no-confirm 'anki-editor-delete-note-at-point)
          (paw-db-update-content word nil)

          ) )
    (unless no-update (paw-search-update-buffer-and-resume))))

(defun paw-anki-gui-select-note()
  (interactive)
  (let* ((entry (get-char-property (point) 'paw-entry))
         (word (alist-get 'word entry))
         (content (alist-get 'content entry))
         (content-json (condition-case nil
                           (let ((output (json-read-from-string content)))
                             (if (and (not (eq output nil))
                                      (not (arrayp output))
                                      (not (numberp output)))
                                 output
                               nil))
                         (error nil)))
         (anki-note-id (alist-get 'anki-note-id content-json)))
    (anki-editor-api-call 'guiSelectNote :note (string-to-number anki-note-id))))


(defun paw-anki-gui-edit-note()
  (interactive)
  (let* ((entry (get-char-property (point) 'paw-entry))
         (word (alist-get 'word entry))
         (content (alist-get 'content entry))
         (content-json (condition-case nil
                           (let ((output (json-read-from-string content)))
                             (if (and (not (eq output nil))
                                      (not (arrayp output))
                                      (not (numberp output)))
                                 output
                               nil))
                         (error nil)))
         (anki-note-id (alist-get 'anki-note-id content-json)))
    (anki-editor-api-call 'guiEditNote :note (string-to-number anki-note-id))))

(defun paw-anki-cards-info ()
  (interactive)
  (let* ((entry (get-char-property (point) 'paw-entry))
         (word (alist-get 'word entry))
         (content (alist-get 'content entry))
         (content-json (condition-case nil
                           (let ((output (json-read-from-string content)))
                             (if (and (not (eq output nil))
                                      (not (arrayp output))
                                      (not (numberp output)))
                                 output
                               nil))
                         (error nil)))
         (anki-note-id (alist-get 'anki-note-id content-json)))
    (anki-editor-api-call 'cardsInfo :cards (vconcat (list (string-to-number anki-note-id))))))

(defun paw-anki-gui-browse()
  (interactive)
  (anki-editor-gui-browse (concat "deck:" paw-anki-deck)))


(cl-flet ((always-yes (&rest _) t))
  (defun paw-no-confirm (fun &rest args)
    "Apply FUN to ARGS, skipping user confirmations."
    (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
              ((symbol-function 'yes-or-no-p) #'always-yes))
      (apply fun args))))


;; WORKAROUND to work with AnkiConnectAndroid which only support some commands
;; See https://github.com/KamWithK/AnkiconnectAndroid/blob/master/docs/api.md
(when (eq system-type 'android)
  (advice-add #'anki-editor--create-note :override 'paw-anki-editor--create-note)
  (advice-add #'anki-editor--update-note :override 'paw-anki-editor--update-note)

  (defun paw-anki-editor--create-note (note)
    "Request AnkiConnect for creating NOTE."
    (thread-last
      (anki-editor-api-call-result 'addNote
                                   :note (anki-editor-api--note note))
      (anki-editor--set-note-id)))

  (defun paw-anki-editor--update-note (note)
    "Request AnkiConnect for updating fields, deck, and tags of NOTE."
    (anki-editor-api-call-result 'updateNoteFields
                                 :note (anki-editor-api--note note))))

(provide 'paw-anki)
