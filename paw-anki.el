;;; paw-anki.el -*- lexical-binding: t; -*-

(require 'anki-editor)

(defvar paw-anki-property-deck anki-editor-prop-deck)
(defvar paw-anki-property-notetype anki-editor-prop-note-type)
(defconst paw-anki-property-note-id anki-editor-prop-note-id)

(defcustom paw-anki-deck "English"
  "The default Anki deck to use."
  :type 'string
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

(defcustom paw-anki-field-values '(word exp sound nil nil note nil choices)
  "The default Anki field values to use.
Currently Supported:
- word: the word to learn
- exp: the explanation of the word
- sound: the sound file of the word
- note: the note of the word
- choices: the choices of the word
- nil: empty field
- Other values: the value of the field, it must be a string"
  :type 'list
  :group 'paw-anki)


(defcustom paw-anki-dir (cond ((eq system-type 'darwin)
                               (file-name-concat (getenv "HOME") "Library/Application Support/Anki2/User 1"))
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
  (setq paw-anki-deck (completing-read "Deck: " (anki-editor-deck-names)))
  (setq paw-anki-note-type (completing-read "Note Type: " (anki-editor-note-types)) )
  (setq paw-anki-field-names (anki-editor-api-call-result 'modelFieldNames
                                                          :modelName paw-anki-note-type))
  (unless (and (= (length paw-anki-field-names) (length paw-anki-field-values)))
    (message "Please configure `paw-anki-field-values' bofore adding anki cards, it should have %d elements which match `paw-anki-field-names'" (length paw-anki-field-names))))


(defun paw-anki-editor-push-notes ()
  "Find all notes/overlays in the same origin-path and save it into an org file under `paw-note-dir.'"
  (interactive)
  (let* ((entry-at-point (get-char-property (point) 'paw-entry))
         (word (alist-get 'word entry-at-point))
         (origin-path-at-point (alist-get 'origin_path (get-char-property (point) 'paw-entry)))
         (marked-entries (if (eq major-mode 'paw-search-mode)
                             (paw-find-marked-candidates)))
         (entries (or marked-entries (paw-candidates-by-origin-path origin-path-at-point))))
    (dolist (entry entries)
      (paw-anki-editor-push-note entry t))
    (paw-search-update-buffer)

    ))


(defun paw-anki-editor-delete-notes ()
  "Find all notes/overlays in the same origin-path and save it into an org file under `paw-note-dir.'"
  (interactive)
  (let* ((entry-at-point (get-char-property (point) 'paw-entry))
         (word (alist-get 'word entry-at-point))
         (origin-path-at-point (alist-get 'origin_path (get-char-property (point) 'paw-entry)))
         (marked-entries (if (eq major-mode 'paw-search-mode)
                             (paw-find-marked-candidates)))
         (entries (or marked-entries (paw-candidates-by-origin-path origin-path-at-point))))
    (dolist (entry entries)
      (paw-anki-editor-delete-note entry t))
    (paw-search-update-buffer)

    ))


(defun paw-anki-editor-push-note (&optional entry no-update)
  "Push note at point to Anki.

If point is not at a heading with an `ANKI_NOTE_TYPE' property,
go up one heading at a time, until heading level 1, and push the
subtree associated with the first heading that has one."
  (interactive)
  (let* ((entry (or entry (get-char-property (point) 'paw-entry) ))
         (word (alist-get 'word entry))
         (sound (alist-get 'sound entry))
         (org-mode-hook nil)
         (content))
    ;; WORKAROUND, if push note in dashboard, we find the mp3 first, and update
    ;; entry, but would not generate mp3 at this stage, because it is very slow
    ;; if too many notes to push
    (unless sound
      (let ((sound (concat (expand-file-name (md5 word) paw-tts-cache-dir) ".mp3")))
        (if (file-exists-p sound)
            (setf (alist-get 'sound entry) sound))))
    (if (buffer-live-p (get-buffer "*anki*"))
        (kill-buffer "*anki*") )
    (with-current-buffer (get-buffer-create "*anki*" )
    ;; (with-temp-buffer
      (org-mode)
      (paw-insert-note entry :find-note t :export t :multiple-notes t :anki-editor t)
      (goto-char (point-min))
      (anki-editor-push-note-at-point)
      (setq content (json-encode `((anki-note-id . ,(org-entry-get nil anki-editor-prop-note-id)))) )
      (paw-db-update-content word content )

      )
    (unless no-update (paw-search-update-buffer))
    content))

(defun paw-anki-editor-delete-note (&optional entry no-update)
  "Push note at point to Anki.

If point is not at a heading with an `ANKI_NOTE_TYPE' property,
go up one heading at a time, until heading level 1, and push the
subtree associated with the first heading that has one."
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
          (paw-insert-note entry :find-note t :export t :multiple-notes t :anki-editor t)
          (goto-char (point-min))
          (paw-no-confirm 'anki-editor-delete-note-at-point)
          (paw-db-update-content word nil)

          ) )
    (unless no-update (paw-search-update-buffer))
    )
  )

(cl-flet ((always-yes (&rest _) t))
  (defun paw-no-confirm (fun &rest args)
    "Apply FUN to ARGS, skipping user confirmations."
    (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
              ((symbol-function 'yes-or-no-p) #'always-yes))
      (apply fun args))))


(provide 'paw-anki)
