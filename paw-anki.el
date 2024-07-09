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

(defun paw-anki-editor-push-note-at-point ()
  "Push note at point to Anki.

If point is not at a heading with an `ANKI_NOTE_TYPE' property,
go up one heading at a time, until heading level 1, and push the
subtree associated with the first heading that has one."
  (interactive)
  (let* ((entry (get-char-property (point) 'paw-entry))
        (word (alist-get 'word entry))
        (id))
    (if (buffer-live-p (get-buffer "*anki*"))
        (kill-buffer "*anki*") )
    (with-current-buffer (get-buffer-create "*anki*" )
    (org-mode)
    (paw-insert-note entry :find-note t :export t :multiple-notes t :anki-editor t)
    (save-excursion
      (let ((note-type))
        (while
            (and (org-back-to-heading)
                 (not (setq note-type
                            (org-entry-get nil anki-editor-prop-note-type)))
                 (org-up-heading-safe)))
        (if (not note-type)
            (user-error "No note to push found")

          (anki-editor--push-note (anki-editor-note-at-point))
          ;; (message (org-entry-get nil anki-editor-prop-note-id) )

          (paw-db-update-content word (org-entry-get nil anki-editor-prop-note-id) )
          (message "Successfully pushed note at point to Anki.")

          )))
    ) )
  )


(provide 'paw-anki)
