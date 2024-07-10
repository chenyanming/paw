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
         (org-mode-hook nil)
         (id))
    (if (buffer-live-p (get-buffer "*anki*"))
        (kill-buffer "*anki*") )
    (with-current-buffer (get-buffer-create "*anki*" )
    ;; (with-temp-buffer
      (org-mode)
      (paw-insert-note entry :find-note t :export t :multiple-notes t :anki-editor t)
      (goto-char (point-min))
      (anki-editor-push-note-at-point)
      (setq id (org-entry-get nil anki-editor-prop-note-id))
      (paw-db-update-content word id )

      )
    (unless no-update (paw-search-update-buffer))
    id))

(defun paw-anki-editor-delete-note (&optional entry no-update)
  "Push note at point to Anki.

If point is not at a heading with an `ANKI_NOTE_TYPE' property,
go up one heading at a time, until heading level 1, and push the
subtree associated with the first heading that has one."
  (interactive)
  (let* ((entry (or entry (get-char-property (point) 'paw-entry) ))
         (word (alist-get 'word entry))
         (content (alist-get 'content entry))
         (id)
         (org-mode-hook nil))
    (if (buffer-live-p (get-buffer "*anki*"))
        (kill-buffer "*anki*") )
    (if content
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
