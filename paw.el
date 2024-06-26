;;; paw.el -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/paw
;; Keywords: tools
;; Created: 31 May 2021
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (request "0.3.3") (emacsql "3.0.0") (s "1.12.0") (dash "2.17.0") (go-translate "3.0.5") (gptel "0.8.6") (focus "1.0.1") (svg-lib "0.3"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs Annotation Tool

;;; Code:

(require 'paw-vars)
(require 'paw-db)
(require 'paw-util)
(require 'paw-note)
(require 'paw-annotation)
(require 'paw-org)
(require 'paw-gptel)
(require 'paw-faces)
(require 'paw-search)
(require 'paw-request)

(require 'thingatpt)
(require 'pcase)
(require 'ido)
(require 'ivy nil t)
(require 'consult nil t)
(require 'evil-core nil t)
(require 's)
(require 'dash)

(declare-function ivy-read "ivy")


(define-derived-mode paw-search-mode fundamental-mode "paw-search"
  "Major mode for display word lists.
\\{paw-search-mode-map}"
  (setq truncate-lines t
        buffer-read-only t
        header-line-format '(:eval (funcall paw-header-function)))
  (buffer-disable-undo)
  (add-function :before-until (local 'eldoc-documentation-function) #'paw-get-eldoc-word)
  (add-hook 'minibuffer-setup-hook 'paw-search-minibuffer-setup))

(defvar paw-header-function #'paw-header)

(defun paw-header ()
  "Header function for *paw* buffer."
  (format "%s"
          (format "Annotations: %s  Total: %s  Keyword: %s "
                  (propertize paw-db-file 'face 'font-lock-keyword-face)
                  (propertize (number-to-string (length paw-search-entries)) 'face 'font-lock-type-face)
                  (propertize paw-search-filter 'face 'font-lock-builtin-face))
          (format "%s%s"
                  (if paw-group-filteringp
                      "Group: "
                    "")
                  (if (equal paw-search-filter "")
                      ""
                    (concat paw-search-filter "   ")))))

(defvar paw-search-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map [mouse-1] #'paw-mouse-1)
    (define-key map "/" #'paw-search-live-filter)
    (define-key map "r" #'paw-search-refresh)
    (define-key map "R" #'paw-search-clear-filter)
    (define-key map "s" #'paw-view-note-query)
    (define-key map "S" #'paw-search-input)
    (define-key map "v" #'paw-view-note)
    (define-key map "V" #'paw-view-notes)
    (define-key map "<RET>" #'paw-find-origin)
    (define-key map "a" #'paw-add-word)
    (define-key map "c" #'paw-change-content)
    (define-key map "C" #'paw-change-note_type)
    (define-key map "d" #'paw-delete-word)
    (define-key map "u" #'paw-update-word)
    (define-key map "U" #'paw-sync-words)
    (define-key map "n" #'paw-next-word)
    (define-key map "y" #'paw-copy-annotation)
    (define-key map "p" #'paw-previous-word)
    (define-key map "N" #'paw-next-annotation)
    (define-key map "P" #'paw-previous-annotation)
    (define-key map "i" #'paw-find-note)
    (define-key map "I" #'paw-find-notes)
    (define-key map "'" #'paw-list-groups)
    (define-key map "q" #'paw-quit)
    (define-key map "m" #'paw-mark-and-forward)
    (define-key map (kbd "<DEL>") #'paw-unmark-and-backward)
    map)
  "Keymap for `paw-search-mode'.")

(if (fboundp 'evil-define-key)
    (evil-define-key '(normal emacs) paw-search-mode-map
      ;; (kbd "<mouse-1>") 'paw-mouse-1
      (kbd "/") 'paw-search-live-filter
      (kbd "g R") 'paw-search-refresh
      (kbd "g r") 'paw-search-clear-filter
      (kbd "s") 'paw-view-note-query
      (kbd "S") 'paw-search-input
      (kbd "v") 'paw-view-note
      (kbd "V") 'paw-view-notes
      (kbd "<RET>") 'paw-find-origin
      (kbd "a") 'paw-add-word
      (kbd "c c") 'paw-change-content
      (kbd "c n") 'paw-change-note_type
      (kbd "c p") 'paw-change-origin_path
      (kbd "D") 'paw-delete-word
      (kbd "d d") 'paw-delete-word
      (kbd "d p") 'paw-delete-words-by-origin_path
      (kbd "u") 'paw-update-word
      (kbd "U") 'paw-sync-words
      (kbd "n") 'paw-next-word
      (kbd "y y") 'paw-org-link-copy
      (kbd "y a") 'paw-copy-annotation
      (kbd "y w") 'paw-copy-word
      (kbd "p") 'paw-paste-word
      ;; (kbd "p") 'paw-previous-word
      (kbd "r") 'paw-replay
      (kbd "N") 'paw-next-annotation
      (kbd "P") 'paw-previous-annotation
      (kbd "i") 'paw-find-note
      (kbd "I") 'paw-find-notes
      (kbd "'") 'paw-list-groups
      (kbd "q") 'paw-quit
      (kbd "m") 'paw-mark-and-forward
      (kbd "o") 'paw-view-notes-outline
      (kbd "<DEL>") 'paw-unmark-and-backward) )

(defvar paw-print-entry-function #'paw-print-entry--default
  "Function to print entries into the *paw-search* buffer.")

(defcustom paw-word-min-width 10
  "Minimum column width for titles in the paw-search buffer."
  :group 'paw
  :type 'integer)

(defcustom paw-word-max-width 25
  "Maximum column width for titles in the paw-search buffer."
  :group 'paw
  :type 'integer)

(defcustom paw-trailing-width 30
  "Space reserved for displaying the feed and tag information."
  :group 'paw
  :type 'integer)

;;;###autoload
(defun paw (&optional silent path)
  (interactive "P")
  (paw-db)
  (let ((beg (point))
        (pos (window-start)))
    (with-current-buffer (paw-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let ((cands (if paw-search-entries
                         paw-search-entries
                       (progn
                         (setq paw-search-entries (nreverse (paw-db-select)))
                         (setq paw-full-entries paw-search-entries)) ))
              (id 0))
          (unless (equal cands '(""))   ; not empty list
            (cl-loop for entry in cands do
                     (progn
                       (setq id (1+ id))
                       (funcall paw-print-entry-function entry id)))))
        (paw-search-mode)))
    (if (eq major-mode 'paw-search-mode)
        (progn
          (set-window-start (selected-window) pos)
          (goto-char beg))
      (unless silent
        (switch-to-buffer (paw-buffer))
        (goto-char (point-min))))
    (when path
      (paw-search-update-buffer-with-keyword
       (if paw-annotation-mode (paw-get-origin-path) "")))))

(defun paw-goto-dashboard (&optional entry)
  (interactive)
  (let* ((entry (or entry (get-char-property (point) 'paw-entry)))
         (word (or paw-note-word (alist-get 'word entry) )))
    (if (window-live-p (get-buffer-window "*paw*"))
        (select-window (get-buffer-window "*paw*"))
      (if (buffer-live-p (get-buffer "*paw*"))
          (switch-to-buffer "*paw*")
        (paw-search-refresh)))
    (goto-char (point-min))
    (while (and
            (not (string= word (alist-get 'word (get-text-property (point) 'paw-entry))) )
            (not (eq (point) (point-max))))
      (forward-line 1))))



(defun paw-change-content ()
  "Change the content filed of entry at point."
  (interactive)
  (let* ((entry (get-char-property (point) 'paw-entry))
         (word (alist-get 'word entry))
         (real-word (paw-get-real-word word) )
         (content (alist-get 'content entry)))
    (let ((content (read-string "Change content: " (or content real-word))))
      ;; update content
      (paw-update-content word content)))
  ;; update buffer
  (if (buffer-live-p (get-buffer "*paw*"))
      (paw t)))

(defun paw-change-origin_point ()
  "Change the origin_point filed of entry at point."
  (interactive)
  (let* ((origin-point (paw-get-location)))
    (if (featurep 'ivy-read)
        (ivy-read (format "New Location %s for: " origin-point) (paw-candidates-format nil)
                  :sort nil
                  :action (lambda (x)
                            (let* ((entry (get-text-property 0 'paw-entry x))
                                   (word (alist-get 'word entry))
                                   (old-word (paw-get-real-word entry))
                                   (new-word (paw-get-word))
                                   (is-same (if (equal old-word new-word)
                                                t
                                              (yes-or-no-p "The word at point/region is different than the annotation, still change? "))))
                              (when is-same
                                (paw-update-origin_point word origin-point)
                                ;; delete the old overlay
                                (-map (lambda (b)
                                        (with-current-buffer b
                                          (if (eq paw-annotation-mode t)
                                              (let ((o (cl-find-if
                                                        (lambda (o)
                                                          (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                                                        (overlays-in (point-min) (point-max)))))
                                                (if o (delete-overlay o))))))
                                      (buffer-list))
                                ;; update back the origin_point to entry
                                (setf (alist-get 'origin_point entry) origin-point)
                                ;; add overlay
                                (paw-add-annotation-overlay entry)
                                ;; quit mark
                                (if (featurep 'evil)
                                    (evil-force-normal-state))
                                ;; update buffer
                                (if (buffer-live-p (get-buffer "*paw*"))
                                    (paw t)) ))))
      (let* ((entry (consult--read (paw-candidates-format nil)
                                   :prompt (format "New Location %s for: " origin-point)
                                   :sort nil
                                   :lookup (lambda(_ candidates cand)
                                             (get-text-property 0 'paw-entry (assoc cand candidates)))
                                   ))
             (word (alist-get 'word entry))
             (old-word (paw-get-real-word entry))
             (new-word (paw-get-word))
             (is-same (if (equal old-word new-word)
                          t
                        (yes-or-no-p "The word at point/region is different than the annotation, still change? "))))
        (when is-same
          (paw-update-origin_point word origin-point)
          ;; delete the old overlay
          (-map (lambda (b)
                  (with-current-buffer b
                    (if (eq paw-annotation-mode t)
                        (let ((o (cl-find-if
                                  (lambda (o)
                                    (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                                  (overlays-in (point-min) (point-max)))))
                          (if o (delete-overlay o))))))
                (buffer-list))
          ;; update back the origin_point to entry
          (setf (alist-get 'origin_point entry) origin-point)
          ;; add overlay
          (paw-add-annotation-overlay entry)
          ;; quit mark
          (if (featurep 'evil)
              (evil-force-normal-state))
          ;; update buffer
          (if (buffer-live-p (get-buffer "*paw*"))
              (paw t)) )))))

(defun paw-change-note_type ()
  "Change the note_type filed of entry at point."
  (interactive)
  (let* ((origin-point (paw-get-location))
         (type (if (featurep 'ivy-read)
                   (ivy-read (format "New Annotation Type: ") paw-note-type-alist
                             :sort nil)
                 (consult--read paw-note-type-alist
                                :prompt "New Annotation Type: "
                                :sort nil)))
         (new-note-type (assoc (intern type) paw-note-type-alist) ))
    (let* ((entry (get-char-property (point) 'paw-entry))
           (word (alist-get 'word entry))
           (old-word (paw-get-real-word entry))
           (new-word (paw-get-word)))
      (paw-update-note_type word new-note-type)
      ;; delete the old overlay
      (-map (lambda (b)
              (with-current-buffer b
                (if (eq paw-annotation-mode t)
                    (let ((o (cl-find-if
                              (lambda (o)
                                (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                              (overlays-in (point-min) (point-max)))))
                      (if o (delete-overlay o))))))
            (buffer-list))
      ;; update back the note_type to entry
      (setf (alist-get 'note_type entry) new-note-type)
      ;; add overlay
      (paw-add-annotation-overlay entry)
      ;; quit mark
      (if (featurep 'evil)
          (evil-force-normal-state))
      ;; update buffer
      (if (buffer-live-p (get-buffer "*paw*"))
          (paw t)))))

(defun paw-change-origin_path ()
  "Change the origin_path filed of entry at point."
  (interactive)
  (let* ((origin-path (paw-get-origin-path))
         (new-origin-path (let ((file (if (yes-or-no-p "Select(y) or input(n) a path? ")
                                          (read-file-name "Select the new path: ")
                                        (read-string "Input the new path: ")) ))
                            (if (file-exists-p file)
                                (abbreviate-file-name file)
                              (user-error "No this file, please input another path")))))
    (paw-db-update-all-origin_path origin-path new-origin-path)
    (paw-entry-update-all-origin_path origin-path new-origin-path)
    (if (buffer-live-p (get-buffer "*paw*"))
        (paw t))))



;;; TODO
;;;###autoload
(defun paw-delete-word (&optional entry)
  "Delete marked word(s)."
  (interactive)
  (let* ((marked-entries (paw-find-marked-candidates))
         (entries
          (or marked-entries
              (if entry (list entry)
                (if (get-text-property (point) 'paw-entry)
                    (list (get-text-property (point) 'paw-entry))
                  ;; any word at point
                  (list (paw-new-entry (word-at-point t) :add-to-known-words t)))))))
    (when (yes-or-no-p (if (eq (length entries) 1)
                           (progn
                             (if (alist-get 'add-to-known-words (car entries))
                                 (format "Add '%s' to known words? " (alist-get 'word (car entries)))
                               (format "Delete: %s " (alist-get 'word (car entries)))))
                         (format "Delete %s entries" (length entries)) ))
      (when entries
        (cl-loop for entry in entries do
                 (let* ((word (alist-get 'word entry))
                        (content (alist-get 'content entry))
                        (content-json (condition-case nil
                                          (let ((output (json-read-from-string content)))
                                            (if (and (not (eq output nil))
                                                     (not (arrayp output))
                                                     (not (numberp output)))
                                                output
                                              nil))
                                        (error nil)))
                        (content-filename (alist-get 'filename content-json))
                        (content-path (alist-get 'path content-json))
                        (note-type (alist-get 'note_type entry))
                        (serverp (alist-get 'serverp entry))
                        (origin_id (alist-get 'origin_id entry))
                        (lang (alist-get 'lang entry))
                        (add-to-known-words (alist-get 'add-to-known-words entry))
                        (file (expand-file-name (concat word ".org") temporary-file-directory))
                        (deleted nil))
                   (if add-to-known-words ;; if add-to-known-words is t, we put the word to known file instead of deleting it in db
                       (pcase lang
                         ("en" (if (and paw-ecdict-default-known-words-file (file-exists-p paw-ecdict-default-known-words-file))
                                 (with-temp-buffer
                                   (insert-file-contents paw-ecdict-default-known-words-file)
                                   (goto-char (point-max))
                                   (insert word "\n")
                                   (write-region (point-min) (point-max) paw-ecdict-default-known-words-file)
                                   (message "Added %s to known words." word)
                                   (setq deleted t))
                                 (message "Known words file not exists.")))
                         ("ja" (if (and paw-jlpt-default-known-words-file (file-exists-p paw-jlpt-default-known-words-file))
                                   (with-temp-buffer
                                     (insert-file-contents paw-jlpt-default-known-words-file)
                                     (goto-char (point-max))
                                     (insert word "\n")
                                     (write-region (point-min) (point-max) paw-jlpt-default-known-words-file)
                                     (message "Added %s to known words." word)
                                     (setq deleted t))
                                 (message "Known words file not exists.")))
                         (_ (message "Unsupport language %s during adding known words." lang)))
                       (if (not (paw-online-p serverp))
                           ;; not in the server delete directly
                           (progn
                             (paw-db-delete word)
                             ;; delete image/attachment
                             (if content-path
                                 (pcase (car note-type)
                                   ('image (let ((png (expand-file-name content-path paw-note-dir)))
                                             (if (file-exists-p png)
                                                 (delete-file png)
                                               (message "Image %s not exists." png))))
                                   ('attachment (let ((attachment (expand-file-name content-path paw-note-dir)))
                                                  (if (file-exists-p attachment)
                                                      (delete-file attachment)
                                                    (message "Attachment %s not exists." attachment))))
                                   (_ nil)))
                             (setq deleted t))
                         ;; it is in the server, must delete server's first
                         (paw-request-delete-words word origin_id))

                     )


                   ;; delete overlay search on the buffers enable `paw-annotation-mode'
                   (if deleted
                       (-map (lambda (b)
                               (with-current-buffer b
                                 (if (eq paw-annotation-mode t)
                                     (let ((overlays-to-delete
                                            (cl-remove-if-not
                                             (lambda (o) (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                                             (overlays-in (point-min) (point-max)))))
                                       (dolist (o overlays-to-delete) ; delete all matching overlays
                                         (delete-overlay o))
                                       (setq-local paw-db-update-p t))))) ; update the
                             (buffer-list)) )))
        (if (eq major-mode 'paw-search-mode)
            (paw-search-refresh)
          (paw-search-refresh t))))))

(defvar paw-copied-entries nil)

(defun paw-copy-word ()
  "Copy marked word(s)."
  (interactive)
  (let* ((marked-entries (paw-find-marked-candidates))
         (entries (or marked-entries (list (get-text-property (point) 'paw-entry)))))
    (paw-clear-marks)
    (message "Copied %s entries" (length entries))
    (setq paw-copied-entries entries)))

(defun paw-paste-word ()
  "Paste marked word(s)."
  (interactive)
  (dolist (entry paw-copied-entries)
    (paw-db-insert
     `(((word . ,(alist-get 'word entry)) (exp . ,(alist-get 'exp entry))))
     :content (alist-get 'content entry)
     :serverp (alist-get 'serverp entry)
     :note (alist-get 'note entry)
     :note_type (alist-get 'note_type entry)
     :origin_type (alist-get 'origin_type entry)
     :origin_path (alist-get 'origin_path entry)
     :origin_id (alist-get 'origin_id entry)
     :origin_point (alist-get 'origin_point entry)
     :created_at (alist-get 'created_at entry)))
  (message "Pasted %s entries" (length paw-copied-entries))
  (paw-search-refresh)
  (setq paw-copied-entries nil))

(defun paw-delete-words-by-origin_path ()
  "TODO: Delete all words by origin_path in database and entries.
It is fast but has drawbacks:
1. Image and attachments could not be deleted.
2. Overlay should be disabled manually."
  (interactive)
  (let ((origin-path (paw-get-origin-path)))
    (when (yes-or-no-p (format "Delete all notes under %s? " origin-path))
      (paw-db-delete-words-by-origin_path origin-path)
      (paw-entry-delete-words-by-origin_path origin-path)
      (if (buffer-live-p (get-buffer "*paw*"))
          (paw t)))))

(defun paw-update-word (prefix)
  "Update the word meaning with sdcv or gptel."
  (interactive "P")
  (let* ((entry (get-text-property (point) 'paw-entry))
         (id (alist-get 'word entry))
         (word (paw-get-real-word id))
         (type (alist-get 'note_type entry))
         (file (expand-file-name (concat (if (string-match ":id:\\(.*\\)" id)
                                             (match-string 1 id)
                                           id) ".org") temporary-file-directory)))
    (when (yes-or-no-p (format "Are your sure to add exp for: %s? " word))

      ;; update content with gptel
      (if prefix
          (paw-gptel-update-note id word (car type)
                                 (lambda ()
                                   (when (file-exists-p file)
                                     (let ((buffer (find-buffer-visiting file)))
                                       (if buffer
                                           (kill-buffer buffer)))
                                       (delete-file file))
                                   (paw-search-refresh)))
        ;; update content with sdcv
        (paw-update-note
         id
         (s-collapse-whitespace (sdcv-translate-result word sdcv-dictionary-complete-list)))
        (message "Update word done.")
        (paw-search-refresh)))))

(defun paw-previous-word ()
  "Jump to previous headline."
  (interactive)
  (paw-previous)
  (paw-view-note))

(defun paw-next-word ()
  "Jump to next headline."
  (interactive)
  (paw-next)
  (paw-view-note))


(defun paw-list-groups ()
  (interactive)
  (if (featurep 'ivy-read)
      (ivy-read "Select the annotation group: " (paw-get-all-origin-path)
                :action (lambda(x)
                          (paw-search-update-buffer-with-keyword (car x))))
    (paw-search-update-buffer-with-keyword
     (consult--read (append (paw-get-all-origin-path) (paw-get-all-study-list))
                     :prompt "Select the annotation group: "
                     ;; :lookup (lambda(_ candidates cand)
                     ;;           (car (assoc cand candidates)))
                     ))))

(defun paw-get-eldoc-note ()
  (let ((entry (get-char-property (point) 'paw-entry)))
    (if entry
        (or (alist-get 'note entry) ""))))

(defun paw-get-eldoc-word ()
  (let ((entry (get-char-property (point) 'paw-entry)))
    (if entry
        (or (s-collapse-whitespace (paw-get-real-word entry)) ""))))

(defun paw-search-input ()
  (interactive)
  (let* ((sdcv-say-word-p t)
         (word (paw-get-real-word (get-text-property (point) 'paw-entry)))
         (default (if word (format " (default %s)" word) ""))
         (final-word (read-string (format "Stardict%s: " default) nil nil word)))
    (sdcv-search-detail final-word)))

(defun paw-print-entry--default (entry num)
  "Print ENTRY to the buffer."
  (unless (equal entry "")
    (let (beg end)
      (setq beg (point))
      (insert (paw-parse-entry-as-string entry))
      (setq end (point))
      (put-text-property beg end 'paw-entry entry)
      (put-text-property beg end 'paw-id num)
      (insert "\n"))))

(defun paw-parse-entry-as-string (entry)
  "Parse the paw ENTRY and return as string."
  (let* ((word (or (alist-get 'word entry) "NO TITLE"))
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
         (serverp (alist-get 'serverp entry))
         (note (alist-get 'note entry))
         (note-type (alist-get 'note_type entry))
         (origin-type (alist-get 'origin_type entry))
         (origin-path (alist-get 'origin_path entry))
         (origin-point (alist-get 'origin_point entry))
         (created-at (alist-get 'created_at entry))
         (word-width (- (window-width (get-buffer-window (paw-buffer))) 10 paw-trailing-width)))
    (format "%s %s  %s %s  %s"
            (paw-format-icon note-type content serverp)
            (pcase serverp
              (0
               (s-pad-right 40 " " (propertize (s-truncate 40 word) 'face '(:foreground "skyblue")) ))
              (1
               (s-pad-right 40 " " (propertize (s-truncate 40 word) 'face 'default) ))
              (2 ;; offline words
               (paw-format-content note-type word content content-path content-filename))
              (_
               (s-pad-right 40 " " (propertize (s-truncate 40 word) 'face 'default) )))
            (s-pad-right 12 " " (s-truncate 10 created-at ""))
            (s-pad-right 30 " " (s-collapse-whitespace (s-truncate 30
                                                                   (if (stringp origin-point)
                                                                       origin-point
                                                                     (if origin-path
                                                                         (pcase origin-type
                                                                           ('wallabag-entry-mode
                                                                            (propertize origin-path 'face 'paw-wallabag-face))
                                                                           ('nov-mode
                                                                            (propertize (file-name-nondirectory origin-path) 'face 'paw-nov-face))
                                                                           ((or 'pdf-view-mode 'nov-mode "pdf-viewer")
                                                                            (propertize (file-name-nondirectory origin-path) 'face 'paw-pdf-face))
                                                                           ((or 'eaf-mode "browser" 'eww-mode)
                                                                            (propertize origin-path 'face 'paw-link-face))
                                                                           (_ (propertize (file-name-nondirectory origin-path ) 'face 'paw-file-face)))
                                                                         "")))))
            (s-pad-right 40 " " (s-collapse-whitespace (s-truncate 40 (or exp note "")))))))

(defun paw-quit ()
  "Quit paw."
  (interactive)
  (when (get-buffer "*paw*")
    (quit-window)
    (kill-buffer "*paw*")
    (setq paw-search-entries nil)
    (setq paw-full-entries nil))
  (let ((buffer (cl-find-if
                 (lambda (b)
                   (with-current-buffer b (eq major-mode 'paw-note-mode)))
                 (buffer-list))))
    (cond ((get-buffer paw-view-note-buffer-name)
           (pop-to-buffer paw-view-note-buffer-name))
          (buffer
           (pop-to-buffer buffer))))

  ;; close the db, so that it will release the db, and start to sync
  ;; it is a trade off, we lost load speed
  ;; TODO Find a better way to release the db
  (paw-close-db)

  ;; (when (eq major-mode 'paw-search-mode)
  ;;   (cond ((get-buffer paw-view-note-buffer-name)
  ;;          (pop-to-buffer paw-view-note-buffer-name)
  ;;          (if (< (length (window-prev-buffers)) 2)
  ;;              (progn
  ;;                (delete-window)
  ;;                (kill-buffer paw-view-note-buffer-name))
  ;;            (kill-buffer paw-view-note-buffer-name)))
  ;;         ((get-buffer "*paw*")
  ;;          (quit-window)
  ;;          (kill-buffer "*paw*"))))
  )


;;; customization
;; (defun +org/dwim-at-point-advice (orig-fun &rest args)
;;   (if (get-char-property (point) 'paw-entry)
;;       (paw-goto-dashboard)
;;     (apply orig-fun args)))

;; (advice-add '+org/dwim-at-point :around #'+org/dwim-at-point-advice)

(provide 'paw)
