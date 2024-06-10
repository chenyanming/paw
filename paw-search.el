;;; paw-search.el -*- lexical-binding: t; -*-

(defvar paw-search-entries nil
  "List of the entries currently on display.")

(defvar paw-full-entries nil
  "List of the entries currently on database.")

(defvar paw-live-filteringp nil)
(defvar paw-group-filteringp nil)
(defvar paw-search-filter-active nil
  "When non-nil, paw is currently reading a filter from the minibuffer.
When live editing the filter, it is bound to :live.")

(defcustom paw-search-filter ""
  "Query string filtering shown entries."
  :group 'paw
  :type 'string)

(defun paw-search-buffer ()
  "Create buffer *paw*."
  (get-buffer-create "*paw*"))

(defun paw-search-live-filter ()
  (interactive)
  (setq paw-live-filteringp t)
  (setq paw-group-filteringp nil)
  (unwind-protect
      (let ((paw-search-filter-active :live))
        (setq paw-search-filter
              (read-from-minibuffer "Filter: " paw-search-filter))
        (message paw-search-filter))
    (progn (paw-search-update-buffer)
           (setq paw-live-filteringp nil))))

(defun paw-search-minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when paw-search-filter-active
    (when (eq :live paw-search-filter-active)
      (add-hook 'post-command-hook 'paw-search-update-buffer-with-minibuffer-contents nil :local))))

(defun paw-search-update-buffer-with-minibuffer-contents ()
  "Update the *paw-search* buffer based on the contents of the minibuffer."
  (when (eq :live paw-search-filter-active)
    ;; (message "HELLO")
    (let ((buffer (paw-search-buffer))
          (current-filter (minibuffer-contents-no-properties)))
      (when buffer
        (with-current-buffer buffer
          (let ((paw-search-filter current-filter))
            (paw-search-update-buffer)))))))

(defun paw-search-update-buffer ()
  "Update the *paw-search* buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed."
  (interactive)
  (with-current-buffer (paw-search-buffer)
    (let ((inhibit-read-only t)
          (standard-output (current-buffer))
          (id 0))
      (erase-buffer)
      (paw-search-update-list)
      (dolist (entry paw-search-entries)
        (setq id (1+ id))
        (funcall paw-print-entry-function entry id))
      ;; (insert "End of entries.\n")
      (goto-char (point-min)))))

(defun paw-search-update-list (&optional filter)
  "Update `paw-search-entries' list."
  ;; replace space with _ (SQL) The underscore represents a single character
  (if filter
      (setq paw-search-filter filter)
    (setq filter paw-search-filter))
  (let* ((filter (paw-search-parse-filter filter)) ;; (replace-regexp-in-string " " "_" paw-search-filter)
         (head (paw-search-filter-candidate filter)))
    ;; Determine the final list order
    (let ((entries head))
      (setf paw-search-entries
            entries))))

(defun paw-search-parse-filter (filter)
  "Parse the elements of a search FILTER into an alist."
  (let ((matches ()))
    (cl-loop for element in (split-string filter) collect
             (when (paw-valid-regexp-p element)
               (push element matches)))
    `(,@(if matches
            (list :matches matches)))))

(defun paw-valid-regexp-p (regexp)
  "Return t if REGEXP is a valid REGEXP."
  (ignore-errors
    (prog1 t
      (string-match-p regexp ""))))

(defun paw-search-filter-candidate (filter)
  "Generate ebook candidate alist.
ARGUMENT FILTER is the filter string."
  (let ((matches (plist-get filter :matches))
        res-list)
    (cl-loop for entry in paw-full-entries do
             (if (eval `(and ,@(cl-loop for regex in matches collect
                                        (or
                                         (string-match-p regex (let ((origin-point (alist-get 'origin_point entry)))
                                                                 (if (stringp origin-point)
                                                                     origin-point
                                                                   "")))
                                         (string-match-p regex (or (alist-get 'origin_path entry) ""))
                                         (string-match-p regex (alist-get 'word entry))
                                         (string-match-p regex (or (alist-get 'note entry) "") )))))
                 (push entry res-list)))
    (nreverse res-list)))

(defun paw-search-clear-filter ()
  "Clear the fitler keyword."
  (interactive)
  (setq paw-group-filteringp nil)
  (paw-search-update-buffer-with-keyword ""))

(defun paw-search-update-buffer-with-keyword (keyword)
  "Filter the *paw-search* buffer with KEYWORD."
  (setq paw-search-filter keyword)
  (paw-search-update-buffer))


(defun paw-search-refresh (&optional silent)
  (interactive)
  (paw-search-clear-filter)
  (setq paw-search-entries (nreverse (paw-db-select)))
  (setq paw-full-entries paw-search-entries)
  (paw silent))

(provide 'paw-search)
