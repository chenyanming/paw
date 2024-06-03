;;; pen/pen-search.el -*- lexical-binding: t; -*-

(defvar pen-search-entries nil
  "List of the entries currently on display.")

(defvar pen-full-entries nil
  "List of the entries currently on database.")

(defvar pen-live-filteringp nil)
(defvar pen-group-filteringp nil)
(defvar pen-search-filter-active nil
  "When non-nil, pen is currently reading a filter from the minibuffer.
When live editing the filter, it is bound to :live.")

(defcustom pen-search-filter ""
  "Query string filtering shown entries."
  :group 'pen
  :type 'string)

(defun pen-search-buffer ()
  "Create buffer *pen*."
  (get-buffer-create "*pen*"))

(defun pen-search-live-filter ()
  (interactive)
  (setq pen-live-filteringp t)
  (setq pen-group-filteringp nil)
  (unwind-protect
      (let ((pen-search-filter-active :live))
        (setq pen-search-filter
              (read-from-minibuffer "Filter: " pen-search-filter))
        (message pen-search-filter))
    (progn (pen-search-update-buffer)
           (setq pen-live-filteringp nil))))

(defun pen-search-minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when pen-search-filter-active
    (when (eq :live pen-search-filter-active)
      (add-hook 'post-command-hook 'pen-search-update-buffer-with-minibuffer-contents nil :local))))

(defun pen-search-update-buffer-with-minibuffer-contents ()
  "Update the *pen-search* buffer based on the contents of the minibuffer."
  (when (eq :live pen-search-filter-active)
    ;; (message "HELLO")
    (let ((buffer (pen-search-buffer))
          (current-filter (minibuffer-contents-no-properties)))
      (when buffer
        (with-current-buffer buffer
          (let ((pen-search-filter current-filter))
            (pen-search-update-buffer)))))))

(defun pen-search-update-buffer ()
  "Update the *pen-search* buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed."
  (interactive)
  (with-current-buffer (pen-search-buffer)
    (let ((inhibit-read-only t)
          (standard-output (current-buffer))
          (id 0))
      (erase-buffer)
      (pen-search-update-list)
      (dolist (entry pen-search-entries)
        (setq id (1+ id))
        (funcall pen-print-entry-function entry id))
      ;; (insert "End of entries.\n")
      (goto-char (point-min)))))

(defun pen-search-update-list (&optional filter)
  "Update `pen-search-entries' list."
  ;; replace space with _ (SQL) The underscore represents a single character
  (if filter
      (setq pen-search-filter filter)
    (setq filter pen-search-filter))
  (let* ((filter (pen-search-parse-filter filter)) ;; (replace-regexp-in-string " " "_" pen-search-filter)
         (head (pen-search-filter-candidate filter)))
    ;; Determine the final list order
    (let ((entries head))
      (setf pen-search-entries
            entries))))

(defun pen-search-parse-filter (filter)
  "Parse the elements of a search FILTER into an alist."
  (let ((matches ()))
    (cl-loop for element in (split-string filter) collect
             (when (pen-valid-regexp-p element)
               (push element matches)))
    `(,@(if matches
            (list :matches matches)))))

(defun pen-valid-regexp-p (regexp)
  "Return t if REGEXP is a valid REGEXP."
  (ignore-errors
    (prog1 t
      (string-match-p regexp ""))))

(defun pen-search-filter-candidate (filter)
  "Generate ebook candidate alist.
ARGUMENT FILTER is the filter string."
  (let ((matches (plist-get filter :matches))
        res-list)
    (cl-loop for entry in pen-full-entries do
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

(defun pen-search-clear-filter ()
  "Clear the fitler keyword."
  (interactive)
  (setq pen-group-filteringp nil)
  (pen-search-update-buffer-with-keyword ""))

(defun pen-search-update-buffer-with-keyword (keyword)
  "Filter the *pen-search* buffer with KEYWORD."
  (setq pen-search-filter keyword)
  (pen-search-update-buffer))


(defun pen-search-refresh (&optional silent)
  (interactive)
  (pen-search-clear-filter)
  (setq pen-search-entries (nreverse (pen-db-select)))
  (setq pen-full-entries pen-search-entries)
  (pen silent))

(provide 'pen-search)
