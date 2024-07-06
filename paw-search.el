;;; paw-search.el -*- lexical-binding: t; -*-

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
  (setq paw-search-current-page (if (= paw-search-pages 0) 0 1))
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

(defvar paw-group-filteringp nil)

(defun paw-search-update-buffer (&optional page)
  "Update the *paw-search* buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed."
  (interactive)
  (with-current-buffer (paw-search-buffer)
    (let* ((inhibit-read-only t)
           (standard-output (current-buffer))
           (id 0)
           (group-p paw-group-filteringp)
           (entries (if group-p
                        (paw-search-get-grouped-entries page)
                      (paw-search-get-filtered-entries page)))
           (len (length entries)))
      (setq paw-search-entries-length (cdaar (paw-db-select
                                              `[:select (funcall count word)
                                               :from
                                               ,(paw-search-parse-filter paw-search-filter :group-p group-p)])))
      (setq paw-search-pages (ceiling paw-search-entries-length paw-search-page-max-rows))
      (erase-buffer)
      (dolist (entry entries)
        (setq id (1+ id))
        (if (<= id paw-search-page-max-rows)
            (funcall paw-print-entry-function entry id)))
      (if (< len paw-search-entries-length)
          (dotimes (i paw-search-pages)
            (insert " " (buttonize (format "%d" (1+ i)) #'paw-search-more-data (1+ i)) " "))
        (insert "End of entries.\n"))
      (goto-char (point-min)))))


(defun paw-search-more-data (page)
  (let ((inhibit-read-only t))
    (setq paw-search-current-page page)
    (beginning-of-line)
    (delete-region (point) (progn (forward-line 1) (point)))
    (paw-search-update-buffer page)))

(defun paw-search-next-page ()
  (interactive)
  (if (< paw-search-current-page paw-search-pages)
      (progn
        (setq paw-search-current-page (1+ paw-search-current-page))
        (paw-search-update-buffer paw-search-current-page) )
    (message "Last page.")))

(defun paw-search-previous-page ()
  (interactive)
  (if (> paw-search-current-page 1)
      (progn
        (setq paw-search-current-page (1- paw-search-current-page))
        (paw-search-update-buffer paw-search-current-page) )
    (message "First page.")))

(defun paw-search-get-filtered-entries (&optional page)
  (let* ((sql (paw-search-parse-filter paw-search-filter :limit paw-search-page-max-rows :page page))
         (entries (paw-db-select sql)))
    entries))

(defun paw-search-get-grouped-entries (&optional page)
  (let* ((sql (paw-search-parse-filter paw-search-filter :limit paw-search-page-max-rows :page page :group-p t))
         (entries (paw-db-select sql)))
    entries))


(defcustom paw-search-page-max-rows 45
  "The maximum number of entries to display in a single page."
  :group 'paw
  :type 'integer)

(defvar paw-search-current-page 1
  "The number of current page in the current search result.")

(defvar paw-search-pages 0
  "The number of pages in the current search result.")

(defun paw-search-parse-filter (filter &rest properties)
  "Parse the elements of a search FILTER into an emacsql."
  (let* ((limit (plist-get properties :limit))
         (page (plist-get properties :page))
         (group-p (plist-get properties :group-p))
         (words))
    (if group-p
        (setq words filter)
      (setq words (split-string filter " ")))
    (apply #'vector
           (append '(:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
                     :inner :join status
                     :on (= items:word status:word))
                   `(,@(when words
                         (if group-p
                             ;; only search group
                             (list :where
                                   `(or
                                     ,@(when words
                                         `((= status:origin_path ,words)))
                                     ,@(when words
                                         `((in origin_path ,(vconcat (-map (lambda (dir)
                                                                            (concat dir (file-name-nondirectory words))) ;; no need to expand, otherwise, the string will be different and can not match in sql
                                                                          paw-annotation-search-paths)))))

                                     ,@(when words
                                         `((= status:origin_point ,words)))))
                           ;; vague search
                             (list :where
                               `(or
                                 ,@(when words
                                     (cl-loop for word in words
                                              collect `(like items:word ,(concat "%" word "%"))))
                                 ,@(when words
                                     (cl-loop for word in words
                                              collect `(like status:created_at ,(concat "%" word "%"))))
                                 ,@(when words
                                     (cl-loop for word in words
                                              collect `(like status:origin_path ,(concat "%" word "%"))))
                                 ,@(when words
                                     (cl-loop for word in words
                                              collect `(like status:origin_point ,(concat "%" word "%"))))
                                 ))))

                     :order-by (desc status:created_at)
                   ,@(when limit
                       (list :limit limit) )
                   ,@(when page
                       (list :offset (* (1- page) paw-search-page-max-rows)))
                   )

                   ))))

(defun paw-valid-regexp-p (regexp)
  "Return t if REGEXP is a valid REGEXP."
  (ignore-errors
    (prog1 t
      (string-match-p regexp ""))))

(defun paw-search-clear-filter ()
  "Clear the fitler keyword."
  (interactive)
  (setq paw-group-filteringp nil)
  (setq paw-search-current-page (if (= paw-search-pages 0) 0 1))
  (paw-search-update-buffer-with-keyword ""))

(defun paw-search-update-buffer-with-keyword (keyword)
  "Filter the *paw-search* buffer with KEYWORD."
  (setq paw-search-filter keyword)
  (paw-search-update-buffer))


(defun paw-search-refresh (&optional silent)
  (interactive)
  (paw-search-clear-filter)
  (paw silent))

(provide 'paw-search)
