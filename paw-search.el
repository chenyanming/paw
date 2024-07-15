;;; paw-search.el -*- lexical-binding: t; -*-

(defvar paw-live-filteringp nil)
(defvar paw-group-filteringp nil)
(defvar paw-search-filter-active nil
  "When non-nil, paw is currently reading a filter from the minibuffer.
When live editing the filter, it is bound to :live.")


(defcustom paw-trailing-width 30
  "Space reserved for displaying the feed and tag information."
  :group 'paw
  :type 'integer)


(defvar paw-print-entry-function #'paw-print-entry--default
  "Function to print entries into the *paw-search* buffer.")

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
         (anki-note-id (alist-get 'anki-note-id content-json))
         (serverp (alist-get 'serverp entry))
         (note (alist-get 'note entry))
         (note-type (alist-get 'note_type entry))
         (origin-type (alist-get 'origin_type entry))
         (origin-path (alist-get 'origin_path entry))
         (origin-point (alist-get 'origin_point entry))
         (created-at (alist-get 'created_at entry))
         (word-width (- (window-width (get-buffer-window (paw-buffer))) 10 paw-trailing-width)))
    (format "%s %s %s %s %s %s %s"
            (paw-format-icon note-type content serverp)
            (pcase serverp
              (2 ;; local annotations
               (paw-format-content note-type word content content-path content-filename))
              (_
               (s-pad-right 10 " " (propertize (s-truncate 10 word) 'face 'default) )))
            (s-pad-right 12 " " (propertize (s-truncate 10 created-at "") 'face 'paw-date-face ))
            (s-collapse-whitespace (propertize (if (stringp origin-point)
                                                   origin-point
                                                 "" ) 'face 'paw-origin-point-face ))
            (s-collapse-whitespace (if origin-path
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
                                     ""))
            (s-collapse-whitespace (propertize (if (s-blank-str? exp) "" (format "| %s |" exp)) 'face 'paw-exp-face ))
            (s-collapse-whitespace (or (replace-regexp-in-string word (propertize word 'face '(bold underline)) note) "")))))

(defun paw-format-content (note-type word content content-path content-filename)
  (pcase (car note-type)
    ('attachment
     (s-pad-right 10 " "
                  (let* ((ext (downcase (file-name-extension content-path)))
                         (ext (if (string= ext "jpg") "jpeg" ext)))
                    (pcase ext
                      ((or "pbm" "xbm" "xpm" "gif" "jpeg" "tiff" "png" "svg" "jpg")
                       (propertize "IMAGS"
                                   'face 'paw-offline-face
                                   'display (create-image (expand-file-name content-path paw-note-dir) nil nil :width (if (eq system-type 'gnu/linux) 200 100) :height nil  :margin '(0 . 1))))
                      (_ (propertize (format "%s %s" (paw-attach-icon-for (expand-file-name content-filename)) (paw-format-column content-filename 40 :left) )
                                     'face 'paw-offline-face))) ) ))
    ('image
     (s-pad-right 10 " "
                  (propertize "IMAGS"
                              'face 'paw-offline-face
                              'display (create-image (expand-file-name content-path paw-note-dir) nil nil :width (if (eq system-type 'gnu/linux) 200 100) :height nil :margin '(0 . 1))) ))
    (_ (s-pad-right 10 " "
                    (propertize (s-truncate 10 (s-collapse-whitespace (or (if (equal content 0) word (if content content "")) (paw-get-real-word word))))
                                'face 'paw-offline-face)))))

;;; format
(defun paw-format-icon (note-type content serverp)
  "Return the icon based on NOTE-TYPE and CONTENT.
CONTENT is useful for sub types, for example, link."
  (pcase (car note-type)
    ('word
     (propertize (cdr note-type) 'display (let* ((content-json (condition-case nil
                                                                   (let ((output (json-read-from-string content)))
                                                                     (if (and (not (eq output nil))
                                                                              (not (arrayp output))
                                                                              (not (numberp output)))
                                                                         output
                                                                       nil))
                                                                 (error nil)))
                                                 (anki-note-id (alist-get 'anki-note-id content-json)))
                                            (if anki-note-id
                                                paw-word-icon
                                              paw-star-face-icon)
                                            ;; (pcase serverp
                                            ;;   (1 )


                                            ;;   paw-word-icon)
                                            )))
    ('image
     (propertize (cdr note-type) 'display paw-image-icon))
    ('bookmark
     (propertize (cdr note-type) 'display paw-bookmark-icon))
    ('attachment
     (propertize (cdr note-type) 'display paw-attachment-icon))
    ('question
     (propertize (cdr note-type) 'display paw-question-icon))
    ('link
     (let* ((json (condition-case nil
                      (let ((output (json-read-from-string content)))
                        (if (and (not (eq output nil))
                                 (not (arrayp output))
                                 (not (numberp output)))
                            output
                          nil))
                    (error nil)))
            (type (or (alist-get 'type json) ""))
            (link (or (alist-get 'link json) "")))
       (pcase type
         ("file"
          (propertize (cdr note-type) 'display paw-file-link-icon))
         ("url"
          (propertize (cdr note-type) 'display paw-url-link-icon))
         ("annotation"
          (propertize (cdr note-type) 'display paw-annotation-link-icon))
         ("org"
          (propertize (cdr note-type) 'display (create-image paw-org-link-file nil nil :width nil :height nil :ascent 'center))))))
    ('todo
     (propertize (cdr note-type) 'display paw-todo-icon))
    ('done
     (propertize (cdr note-type) 'display paw-done-icon))
    ('cancel
     (propertize (cdr note-type) 'display paw-cancel-icon))
    ('highlight-1
     (propertize "  " 'face (cdr note-type)))
    ('highlight-2
     (propertize "  " 'face (cdr note-type)))
    ('highlight-3
     (propertize "  " 'face (cdr note-type)))
    ('highlight-4
     (propertize "  " 'face (cdr note-type)))
    ('highlight
     (propertize "HL" 'face (cdr note-type)))
    ('stamp
     (propertize (cdr note-type) 'display (cdr note-type)))
    (_ " ")))


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

(defun paw-search-update-buffer-and-resume (&optional page)
  (interactive)
  (let (beg pos)
    (setq beg (point))
    (setq pos (window-start))
    (paw-search-update-buffer paw-search-current-page)
    (set-window-start (selected-window) pos)
    (goto-char beg)
    ;; (hl-line-mode 1)
    )
  )


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


(defcustom paw-search-page-max-rows 41
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
                                              collect `(like status:content ,(concat "%" word "%"))))
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


(defun paw-search-refresh ()
  (interactive)
  (paw-search-update-buffer-with-keyword paw-search-filter))

(provide 'paw-search)
