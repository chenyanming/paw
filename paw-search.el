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

(defun paw-parse-entry-as-string (entry &optional print-full-content)
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
    (if print-full-content
        (format "%s %s"
                (propertize (paw-format-icon note-type content serverp origin-path) 'paw-entry entry)
                (pcase serverp
                  (2 ;; local annotations
                   (format
                    "%s | %s"
                    (paw-format-full-content note-type word content content-path content-filename)
                    note))
                  (_
                   (propertize (paw-get-eldoc-word entry) 'face 'default))))
      (format "%s %s %s %s %s %s %s"
              (propertize (paw-format-icon note-type content serverp origin-path) 'paw-entry entry)
              (pcase serverp
                (2 ;; local annotations
                 (paw-format-content note-type word content content-path content-filename))
                (_
                 (s-pad-right 15 " " (propertize (s-truncate 15 word) 'face 'default) )))
              (s-pad-right 12 " " (propertize (s-truncate 10 (if (stringp created-at)
                                                                 created-at
                                                               "" ) "") 'face 'paw-date-face ))
              (s-collapse-whitespace (propertize (if (stringp origin-point)
                                                     origin-point
                                                   "" ) 'face 'paw-origin-point-face ))
              (s-collapse-whitespace (if origin-path
                                         (pcase origin-type
                                           ('wallabag-entry-mode
                                            (propertize origin-path 'face 'paw-wallabag-face))
                                           ('elfeed-show-mode
                                            (propertize origin-path 'face 'paw-link-face))
                                           ('telega-webpage-mode
                                            (propertize origin-path 'face 'paw-link-face))
                                           ('nov-mode
                                            (propertize (file-name-nondirectory origin-path) 'face 'paw-nov-face))
                                           ((or 'pdf-view-mode 'nov-mode "pdf-viewer")
                                            (propertize (file-name-nondirectory origin-path) 'face 'paw-pdf-face))
                                           ((or 'eaf-mode "browser" 'eww-mode)
                                            (propertize origin-path 'face 'paw-link-face))
                                           (_ (propertize (file-name-nondirectory origin-path ) 'face 'paw-file-face)))
                                       ""))
              (s-collapse-whitespace (propertize (if (s-blank-str? exp) "" (format "| %s |" exp)) 'face 'paw-exp-face ))
              (s-collapse-whitespace (or (if note (replace-regexp-in-string word (propertize word 'face '(bold underline)) note) "") ""))))))

(defun paw-parse-entry-as-string-2 (entry)
  (concat
   (propertize (or (alist-get 'created_at entry) "") 'paw-entry entry)
   "  "
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
          (content-filename (or (alist-get 'filename content-json) ""))
          (content-path (or (alist-get 'path content-json) ""))
          (serverp (or (alist-get 'serverp content-json) 2))
          (origin-path (alist-get 'origin_path entry))
          (origin-point (alist-get 'origin_point entry))
          (origin-type (alist-get 'origin_type entry))
          (note-type (alist-get 'note_type entry)))
     (concat
      (paw-format-column
       (paw-format-icon note-type content serverp origin-path)
       2 :left)
      "  "
      (propertize (s-truncate 55 (paw-get-real-word word) ) 'face 'paw-offline-face)
      "  "
      (if (stringp origin-point)
          origin-point
        (if origin-path
            (pcase origin-type
              ('wallabag-entry-mode
               (propertize origin-path 'face 'paw-wallabag-face))
              ('elfeed-show-mode
               (propertize origin-path 'face 'paw-wallabag-face))
              ('telega-webpage-mode
               (propertize origin-path 'face 'paw-wallabag-face))
              ('nov-mode
               (propertize (file-name-nondirectory origin-path) 'face 'paw-nov-face))
              ((or 'pdf-view-mode 'nov-mode "pdf-viewer")
               (propertize (file-name-nondirectory origin-path) 'face 'paw-pdf-face))
              ((or 'eaf-mode "browser" 'eww-mode)
               (propertize origin-path 'face 'paw-link-face))
              (_ (propertize (file-name-nondirectory origin-path ) 'face 'paw-file-face)))
          ""))

      ))
   "  "
   (s-collapse-whitespace (or (s-truncate 120 (alist-get 'note entry)) ""))))

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
    (_ (s-pad-right 15 " "
                    (propertize (s-truncate 15 (s-collapse-whitespace (or (if (equal content 0) word (if content content "")) (paw-get-real-word word))))
                                'face 'paw-offline-face)))))

(defun paw-format-full-content (note-type word content content-path content-filename)
  (pcase (car note-type)
    ('attachment
     (let* ((ext (downcase (file-name-extension content-path)))
            (ext (if (string= ext "jpg") "jpeg" ext)))
       (pcase ext
         ((or "pbm" "xbm" "xpm" "gif" "jpeg" "tiff" "png" "svg" "jpg")
          (propertize "IMAGS"
                      'face 'paw-offline-face
                      'display (create-image (expand-file-name content-path paw-note-dir) nil nil :width (if (eq system-type 'gnu/linux) 200 100) :height nil  :margin '(0 . 1))))
         (_ (propertize (format "%s %s" (paw-attach-icon-for (expand-file-name content-filename)) content-filename )
                        'face 'paw-offline-face))) ))
    ('image
     (propertize "IMAGS"
                 'face 'paw-offline-face
                 'display (create-image (expand-file-name content-path paw-note-dir) nil nil :width (if (eq system-type 'gnu/linux) 200 100) :height nil :margin '(0 . 1))))
    (_ (propertize (s-collapse-whitespace (or (if (equal content 0) word (if content content "")) (paw-get-real-word word)))
                   'face 'paw-offline-face))))

;;; format
(defun paw-format-icon (note-type content serverp origin-path)
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
     (propertize (cdr note-type) 'display (let* ((origin-path (let* ((url (url-generic-parse-url origin-path))
                                                                  (https (url-type url))
                                                                  (host (url-host url)))
                                                             (concat https "://" host)))
                                                 (ico-file-hash (md5 origin-path))
                                                 (icon-file-path (concat (expand-file-name ico-file-hash paw-cache-dir) ".png")))
                                            (make-directory paw-cache-dir t)
                                            (if (file-exists-p icon-file-path)
                                                (create-image icon-file-path nil nil :ascent 'center)
                                              paw-bookmark-icon))))
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
    ('comment paw-comment-button)
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
    ('underline-1
     (propertize "UU" 'face (cdr note-type)))
    ('underline-2
     (propertize "UU" 'face (cdr note-type)))
    ('underline-3
     (propertize "UU" 'face (cdr note-type)))
    ('underline-4
     (propertize "LL" 'face (cdr note-type)))
    ('underline-5
     (propertize "LL" 'face (cdr note-type)))
    ('underline-6
     (propertize "LL" 'face (cdr note-type)))
    ('stamp
     (propertize (cdr note-type) 'display (cdr note-type)))
    (_ (propertize "AA" 'face (cdr note-type)))))


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
           (len (length entries))
           (rows (paw-search-page-max-rows)))
      (setq paw-search-entries-length (or (cdaar (paw-db-select
                                              `[:select (funcall count word)
                                               :from
                                               ,(paw-search-parse-filter paw-search-filter :group-p group-p)])) 0 ))
      (setq paw-search-pages (ceiling paw-search-entries-length rows))
      (erase-buffer)
      (dolist (entry entries)
        (setq id (1+ id))
        (if (<= id rows)
            (funcall paw-print-entry-function entry id)))
      (if (< len paw-search-entries-length)
          (dotimes (i paw-search-pages)
            (let ((button-string (format "%d" (1+ i))))
              (if (equal (string-to-number button-string) paw-search-current-page)
                  (add-face-text-property 0 (length button-string) 'paw-current-page-button-face t button-string))
              (insert " " (buttonize button-string #'paw-search-more-data (1+ i)) " ") ))
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
  (let* ((sql (paw-search-parse-filter paw-search-filter :limit (paw-search-page-max-rows) :page page))
         (entries (paw-db-select sql)))
    entries))

(defun paw-search-get-grouped-entries (&optional page)
  (let* ((sql (paw-search-parse-filter paw-search-filter :limit (paw-search-page-max-rows) :page page :group-p t))
         (entries (paw-db-select sql)))
    entries))


(defcustom paw-search-page-max-rows-auto-adjust nil
  "WORKAROUND: When non-nil, adjust the max rows of the page.
It is unstable when live search, and the algorithm is also inaccurate."
  :group 'paw
  :type 'boolean)

(defcustom paw-search-page-max-rows-auto-adjust-offset 4
  "WORKAROUND: The offset when auto adjust the max rows.
It may not be accurate, but it is a good guess."
  :group 'paw
  :type 'integer)

(defcustom paw-search-page-max-rows 41
  "The maximum number of entries to display in a single page."
  :group 'paw
  :type 'integer)

(defvar paw-search-current-page 1
  "The number of current page in the current search result.")

(defvar paw-search-pages 0
  "The number of pages in the current search result.")

(defvar paw-search-buffer-line-pixel-height nil
  "The pixel height of the *paw* buffer line.")

(defun paw-search-page-max-rows ()
  "Return the maximum number of entries to display.
In the *paw* window."
  (let ((win (get-buffer-window "*paw*" 'visible)))
    (if paw-search-page-max-rows-auto-adjust
        (if (window-live-p win)
            (progn
              (unless paw-search-buffer-line-pixel-height
                (setq paw-search-buffer-line-pixel-height (line-pixel-height)))
              (let* ((window-pixel-height (window-pixel-height win))
                     (font-height paw-search-buffer-line-pixel-height)
                     (offset (* paw-search-page-max-rows-auto-adjust-offset font-height)))  ;; Height of mode line
                ;; Calculate visible height by subtracting header and mode line heights
                (let ((visible-pixel-height (- window-pixel-height offset)))
                  ;; Calculate the number of lines that fit in the visible height
                  (max 1 (floor visible-pixel-height font-height)))) )
          paw-search-page-max-rows)
      paw-search-page-max-rows)))

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
                                                                            (concat dir (file-name-nondirectory words)));; pure concat
                                                                          paw-annotation-search-paths)
                                                                     (-map (lambda (dir)
                                                                             (expand-file-name (file-name-nondirectory words) dir)) ;; expand
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
                       (list :offset (* (1- page) (paw-search-page-max-rows))))
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
  (paw-search-update-buffer-with-keyword paw-search-filter)
  (paw-refresh-inline-annotations))


(defun paw-get-eldoc-word (&optional entry)
  (let* ((entry (or entry (get-char-property (point) 'paw-entry) ))
         (word (alist-get 'word entry))
         (note-type (alist-get 'note_type entry))
         (exp (alist-get 'exp entry))
         (note (alist-get 'note entry)))
    (pcase (car note-type)
      ('word
       (if entry
           (format "%s | %s | %s"
                   (propertize word 'face 'bold)
                   (or (s-collapse-whitespace exp) "")
                   (or (s-collapse-whitespace (replace-regexp-in-string word (propertize word 'face '(bold underline)) note)) "") )))
      (_ (format "%s | %s"
                 (propertize (paw-get-real-word word) 'face 'bold)
                 (or (s-collapse-whitespace note)))))))

(provide 'paw-search)
