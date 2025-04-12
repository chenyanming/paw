;;; paw-db.el -*- lexical-binding: t; -*-


(require 'emacsql)
;; REVIEW: is this require needed?
;; emacsql-sqlite provides a common interface to an emacsql SQLite backend (e.g. emacs-sqlite-builtin)
;; not to be confused with a backend itself named emacsql-sqlite that existed in emacsql < 4.0.
(require 'emacsql-sqlite)
(require 'dash)

(defcustom paw-db-file
  (expand-file-name (concat user-emacs-directory ".cache/paw.sqlite"))
  "paw sqlite file for all entries."
  :group 'paw
  :type 'file)


(defvar paw-db-connection nil
  "The EmacSQL database connection.")

(defconst paw-db-version 1)

(defvar paw-db-newp nil)
(defvar paw-db-update-p nil)

(defun paw-db ()
  "Connect or create database.
serverp:
0: initial state
1: NEW online word, no unique id
2: local annotation, it has an unique id
3: Reserve for paw-new-entry, it is not in the database yet
4: RECOGNIZED online word
5: FAMILIAR online word
6: LEARNED online word
7: KNOWN online word
8~12: NEW offline word, no unique id"
  (unless (and paw-db-connection (emacsql-live-p paw-db-connection))
    (unless (file-exists-p (concat user-emacs-directory ".cache/"))
      (make-directory (concat user-emacs-directory ".cache/")))
    (setq paw-db-connection (emacsql-sqlite-open paw-db-file))

    ;; create items table
    (emacsql paw-db-connection [:create-table :if-not-exists items ([(word STRING :primary-key)
                                                                       (exp STRING)])])
    ;; online status
    (emacsql paw-db-connection [:create-table :if-not-exists status ([(word STRING :primary-key)
                                                                       (content STRING)
                                                                       (serverp INTEGER)
                                                                       (note STRING)
                                                                       (note_type STRING)
                                                                       (origin_type STRING)
                                                                       (origin_path STRING)
                                                                       (origin_id STRING)
                                                                       (origin_point STRING)
                                                                       (created_at STRING)])])
    ;; create version table
    (emacsql paw-db-connection [:create-table :if-not-exists version ([user-version])])

    (let* ((db paw-db-connection)
           (version (paw-db-maybe-update db paw-db-version)))
      (cond
       ((> version paw-db-version)
        (emacsql-close db)
        (user-error
         "The anki database was created with a newer paw version.  %s"
         "You need to update the paw package."))
       ((< version paw-db-version)
        (emacsql-close db)
        (error "BUG: The paw database scheme changed %s"
               "and there is no upgrade path")))))
  paw-db-connection)

(defun paw-db-maybe-update (db version)
  (if (emacsql-live-p db)
      (cond ((eq version 1)
             (paw-db-set-version db (setq version 1))
             (message "paw database is version 1...done"))
            ((eq version 2)
             (message "Upgrading paw database from version 2 to 3...")
             (paw-db-set-version db (setq version 3))
             (message "Upgrading paw database from version 2 to 3...done"))
            (t (setq version paw-db-version))))
  version)

(defun paw-db-get-version (db)
  (caar (emacsql db [:select user-version :from version])))

(defun paw-db-set-version (db dbv)
  "Insert user-version if not exists."
  (cl-assert (integerp dbv))
  (if (paw-db-get-version db)
      (emacsql db `[:update version :set  (= user-version ,dbv)])
    (emacsql db `[:insert :into version :values ([(,@paw-db-version)])])))


(defun paw-db-sql (sql &rest args)
  (if (stringp sql)
      (emacsql (paw-db) (apply #'format sql args))
    (apply #'emacsql (paw-db) sql args)))

;;; database operation

;;; select
(defun paw-db-select (&optional sql)
  (let (candidates)
    (setq candidates (mapcar (lambda(x)
                               (cl-pairlis
                                '(word
                                  exp
                                  content
                                  serverp
                                  note
                                  note_type
                                  origin_type
                                  origin_path
                                  origin_id
                                  origin_point
                                  created_at)
                                x))
                             (paw-db-sql (or sql
                                             [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
                                              :inner :join status
                                              :on (= items:word status:word)] ))))

    (if candidates
        candidates
      (setq paw-db-newp t)
      nil)))

;;; insert
(defun paw-db-insert (entries &rest props)
  (setq-local paw-db-update-p t)
  (let ((status
         (cl-loop for entry in entries collect
                  (cl-map 'array #'identity
                          (list (alist-get 'word entry)
                                (or (plist-get props :content) nil)
                                (or (plist-get props :serverp) 0)
                                (or (plist-get props :note) nil)
                                (or (plist-get props :note_type) nil)
                                (or (plist-get props :origin_type) nil)
                                (or (plist-get props :origin_path) nil)
                                (or (plist-get props :origin_id) nil)
                                (or (plist-get props :origin_point) nil)
                                (or (plist-get props :created_at) nil)))))
        (entries
         (cl-loop for entry in entries collect
                  (cl-map 'array #'identity (mapcar 'cdr entry)))))
    (paw-db-sql `[:insert :or :ignore :into items
                    :values ,entries])
    (paw-db-sql `[:insert :or :ignore :into status
                   :values ,status])))

;;; delete
(defun paw-db-delete (words)
  (setq-local paw-db-update-p t)
  (cond ((vectorp words)
         (paw-db-sql `[:delete :from items :where (in word ,words)])
         (paw-db-sql `[:delete :from status :where (in word ,words)]))
        ((stringp words)
         (paw-db-sql `[:delete :from items :where (= word ,words)])
         (paw-db-sql `[:delete :from status :where (= word ,words)]))
        (t nil)))

(defun paw-db-delete-words-by-origin_path (old)
  (setq-local paw-db-update-p t)
  (paw-db-sql `[:delete :from status
                :where (= origin_path ,old)]))

;;; update
(defmacro paw-db-update (field)
  `(defun ,(intern (format "paw-db-update-%s" field)) (word new)
     (setq-local paw-db-update-p t)
     (paw-db-sql (vector ':update 'status
                         ':set (list '= ',(intern field) (vector new ))
                         ':where (list '= 'word word) ))))

(paw-db-update "content")
(paw-db-update "serverp")
(paw-db-update "note")
(paw-db-update "note_type")
(paw-db-update "origin_type")
(paw-db-update "origin_path")
(paw-db-update "origin_id")
(paw-db-update "origin_point")
(paw-db-update "created_at")

(defun paw-db-update-exp (word new)
  (setq-local paw-db-update-p t)
  (paw-db-sql `[:update items
                 :set (= exp ,new)
                 :where (= word ,word)]))

(defun paw-db-update-all-origin_path (old new)
  (setq-local paw-db-update-p t)
  (paw-db-sql `[:update status
                 :set (= origin_path ,new)
                 :where (= origin_path ,old)]))

(defmacro paw-update (field)
  `(defun ,(intern (format "paw-update-%s" field)) (word new)
     ,(format "Update \"%s\" of entry which word is WORD as NEW for database." field)
     (funcall ',(intern (format "paw-db-update-%s" field)) word new)))

(paw-update "exp")
(paw-update "content")
(paw-update "serverp")
(paw-update "note")
(paw-update "note_type")
(paw-update "origin_type")
(paw-update "origin_path")
(paw-update "origin_id")
(paw-update "origin_point")
(paw-update "created_at")

;;; online
(defun paw-db-online (words)
  (cond ((vectorp words)
         (paw-db-update-serverp 1))
        ((stringp words)
         (paw-db-update-serverp 1))
        (t nil)))

;;; candidates
(defun paw-candidate-by-id (id)
  (mapcar
   (lambda(x)
     (cl-pairlis
      '(word
        exp
        content
        serverp
        note
        note_type
        origin_type
        origin_path
        origin_id
        origin_point
        created_at)
      x))
   (paw-db-sql
    `[:select * :from
      [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
       :inner :join status
       :on (= items:word status:word)]
      :where (like word ',(concat "%" id "%"))])))

(defun paw-all-candidates ()
  (mapcar
   (lambda(x)
     (cl-pairlis
      '(word
        exp
        content
        serverp
        note
        note_type
        origin_type
        origin_path
        origin_id
        origin_point
        created_at)
      x))
   (paw-db-sql
    `[:select * :from
      [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
       :inner :join status
       :on (= items:word status:word)]])))

(defun paw-candidate-by-word (word)
  (mapcar
   (lambda(x)
     (cl-pairlis
      '(word
        exp
        content
        serverp
        note
        note_type
        origin_type
        origin_path
        origin_id
        origin_point
        created_at)
      x))
   (paw-db-sql
    `[:select * :from
      [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
       :inner :join status
       :on (= items:word status:word)]
      :where (= word ,word)])))

(defun paw-candidates-by-origin-path (&optional path)
  (mapcar
   (lambda(x)
     (cl-pairlis
      '(word
        exp
        content
        serverp
        note
        note_type
        origin_type
        origin_path
        origin_id
        origin_point
        created_at)
      x))
   (let* ((origin-path (paw-get-origin-path))
          (search-pathes (vconcat (-map (lambda (dir)
                                          (concat dir (file-name-nondirectory origin-path))) ;; no need to expand, otherwise, the string will be different and can not match in sql
                                        paw-annotation-search-paths))))
     (paw-db-sql
      `[:select * :from
        [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
         :inner :join status
         :on (= items:word status:word)]
        :where (or (= origin_path ,(or path origin-path))
                   (in origin_path ,search-pathes))]))))

(defun paw-candidates-by-origin-path-length (&optional path)
  (caar (let* ((origin-path (paw-get-origin-path))
                (search-pathes (vconcat (-map (lambda (dir)
                                                (concat dir (file-name-nondirectory origin-path))) ;; no need to expand, otherwise, the string will be different and can not match in sql
                                              paw-annotation-search-paths))))
           (paw-db-sql
            `[:select (funcall count word) :from
              [:select [items:word status:origin_path] :from items
               :inner :join status
               :on (= items:word status:word)]
              :where (or (= origin_path ,(or path origin-path))
                         (in origin_path ,search-pathes))]))))

(defun paw-candidates-by-origin-path-serverp (&optional random-p)
  (mapcar
   (lambda(x)
     (cl-pairlis
      '(word
        exp
        content
        serverp
        note
        note_type
        origin_type
        origin_path
        origin_id
        origin_point
        created_at)
      x))
   (let* ((origin-path (paw-get-origin-path))
          (search-pathes (vconcat (-map (lambda (dir)
                                          (concat dir (file-name-nondirectory origin-path))) ;; no need to expand, otherwise, the string will be different and can not match in sql
                                    paw-annotation-search-paths))))
     (paw-db-sql
      `[:select * :from
        [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
         :inner :join status
         :on (= items:word status:word)]
        :where (or (= origin_path ,origin-path) ;; select the origin_path
                   (in origin_path ,search-pathes) ;; or the search-pathes
                   (= serverp 1)
                   (= serverp 4)
                   (= serverp 5)
                   (= serverp 6)

                   (= serverp 8)
                   (= serverp 9)
                   (= serverp 10)
                   (= serverp 11))
        ,@(when random-p
            `(:order-by (funcall random)))
        ,@(when random-p
            `(:limit 5)
            )
        ])))) ;; or all the online words but not known words


(defun paw-online-p (serverp)
  "Verify if the word is online word."
  (or (eq serverp 1)
      (eq serverp 4)
      (eq serverp 5)
      (eq serverp 6)
      (eq serverp 7)))

(defun paw-offline-p (serverp)
  "Verify if the word is offline word."
  (or (eq serverp 8)
      (eq serverp 9)
      (eq serverp 10)
      (eq serverp 11)
      (eq serverp 12)))


(defun paw-candidates-only-online-words ()
  (mapcar
   (lambda(x)
     (cl-pairlis
      '(word
        exp
        content
        serverp
        note
        note_type
        origin_type
        origin_path
        origin_id
        origin_point
        created_at)
      x))
   (paw-db-sql
    `[:select * :from
      [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
       :inner :join status
       :on (= items:word status:word)]
      :where (or (= serverp 1)
                 (= serverp 4)
                 (= serverp 5)
                 (= serverp 6))])))

(defun paw-candidates-only-words ()
  (mapcar
   (lambda(x)
     (cl-pairlis
      '(word
        exp
        content
        serverp
        note
        note_type
        origin_type
        origin_path
        origin_id
        origin_point
        created_at)
      x))
   (paw-db-sql
    `[:select * :from
      [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
       :inner :join status
       :on (= items:word status:word)]
      :where (not (like word "%:id:%"))])))


(defun paw-candidates-only-word-without-spaces ()
  (mapcar
   (lambda(x)
     (cl-pairlis
      '(word
        exp
        content
        serverp
        note
        note_type
        origin_type
        origin_path
        origin_id
        origin_point
        created_at)
      x))
   (paw-db-sql
    `[:select * :from
      [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
       :inner :join status
       :on (= items:word status:word)]
      :where (and (not (like word "%:id:%"))
                  (not (like word "% %")))])))



(defun paw-candidates-only-words-with-spaces ()
  (mapcar
   (lambda(x)
     (cl-pairlis
      '(word
        exp
        content
        serverp
        note
        note_type
        origin_type
        origin_path
        origin_id
        origin_point
        created_at)
      x))
   (paw-db-sql
    `[:select * :from
      [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
       :inner :join status
       :on (= items:word status:word)]
      :where (and (not (like word "%:id:%"))
                  (like word "% %"))])))


(defun paw-candidates-only-links ()
  (mapcar
   (lambda(x)
     (cl-pairlis
      '(word
        exp
        content
        serverp
        note
        note_type
        origin_type
        origin_path
        origin_id
        origin_point
        created_at)
      x))
   (paw-db-sql
    `[:select * :from
      [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
       :inner :join status
       :on (= items:word status:word)]
      :where (and (or (= origin_type 'eaf-mode)
                      (= origin_type "browser")
                      (= origin_type 'eww-mode))
                   (= note_type '(bookmark . "🔖")))
      :order-by created_at :desc])))


(defun paw-delete-all-online-words()
  ;; !!! only for maintainance and testing !!!
  (paw-db-sql
   [:delete :from items
    :where :exists
    [:select * :from status
     :where  (and (= status:word items:word)
                  (= status:serverp 1))]])
  (paw-db-sql
   [:delete :from status
    :where (= status:serverp 1)])
  )

(defun paw-get-origin-path ()
  (pcase major-mode
    ('paw-search-mode
     (alist-get 'origin_path (get-char-property (point) 'paw-entry)))
    ('wallabag-entry-mode
     (alist-get 'title (get-text-property 1 'wallabag-entry)))
    ('nov-mode
     (abbreviate-file-name nov-file-name))
    ('eww-mode
     (plist-get eww-data :url))
    ('eaf-mode
     (abbreviate-file-name eaf--buffer-url))
    ('paw-view-note-mode
     (alist-get 'origin_path paw-current-entry))
    ('elfeed-show-mode
     (if elfeed-show-entry (elfeed-entry-link elfeed-show-entry) "" ))
    ('telega-webpage-mode
     telega-webpage--url)
    (_
     (if (buffer-file-name)
         (abbreviate-file-name (buffer-file-name))
       (buffer-name)))))

(defun paw-close-db ()
  (when (and paw-db-connection (emacsql-live-p paw-db-connection))
    (emacsql-close paw-db-connection)
    (setq paw-db-connection nil)))

(defun paw-get-all-origin-path ()
  (paw-db-sql [:select :distinct origin_path
               :from status
               :where (not origin_path nil)]))

(defun paw-get-all-study-list ()
  (paw-db-sql [:select :distinct origin_point :from
                [:select [serverp origin_point] :from status
                 :where (and (not origin_point nil)
                             (or (= serverp 1)
                                 (= serverp 4)
                                 (= serverp 5)
                                 (= serverp 6)
                                 (= serverp 8)
                                 (= serverp 9)
                                 (= serverp 10)
                                 (= serverp 11)))]]))

(defun paw-check-word-like-p (word)
  (paw-db-sql `[:select [items:word]
                :from items
                :where (like word ,word)]))

(defun paw-check-word-exist-p (word)
  (paw-db-sql `[:select [items:word]
                :from items
                :where (= word ,word)]))

;; TODO
(defun paw-db-sync()
  (interactive)
  (paw-close-db)
  (message "Syncing paw database..."))

(defun paw-db-recreate ()
  (interactive)
  (when (yes-or-no-p "Warning: Are you sure to recreate the database? ")
    (emacsql-close (paw-db))
    (delete-file paw-db-file)
    (paw-sync-words)))

(defun paw-show-diff ()
  "Run sqldiff between DB1 and DB2 and display results in a buffer."
  (interactive)
  (let* ((buf-name "*SQLite Diff*")
         (buf (get-buffer-create buf-name))
         (file1 (read-file-name "Please select the first database file (to be updated): " (expand-file-name "paw." (file-name-directory paw-db-file) )))
         (file2 (read-file-name "Please select the second database file: " (expand-file-name "paw.sync-conflict" (file-name-directory paw-db-file) )))
         (cmd (format "sqldiff %s %s" file1 file2))
         (diff (shell-command-to-string cmd)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert diff)
      (sql-mode)
      (read-only-mode 1))
    (switch-to-buffer buf)
    (with-current-buffer buf-name
      (paw-apply-diff file1))))

(defun paw-apply-diff (db-path)
  "Apply SQL diff line-by-line to the given DB-PATH, asking before each command.
Moves point to the corresponding line in the buffer while prompting."
  (interactive "fApply diff to SQLite DB: ")
  (let ((lines (split-string (substring-no-properties (buffer-string)) "\n" t))
        (applied 0)
        (skipped 0)
        (line-number 1))
    (save-excursion
      (goto-char (point-min))
      (dolist (line lines)
        (let ((start (point)))
          (when (string-match-p "^\\s-*\\(INSERT\\|UPDATE\\|DELETE\\|REPLACE\\)\\s-+" line)
            ;; Move point to start of current SQL line
            (goto-char (point-min))
            (forward-line (1- line-number))
            (recenter) ;; Optional: scroll line to center
            (if (y-or-n-p (format "Apply SQL? %s" line))
                (let* ((cmd (format "sqlite3 %s" (shell-quote-argument db-path)))
                       (exit (call-process-region
                              (point) (line-end-position)
                              "sqlite3" nil "*paw-sqlite-output*" nil
                              db-path)))
                  (if (zerop exit)
                      (setq applied (1+ applied))
                    (message "❌ Failed: %s" line)))
              (setq skipped (1+ skipped)))))
        (setq line-number (1+ line-number))))
    (message "✅ Applied: %d, ❎ Skipped: %d" applied skipped)))




(provide 'paw-db)
