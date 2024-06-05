;;; pen/pen-db.el -*- lexical-binding: t; -*-


(require 'emacsql)
(require 'emacsql-sqlite)

(defcustom pen-db-file
  (expand-file-name (concat user-emacs-directory ".cache/pen.sqlite"))
  "pen sqlite file for all entries."
  :group 'pen
  :type 'file)

(defcustom pen-db-connector (if (and (progn
                                            (require 'emacsql-sqlite-builtin nil t)
                                            (functionp 'emacsql-sqlite-builtin))
                                          (functionp 'sqlite-open))
                                     'sqlite-builtin
                                   'sqlite)
  "The database connector used by pen.
This must be set before `pen' is loaded.  To use an alternative
connector you must install the respective package explicitly.
The default is `sqlite', which uses the `emacsql-sqlite' library
that is being maintained in the same repository as `emacsql'
itself.
If you are using Emacs 29, then the recommended connector is
`sqlite-builtin', which uses the new builtin support for SQLite.
You need to install the `emacsql-sqlite-builtin' package to use
this connector.
If you are using an older Emacs release, then the recommended
connector is `sqlite-module', which uses the module provided by
the `sqlite3' package.  This is very similar to the previous
connector and the built-in support in Emacs 29 derives from this
module.  You need to install the `emacsql-sqlite-module' package
to use this connector.
For the time being `libsqlite3' is still supported.  Do not use
this, it is an older version of the `sqlite-module' connector
from before the connector and the package were renamed.
For the time being `sqlite3' is also supported.  Do not use this.
This uses the third-party `emacsql-sqlite3' package, which uses
the official `sqlite3' cli tool, which is not intended
to be used like this.  See https://nullprogram.com/blog/2014/02/06/."
  :group 'wallabag
  :type '(choice (const sqlite)
          (const sqlite-builtin)
          (const sqlite-module)
          (const :tag "libsqlite3 (OBSOLETE)" libsqlite3)
          (const :tag "sqlite3 (BROKEN)" sqlite3)))


(defvar pen-db-connection nil
  "The EmacSQL database connection.")

(defconst pen-db-version 1)

(defvar pen-db-newp nil)
(defvar pen-db-update-p nil)

(defun pen-db--conn-fn ()
  "Return the function for creating the database connection."
  (cl-case pen-db-connector
    (sqlite
     (progn
       (require 'emacsql-sqlite)
       #'emacsql-sqlite))
    (sqlite-builtin
     (progn
       (require 'emacsql-sqlite-builtin)
       #'emacsql-sqlite-builtin))
    (sqlite-module
     (progn
       (require 'emacsql-sqlite-module)
       #'emacsql-sqlite-module))
    (libsqlite3
     (progn
       (require 'emacsql-libsqlite3)
       #'emacsql-libsqlite3))
    (sqlite3
     (progn
       (require 'emacsql-sqlite3)
       #'emacsql-sqlite3))))


(defun pen-db ()
  "Connect or create database."
  (unless (and pen-db-connection (emacsql-live-p pen-db-connection))
    (unless (file-exists-p (concat user-emacs-directory ".cache/"))
      (make-directory (concat user-emacs-directory ".cache/")))
    (setq pen-db-connection (funcall (pen-db--conn-fn) pen-db-file))

    ;; create items table
    (emacsql pen-db-connection [:create-table :if-not-exists items ([(word STRING :primary-key)
                                                                       (exp STRING)])])
    ;; online status
    (emacsql pen-db-connection [:create-table :if-not-exists status ([(word STRING :primary-key)
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
    (emacsql pen-db-connection [:create-table :if-not-exists version ([user-version])])

    (let* ((db pen-db-connection)
           (version (pen-db-maybe-update db pen-db-version)))
      (cond
       ((> version pen-db-version)
        (emacsql-close db)
        (user-error
         "The anki database was created with a newer pen version.  %s"
         "You need to update the pen package."))
       ((< version pen-db-version)
        (emacsql-close db)
        (error "BUG: The pen database scheme changed %s"
               "and there is no upgrade path")))))
  pen-db-connection)

(defun pen-db-maybe-update (db version)
  (if (emacsql-live-p db)
      (cond ((eq version 1)
             (pen-db-set-version db (setq version 1))
             (message "pen database is version 1...done"))
            ((eq version 2)
             (message "Upgrading pen database from version 2 to 3...")
             (pen-db-set-version db (setq version 3))
             (message "Upgrading pen database from version 2 to 3...done"))
            (t (setq version pen-db-version))))
  version)

(defun pen-db-get-version (db)
  (caar (emacsql db [:select user-version :from version])))

(defun pen-db-set-version (db dbv)
  "Insert user-version if not exists."
  (cl-assert (integerp dbv))
  (if (pen-db-get-version db)
      (emacsql db `[:update version :set  (= user-version ,dbv)])
    (emacsql db `[:insert :into version :values ([(,@pen-db-version)])])))


(defun pen-db-sql (sql &rest args)
  (if (stringp sql)
      (emacsql (pen-db) (apply #'format sql args))
    (apply #'emacsql (pen-db) sql args)))

;;; database operation

;;; select
(defun pen-db-select ()
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
                             (pen-db-sql [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
                                           :inner :join status
                                           :on (= items:word status:word)])))

    (if candidates
        candidates
      (message "No items in pen database, try to update with 'u'.")
      (setq pen-db-newp t)
      nil)))

;;; insert
(defun pen-db-insert (entries &rest props)
  (setq-local pen-db-update-p t)
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
    (pen-db-sql `[:insert :or :ignore :into items
                    :values ,entries])
    (pen-db-sql `[:insert :or :ignore :into status
                   :values ,status])))

;;; delete
(defun pen-db-delete (words)
  (setq-local pen-db-update-p t)
  (cond ((vectorp words)
         (pen-db-sql `[:delete :from items :where (in word ,words)])
         (pen-db-sql `[:delete :from status :where (in word ,words)]))
        ((stringp words)
         (pen-db-sql `[:delete :from items :where (= word ,words)])
         (pen-db-sql `[:delete :from status :where (= word ,words)]))
        (t nil)))

(defun pen-db-delete-words-by-origin_path (old)
  (setq-local pen-db-update-p t)
  (pen-db-sql `[:delete :from status
                :where (= origin_path ,old)]))

(defun pen-entry-delete-words-by-origin_path (old)
  (setq pen-full-entries (-remove (lambda (x) (equal old (alist-get 'origin_path x))) pen-full-entries))
  (setq pen-search-entries pen-full-entries))

;;; update
(defmacro pen-db-update (field)
  `(defun ,(intern (format "pen-db-update-%s" field)) (word new)
     (setq-local pen-db-update-p t)
     (pen-db-sql (vector ':update 'status
                         ':set (list '= ',(intern field) (vector new ))
                         ':where (list '= 'word word) ))))

(pen-db-update "content")
(pen-db-update "serverp")
(pen-db-update "note")
(pen-db-update "note_type")
(pen-db-update "origin_type")
(pen-db-update "origin_path")
(pen-db-update "origin_id")
(pen-db-update "origin_point")
(pen-db-update "created_at")

(defun pen-db-update-exp (word new)
  (setq-local pen-db-update-p t)
  (pen-db-sql `[:update items
                 :set (= exp ,new)
                 :where (= word ,word)]))

(defun pen-db-update-all-origin_path (old new)
  (setq-local pen-db-update-p t)
  (pen-db-sql `[:update status
                 :set (= origin_path ,new)
                 :where (= origin_path ,old)]))

(defun pen-entry-update-all-origin_path (old new)
  (-map (lambda (x)
          (if (equal old (alist-get 'origin_path x))
              (setf (alist-get 'origin_path x) new))) pen-full-entries)
  (setq pen-search-entries pen-full-entries))

(defmacro pen-entry-update (field)
  `(defun ,(intern (format "pen-entry-update-%s" field)) (word new &optional original)
     ,(format "Update \"%s\" of entry which word is WORD as NEW `pen-search-entries' and `pen-full-entries'." field)
     (let ((entry (if original
                      (-first (lambda (x) (equal x original)) pen-full-entries)
                    (-first (lambda (x) (equal word (alist-get 'word x))) pen-full-entries) )))
       (setf (alist-get ',(intern field) entry) new)
       (setq pen-search-entries pen-full-entries))))

(pen-entry-update "exp")
(pen-entry-update "content")
(pen-entry-update "serverp")
(pen-entry-update "note")
(pen-entry-update "note_type")
(pen-entry-update "origin_type")
(pen-entry-update "origin_path")
(pen-entry-update "origin_id")
(pen-entry-update "origin_point")
(pen-entry-update "created_at")

(defmacro pen-update (field)
  `(defun ,(intern (format "pen-update-%s" field)) (word new)
     ,(format "Update \"%s\" of entry which word is WORD as NEW for database, `pen-search-entries', and `pen-full-entries'." field)
     (funcall ',(intern (format "pen-db-update-%s" field)) word new)
     (funcall ',(intern (format "pen-entry-update-%s" field)) word new)))

(pen-update "exp")
(pen-update "content")
(pen-update "serverp")
(pen-update "note")
(pen-update "note_type")
(pen-update "origin_type")
(pen-update "origin_path")
(pen-update "origin_id")
(pen-update "origin_point")
(pen-update "created_at")

;;; online
(defun pen-db-online (words)
  (cond ((vectorp words)
         (pen-db-update-serverp 1))
        ((stringp words)
         (pen-db-update-serverp 1))
        (t nil)))

;;; candidates
(defun pen-candidate-by-id (id)
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
   (pen-db-sql
    `[:select * :from
      [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
       :inner :join status
       :on (= items:word status:word)]
      :where (like word ',(concat "%" id "%"))])))

(defun pen-all-candidates ()
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
   (pen-db-sql
    `[:select * :from
      [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
       :inner :join status
       :on (= items:word status:word)]])))

(defun pen-candidate-by-word (word)
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
   (pen-db-sql
    `[:select * :from
      [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
       :inner :join status
       :on (= items:word status:word)]
      :where (= word ,word)])))

(defun pen-candidates-by-origin-path (&optional path)
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
   (pen-db-sql
    `[:select * :from
      [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
       :inner :join status
       :on (= items:word status:word)]
      :where (= origin_path ,(or path (pen-get-origin-path)))])))

(defun pen-candidates-by-origin-path-serverp (&optional path)
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
   (let* ((origin-path (pen-get-origin-path))
          (search-pathes (vconcat (-map (lambda (dir)
                                          (concat dir (file-name-nondirectory origin-path))) ;; no need to expand, otherwise, the string will be different and can not match in sql
                                    pen-annotation-search-paths))))
     (pen-db-sql
      `[:select * :from
        [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
         :inner :join status
         :on (= items:word status:word)]
        :where (or (= origin_path ,(or path origin-path)) ;; select the origin_path
                   (in origin_path ,search-pathes) ;; or the search-pathes
                   (= serverp 1))])))) ;; or all the online words

(defun pen-candidates-only-online-words ()
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
   (pen-db-sql
    `[:select * :from
      [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
       :inner :join status
       :on (= items:word status:word)]
      :where (= serverp 1)])))

(defun pen-candidates-only-links ()
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
   (pen-db-sql
    `[:select * :from
      [:select [items:word items:exp status:content status:serverp status:note status:note_type status:origin_type status:origin_path status:origin_id status:origin_point status:created_at] :from items
       :inner :join status
       :on (= items:word status:word)]
      :where (or (= origin_type "eaf-mode")
                 (= origin_type "browser")
                 (= origin_type "pdf-viewer"))])))




(defun pen-delete-all-online-words()
  ;; !!! only for maintainance and testing !!!
  (pen-db-sql
   [:delete :from items
    :where :exists
    [:select * :from status
     :where  (and (= status:word items:word)
                  (= status:serverp 1))]])
  (pen-db-sql
   [:delete :from status
    :where (= status:serverp 1)])
  )

(defun pen-get-origin-path ()
  (pcase major-mode
    ('pen-search-mode
     "WORDLIST")
    ('wallabag-entry-mode
     (alist-get 'title (get-text-property 1 'wallabag-entry)))
    ('nov-mode
     ;; TODO Workaround to handle file path for wsl
     (if IS-LINUX
         (if (s-contains? "/mnt/c/Users/elecm" nov-file-name)
             (s-prepend "~" (s-chop-prefix "/mnt/c/Users/elecm" nov-file-name))
           (abbreviate-file-name nov-file-name))
       (abbreviate-file-name nov-file-name)))
    ('eww-mode
     (plist-get eww-data :url))
    ('eaf-mode
     (abbreviate-file-name eaf--buffer-url))
    (_
     (if (buffer-file-name)
         ;; TODO Workaround to handle file path for wsl
         (if IS-LINUX
             (if (s-contains? "/mnt/c/Users/elecm" buffer-file-name)
                 (s-prepend "~" (s-chop-prefix "/mnt/c/Users/elecm" buffer-file-name))
               (abbreviate-file-name (buffer-file-name)))
           (abbreviate-file-name (buffer-file-name)))
       (buffer-name)))))

(defun pen-close-db ()
  (when (and pen-db-connection (emacsql-live-p pen-db-connection))
    (emacsql-close pen-db-connection)
    (setq pen-db-connection nil)))

;; ;;;###autoload
;; (defun pen-find-db (&optional file)
;;   "Find the database file and open it as current database file."
;;   (interactive)
;;   (if pen-annotation-mode
;;       (pen-annotation-mode -1))
;;   (when (and pen-db-connection (emacsql-live-p pen-db-connection))
;;     (emacsql-close pen-db-connection)
;;     (setq pen-db-connection nil))
;;   (setq pen-db-file (or file (read-file-name "Open the db file: ") ))
;;   (setq pen-search-entries nil)
;;   (setq pen-full-entries nil)
;;   (pen-search-clear-filter)
;;   (pen))

;; TODO better way to treat database file as pen
;; (defun pen-find-db-advice (orig-fun &rest args)
;;   (let ((file (apply orig-fun args)))
;;     (when (string= (file-name-extension file) "sqlite")
;;       ;; (kill-buffer (current-buffer))
;;       (pen-find-db file)
;;       ;; workaround: kill all buffer with extension sqlite
;;       (-map (lambda (b)
;;               (with-current-buffer b
;;                 (if (string= (file-name-extension (buffer-name)) "sqlite")
;;                     (kill-buffer b))))
;;             (buffer-list)))))

;; (advice-add 'counsel--find-file-1 :around #'pen-find-db-advice)

(defun pen-get-all-origin-path ()
  (pen-db-sql [:select :distinct origin_path
               :from status
               :where (not origin_path nil)]))

(defun pen-get-all-study-list ()
  (pen-db-sql [:select :distinct origin_point :from
                [:select [serverp origin_point] :from status
                 :where (and (not origin_point nil)
                             (= serverp 1))]]))

(defun pen-check-word-exist-p (word)
  (pen-db-sql `[:select [items:word]
                :from items
                :where (= word ,word)]))


;; TODO
(defun pen-db-sync()
  (interactive)
  (pen-close-db)
  (message "Syncing pen database..."))

(defun pen-db-recreate ()
  (interactive)
  (when (yes-or-no-p "Warning: Are you sure to recreate the database? ")
    (emacsql-close (pen-db))
    (delete-file pen-db-file)
    (pen-sync-words)))


(provide 'pen-db)
