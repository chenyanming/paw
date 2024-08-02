;;; paw-db.el -*- lexical-binding: t; -*-


(require 'emacsql)
(require 'emacsql-sqlite)
(require 'dash)

(defcustom paw-db-file
  (expand-file-name (concat user-emacs-directory ".cache/paw.sqlite"))
  "paw sqlite file for all entries."
  :group 'paw
  :type 'file)

(defcustom paw-db-connector (if (and (progn
                                            (require 'emacsql-sqlite-builtin nil t)
                                            (functionp 'emacsql-sqlite-builtin))
                                          (functionp 'sqlite-open))
                                     'sqlite-builtin
                                   'sqlite)
  "The database connector used by paw.
This must be set before `paw' is loaded.  To use an alternative
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


(defvar paw-db-connection nil
  "The EmacSQL database connection.")

(defconst paw-db-version 1)

(defvar paw-db-newp nil)
(defvar paw-db-update-p nil)

(defun paw-db--conn-fn ()
  "Return the function for creating the database connection."
  (cl-case paw-db-connector
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
    (setq paw-db-connection (funcall (paw-db--conn-fn) paw-db-file))

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
     "WORDLIST")
    ('wallabag-entry-mode
     (alist-get 'title (get-text-property 1 'wallabag-entry)))
    ('nov-mode
     (abbreviate-file-name nov-file-name))
    ('eww-mode
     (plist-get eww-data :url))
    ('eaf-mode
     (abbreviate-file-name eaf--buffer-url))
    ('paw-view-note
     (alist-get 'origin-path (paw-note-word)))
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


(provide 'paw-db)
