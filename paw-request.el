;;; paw-request.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'paw-search)
(require 'paw-db)
(require 'paw-util)

(require 'request)
(require 'consult nil t)
(require 's)

(defvar paw-studylist nil
  "List of words obtained from API.")

(defcustom paw-authorization-keys ""
  "paw authorization keys for eudic
Apply on https://my.eudic.net/OpenAPI/Authorization"
  :group 'paw
  :type 'string)

;;;###autoload
(defun paw-add-online-word (word &optional note)
  "Add a word to online server (Eudic), please fill in
`paw-authorization-keys' before using it."
  (interactive (list (cond ((eq major-mode 'paw-search-mode)
                            (read-string "Add word: "))
                           ((eq major-mode 'paw-view-note-mode)
                            paw-note-word)
                           ((eq major-mode 'eaf-mode)
                            (pcase eaf--buffer-app-name
                              ("browser"
                               (eaf-execute-app-cmd 'eaf-py-proxy-copy_text)
                               (sleep-for 0.01) ;; TODO small delay to wait for the clipboard
                               (eaf-call-sync "execute_function" eaf--buffer-id "get_clipboard_text"))
                              ("pdf-viewer"
                               (eaf-execute-app-cmd 'eaf-py-proxy-copy_select)
                               (sleep-for 0.01) ;; TODO small delay to wait for the clipboard
                               (eaf-call-sync "execute_function" eaf--buffer-id "get_clipboard_text"))))
                           (mark-active
                            (buffer-substring-no-properties (region-beginning) (region-end)))
                           (t (substring-no-properties (or (thing-at-point 'word t) ""))))))
  ;; reposition the frame so that it would not block me inputing
  (if paw-posframe-p
      (when (frame-visible-p (posframe--find-existing-posframe (get-buffer paw-view-note-buffer-name )))
        (posframe-hide (get-buffer paw-view-note-buffer-name))
        (other-frame 1)
        (posframe-show (get-buffer paw-view-note-buffer-name)
                       :poshandler 'posframe-poshandler-frame-top-center
                       :width (min 100 (round (* 0.95 (window-width))) )
                       :height (min 100 (round (* 0.5 (window-height))) )
                       :respect-header-line t
                       :cursor 'box
                       :internal-border-width 2
                       :accept-focus t
                       ;; :refposhandler nil
                       :hidehandler (lambda(_)
                                      (or (eq last-command 'keyboard-quit)
                                          (eq this-command 'keyboard-quit)))
                       :internal-border-color (if (eq (frame-parameter nil 'background-mode) 'light)
                                                  "#888888"
                                                "#F4F4F4"))
        (select-frame-set-input-focus (posframe--find-existing-posframe (get-buffer paw-view-note-buffer-name)))
        ;; (display-buffer-other-frame buffer)
        (unless (search-forward "** Saved Meanings" nil t)
          (search-forward "** Translation" nil t))
        (beginning-of-line)
        (recenter 0)
        (other-frame 1)))
  ;; add word to server, if succeed, update db
  (let ((exp (cond ((eq major-mode 'paw-view-note-mode)
                    (read-string (format "Add meaning for '%s': " word) (if mark-active
                                                                            (buffer-substring-no-properties (region-beginning) (region-end))
                                                                          (thing-at-point 'word t))))
                   (t (read-string (format "Add meaning for '%s': " word)) ) )))
    (if paw-studylist
        (paw-add-online-word-callback word exp (if note note (paw-get-note) ))
      (paw-get-all-studylist nil
                             (lambda ()
                               (paw-add-online-word-callback word exp (if note note (paw-get-note) ))))) )
  (if (featurep 'evil)
      (evil-force-normal-state)))

(defun paw-add-online-word-callback (word exp note)
  (let* ((choice
          (ido-completing-read (format "Add: %s, meaning: %s, to: " word exp) paw-studylist)
          ;; (consult--read paw-studylist
          ;;                       :prompt (format "Add: %s, meaning: %s, to: " word exp)
          ;;                       :history 'studylist-history
          ;;                       :sort nil)
          )
         (item (assoc-default choice paw-studylist))
         (studylist_id (assoc-default 'id item))
         (name (assoc-default 'name item)) entry)
    (paw-request-add-words word studylist_id
                           (lambda ()
                             ;; add word after adding to server
                             (if (eq 1 (caar (paw-db-sql `[:select :exists
                                                           [:select word :from items
                                                            :where (= word ,word)]])))
                                 (progn
                                   (if (s-blank-str? exp)
                                       (alert (format "'%s' already exists" word)
                                          :title "Dictionary Manager")
                                     ;; has exp, update it
                                     (paw-db-update-exp word exp)
                                     ;; get the updated entry
                                     (setq entry (car (paw-candidate-by-word word) ))

                                     ))
                               (paw-db-insert
                                `(((word . ,word) (exp . ,exp)
                                   ;; query sdcv and add to expression, but it is not very useful, since it can not be viewed
                                   ;; use word type instead
                                   ;; (exp . ,(s-collapse-whitespace (sdcv-translate-result word sdcv-dictionary-complete-list)))
                                   ))
                                :serverp 1
                                :note note
                                :note_type (assoc 'word paw-note-type-alist)
                                :origin_type (or paw-note-origin-type major-mode)
                                :origin_path (or paw-note-origin-path (paw-get-origin-path))
                                :origin_id studylist_id
                                :origin_point name
                                :created_at (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
                               (alert (format "Added \"%s\" in server." word))

                               (setq entry (car (paw-candidate-by-word word) )))


                             (if (eq major-mode 'paw-search-mode)
                                 (paw-search-refresh)
                               (paw-search-refresh t))

                             ;; update overlays in all buffers with paw-annotation-mode
                             (-map (lambda (b)
                                     (with-current-buffer b
                                       (if (eq paw-annotation-mode t)
                                           (paw-show-all-annotations))))
                                   (buffer-list))

                             ;; show the word again
                             (if (eq major-mode 'paw-view-note-mode)
                                 (paw-view-note entry nil))

                             (alert (format "Add word done." word))
                             ))))



;;;###autoload
(defun paw-sync-all-words ()
  "Sync all words from all studylists in Eudic."
  (interactive)
  (dolist (item paw-studylist)
    (paw-sync-studylist nil item)))


;;;###autoload
(defun paw-get-all-studylist (prefix &optional callback)
  "Get all studylist in Eudic server."
  (interactive "P")
  (request "https://api.frdic.com/api/open/v1/studylist/category"
    :parser 'json-read
    :params '(("language" . "en"))
    :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
               ("Content-Type" . "application/xml")
               ("Authorization" . ,paw-authorization-keys))
    :timeout 5
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (if data
                    (let* ((studylist-data (cdr (assoc 'data data)))
                           (studylist (mapcar (lambda (item)
                                               (cons
                                                (format "ID: %s, Language: %s, Name: %s"
                                                        (cdr (assoc 'id item))
                                                        (cdr (assoc 'language item))
                                                        (cdr (assoc 'name item)))
                                                item))
                                             studylist-data)))
                      (setq paw-studylist studylist)
                      (if prefix
                          (let* ((choice (consult--read paw-studylist
                                                        :prompt "Select a studylist: "
                                                        :history 'studylist-history
                                                        :sort nil))
                                 (item (assoc-default choice paw-studylist)))
                            (message "You selected ID: %s" (assoc-default 'id item)))
                          (mapc (lambda (item) (message "%s" (car item))) paw-studylist))
                      (if callback (funcall callback) )))))
    :error
    (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                   (message "%s" error-thrown)))))




;;;###autoload
(defun paw-sync-studylist(prefix &optional item)
  "Sync a studylist in Eudic server, if prefix is t, select a studylist to sync."
  (interactive "P")
  (let* ((item (if prefix
                   (assoc-default (consult--read paw-studylist
                                                 :prompt "Select a studylist: "
                                                 :history 'studylist-history
                                                 :sort nil) paw-studylist)
                 (or item (assoc-default "ID: 0, Language: en, Name: 我的生词本" paw-studylist))))
         (studylist_id (assoc-default 'id item))
         (name (assoc-default 'name item)))
    (request (format "https://api.frdic.com/api/open/v1/studylist/words/%s" studylist_id)
      :parser 'buffer-string
      :params '(("language" . "en")
                ;; ("category" . 0)
                ;; ("page" . 1)
                ;; ("page_size" . 100)
                )
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                 ("Content-Type" . "application/xml")
                 ("Authorization" . ,paw-authorization-keys))
      :timeout 5
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "%s" error-thrown)
                     ))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (alert "Request words successed."
                         :title "Dictionary Manager")
                  (let ((json-file (concat user-emacs-directory ".cache/" studylist_id "_" name  ".json")) )
                    (if (file-exists-p json-file)
                        (delete-file json-file)
                      (with-temp-file json-file
                        (insert data))))
                  (paw-db)
                  (let* ((json (json-read-from-string data))
                         (data (paw-parse-json json))
                         (number (length data))
                         (db-words (cl-loop for item in (paw-db-sql `[:select word :from
                                                                      [:select [items:word items:exp status:serverp status:origin_id status:origin_path] :from items
                                                                       :inner :join status
                                                                       :on (= items:word status:word)]
                                                                      :where (and (= serverp 1)
                                                                                  (= origin_id ,studylist_id))]) collect
                                                                                  (car item)))
                         (server-words (cl-loop for entry in (paw-parse-json json) collect
                                                (alist-get 'word entry)))
                         (new-server-words (vconcat (-difference server-words db-words)))
                         (db-words-to-delete (vconcat (-difference db-words server-words))))
                    (when (> number 0)
                      ;; ;; TODO add offline words to server
                      ;; (paw-request-add-words (vconcat (cl-loop for item in (paw-db-sql [:select word :from
                      ;;                                                                           [:select [items:word items:exp status:serverp] :from items
                      ;;                                                                            :inner :join status
                      ;;                                                                            :on (= items:word status:word)]
                      ;;                                                                           :where (= serverp 0)]) collect
                      ;;                                                                           (car item))) studylist_id)
                      ;; delete the online words that already deleted in server
                      (when (yes-or-no-p (format "Db words to delete: %s" db-words-to-delete))
                        (paw-db-delete db-words-to-delete))

                      ;; insert new server words
                      (when (yes-or-no-p (format "New server words to add: %s" new-server-words))
                        (paw-db-insert
                         (-map (lambda(item) `((word . ,item) (exp . ""))) new-server-words)
                         :serverp 1
                         :note ""
                         :note_type (assoc 'word paw-note-type-alist)
                         :origin_type "eudic"
                         :origin_path nil
                         :origin_id studylist_id
                         :origin_point name
                         :created_at (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))))
                    (message "Synced %s, total %s words" name number))
                  ;; (paw-search-refresh)
                  )))))



(defun paw-request-add-words (word studylist_id &optional callback)
  (let ((wordv (if (vectorp word)
                   word
                 (vector word))))
    (request "https://api.frdic.com/api/open/v1/studylist/words"
      :parser 'buffer-string
      :type "POST"
      :data (json-encode `(("id" . ,studylist_id)
                           ("language" . "en")
                           ("words" . ,wordv)))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                 ("Content-Type" . "application/json")
                 ("Authorization" . ,paw-authorization-keys))
      :timeout 5
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "%s" error-thrown)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (message "%s is updated to server, %s" word data)
                  (if callback
                      (funcall callback))))) ))

(defun paw-request-delete-words (word studylist_id &optional callback)
  (let ((wordv (if (vectorp word)
                   word
                 (vector word))))
    (request "https://api.frdic.com/api/open/v1/studylist/words"
      :parser 'buffer-string
      :type "DELETE"
      :data (json-encode `(("id" . ,studylist_id)
                           ("language" . "en")
                           ("words" . ,wordv)) )
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                 ("Content-Type" . "application/json")
                 ("Authorization" . ,paw-authorization-keys))
      :timeout 5
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "%s" error-thrown)))
      :success (cl-function
                (lambda (&key _data &allow-other-keys)
                  (alert (format "Deleted \"%s\" in server." word)
                         :title "Dictionary Manager")
                  (paw-db-delete word)
                  (if callback (funcall callback) )))) ))

;;;###autoload
(defun paw-new-studylist (name &optional callback)
  "Create a new studylist in Eudic server."
  (interactive "sStudylist name: ")
  (request "https://api.frdic.com/api/open/v1/studylist/category"
    :parser 'buffer-string
    :type "POST"
    :data (json-encode `(("language" . "en")
                         ("name" . ,name)))
    :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
               ("Content-Type" . "application/json")
               ("Authorization" . ,paw-authorization-keys))
    :timeout 5
    :error
    (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                   (message "%s" error-thrown)))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let* ((json-object-type 'alist)
                       (json-array-type 'list)
                       (json (json-read-from-string data))
                       (data (assoc-default 'data json))
                       (id (assoc-default 'id data))
                       (name (assoc-default 'name data))
                       (language (assoc-default 'language data)))
                  (push (list (format "ID: %s, Language: %s, Name: %s" id language name)
                              (cons 'id id)
                              (cons 'language language)
                              (cons 'name name))
                        paw-studylist)
                  (message "Studylist: %s is added to server" name)
                  (if callback (funcall callback)))))))

;;;###autoload
(defun paw-delete-studylist ()
  "Delete a studylist in Eudic server."
  (interactive)
  (let ((studylist-action
         (lambda ()
           (let* ((choice (consult--read paw-studylist
                                         :prompt "Select a studylist: "
                                         :history 'studylist-history
                                         :sort nil))
                  (item (assoc-default choice paw-studylist)))
             (paw-delete-studylist-callback (assoc-default 'id item) (assoc-default 'name item))))))
    (if paw-studylist
        (funcall studylist-action)
      (paw-get-all-studylist nil studylist-action))))


(defun paw-delete-studylist-callback(id name &optional callback)
  (when (yes-or-no-p (format "Delete studylist: %s %s" id name))
    (request "https://api.frdic.com/api/open/v1/studylist/category"
      :parser 'buffer-string
      :type "DELETE"
      :data (json-encode `(("id" . ,id)
                           ("language" . "en")
                           ("name" . ,name)))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                 ("Content-Type" . "application/json")
                 ("Authorization" . ,paw-authorization-keys))
      :timeout 5
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "%s" error-thrown)))
      :success (cl-function
                (lambda (&key _data &allow-other-keys)
                  (message "Studylist: %s is deleted on server" name)
                  (setq paw-studylist
                        (cl-remove-if (lambda (x)
                                        (string= (cdr (assoc 'id x)) id))
                                      paw-studylist))
                  (if callback
                      (funcall callback))))) ))


;;;###autoload
(defun paw-rename-studylist ()
  "Rename a studylist in Eudic server."
  (interactive)
  (let ((studylist-action
         (lambda ()
           (let* ((choice (consult--read paw-studylist
                                         :prompt "Select a studylist to rename: "
                                         :history 'studylist-history
                                         :sort nil))
                  (item (assoc-default choice paw-studylist))
                  (new-name (read-string "Enter new name: " (assoc-default 'name item))))
             (paw-rename-studylist-callback (assoc-default 'id item) new-name (assoc-default 'name item))))))
    (if paw-studylist
        (funcall studylist-action)
      (paw-get-all-studylist nil studylist-action))))

(defun paw-rename-studylist-callback (id new-name old-name &optional callback)
  (when (yes-or-no-p (format "Rename studylist: %s %s to %s" id old-name new-name))
    (request "https://api.frdic.com/api/open/v1/studylist/category"
      :parser 'buffer-string
      :type "PATCH"
      :data (json-encode `(("id" . ,id)
                           ("language" . "en")
                           ("name" . ,new-name)))
      :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                 ("Content-Type" . "application/json")
                 ("Authorization" . ,paw-authorization-keys))
      :timeout 5
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Error: %s" error-thrown)))
      :success (cl-function
                (lambda (&key _data &allow-other-keys)
                  (message "Studylist: %s is renamed to %s on server" old-name new-name)
                  (paw-get-all-studylist nil)
                  ;; TODO use emacs way to update paw-studylist instead of request one more time
                  (if callback
                      (funcall callback))))) ))

;;;###autoload
(defun paw-change-studylist()
  "Change the studylist of entry at point. It is mainly used in
`paw-search-mode.'"
  (interactive)
  (-let* ((marked-entries (paw-find-marked-candidates))
          (entries
           (or marked-entries
               (if entry (list entry)
                 (if (get-text-property (point) 'paw-entry)
                     (list (get-text-property (point) 'paw-entry))
                   (with-current-buffer paw-view-note-buffer-name
                     (list paw-note-entry))))))
          ((id . name) (let* ((choice (consult--read paw-studylist
                                                     :prompt "Select a studylist to change: "
                                                     :history 'studylist-history
                                                     :sort nil))
                              (item (assoc-default choice paw-studylist)))
                         (cons (assoc-default 'id item) (assoc-default 'name item)))))
    (when entries
      (paw-request-add-words (vconcat (cl-loop for entry in entries collect (alist-get 'word entry)) ) id
                             (lambda ()
                               (cl-loop for entry in entries do
                                        (let* ((word (alist-get 'word entry))
                                               (origin-id (alist-get 'origin_id entry)))
                                          (paw-db-update-origin_id word id)
                                          (paw-db-update-origin_point word name)))
                               ;; dangerous, delete old words!!!
                               (paw-request-delete-words (vconcat (cl-loop for entry in entries collect (alist-get 'word entry)) ) (alist-get 'origin_id (car entries)))
                               (if (eq major-mode 'paw-search-mode)
                                   (paw-search-refresh)
                                 (paw-search-refresh t))))


      )))

(provide 'paw-request)
