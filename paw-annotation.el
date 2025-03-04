;;; paw-annotation.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'paw-db)
(require 'paw-util)
(require 'paw-faces)
(require 'paw-svg)
(require 'paw-gptel)
(require 'paw-request)
(require 'paw-focus)
(require 'paw-search)

(require 'org)
(require 's)
(require 'dash)
(require 'focus)
(require 'thingatpt)
(require 'evil-core nil t)
(require 'immersive-translate nil t)

(declare-function evil-define-key* "ext:evil-core.el" t t)

(defcustom paw-annotation-mode-supported-modes
  '(nov-mode org-mode paw-view-note-mode wallabag-entry-mode eww-mode eaf-mode elfeed-show-mode pdf-view-mode)
  "Supported modes for paw-annotation-mode."
  :group 'paw
  :type 'list)

(defvar paw-annotation-current-highlight-type (assoc 'highlight-1 paw-note-type-alist))

(defcustom paw-annotation-stamps
  '("❗"
    "❤"
    "😊")
  "Stamps could be a list of display properties, it can make use of svg-lib rich icons library, and no limit how many items."
  :group 'paw
  :type 'list)

(defcustom paw-annotation-after-string-space (if (eq system-type 'android) " " "\u2009")
  "TODO Space string for annotation. Currently only support Japanese.
This is disabled since it does not work well, please don't use it at this moment."
  :group 'paw
  :type '(choice (const :tag "Normal space" " ")
          (const :tag "Thin space" "\u2009")
          (const :tag "Hair space" "\u200A")
          (string :tag "Custom string")))

(defvar paw-annotation-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "," 'paw-list-annotations)
    (define-key map (kbd "<RET>") 'paw-goto-dashboard)
    (define-key map "D" 'paw-delete-annotation) ;; d is used for scroll
    (define-key map "n" 'paw-next-annotation)
    (define-key map "N" 'paw-previous-annotation)
    (define-key map "p" 'paw-previous-annotation) ;; may impact edit mode
    (define-key map "y" 'paw-copy-annotation)
    (define-key map "r" 'paw-view-note-play)
    (define-key map "s" 'paw-view-note-in-minibuffer)
    (define-key map "i" 'paw-find-note)
    (define-key map "&" 'paw-find-note)
    (define-key map "I" 'paw-find-notes)
    (define-key map "v" 'paw-view-note)
    (define-key map "V" 'paw-view-notes)
    (define-key map "c" 'paw-change-word-learning-level)
    (define-key map "C" 'paw-change-note_type)
    (define-key map "f" 'paw-follow-link)
    ;; (define-key map (kbd "<mouse-8>") 'paw-mouse-8)
    ;; (define-key map (kbd "<mouse-9>") 'paw-mouse-9)
    ;; (define-key map (kbd "<mouse-1>") 'paw-mouse-2) ; have to enable mouse 2 will also binds mouse 1 (no idea)
    ;; (define-key map (kbd "<mouse-2>") 'paw-mouse-2) ; have to enable mouse 2 will also binds mouse 1 (no idea)
    map)
  "Keymap for annotation overlay.")

(defun paw-mouse-8 (event)
  "Browser the url click on with eww.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No word chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (paw-view-note))))


(defun paw-mouse-9 (event)
  "Browser the url click on with eww.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No word chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (paw-view-notes))))

(defun paw-mouse-2 (event)
  "Browser the url click on with eww.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No word chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (paw-view-note))))

(defcustom paw-cache-dir
  (expand-file-name (concat user-emacs-directory ".cache/paw"))
  "paw cache directory."
  :group 'paw
  :type 'directory)


(defun paw-add-general (word type location &optional gptel note path origin_type)
  (let* ((word (pcase (car type)
                 ('bookmark
                  (pcase major-mode
                    ('eaf-mode (pcase eaf--buffer-app-name
                                 ("browser"  eaf--bookmark-title)
                                 ("pdf-viewer" (file-name-nondirectory eaf--buffer-url ))
                                 (_ nil)))
                    ('eww-mode (plist-get eww-data :title))
                    (_ (format "Bookmark %s" (pp location)))))
                 (_ word)))
         (id (concat word ":id:" (org-id-uuid))))
    (paw-db-insert
     `(((word . ,id) (exp . nil)))
     :content (pcase (car type)
                ('image (let ((image (condition-case nil
                                         (or (plist-get (cdr (image--get-image)) :file)
                                             (plist-get (cdr (image--get-image)) :data))
                                       (error nil))))
                          (if image
                              (paw-image-content-json image) ; if image under point, no need to prompt, just add it as attachment
                            (let* ((png (format "%s.png" (org-id-uuid)))
                                   (path (expand-file-name png paw-note-dir))
                                   (json (json-encode `((filename . "screenshot.png") (path . ,png)))))
                              (let ((result
                                     (shell-command-to-string
                                      (format
                                       (cl-case system-type
                                         (gnu/linux
                                          (if (string= "wayland" (getenv "XDG_SESSION_TYPE"))
                                              (if (executable-find "wl-paste")
                                                  "wl-paste -t image/png > %s"
                                                (user-error
                                                 "Please install the \"wl-paste\" program included in wl-clipboard"))
                                            (if (executable-find "xclip")
                                                "xclip -selection clipboard -t image/png -o > %s"
                                              (user-error
                                               "Please install the \"xclip\" program"))))
                                         ((windows-nt cygwin)
                                          (if (executable-find "convert")
                                              "/mingw64/bin/convert.exe clipboard: %s"
                                            (user-error
                                             "Please install the \"convert\" program included in ImageMagick")))
                                         ((darwin berkeley-unix)
                                          (if (executable-find "pngpaste")
                                              "pngpaste %s"
                                            (user-error
                                             "Please install the \"pngpaste\" program from Homebrew.")))) path))))
                                (if (s-contains? "No image data found on the clipboard" result)
                                    (error (s-collapse-whitespace result))))
                              json))))
                ('attachment (let* ((image (condition-case nil
                                               (or (plist-get (cdr (image--get-image)) :file)
                                                   (plist-get (cdr (image--get-image)) :data))
                                             (error nil))))
                               (if image
                                   (paw-image-content-json image) ; if image under point, no need to prompt, just add it as attachment
                                 (let* ((src (read-file-name "Select the attachment: "))
                                        (attachment (format "%s.%s" (org-id-uuid) (file-name-extension (file-name-nondirectory src))))
                                        (dst (expand-file-name attachment paw-note-dir))
                                        (json (json-encode `((filename . ,(file-name-nondirectory src)) (path . ,attachment)))))
                                   (copy-file src dst t)
                                   json))))
                ('link (let* ((type (ido-completing-read "Select the link type: " '("file" "url" "annotation" "org")))
                              (link
                               (pcase type
                                 ("file"
                                  (read-file-name "Select a file as link: "))
                                 ("url"
                                  (read-from-minibuffer "Please insert an url link: "))
                                 ("annotation"
                                  (let* ((entry (get-text-property 0 'paw-entry
                                                                   (ivy-read "Please insert an annotation: "
                                                                             (paw-candidates-format :all t)
                                                                             :sort nil)))
                                         (word (alist-get 'word entry)))
                                    word))
                                 ("org"
                                  (with-temp-buffer
                                    (call-interactively 'org-insert-link)
                                    (buffer-string)))))
                              (json (json-encode `((type . ,type) (link . ,link)))))
                         json))
                ('bookmark
                 (pcase major-mode
                   ('eaf-mode (pcase eaf--buffer-app-name
                                ("browser"  eaf--bookmark-title)
                                ("pdf-viewer" (file-name-nondirectory eaf--buffer-url ))
                                (_ nil)))
                   ('eww-mode (plist-get eww-data :title))
                   (_ (format "Bookmark %s" (pp location)))))
                (_ word))
     :serverp 2
     :note (or note
               (pcase (car type)
                 ('attachment "")           ; attachment empty note, mark also empty
                 ('image "")
                 ('bookmark "")
                 ('word (paw-get-note))
                 (_ (if mark-active
                        "" ; mark is empty, since the note not that useful, if need sentence note, use `paw-add-word'
                      (paw-get-note)))) )
     :note_type type
     :origin_type (or origin_type
                      (if (derived-mode-p 'eaf-mode)
                          eaf--buffer-app-name
                        major-mode))
     :origin_path (or path (paw-get-origin-path))
     :origin_id (paw-get-id)
     :origin_point location
     :created_at (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))

    ;; update content with gptel
    (if gptel
        (paw-gptel-update-note id word (car type)
                               (lambda ()
                                 (if (eq major-mode 'paw-search-mode)
                                     (paw-search-refresh)))))

    ;; quit mark
    (if (featurep 'evil)
        (evil-force-normal-state))

    ;; query back the entry and add overylay
    (let ((candidates (paw-candidate-by-id (if (string-match ":id:\\(.*\\)" id)
                                               (match-string 1 id)
                                             id))))
      ;; attachment do not need overlay
      (pcase (car type)
        ((or 'attachment 'image)
         (paw-find-note (car candidates)))
        ('bookmark
         (paw-download-ico (paw-get-origin-path))
         (message (format "Added bookmark: %s -> %s" (paw-get-origin-path) location)))
        ('sdcv
         (message (format "Added sdcv: %s" word)))
        (_ (pcase major-mode
             ('pdf-view-mode nil)
             ('eaf-mode nil)
             ('paw-search-mode nil)
             ('paw-view-note-mode
              ;; add highlight/done/cancel etc in paw-view-note-mode, only add annotation to that file
              (if-let* ((buffer (-first (lambda (b)
                                          (with-current-buffer b
                                            (equal buffer-file-truename (paw-get-origin-path))))
                                        (buffer-list))))
                (with-current-buffer buffer
                    (paw-add-annotation-overlay (car candidates))))) ;; add highlight etc in side paw-view-note-mode
             (_ (paw-add-annotation-overlay (car candidates))))))
      ;; update *paw* buffer
      (if (buffer-live-p (get-buffer "*paw*"))
          (paw t)))

    ;; enable paw annotation mode after adding
    ;; not add from paw-view-note
    (unless (or paw-annotation-mode (not (eq major-mode 'pdf-view-mode)))
      (paw-annotation-mode 1))

    ;; pdf-view-mode can not highlight inline, print out instead
    (if (and (eq major-mode 'pdf-view-mode))
        (message "Added: %s" word))))

(defun paw-download-ico (&optional location)
  "Download ico file from location."
  (interactive)
  (let* ((location (or location (alist-get 'origin_path (paw-find-candidate-at-point))) )
         (location (let* ((url (url-generic-parse-url location))
                     (https (url-type url))
                     (host (url-host url)))
                (concat https "://" host)))
         (ico-file-hash (md5 location))
         (icon-file-path (concat (expand-file-name ico-file-hash paw-cache-dir) ".ico"))
         (output-icon-file-path (concat (expand-file-name ico-file-hash paw-cache-dir) ".png")))
    (when (string-match-p "http" location)
      (unless (file-exists-p output-icon-file-path)
        (message "Downloading ico file from %s" location)
        (request location
          :parser 'buffer-string
          :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                     ("Content-Type" . "application/xml"))
          :timeout 5
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (when-let* ((parsed-html (with-temp-buffer
                                                 (insert data)
                                                 (libxml-parse-html-region (point-min) (point-max))))
                                  (icons (dom-elements parsed-html 'rel "icon"))
                                  (icon (dom-attr icons 'href))
                                  (icon (if (string-match-p "https" icon)
                                            icon
                                          (let* ((url (url-generic-parse-url location))
                                                 (https (url-type url))
                                                 (host (url-host url)))
                                            (concat https "://" host icon)))))
                        (message "Found ico: %s" icon)
                        (make-directory paw-cache-dir t)
                        (set-process-sentinel
                         (start-process
                          (executable-find "curl")
                          "*paw-favicon-downloder*"
                          (executable-find "curl")
                          "-L"
                          "--user-agent"
                          "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"
                          icon
                          "--output"
                          icon-file-path)
                         (lambda (process event)
                           (when (string= event "finished\n")
                             (call-process-shell-command (format "convert %s -thumbnail 16x16 -alpha on -background none -flatten %s" icon-file-path output-icon-file-path) nil 0)
                             (message "Download ico to %s" output-icon-file-path)
                             (paw-search-refresh)))))))
          :error
          (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                         (message "%s" error-thrown))))))))

(defun paw-download-all-icos()
  "Download all ico files. Be careful, it will try to download all
icos of all links (`paw-list-all-links') in database."
  (interactive)
  (cl-loop for entry in (paw-candidates-only-links) do
           (paw-download-ico (alist-get 'origin_path entry) )))

(defun paw-image-content-json (image)
  (if (file-exists-p image)
      (let* ((src image)
             (attachment (format "%s.%s" (org-id-uuid) (file-name-extension (file-name-nondirectory src))))
             (dst (expand-file-name attachment paw-note-dir))
             (json (json-encode `((filename . ,(file-name-nondirectory src)) (path . ,attachment)))))
        (copy-file src dst t)
        json)
    (let* ((src (expand-file-name (concat "picture." (symbol-name (plist-get (cdr (image--get-image)) :type) )) temporary-file-directory))
           (attachment (format "%s.%s" (org-id-uuid) (file-name-extension (file-name-nondirectory src))))
           (dst (expand-file-name attachment paw-note-dir))
           (json (json-encode `((filename . ,(file-name-nondirectory src)) (path . ,attachment)))))
      (with-temp-buffer
        (insert image)
        (write-region (point-min) (point-max) src))
      (copy-file src dst t)
      json)))


;;;###autoload
(defun paw-add-highlight (prefix)
  "Add an annotation."
  (interactive "P")
  (let* ((word (paw-get-word))
         (type paw-annotation-current-highlight-type )
         (location (pcase major-mode
                     ('nov-mode
                      (if mark-active
                          (cons nov-documents-index (cons (region-beginning) (region-end)))
                        (cons nov-documents-index (save-excursion
                                                    (let (beg end)
                                                      (forward-thing 'symbol 1)
                                                      (setq end (point))
                                                      (forward-thing 'symbol -1)
                                                      (setq beg (point))
                                                      (cons beg end)) ))))
                     ('pdf-view-mode
                      (cons (image-mode-window-get 'page) 0))
                     ('eaf-mode
                      (pcase eaf--buffer-app-name
                        ("pdf-viewer"
                         (string-to-number (eaf-call-sync "execute_function" eaf--buffer-id "current_page")))
                        ("browser" 0)
                        (_ 0)))
                     (_ (if mark-active
                            (cons (region-beginning) (region-end))
                          (save-excursion
                            (let (beg end)
                              (forward-thing 'symbol 1)
                              (setq end (point))
                              (forward-thing 'symbol -1)
                              (setq beg (point))
                              (cons beg end))))))))
    (paw-add-general word type location prefix)))

;;;###autoload
(defun paw-add-stamp (prefix)
  "Add an annotation."
  (interactive "P")
  (let* ((word (paw-get-word))
         (type (paw-get-stamp))
         (location (paw-get-location)))
    (paw-add-general word type location prefix)))

(defun paw-get-stamp ()
  (cons 'stamp  (ido-completing-read "Select a stamp: " paw-annotation-stamps)))


;;;###autoload (autoload 'paw-add "paw-annotation")
(defmacro paw-add (field)
  `(defun ,(intern (format "paw-add-%s" field)) (prefix)
     (interactive "P")
     (let* ((word (paw-get-word))
            (type (assoc ',(intern field) paw-note-type-alist))
            (location (paw-get-location)))
       (paw-add-general word type location prefix))))

(paw-add "word")

(paw-add "todo")

(paw-add "done")

(paw-add "cancel")

(paw-add "question")

(paw-add "link")

(paw-add "bookmark")

(defun paw-follow-link ()
  (interactive)
  (let* ((entry (get-char-property (point) 'paw-entry))
         (note-type (alist-get 'note_type entry))
         (content (alist-get 'content entry)))
    (pcase (car note-type)
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
            (if (file-exists-p link)
                (find-file link)))
           ("url"
            (browse-url link))
           ("annotation"
            (paw-find-origin
             (cl-find-if (lambda (x) (equal link (alist-get 'word x))) paw-search-entries)))))))))

;;;###autoload
(defun paw-add-attachment (&optional word)
  "Add an attachment."
  (interactive)
  (let ((word (or word "ATTACHMENT"))
        (location (pcase major-mode
                    ('nov-mode
                     (cons nov-documents-index (point)))
                    ('pdf-view-mode
                     (cons (image-mode-window-get 'page) 0))
                    ('eaf-mode
                     (string-to-number (eaf-call-sync "execute_function" eaf--buffer-id "current_page")))
                    (_ (point))))
        (type (assoc 'attachment paw-note-type-alist)))
    (paw-add-general word type location)))

;;;###autoload
(defun paw-add-image (&optional word)
  "Add an image."
  (interactive)
  (let ((word (or word "IMAGE"))
        (location (pcase major-mode
                    ('nov-mode
                     (cons nov-documents-index (point)))
                    ('pdf-view-mode
                     (cons (image-mode-window-get 'page) 0))
                    ('eaf-mode
                     (string-to-number (eaf-call-sync "execute_function" eaf--buffer-id "current_page")))
                    (_ (point))))
        (type (assoc 'image paw-note-type-alist)))
    (paw-add-general word type location)))

(defun paw-show-all-annotations (&optional candidates)
  "when candidates, just check and show the candidates overlays, this is much faster"
  (interactive)
  (if (or candidates paw-annotation-mode)
      (if (eq major-mode 'eaf-mode)
          (eaf-call-async "execute_function_with_args" eaf--buffer-id "paw_annotation_mode" `,paw-db-file)
        (let ((candidates (if candidates candidates (paw-candidates-by-origin-path-serverp) )))
          (save-excursion
            (cl-loop for entry in candidates do
                     (pcase (car (alist-get 'note_type entry))
                       ('attachment)
                       ('bookmark)
                       ('image)
                       (_ (paw-add-annotation-overlay entry)))))) )))

(defun paw-get-highlight-type ()
  (interactive)
  (let ((choice (read-char-from-minibuffer
                 (format "Annotation Type: %s %s %s %s (o)ther (q)uit "
                         (propertize "highlight-(1)" 'face 'paw-highlight-1-face)
                         (propertize "highlight-(2)" 'face 'paw-highlight-2-face)
                         (propertize "highlight-(3)" 'face 'paw-highlight-3-face)
                         (propertize "highlight-(4)" 'face 'paw-highlight-4-face)) '(?1 ?2 ?3 ?4 ?o ?q))))
    (unless (eq choice ?q)
      (let* ((c (char-to-string choice))
             (uppercasep (and (stringp c) (string-equal c (upcase c)) ))
             (cc (downcase c)))
        (cond
         ((string-equal cc "1") (message "") (assoc 'highlight-1 paw-note-type-alist))
         ((string-equal cc "2") (message "") (assoc 'highlight-2 paw-note-type-alist))
         ((string-equal cc "3") (message "") (assoc 'highlight-3 paw-note-type-alist))
         ((string-equal cc "4") (message "") (assoc 'highlight-4 paw-note-type-alist))
         ((string-equal cc "o")
          (message "")
          (cons 'highlight (let* ((names (mapcar #'symbol-name (face-list)))
                                  (counsel--faces-format
                                   (format "%%-%ds %%s"
                                           (apply #'max 0 (mapcar #'string-width names)))))
                             (intern (ivy-read "Face: " names
                                               :require-match t
                                               :history 'face-name-history
                                               :preselect (counsel--face-at-point)
                                               :caller 'counsel-faces)))))
         ((string-equal cc "q") (message "") (error "quit"))
         (t (message "") (assoc 'highlight-1 paw-note-type-alist)))))))

(defun paw-get-todo-type ()
  (interactive)
  (let ((choice (read-char-from-minibuffer
                 (format "TODO Type: %s %s %s (q)uit "
                         (propertize "(t/T)ODO" 'face 'paw-todo-face)
                         (propertize "(d/D)ONE" 'face 'paw-done-face)
                         (propertize "(c/C)ANCEL" 'face 'paw-cancel-face)) '(?t ?T ?d ?D ?c ?C ?q))))
    (unless (eq choice ?q)
      (let* ((c (char-to-string choice))
             (uppercasep (and (stringp c) (string-equal c (upcase c)) ))
             (cc (downcase c)))
        (cond
         ((string-equal cc "t") (message "") (assoc 'todo paw-note-type-alist))
         ((string-equal cc "d") (message "") (assoc 'done paw-note-type-alist))
         ((string-equal cc "c") (message "") (assoc 'cancel paw-note-type-alist))
         ((string-equal cc "q") (message "") (error "quit"))
         (t (message "") (assoc 'todo paw-note-type-alist)))))))

(defun paw-add-annotation-overlay (entry &optional type)
  (let* ((location (alist-get 'origin_point entry))
         (note (alist-get 'note entry))
         (word (alist-get 'word entry))
         (serverp (alist-get 'serverp entry))
         (real-word (paw-get-real-word word))
         (note-type (if type
                        (pcase (car (alist-get 'note_type entry))
                          ('word
                           (let ((wd (read-from-minibuffer "Insert the annotation symbol: ")))
                             ;; update note type
                             (paw-update-note_type word `(word . ,wd))
                             ;; delete overlay
                             (delete-overlay
                              (cl-find-if
                               (lambda (o)
                                 (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                               (overlays-at (point))))
                             (cons 'word wd)))
                          ('question
                           (let ((wd (read-from-minibuffer "Insert the question symbol: ")))
                             ;; update note type
                             (paw-update-note_type word `(question . ,wd))
                             ;; delete overlay
                             (delete-overlay
                              (cl-find-if
                               (lambda (o)
                                 (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                               (overlays-at (point))))
                             (cons 'question wd)))
                          ((or 'todo 'done 'cancel)
                           (let ((td (paw-get-todo-type)))
                             ;; update note type
                             (paw-update-note_type word td)
                             ;; delete overlay
                             (delete-overlay
                              (cl-find-if
                               (lambda (o)
                                 (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                               (overlays-at (point))))
                             td))
                          ('stamp
                           (let ((td (paw-get-stamp)))
                             ;; update note type
                             (paw-update-note_type word td)
                             ;; delete overlay
                             (delete-overlay
                              (cl-find-if
                               (lambda (o)
                                 (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                               (overlays-at (point))))
                             td))
                          (_
                           (let ((tp (setq paw-annotation-current-highlight-type (paw-get-highlight-type))))
                             ;; update note type
                             (paw-update-note_type word tp)
                             ;; delete overlay
                             (delete-overlay
                              (cl-find-if
                               (lambda (o)
                                 (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                               (overlays-at (point))))
                             tp)))
                      (alist-get 'note_type entry)))
         beg end)
    ;; if `type' is provided, the entry should be updated
    ;; here, we update the note-type back to entry
    (if type
        (setf (alist-get 'note_type entry) note-type))
    (unless (eq major-mode 'pdf-view-mode)
      ;; get the location, beg, and end based on modes
      (setq location
            (pcase major-mode
              ('nov-mode
               (cond ((stringp location) nil) ;; it is an online word
                     ((consp (cdr location)) ;; it is a highlight
                      (if (eq nov-documents-index (car location))
                          (progn
                           (setq beg (cadr location))
                           (setq end (cddr location))
                           (cdr location))
                        nil))
                     ((consp location) ;; it is an offline word
                      (if (eq nov-documents-index (car location))
                          (cdr location)
                        "unmatch-index"))
                     (t nil)))
              (_
               (cond ((consp location)
                      (setq beg (car location))
                      (setq end (cdr location))
                      location)
                     ((numberp location) location)
                     (t nil)))))
      ;; add overlay to location
      (cond ((numberp location)
             ;; if has location, that means it is local word, hightlight one occurrence in that location
             (goto-char location)
             ;; serch and verify the word
             (if (string-match-p (regexp-quote (or (thing-at-point 'word t) "")) real-word)
                 (progn
                   (forward-thing 'word 1)
                   (setq end (point))
                   (forward-thing 'word -1)
                   (setq beg (point)))
               ;; first search -50 ~ 50
               (goto-char (- location 50))
               (cond ((re-search-forward (regexp-quote real-word) (+ location 50) t)
                      (setq beg (match-beginning 0))
                      (setq end (match-end 0)))
                     (t
                      ;; second search (point-min) (point-max)
                      (goto-char (point-min))
                      (when (re-search-forward (concat "\\b" real-word "\\b") (point-max) t)
                        (setq beg (match-beginning 0))
                        (setq end (match-end 0))))))
             (if (and beg end)
                 (unless (cl-find-if
                          (lambda (o)
                            (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                          (overlays-in (point-min) (point-max)))
                   (paw-add-overlay beg end note-type note entry))
               (message "Can not find word \"%s\", maybe the location is changed." real-word)))
            ;; if it is offline word, major-mode is nov-mode, and no location is
            ;; found, just skip it.
            ((and (stringp location) (string= "unmatch-index" location) ) nil)
            ;; if serverp is 1, that means it is an online word, hightlight all occurrences in the buffer
            ;; if location is nil, that means it is not in current buffer, hightlight all occurrences in the buffer
            ((or (paw-online-p serverp) (eq location nil))
             (goto-char (point-min))
             (let ((case-fold-search t)
                   (cur-loc)
                   (last-loc))  ; or nil for case-sensitive
               (while (if (string-match-p "[[:ascii:]]+" real-word)
                          ;; english
                          (progn
                            (setq cur-loc (re-search-forward (concat "\\b" real-word "\\b") nil t))
                            (if (eq cur-loc last-loc)
                                nil ;; if always the same, quit the while to avoid forever loop
                              (setq last-loc cur-loc)))
                        ;; non-english
                        (setq cur-loc (re-search-forward real-word nil t))
                        (if (eq cur-loc last-loc)
                            nil ;; if always the same, quit the while to avoid forever loop
                          (setq last-loc cur-loc)))
                 (let* ((beg (match-beginning 0))
                        (end (match-end 0))
	                (existing-overlays (overlays-in beg end)))
                   (unless (cl-find-if
                            (lambda (o)
                              (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                            existing-overlays)
                     (paw-add-overlay beg end note-type note entry))))))
            (t
             ;; TODO the match is not robust
             ;; first check string between beg and end
             (if (and
                  (< beg (point-max))
                  (< end (point-max))
                  (string-match-p (regexp-quote (s-trim (s-collapse-whitespace (buffer-substring-no-properties beg end)) ) ) (s-trim (s-collapse-whitespace real-word) )) )
                 (unless (cl-find-if
                          (lambda (o)
                            (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                          (overlays-in (point-min) (point-max)))
                   (paw-add-overlay beg end note-type note entry))

               ;; find the beg and end
               ;; first search -500 ~ 500
               ;; TODO the buffer search is not robust
               (goto-char (- beg 500))
               (if (re-search-forward (regexp-quote real-word) (+ end 500) t)
                   (progn
                     (setq beg (match-beginning 0))
                     (setq end (match-end 0)))
                 (goto-char (point-min))
                 ;; second search 0 (point-max)
                 (if (re-search-forward (regexp-quote real-word) (point-max) t)
                     (progn
                       (setq beg (match-beginning 0))
                       (setq end (match-end 0)))
                   ;; (message "Location of \"%s\"is incorrect."
                   ;;          (s-truncate 40 (s-collapse-whitespace real-word)))
                   ))
               (goto-char beg)
               ;; add overlay even if it can not be found
               (unless (cl-find-if
                        (lambda (o)
                          (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                        (overlays-at (point)))
                 (paw-add-overlay beg end note-type note entry))))))))


(defun paw-previous-annotation ()
  "Go to the previous annotation, if run it in *paw-view-note* buffer,
it will go to the previous annotation and view it."
  (interactive)
  (cond ((eq major-mode 'paw-search-mode)
         (progn
           (paw-previous)
           (paw-find-origin)))
        ((bound-and-true-p focus-mode)
         (paw-focus-find-prev-thing-segment))
        ((eq major-mode 'paw-view-note-mode)
         (if (buffer-live-p paw-note-target-buffer)
             (let ((window (get-buffer-window paw-note-target-buffer)))
               (if (window-live-p window)
                   (progn
                     (with-selected-window (select-window window)
                       (call-interactively 'paw-previous-annotation))
                     (recenter)
                     (call-interactively  'paw-view-note) )
                 (switch-to-buffer-other-window paw-note-target-buffer)
                 (call-interactively 'paw-previous-annotation)
                 (recenter)
                 (call-interactively  'paw-view-note)))
           (message "The original buffer is already closed.")))
        (t (let* ((previous-overlays (reverse (-filter
                                               (lambda (o)
                                                 (overlay-get o 'paw-entry))
                                               (overlays-in (point-min) (point))) ))
                  (overlay (car previous-overlays))
                  (beg (if overlay (overlay-start overlay)))
                  (end (if overlay (overlay-end overlay)))
                  (next-overlay (nth 1 previous-overlays))
                  (next-beg (if next-overlay (overlay-start next-overlay)))
                  (next-end (if next-overlay (overlay-end next-overlay))))
             (cond
              ((and (equal beg (point)) next-beg)
               (goto-char next-beg))
              ((not next-beg)
               (if overlay (goto-char beg))
               (message "This is the first annotation."))
              (overlay (goto-char beg)))) )))


(defun paw-next-annotation ()
  "Go to the next annotation, if run it in *paw-view-note* buffer,
it will go to the next annotation and view it."
  (interactive)
  (cond ((eq major-mode 'paw-search-mode)
         (progn
           (paw-next)
           (paw-find-origin)) )
        ((bound-and-true-p focus-mode)
         (paw-focus-find-next-thing-segment))
        ((eq major-mode 'paw-view-note-mode)
         (if (buffer-live-p paw-note-target-buffer)
             (let ((window (get-buffer-window paw-note-target-buffer)))
               (if (window-live-p window)
                   (progn
                     (with-selected-window (select-window window)
                       (call-interactively 'paw-next-annotation))
                     (recenter)
                     (call-interactively  'paw-view-note))
                 (switch-to-buffer-other-window paw-note-target-buffer)
                 (call-interactively 'paw-next-annotation)
                 (recenter)
                 (call-interactively  'paw-view-note)))
           (message "The original buffer is already closed.")))
        (t (let* ((overlay (cl-find-if
                            (lambda (o)
                              (overlay-get o 'paw-entry))
                            (overlays-in (point) (point-max))))
                  (beg (if overlay (overlay-start overlay)))
                  (end (if overlay (overlay-end overlay)))
                  (next-overlay (cl-find-if
                                 (lambda (o)
                                   (overlay-get o 'paw-entry))
                                 (overlays-in end (point-max))))
                  (next-beg (if next-overlay (overlay-start next-overlay)))
                  (next-end (if next-overlay (overlay-end next-overlay))))
             (cond
              ((and (equal beg (point)) next-beg)
               (goto-char next-beg))
              ((not next-beg)
               (if overlay (goto-char beg))
               (message "This is the last annotation."))
              (overlay (goto-char beg)))) )))



(defun paw-copy-annotation ()
  (interactive)
  (let ((entry (get-char-property (point) 'paw-entry)))
    (if entry
      (let ((word (or (paw-get-real-word entry) "")))
        (kill-new word)
        (message "Copied \"%s\"" word)))))

(defun paw-delete-annotation (&optional entry)
  (interactive)
  (paw-delete-word (or entry (get-char-property (point) 'paw-entry))))

(defun paw-clear-annotation-overlay (&optional overlays)
  "Clear overlays or all paw-entry overlays in the buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((ovs (if overlays overlays (paw-get-all-overlays))))
      (dolist (ov ovs)
        (delete-overlay ov)))))

(defun paw-get-all-overlays()
  (-filter
   (lambda (o)
     (overlay-get o 'paw-entry))
   (overlays-in (point-min) (point-max))))

(defvar paw-get-note-during-paw-view-notes nil)

(defvar paw-get-note-limit-during-paw-view-notes 200
  "The limit of getting note during paw-view-notes for unknown
words. If more than it, it will not get note. If the buffer has
too many unknown words, the process of getting note for each word
will be very slow, make a resonable limit is a workaround.

Only when the note is empty, it will get note for unknown words.
That means if the note is not empty (already set by last time),
it will not get note for unknown words.

100 words ~ 1 second on my laptop.

For example, if a buffer has 200 words. The first time we run
`paw-view-notes', the notes of first 100 words will be updated.
The second time we run `paw-view-notes', the notes of last 100
words will be updated.")

(defun paw-get-all-entries-from-overlays()
  (let ((overlays (overlays-in (point-min) (point-max)))
        (counter 0))
    (cl-remove-duplicates
     (mapcar
      (lambda (o)
        (let ((entry (overlay-get o 'paw-entry)))
          (if (and entry paw-get-note-during-paw-view-notes)
              ;; only unknown words
              (when (eq (alist-get 'serverp entry) 3)
                ;; if note is not empty, that means it already had note
                (when (string= (alist-get 'note entry) "")
                  (setq counter (1+ counter))
                  (when (<= counter paw-get-note-limit-during-paw-view-notes)
                      ;; (message "%s \n" counter)
                      (setf (alist-get 'note entry)
                            (save-excursion
                              (goto-char (overlay-start o))
                              (paw-get-note)))
                      (setf (alist-get 'origin_point entry) (overlay-end o))
                      ))))
          entry))
      overlays))))

(defun paw-list-default-action (x)
    (let* ((entry (get-text-property 0 'paw-entry x))
           (word (paw-get-real-word entry))
           (origin-point (alist-get 'origin_point entry))
           (note-type (alist-get 'note_type entry))
           (old-origin-path (alist-get 'origin_path entry))
           (new-origin-path (paw-get-origin-path)))
      (if entry
          (pcase (car note-type)
            ('attachment (paw-find-note entry))
            ('image (paw-find-note entry))
            ;; if same path, we just need to call `paw-goto-location'
            (_ (if (equal old-origin-path new-origin-path)
                   (paw-goto-location origin-point word)
                 (paw-find-origin entry))))
        (paw-add-attachment x))))

(defun paw-list-delete-action (x)
  (let* ((entry (get-text-property 0 'paw-entry x)))
    (if entry
        (paw-delete-annotation entry))))

;;;###autoload
(defun paw-list-annotations ()
  (interactive)
  (consult--read (paw-candidates-format :sort t :print-full-content t)
                 :prompt "Annotations: "
                 :sort nil
                 :history nil
                 :lookup (lambda(cand candidates input-string _)
                           (let* ((entry (get-text-property 0 'paw-entry (cl-find-if
                                                                          (lambda (input)
                                                                            (string= input cand)) candidates) ))
                                  (word (paw-get-real-word entry))
                                  (origin-point (alist-get 'origin_point entry))
                                  (note-type (alist-get 'note_type entry))
                                  (old-origin-path (alist-get 'origin_path entry))
                                  (new-origin-path (paw-get-origin-path))
                                  (note-type (car (alist-get 'note_type entry))))
                             (paw-find-origin entry)))))

;;;###autoload
(defun paw-list-all-annotations ()
  (interactive)
  (consult--read (nreverse (paw-candidates-format :all t))
                 :prompt "All Annotations: "
                 :sort nil
                 :history nil
                 :lookup (lambda(cand candidates input-string _)
                           (paw-list-default-action
                            (cl-find-if
                             (lambda (input)
                               (string= input cand)) candidates)))))


(defvar paw-annotation-links-source
  (list
   :name "Paw Annotation Links"
   :narrow   ?l
   :items    (lambda()
               (paw-candidates-format :only-links t) )
   :action   (lambda(cand)
               (paw-list-default-action cand))))


(defvar paw-annotation-source
  (list
   :name "Paw Annotations"
   :narrow   ?p
   :items    (lambda()
               (paw-candidates-format :all t) )
   :action   (lambda(cand)
               (paw-list-default-action cand))))

(defvar paw-list-add-links-history nil)

;;;###autoload
(defun paw-list-all-links ()
  "List all eaf/eww links."
  (interactive)
  (consult--read (paw-candidates-format :only-links t)
                 :prompt "URL: "
                 :sort nil
                 :history 'paw-list-add-links-history
                 :lookup (lambda(cand candidates input-string _)
                           (paw-list-default-action
                            (cl-find-if
                             (lambda (input)
                               (string= input cand)) candidates)))))


(defun paw-change-annotation-note-type ()
  "Change the annotation note type"
  (interactive)
  (if (bound-and-true-p focus-mode)
      (paw-focus-find-current-thing-segment)
    (paw-add-annotation-overlay (get-char-property (point) 'paw-entry) t)
    ;; update buffer
    (if (buffer-live-p (get-buffer "*paw*"))
        (paw t)) ))

(defvar paw-online-word-learn-level-alist
  '(("1 New" . 1)
    ("2 Recognized" . 4)
    ("3 Familiar" . 5)
    ("4 Learned" . 6)
    ("5 Known" . 7))
  "mapping to database serverp value")


(defvar paw-offline-word-learn-level-alist
  '(("1 New" . 8)
    ("2 Recognized" . 9)
    ("3 Familiar" . 10)
    ("4 Learned" . 11)
    ("5 Known" . 12))
  "mapping to database serverp value")


(defcustom paw-view-note-after-change-online-word-learning-level nil
  "Whether to view note after changing learning level of online word."
  :group 'paw
  :type 'boolean)


(defun paw-change-word-learning-level()
  (interactive)
  (let* ((word (paw-note-word))
         (entry (car (paw-candidate-by-word word)))
         (serverp (alist-get 'serverp entry)))
    (cond ((paw-online-p serverp)
           (call-interactively 'paw-change-online-word-learning-level))
          ((paw-offline-p serverp)
           (call-interactively 'paw-change-offline-word-learning-level))
          (t (message "This is not a word"))) ))

(defun paw-change-online-word-learning-level ()
  "Change word learning level for online words."
  (interactive)
  (let* ((word (paw-note-word))
         (entry (car (paw-candidate-by-word word)))
         (serverp (alist-get 'serverp entry)))
    (unless (paw-online-p serverp)
      (error "This is an offline word."))
    (let ((level (cdr (assoc (completing-read (format "Select a level for '%s': " word) paw-online-word-learn-level-alist) paw-online-word-learn-level-alist))))
      (paw-db-update-serverp word level))

    ;; TODO: update the overlays, same as `paw-add-online-word-callback'
    ;; query back the candidate from database
    (setq entry (car (paw-candidate-by-word word) ))
    (paw-search-refresh)
    ;; in all buffers with paw-annotation-mode, clear
    ;; all overlays of this word, if any, if we update
    ;; the word, we should delete the old overlay
    ;; first, finally add this entry's overlays
    (-map (lambda (b)
            (with-current-buffer b
              (when (eq paw-annotation-mode t)
                (let ((overlays (-filter
                                 (lambda (o)
                                   (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                                 (overlays-in (point-min) (point-max)))))
                  (if overlays
                      (paw-clear-annotation-overlay overlays)))
                (paw-show-all-annotations (list entry)))))
          (buffer-list))

    ;; show the word again
    (if paw-view-note-after-change-online-word-learning-level
        (paw-view-note-refresh))

    ))



(defun paw-change-offline-word-learning-level ()
  "Change word learning level for offline words."
  (interactive)
  (let* ((word (paw-note-word))
         (entry (car (paw-candidate-by-word word)))
         (serverp (alist-get 'serverp entry)))
    (unless (paw-offline-p serverp)
      (error "This is an online word."))
    (let ((level (cdr (assoc (completing-read (format "Select a level for '%s': " word) paw-offline-word-learn-level-alist) paw-offline-word-learn-level-alist))))
      (paw-db-update-serverp word level))

    ;; TODO: update the overlays, same as `paw-add-online-word-callback'
    ;; query back the candidate from database
    (setq entry (car (paw-candidate-by-word word) ))
    (paw-search-refresh)
    ;; in all buffers with paw-annotation-mode, clear
    ;; all overlays of this word, if any, if we update
    ;; the word, we should delete the old overlay
    ;; first, finally add this entry's overlays
    (-map (lambda (b)
            (with-current-buffer b
              (when (eq paw-annotation-mode t)
                (let ((overlays (-filter
                                 (lambda (o)
                                   (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                                 (overlays-in (point-min) (point-max)))))
                  (if overlays
                      (paw-clear-annotation-overlay overlays)))
                (paw-show-all-annotations (list entry)))))
          (buffer-list))

    ;; show the word again
    (if paw-view-note-after-change-online-word-learning-level
        (paw-view-note-refresh))

    ))


(defun paw-candidates-by-mode (&optional sort current-buffer)
  "Match major modes and return the list of candidates.
If WHOLE-FILE is t, always index the whole file."
  (if current-buffer
      (let* ((candidates (paw-get-all-entries-from-overlays))
             (len (length candidates)))
        (cons candidates len))
  (pcase major-mode
    ('nov-mode
     (let* ((candidates (if sort
                            (-sort (lambda (ex ey)
                                     (let ((x (alist-get 'created_at ex))
                                           (y (alist-get 'created_at ey)))
                                       (time-less-p (date-to-time x) (date-to-time y))))
                                   (paw-candidates-by-origin-path))
                            (paw-candidates-by-origin-path) ))
            (len (length candidates)))
       ;; TODO Disable of filter candidates by current index, since epub page is small, showing whole file's annotations is better.
       ;; (cons (if whole-file
       ;;           candidates
       ;;         ;; filter current index candidates
       ;;         (-filter (lambda (x)
       ;;                    (let ((origin-point (alist-get 'origin_point x)))
       ;;                      (cond ((listp origin-point)
       ;;                             (eq nov-documents-index (car origin-point)))
       ;;                            (t nil))))
       ;;                  candidates) ) len)
       (cons candidates len)))
    ('wallabag-mode
     (let* ((candidates (if sort
                            (-sort (lambda (ex ey)
                                     (let ((x (alist-get 'created_at ex))
                                           (y (alist-get 'created_at ey)))
                                       (if (and x y)
                                           (time-less-p (date-to-time x) (date-to-time y))
                                         t)))
                                   (paw-candidates-by-origin-path))
                          (paw-candidates-by-origin-path) ))
            (len (length candidates)))
       (cons candidates len)))
    ('eww-mode
     (let* ((candidates (if sort
                            (-sort (lambda (ex ey)
                                     (let ((x (alist-get 'created_at ex))
                                           (y (alist-get 'created_at ey)))
                                       (if (and x y)
                                           (time-less-p (date-to-time x) (date-to-time y))
                                         t)))
                                   (paw-candidates-by-origin-path))
                          (paw-candidates-by-origin-path) ))
            (len (length candidates)))
       (cons candidates len)))
    ('pdf-view-mode
     (let* ((candidates (if sort
                            (-sort (lambda (ex ey)
                                     (let ((x (alist-get 'created_at ex))
                                           (y (alist-get 'created_at ey)))
                                       (if (and x y)
                                           (time-less-p (date-to-time x) (date-to-time y))
                                         t)))
                                   (paw-candidates-by-origin-path))
                          (paw-candidates-by-origin-path) ))
            (len (length candidates)))
       (cons candidates len)))
    (_
     (let* ((candidates (if sort
                            (-sort (lambda (ex ey)
                                     (let ((x (alist-get 'created_at ex))
                                           (y (alist-get 'created_at ey)))
                                       (if (and x y)
                                           (time-less-p (date-to-time x) (date-to-time y))
                                         t)))
                                   (paw-candidates-by-origin-path))
                          (paw-candidates-by-origin-path) ))
            (len (length candidates)))
       (cons candidates len))))

  )
  )

(defun paw-candidates-format (&rest properties)
  "Match major modes and return the list of formated candidates."
  (let ((all (plist-get properties :all))
        (sort (plist-get properties :sort))
        (current-buffer (plist-get properties :current-buffer))
        (only-links (plist-get properties :only-links))
        (print-full-content (plist-get properties :print-full-content))
        (only-words (plist-get properties :only-words)))
    (-map
     (lambda (entry)
       (paw-parse-entry-as-string entry print-full-content))
     (cond (all
            ;; if all is t, return all candidates which is serverp equals 2
            ;; (-filter (lambda (entry)
            ;;            (eq (alist-get 'serverp entry) 2)) (paw-all-candidates))
            (paw-all-candidates)
            )
           (only-links (paw-candidates-only-links))
           (only-words (paw-candidates-only-words))
           ((derived-mode-p 'eaf-mode)
            (car (paw-candidates-by-mode t)))
           (t (car (paw-candidates-by-mode sort current-buffer))))) ))

(defvar paw-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") 'paw-list-annotations)
    (define-key map (kbd "C-c C-,") 'paw-add-attachment)
    (define-key map (kbd "C-c C-n") 'paw-next-annotation)
    (define-key map (kbd "C-c C-p") 'paw-previous-annotation)
    (define-key map (kbd "C-c s") 'paw-view-note-in-minibuffer)
    (define-key map (kbd "C-c v") 'paw-view-note)
    (define-key map (kbd "C-c t") 'paw-view-note-translate)
    (define-key map (kbd "C-c h") 'paw-add-highlight)
    (define-key map (kbd "C-c a") 'paw-add-online-word)
    (define-key map (kbd "C-c A") 'paw-add-offline-word)
    (define-key map (kbd "C-c u") 'paw-scroll-down)
    (define-key map (kbd "C-c d") 'paw-scroll-up)
    (define-key map (kbd "C-c c") 'paw-view-note-current-thing)
    (define-key map (kbd "C-c n") 'paw-view-note-next-thing)
    (define-key map (kbd "C-c p") 'paw-view-note-prev-thing)
    (define-key map (kbd "C-c f") 'focus-mode)
    (define-key map (kbd "C-c r") 'paw-view-note-play)
    (define-key map (kbd "C-c q") 'paw-view-note-quit)
    (define-key map [mouse-1] 'paw-view-note-click)
    (define-key map [mouse-2] 'paw-view-note-click) ;; this can replace shr-map and nov-mode-map browse-url
    map)
  "Keymap for function `paw-annotation-mode'.")


(if (bound-and-true-p evil-mode)
    (evil-define-key* '(normal visual insert) paw-annotation-mode-map
      (kbd "s") 'paw-view-note-in-minibuffer
      (kbd "t") 'paw-view-note-translate
      ;; (kbd "i") 'paw-add-highlight
      (kbd "a") 'paw-add-online-word
      (kbd "m") 'paw-add-highlight
      (kbd "u") 'paw-scroll-down
      (kbd "d") 'paw-scroll-up
      (kbd "c") 'paw-view-note-current-thing
      (kbd "n") 'paw-next-annotation
      (kbd "N") 'paw-previous-annotation
      (kbd "p") 'paw-previous-annotation
      (kbd "f") 'focus-mode
      (kbd "r") 'paw-view-note-play
      [mouse-1] 'paw-view-note-click
      [mouse-2] 'paw-view-note-click
      ;; (kbd "q") 'paw-view-note-quit
      ) )

(defcustom paw-view-note-translate-function 'paw-immersive-translate
  "paw view note translate function"
  :group 'paw
  :type '(choice (function-item paw-nov-translate)
          (function-item paw-immersive-translate)
          function))

(define-obsolete-variable-alias 'paw-view-note-transalate-function
  'paw-view-note-translate-function "paw 1.1.1")


(defun paw-view-note-translate ()
  (interactive)
  (funcall paw-view-note-translate-function))


(defcustom paw-view-note-click-function 'paw-click-to-view-note
  "paw view note click function"
  :group 'paw
  :type '(choice (function-item paw-click-to-view-note)
          function))

(defun paw-view-note-click (event)
  (interactive "e")
  (funcall-interactively paw-view-note-click-function event))


(defun paw-click-to-view-note (event)
  "Click to view note Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No URL chosen"))
    (with-current-buffer (window-buffer window)
      ;; Avoid click but turn into drag issue for evil
      (if (fboundp 'evil-normal-state)
          (evil-normal-state))
      (goto-char pos)
      (if (paw-shr-link-at-point-p)
          (if (eq major-mode 'nov-mode)
              (nov-browse-url)
            (shr-browse-url))
        (paw-view-note)))))

(defun paw-shr-link-at-point-p ()
  "Return non-nil if point is on a link rendered by shr."
  (let ((properties (text-properties-at (point))))
    (plist-get properties 'shr-url)))



(defcustom paw-view-note-current-thing-function 'paw-focus-find-current-thing-segment
  "paw view note current thing function"
  :group 'paw
  :type '(choice (function-item paw-focus-find-current-thing-segment)
          function))

;;;###autoload
(defun paw-view-note-current-thing (&optional thing)
  "View current thing defined by `focus-mode-to-thing'.
Analyse the thing and display the note inside a
`paw-view-note-mode' buffer (for example *paw-view-note*).

If the thing is English, analyse and segment with ecdict; if the
thing is Japanese, analyse and segment with kagome, default
function is `paw-focus-find-current-thing-segment.'

You can turn on `focus-mode' for better reading and easy thing
selection experience (the selected thing is just the focused
thing). However, if `focus-mode' is nil, it will still work."
  (interactive)
  (funcall paw-view-note-current-thing-function thing))



(defcustom paw-view-note-next-thing-function 'paw-focus-find-next-thing-segment
  "paw view note next thing function"
  :group 'paw
  :type '(choice (function-item paw-focus-find-next-thing-segment)
          function))

;;;###autoload
(defun paw-view-note-next-thing ()
  "View next thing defined by `focus-mode-to-thing'.
Analyse the thing and display the note inside a
`paw-view-note-mode' buffer (for example *paw-view-note*).

If run inside a note buffer (for example *paw-view-note*), it
will go to the `paw-note-target-buffer' that links to the note
buffer or window and move to next thing, finally view the thing."
  (interactive)
  (if (or (process-live-p paw-sdcv-running-process)
          paw-go-translate-running-p
          (process-live-p paw-ecdict-running-process)
          (process-live-p paw-kagome-running-process))
      (error "Please wait for the translation process to finish."))
  (if (buffer-live-p paw-note-target-buffer)
      (let ((window (get-buffer-window paw-note-target-buffer)))
        (if (window-live-p window)
            (progn
              (with-selected-window (select-window window)
                (call-interactively 'paw-focus-next-thing))
              (call-interactively  'paw-focus-find-current-thing-segment) )
          (with-current-buffer paw-note-target-buffer
            (funcall paw-view-note-next-thing-function))))
    (with-selected-window (select-window (get-buffer-window (current-buffer)))
      (funcall paw-view-note-next-thing-function))))


(defcustom paw-view-note-prev-thing-function 'paw-focus-find-prev-thing-segment
  "paw view note previous thing function"
  :group 'paw
  :type '(choice (function-item paw-focus-find-prev-thing-segment)
          function))

;;;###autoload
(defun paw-view-note-prev-thing ()
  "View previous thing defined by `focus-mode-to-thing'.
Analyse the thing and display the note inside a
`paw-view-note-mode' buffer (for example *paw-view-note*).

If run inside a note buffer (for example *paw-view-note*), it
will go to the `paw-note-target-buffer' that links to the note
buffer or window and move to previous thing, finally view the
thing."
  (interactive)
  (if (or (process-live-p paw-sdcv-running-process)
          paw-go-translate-running-p
          (process-live-p paw-ecdict-running-process)
          (process-live-p paw-kagome-running-process))
      (error "Please wait for the translation process to finish."))
  ;; inside *paw-view-note*
  (if (buffer-live-p paw-note-target-buffer)
      (let ((window (get-buffer-window paw-note-target-buffer)))
        (if (window-live-p window)
            (progn
              (with-selected-window (select-window window)
                (call-interactively 'paw-focus-prev-thing))
              (call-interactively  'paw-focus-find-current-thing-segment) )
          (with-current-buffer paw-note-target-buffer
            (funcall paw-view-note-prev-thing-function))))
    ;; not inside *paw-view-note*
    (with-selected-window (select-window (get-buffer-window (current-buffer)))
      (funcall paw-view-note-prev-thing-function))))


(defcustom paw-annotation-read-only-enable nil
  "Enable read-only-mode when paw-annotation-mode is enabled."
  :group 'paw
  :type 'boolean)


(defcustom paw-annotation-show-wordlists-words-p nil
  "Set t to show wordlists words in paw-annotation-mode."
  :group 'paw
  :type 'boolean)

(defcustom paw-annotation-show-unknown-words-p nil
  "Set t to show unknown words in paw-annotation-mode."
  :group 'paw
  :type 'boolean)

(defvar-local paw-annotation-read-only nil
  "Buffer local variable to store the original read-only state of the buffer.")

;;;###autoload
(define-minor-mode paw-annotation-mode
  "Toggle `paw-annotation-mode'.
When t,
1. show all annatiations on current buffer.
2. Turn on `read-only-mode' if `paw-annotation-read-only-enable'
is t."
  :group 'paw
  (unless (memq major-mode paw-annotation-mode-supported-modes)
    (setq-local minor-mode-map-alist
                (assq-delete-all 'paw-annotation-mode minor-mode-map-alist))
    (error "Please add %s to `paw-annotation-mode-supported-modes' for enabling `paw-annotation-mode.'" major-mode))
  (let ((mode-line-segment '(:eval (paw-annotation-mode-line-text))))
    (cond
     (paw-annotation-mode
      (setq-local minor-mode-map-alist
                  (cons (cons 'paw-annotation-mode paw-annotation-mode-map)
                        minor-mode-map-alist))
      (pcase major-mode
        ('eaf-mode
         (eaf-call-async "execute_function_with_args" eaf--buffer-id "paw_annotation_mode" `,paw-db-file))
        ('pdf-view-mode
         ;; then update and show the mode line
         (paw-annotation-get-mode-line-text)
         (if (symbolp (car-safe mode-line-format))
             (setq mode-line-format (list mode-line-segment mode-line-format))
           (push mode-line-segment mode-line-format)))
        (_
         ;; show all annotations first
         (paw-show-all-annotations)

         ;; show all words from wordlists
         (if paw-annotation-show-wordlists-words-p
             (paw-focus-find-words :wordlist t) )

         ;; show all unknown words
         (if paw-annotation-show-unknown-words-p
             (paw-focus-find-words))

         ;; then update and show the mode line
         (paw-annotation-get-mode-line-text)
         (if (symbolp (car-safe mode-line-format))
             (setq mode-line-format (list mode-line-segment mode-line-format))
           (push mode-line-segment mode-line-format))
         (unless (eq major-mode 'nov-mode)
           (when paw-annotation-read-only-enable
             ;; Save the original read-only state of the buffer
             (setq paw-annotation-read-only buffer-read-only)
             (read-only-mode 1)))))
      (run-hooks 'paw-annotation-mode-hook))
     (t
      (setq-local minor-mode-map-alist
                  (assq-delete-all 'paw-annotation-mode minor-mode-map-alist))
      (setq mode-line-format (delete mode-line-segment mode-line-format))
      (if (eq major-mode 'eaf-mode)
          (eaf-call-async "eval_function" eaf--buffer-id "paw_annotation_mode_disable" (key-description (this-command-keys-vector)))
        (unless (or (eq major-mode 'nov-mode)
                    (eq major-mode 'pdf-view-mode))
          (when paw-annotation-read-only-enable
            ;; Restore the original read-only state of the buffer
            (setq buffer-read-only paw-annotation-read-only)) )
        (if paw-click-overlay
            (delete-overlay paw-click-overlay))
        (paw-clear-annotation-overlay) )))))

(defvar paw-annotation--menu-contents
  '("Paw Annotation"
    ["Paw Dashboard" paw
     :help "Open Paw Dashboard"]
    ["Toggle paw annotation mode" paw-annotation-mode
     :help "Toggle paw annotation mode"]
    ["Go to Paw Dashboard" paw-goto-dashboard
     :help "Go to Paw Dashboard"
     :enable paw-annotation-mode]
    ["List annotations" paw-list-annotations
     :help "List annotations in the buffer"
     :enable paw-annotation-mode]
    ["List all annotations" paw-list-all-annotations
     :help "List all annotations in the buffer"]
    ["View Definition" paw-view-note-in-minibuffer
     :help "View the annotation"]
    ["View note" paw-view-note
     :help "View the annotation"]
    ["View all notes in the buffer" paw-view-notes
     :help "View all annotations in the buffer"
     :enable paw-annotation-mode]
    ["Edit annotation" paw-find-note
     :help "View the annotation"
     :enable paw-annotation-mode]
    ["Change annotation type" paw-change-annotation-note-type
     :help "Change the annotation type"
     :enable paw-annotation-mode]
    ["Change note type" paw-change-note_type
     :help "Change the note type"
     :enable paw-annotation-mode]
    ["Copy annotation" paw-copy-annotation
     :help "Copy the annotation"
     :enable paw-annotation-mode]
    ["Next annotation" paw-next-annotation
     :help "Go to the next annotation"
     :enable paw-annotation-mode]
    ["Previous annotation" paw-previous-annotation
     :help "Go to the previous annotation"
     :enable paw-annotation-mode]
    ["Follow link" paw-follow-link
     :help "Follow the link"
     :enable paw-annotation-mode]
    ["Replay audio" paw-view-note-play
     :help "Replay the audio"
     :enable paw-annotation-mode]
    "---"
    ["Add a word (online)" paw-add-online-word
     :help "Add a word to Eudic"]
    ["Add a word (offline)" paw-add-offline-word
     :help "Add a word locally"]
    ["Add a word" paw-add-word
     :help "Add a word annotation locally"]
    ["Add a highlight" paw-add-highlight
     :help "Add a highlight annotation"]
    ["Add a link" paw-add-link
     :help "Add a link annotation"]
    ["Add a question" paw-add-question
     :help "Add a question annotation"]
    ["Add a todo" paw-add-todo
     :help "Add a todo annotation"]
    ["Add a done" paw-add-done
     :help "Add a done annotation"]
    ["Add a cancel" paw-add-cancel
     :help "Add a cancel annotation"]
    ["Add a bookmark" paw-add-bookmark
     :help "Add a bookmark annotation"]
    ["Delete annotation" paw-delete-annotation
     :help "Delete the annotation"
     :enable paw-annotation-mode])
  "Contents of the Paw Annotation menu.")


(defun paw-annotation--menu-bar-enable ()
  "Enable paw-annotation menu bar."
  (define-key-after global-map [menu-bar paw-annotation]
    (easy-menu-binding
     (easy-menu-create-menu "Paw Annotation" paw-annotation--menu-contents) "Paw Annotation")
    "Tools"))


;; Enable Paw Annotation menu bar by default
(paw-annotation--menu-bar-enable)

;;;###autoload
(define-minor-mode paw-annotation-menu-bar-mode "Show Paw Annotation menu bar."
  :global t
  :init-value t
  (if paw-annotation-menu-bar-mode
      (paw-annotation--menu-bar-enable)
    (define-key global-map [menu-bar paw-annotation] nil)))


(defun paw-annotation-context-menu (menu _click)
  "Populate MENU with Paw Annotation commands at CLICK."
  (define-key menu [paw-annotation-separator] menu-bar-separator)
  (let ((easy-menu (make-sparse-keymap "Paw Annotation")))
    (easy-menu-define nil easy-menu nil
      paw-annotation--menu-contents)
    (dolist (item (reverse (lookup-key easy-menu [menu-bar])))
      (when (consp item)
        (define-key menu (vector (car item)) (cdr item)))))
  menu)

(defvar paw-annotation-mode-line-text "0 notes")

(defun paw-annotation-mode-line-text ()
  (if (and paw-db-update-p paw-annotation-mode)
      (progn
        (setq-local paw-db-update-p nil)
        (paw-annotation-get-mode-line-text))
    paw-annotation-mode-line-text))

(defun paw-annotation-get-mode-line-text ()
  (let* ((number-of-notes (or (paw-candidates-by-origin-path-length) 0)))
    (setq-local paw-annotation-mode-line-text
                (cond ((= number-of-notes 0) (propertize (format " 0 notes " number-of-notes) 'face 'paw-no-notes-exist-face))
                      ((= number-of-notes 1) (propertize (format " 1 note " number-of-notes) 'face 'paw-notes-exist-face))
                      (t (propertize (format " %d notes " number-of-notes) 'face 'paw-notes-exist-face))) )))


(defun paw-add-overlay (beg end note-type note entry)
  "Add overlay between BEG and END based on NOTE-TYPE.
Add NOTE and ENTRY as overlay properties."
  (pcase (car note-type)
    ('word
     (let ((ov (make-overlay beg end))
           (ov2 (make-overlay beg end)))
       ;; (overlay-put ov 'before-string
       ;;              (let ((serverp (alist-get 'serverp entry)))
       ;;                (if (paw-online-p serverp)
       ;;                    (propertize (cdr note-type) 'display paw-star-face-icon)
       ;;                  (propertize (cdr note-type) 'display paw-word-icon))))
       (pcase (alist-get 'serverp entry)
         (1 (overlay-put ov 'face 'paw-level-1-word-face))
         (3 (overlay-put ov 'face 'paw-unknown-word-face)
            ;; FIXME This is a hack to show the space after the word, disabled because the overlays will be clear by immersive-translate
            ;; (when (string= (alist-get 'lang entry) "ja")
            ;;   ;; seperate the word with space
            ;;   (overlay-put ov 'before-string (propertize paw-annotation-after-string-space 'mouse-face nil)))
            )
         (4 (overlay-put ov 'face 'paw-level-2-word-face))
         (5 (overlay-put ov 'face 'paw-level-3-word-face))
         (6 (overlay-put ov 'face 'paw-level-4-word-face))
         (7 nil)
         (8 (overlay-put ov 'face 'paw-level-1-offline-word-face))
         (9 (overlay-put ov 'face 'paw-level-2-offline-word-face))
         (10 (overlay-put ov 'face 'paw-level-3-offline-word-face))
         (11 (overlay-put ov 'face 'paw-level-4-offline-word-face))
         (12 nil)
         (_ (overlay-put ov 'face 'paw-word-face)))
       ;; show exp or note
       (overlay-put ov 'help-echo (if (alist-get 'exp entry)
                                      (alist-get 'exp entry)
                                    note))
       (overlay-put ov 'keymap paw-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'paw-entry entry)
       (pcase (alist-get 'serverp entry)
         (3 (if (string= (alist-get 'lang entry) "ja")
                (overlay-put ov2 'mouse-face 'paw-unknown-word-hover-face)
              (overlay-put ov 'mouse-face 'paw-unknown-word-hover-face)))
         (_ (overlay-put ov 'mouse-face 'paw-word-hover-face) ) )))
    ('question
     (let ((ov (make-overlay beg end)))
       (overlay-put ov 'before-string (propertize (cdr note-type) 'display paw-question-icon))
       (overlay-put ov 'face '(:underline t))
       (overlay-put ov 'help-echo note)
       (overlay-put ov 'keymap paw-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'paw-entry entry)
       (overlay-put ov 'mouse-face 'paw-mouse-face)))
    ('todo
     (let ((ov (make-overlay beg end)))
       (overlay-put ov 'before-string (propertize (cdr note-type) 'display paw-todo-icon))
       (overlay-put ov 'face 'paw-todo-face)
       (overlay-put ov 'help-echo note)
       (overlay-put ov 'keymap paw-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'paw-entry entry)
       (overlay-put ov 'mouse-face 'paw-mouse-face)))
    ('done
     (let ((ov (make-overlay beg end)))
       (overlay-put ov 'before-string (propertize (cdr note-type) 'display paw-done-icon))
       (overlay-put ov 'face 'paw-done-face)
       (overlay-put ov 'help-echo note)
       (overlay-put ov 'keymap paw-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'paw-entry entry)
       (overlay-put ov 'mouse-face 'paw-mouse-face)))
    ('cancel
     (let ((ov (make-overlay beg end)))
       (overlay-put ov 'before-string (propertize (cdr note-type) 'display paw-cancel-icon))
       (overlay-put ov 'face 'paw-cancel-face)
       (overlay-put ov 'help-echo note)
       (overlay-put ov 'keymap paw-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'paw-entry entry)
       (overlay-put ov 'mouse-face 'paw-mouse-face)))
    ('link
     (let ((ov (make-overlay beg end)))
       (let* ((json (condition-case nil
                        (let ((output (json-read-from-string (alist-get 'content entry))))
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
            (overlay-put ov 'before-string (propertize (cdr note-type) 'display paw-file-link-icon)))
           ("url"
            (overlay-put ov 'before-string (propertize (cdr note-type) 'display paw-url-link-icon)))
           ("annotation"
            (overlay-put ov 'before-string (propertize (cdr note-type) 'display paw-annotation-link-icon)))
           ("org"
            (overlay-put ov 'before-string (propertize (cdr note-type) 'display (create-image paw-org-link-file nil nil :width nil :height nil :ascent 'center))))))
       (overlay-put ov 'face 'paw-link-face)
       (overlay-put ov 'help-echo (alist-get 'content entry))
       (overlay-put ov 'keymap paw-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'paw-entry entry)
       (overlay-put ov 'mouse-face 'paw-mouse-face)))
    ('stamp
     (let ((ov (make-overlay beg end)))
       (overlay-put ov 'before-string (propertize (cdr note-type) 'display (cdr note-type)))
       (overlay-put ov 'face '(:underline t))
       (overlay-put ov 'help-echo note)
       (overlay-put ov 'keymap paw-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'paw-entry entry)
       (overlay-put ov 'mouse-face 'paw-mouse-face)))
    (_
     (let ((ov (make-overlay beg end)))
       (overlay-put ov 'face (cdr note-type))
       (overlay-put ov 'help-echo note)
       (overlay-put ov 'keymap paw-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'paw-entry entry)
       (overlay-put ov 'mouse-face 'paw-mouse-face)))))

(provide 'paw-annotation)
