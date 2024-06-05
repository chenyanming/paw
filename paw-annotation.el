;;; pen/pen-annotation.el -*- lexical-binding: t; -*-

(require 'pen-db)
(require 'pen-util)
(require 'org)
(require 'evil-core nil t)
(require 's)
(require 'pen-svg)
(require 'pen-gptel)
(require 'pen-request)
(require 'pen-focus)

(defconst pen-note-type-alist
  '((word . "✎")
    (highlight-1 . pen-highlight-1-face)
    (highlight-2 . pen-highlight-2-face)
    (highlight-3 . pen-highlight-3-face)
    (highlight-4 . pen-highlight-4-face)
    (attachment . "📝")
    (question . "❓")
    (image . "📷")
    (bookmark . "🔖")
    (todo . "☐")
    (done . "☑")
    (cancel . "☒")
    (link . "🔗")
    (sdcv . "✎"))
  "Const annotation types and their characters or faces.")

(defcustom pen-annotation-mode-supported-modes
  '(nov-mode org-mode pen-view-note-mode wallabag-entry-mode)
  "Supported modes for pen-annotation-mode."
  :group 'pen
  :type 'list)

(defcustom pen-annotation-search-paths '()
  "Alternative pathes for pen-annotation-mode. The books pathes
 that are possibly used for pen-annotation-mode."
  :group 'pen
  :type 'list)


(defvar pen-annotation-current-highlight-type (assoc 'highlight-1 pen-note-type-alist))

(defcustom pen-annotation-stamps
  '("❗"
    "❤"
    "😊")
  "Stamps could be a list of display properties, it can make use of svg-lib rich icons library, and no limit how many items."
  :group 'pen
  :type 'list)

(defvar pen-annotation-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "," 'pen-list-annotations)
    (define-key map (kbd "<RET>") 'pen-goto-dashboard)
    (define-key map "D" 'pen-delete-annotation) ;; d is used for scroll
    (define-key map "n" 'pen-next-annotation)
    (define-key map "N" 'pen-previous-annotation)
    (define-key map "p" 'pen-previous-annotation) ;; may impact edit mode
    (define-key map "y" 'pen-copy-annotation)
    (define-key map "r" 'pen-replay)
    (define-key map "i" 'pen-find-note)
    (define-key map "&" 'pen-find-note)
    (define-key map "I" 'pen-find-notes)
    (define-key map "v" 'pen-view-note)
    (define-key map "V" 'pen-view-notes)
    (define-key map "c" 'pen-change-annotation-note-type)
    (define-key map "C" 'pen-change-note_type)
    (define-key map "f" 'pen-follow-link)
    (define-key map (kbd "<mouse-8>") 'pen-mouse-8)
    (define-key map (kbd "<mouse-9>") 'pen-mouse-9)
    (define-key map (kbd "<mouse-1>") 'pen-mouse-2) ; have to enable mouse 2 will also binds mouse 1 (no idea)
    (define-key map (kbd "<mouse-2>") 'pen-mouse-2) ; have to enable mouse 2 will also binds mouse 1 (no idea)
    map)
  "Keymap for annotation overlay.")

(defun pen-mouse-8 (event)
  "Browser the url click on with eww.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No word chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (pen-view-note))))


(defun pen-mouse-9 (event)
  "Browser the url click on with eww.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No word chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (pen-view-notes))))

(defun pen-mouse-2 (event)
  "Browser the url click on with eww.
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No word chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (pen-view-note))))

(defun pen-add-general (word type location &optional gptel note path)
  (let* ((word (pcase (car type)
                 ('bookmark
                  (pcase major-mode
                    ('eaf-mode (pcase eaf--buffer-app-name
                                 ("browser"  eaf--bookmark-title)
                                 ("pdf-viewer" (file-name-nondirectory eaf--buffer-url ))
                                 (_ nil)))
                    (_ (format "Bookmark %s" (pp location)))))
                 (_ word)))
         (id (concat word ":id:" (org-id-uuid))))
    (pen-db-insert
     `(((word . ,id) (exp . nil)))
     :content (pcase (car type)
                ('image (let ((image (condition-case nil
                                         (or (plist-get (cdr (image--get-image)) :file)
                                             (plist-get (cdr (image--get-image)) :data))
                                       (error nil))))
                          (if image
                              (pen-image-content-json image) ; if image under point, no need to prompt, just add it as attachment
                            (let* ((png (format "%s.png" (org-id-uuid)))
                                   (path (expand-file-name png pen-note-dir))
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
                                   (pen-image-content-json image) ; if image under point, no need to prompt, just add it as attachment
                                 (let* ((src (read-file-name "Select the attachment: "))
                                        (attachment (format "%s.%s" (org-id-uuid) (file-name-extension (file-name-nondirectory src))))
                                        (dst (expand-file-name attachment pen-note-dir))
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
                                  (let* ((entry (get-text-property 0 'pen-entry
                                                                   (ivy-read "Please insert an annotation: " (pen-candidates-format t)
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
                   (_ (format "Bookmark %s" (pp location)))))
                (_ word))
     :serverp 2
     :note (or note
               (pcase (car type)
                 ('attachment "")           ; attachment empty note, mark also empty
                 ('image "")
                 ('bookmark "")
                 ('word (pen-get-note))
                 (_ (if mark-active
                        "" ; mark is empty, since the note not that useful, if need sentence note, use `pen-add-word'
                      (pen-get-note)))) )
     :note_type type
     :origin_type (if (derived-mode-p 'eaf-mode)
                      eaf--buffer-app-name
                    major-mode)
     :origin_path (or path (pen-get-origin-path))
     :origin_id (pen-get-id)
     :origin_point location
     :created_at (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))

    ;; update content with gptel
    (if gptel
        (pen-gptel-update-note id word (car type)
                               (lambda ()
                                 (if (eq major-mode 'pen-search-mode)
                                     (pen-search-refresh)))))

    ;; quit mark
    (if (featurep 'evil)
        (evil-force-normal-state))

    ;; query back the entry and add overylay
    (let ((candidates (pen-candidate-by-id (if (string-match ":id:\\(.*\\)" id)
                                               (match-string 1 id)
                                             id))))
      ;; attachment do not need overlay
      (pcase (car type)
        ((or 'attachment 'image)
         (pen-find-note (car candidates)))
        ('bookmark
         (message (format "Added bookmark: %s -> %s" (pen-get-origin-path) location)))
        ('sdcv
         (message (format "Added sdcv: %s" word)))
        (_ (pcase major-mode
             ('pdf-view-mode nil)
             ('eaf-mode nil)
             ('pen-search-mode nil)
             (_ (pen-add-annotation-overlay (car candidates))))))
      ;; push to pen-search-entries and pen-full-entries, so that we donot need to refresh the database
      ;; only if pen-full-entries is not nil, we use push
      ;; since if pen-full-entries is nil, it maybe the first time to load
      (if pen-full-entries
          (progn
            (push (car candidates) pen-search-entries)
            (push (car candidates) pen-full-entries)
            ;; update *pen* buffer
            (if (buffer-live-p (get-buffer "*pen*"))
                (pen t)))
        (pen t)))

    ;; enable pen annotation mode after adding
    (unless pen-annotation-mode
      (pen-annotation-mode 1))

    (if (derived-mode-p 'eaf-mode) (pen-view-note (car (pen-candidate-by-word id) )) )))

(defun pen-image-content-json (image)
  (if (file-exists-p image)
      (let* ((src image)
             (attachment (format "%s.%s" (org-id-uuid) (file-name-extension (file-name-nondirectory src))))
             (dst (expand-file-name attachment pen-note-dir))
             (json (json-encode `((filename . ,(file-name-nondirectory src)) (path . ,attachment)))))
        (copy-file src dst t)
        json)
    (let* ((src (expand-file-name (concat "picture." (symbol-name (plist-get (cdr (image--get-image)) :type) )) temporary-file-directory))
           (attachment (format "%s.%s" (org-id-uuid) (file-name-extension (file-name-nondirectory src))))
           (dst (expand-file-name attachment pen-note-dir))
           (json (json-encode `((filename . ,(file-name-nondirectory src)) (path . ,attachment)))))
      (with-temp-buffer
        (insert image)
        (write-region (point-min) (point-max) src))
      (copy-file src dst t)
      json)))

;;;###autoload
(defun pen-add-highlight (prefix)
  "Add an annotation."
  (interactive "P")
  (let* ((word (cond ((eq major-mode 'pen-search-mode) "")
                     ((eq major-mode 'pdf-view-mode)
                      (if (pdf-view-active-region-p)
                          (mapconcat 'identity (pdf-view-active-region-text) ? )
                        "EMPTY ANNOTATION"))
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
                     (mark-active (if (eq major-mode 'nov-mode)
                                      (pen-remove-spaces-based-on-ascii-rate (buffer-substring-no-properties (region-beginning) (region-end)))
                                    (buffer-substring-no-properties (region-beginning) (region-end))))
                     (t (substring-no-properties (or (thing-at-point 'symbol t) "")))))
         (type pen-annotation-current-highlight-type )
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
                      (require 'org-noter)
                      (org-noter--doc-approx-location
                       (org-noter--conv-page-scroll-percentage
                        (+ (window-vscroll)
                           (cdr (posn-col-row (event-start (read-event "Click the location"))))))))
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
    (pen-add-general word type location prefix)))

;;;###autoload
(defun pen-add-stamp (prefix)
  "Add an annotation."
  (interactive "P")
  (let* ((word (pen-get-word))
         (type (pen-get-stamp))
         (location (pen-get-location)))
    (pen-add-general word type location prefix)))

(defun pen-get-stamp ()
  (cons 'stamp  (ido-completing-read "Select a stamp: " pen-annotation-stamps)))

(defun pen-get-word ()
  "Get the word at point or marked region."
  (cond ((eq major-mode 'pen-search-mode) (read-string "Add word: "))
        ((eq major-mode 'pdf-view-mode)
         (if (pdf-view-active-region-p)
             (mapconcat 'identity (pdf-view-active-region-text) ? )
           "EMPTY ANNOTATION"))
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
        (mark-active (buffer-substring-no-properties (region-beginning) (region-end)))
        (t (substring-no-properties (or (thing-at-point 'word t) "")))))

(defun pen-get-location ()
  "Get location at point or marked region."
  (pcase major-mode
    ('nov-mode
     (if mark-active
         (cons nov-documents-index (cons (region-beginning) (region-end)))
       (cons nov-documents-index (point))))
    ('pdf-view-mode
     (require 'org-noter)
     (org-noter--doc-approx-location
      (org-noter--conv-page-scroll-percentage
       (+ (window-vscroll)
          (cdr (posn-col-row (event-start (read-event "Click the location"))))))))
    ('eaf-mode
     (pcase eaf--buffer-app-name
       ("pdf-viewer"
        (string-to-number (eaf-call-sync "execute_function" eaf--buffer-id "current_page")))
       ("browser" 0)
       (_ 0)))
    (_ (if mark-active (cons (region-beginning) (region-end))
         (point)))))

;;;###autoload
(defmacro pen-add (field)
  `(defun ,(intern (format "pen-add-%s" field)) (prefix)
     (interactive "P")
     (let* ((word (pen-get-word))
            (type (assoc ',(intern field) pen-note-type-alist))
            (location (pen-get-location)))
       (pen-add-general word type location prefix))))

;;;###autoload
(pen-add "word")

;;;###autoload
(pen-add "todo")

;;;###autoload
(pen-add "done")

;;;###autoload
(pen-add "cancel")

;;;###autoload
(pen-add "question")

;;;###autoload
(pen-add "link")

;;;###autoload
(pen-add "bookmark")

(defun pen-follow-link ()
  (interactive)
  (let* ((entry (get-char-property (point) 'pen-entry))
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
            (pen-find-origin
             (cl-find-if (lambda (x) (equal link (alist-get 'word x))) pen-full-entries)))))))))

;;;###autoload
(defun pen-add-attachment (&optional word)
  "Add an attachment."
  (interactive)
  (let ((word (or word "ATTACHMENT"))
        (location (pcase major-mode
                    ('nov-mode
                     (cons nov-documents-index (point)))
                    ('pdf-view-mode
                     (require 'org-noter)
                     (org-noter--doc-approx-location
                      (org-noter--conv-page-scroll-percentage
                       (+ (window-vscroll)
                          (cdr (posn-col-row (event-start (read-event "Click the location"))))))))
                    ('eaf-mode
                     (string-to-number (eaf-call-sync "execute_function" eaf--buffer-id "current_page")))
                    (_ (point))))
        (type (assoc 'attachment pen-note-type-alist)))
    (pen-add-general word type location)))

;;;###autoload
(defun pen-add-image (&optional word)
  "Add an image."
  (interactive)
  (let ((word (or word "IMAGE"))
        (location (pcase major-mode
                    ('nov-mode
                     (cons nov-documents-index (point)))
                    ('pdf-view-mode
                     (require 'org-noter)
                     (org-noter--doc-approx-location
                      (org-noter--conv-page-scroll-percentage
                       (+ (window-vscroll)
                          (cdr (posn-col-row (event-start (read-event "Click the location"))))))))
                    ('eaf-mode
                     (string-to-number (eaf-call-sync "execute_function" eaf--buffer-id "current_page")))
                    (_ (point))))
        (type (assoc 'image pen-note-type-alist)))
    (pen-add-general word type location)))

(defun pen-show-all-annotations (&optional candidates)
  "when candidates, just check and show the candidates overlays, this is much faster"
  (interactive)
  (if (or candidates pen-annotation-mode)
      (let ((candidates (if candidates candidates (pen-candidates-by-origin-path-serverp) )))
        (save-excursion
          (cl-loop for entry in candidates do
                   (pcase (car (alist-get 'note_type entry))
                     ('attachment)
                     ('bookmark)
                     ('image)
                     (_ (pen-add-annotation-overlay entry))))))))

(defun pen-get-highlight-type ()
  (interactive)
  (let ((choice (read-char-from-minibuffer
                 (format "Annotation Type: %s %s %s %s (o)ther (q)uit "
                         (propertize "highlight-(1)" 'face 'pen-highlight-1-face)
                         (propertize "highlight-(2)" 'face 'pen-highlight-2-face)
                         (propertize "highlight-(3)" 'face 'pen-highlight-3-face)
                         (propertize "highlight-(4)" 'face 'pen-highlight-4-face)) '(?1 ?2 ?3 ?4 ?o ?q))))
    (unless (eq choice ?q)
      (let* ((c (char-to-string choice))
             (uppercasep (and (stringp c) (string-equal c (upcase c)) ))
             (cc (downcase c)))
        (cond
         ((string-equal cc "1") (message "") (assoc 'highlight-1 pen-note-type-alist))
         ((string-equal cc "2") (message "") (assoc 'highlight-2 pen-note-type-alist))
         ((string-equal cc "3") (message "") (assoc 'highlight-3 pen-note-type-alist))
         ((string-equal cc "4") (message "") (assoc 'highlight-4 pen-note-type-alist))
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
         (t (message "") (assoc 'highlight-1 pen-note-type-alist)))))))

(defun pen-get-todo-type ()
  (interactive)
  (let ((choice (read-char-from-minibuffer
                 (format "TODO Type: %s %s %s (q)uit "
                         (propertize "(t/T)ODO" 'face 'pen-todo-face)
                         (propertize "(d/D)ONE" 'face 'pen-done-face)
                         (propertize "(c/C)ANCEL" 'face 'pen-cancel-face)) '(?t ?T ?d ?D ?c ?C ?q))))
    (unless (eq choice ?q)
      (let* ((c (char-to-string choice))
             (uppercasep (and (stringp c) (string-equal c (upcase c)) ))
             (cc (downcase c)))
        (cond
         ((string-equal cc "t") (message "") (assoc 'todo pen-note-type-alist))
         ((string-equal cc "d") (message "") (assoc 'done pen-note-type-alist))
         ((string-equal cc "c") (message "") (assoc 'cancel pen-note-type-alist))
         ((string-equal cc "q") (message "") (error "quit"))
         (t (message "") (assoc 'todo pen-note-type-alist)))))))

(defun pen-add-annotation-overlay (entry &optional type)
  (let* ((location (alist-get 'origin_point entry))
         (note (alist-get 'note entry))
         (word (alist-get 'word entry))
         (serverp (alist-get 'serverp entry))
         (real-word (pen-get-real-word word))
         (note-type (if type
                        (pcase (car (alist-get 'note_type entry))
                          ('word
                           (let ((wd (read-from-minibuffer "Insert the annotation symbol: ")))
                             ;; update note type
                             (pen-update-note_type word `(word . ,wd))
                             ;; delete overlay
                             (delete-overlay
                              (cl-find-if
                               (lambda (o)
                                 (equal (alist-get 'word (overlay-get o 'pen-entry)) word))
                               (overlays-at (point))))
                             (cons 'word wd)))
                          ('question
                           (let ((wd (read-from-minibuffer "Insert the question symbol: ")))
                             ;; update note type
                             (pen-update-note_type word `(question . ,wd))
                             ;; delete overlay
                             (delete-overlay
                              (cl-find-if
                               (lambda (o)
                                 (equal (alist-get 'word (overlay-get o 'pen-entry)) word))
                               (overlays-at (point))))
                             (cons 'question wd)))
                          ((or 'todo 'done 'cancel)
                           (let ((td (pen-get-todo-type)))
                             ;; update note type
                             (pen-update-note_type word td)
                             ;; delete overlay
                             (delete-overlay
                              (cl-find-if
                               (lambda (o)
                                 (equal (alist-get 'word (overlay-get o 'pen-entry)) word))
                               (overlays-at (point))))
                             td))
                          ('stamp
                           (let ((td (pen-get-stamp)))
                             ;; update note type
                             (pen-update-note_type word td)
                             ;; delete overlay
                             (delete-overlay
                              (cl-find-if
                               (lambda (o)
                                 (equal (alist-get 'word (overlay-get o 'pen-entry)) word))
                               (overlays-at (point))))
                             td))
                          (_
                           (let ((tp (setq pen-annotation-current-highlight-type (pen-get-highlight-type))))
                             ;; update note type
                             (pen-update-note_type word tp)
                             ;; delete overlay
                             (delete-overlay
                              (cl-find-if
                               (lambda (o)
                                 (equal (alist-get 'word (overlay-get o 'pen-entry)) word))
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
               (cond ((stringp location) nil)
                     ((consp (cdr location))
                      (if (eq nov-documents-index (car location))
                          (progn
                           (setq beg (cadr location))
                           (setq end (cddr location))
                           (cdr location))
                        nil))
                     ((consp location)
                      (if (eq nov-documents-index (car location))
                          (cdr location)
                        nil))
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
                            (equal (alist-get 'word (overlay-get o 'pen-entry)) word))
                          (overlays-in (point-min) (point-max)))
                   (pen-add-overlay beg end note-type note entry))
               (message "Can not find word \"%s\", maybe the location is changed." real-word)))
            ((or (eq serverp 1) (eq location nil))
             ;; if serverp is 1, that means it is an online word, hightlight all occurrences in the buffer
             ;; if location is nil, that means it is not in current buffer, hightlight all occurrences in the buffer
             (goto-char (point-min))
             (let ((case-fold-search t))  ; or nil for case-sensitive
               (while (if (string-match-p "[[:ascii:]]+" real-word)
                          ;; english
                          (re-search-forward (concat "\\b" real-word "\\b") nil t)
                        ;; non-english
                        (re-search-forward real-word nil t))
                 (let* ((beg (match-beginning 0))
                        (end (match-end 0))
	                (existing-overlays (overlays-in beg end)))
                   (unless (cl-find-if
                            (lambda (o)
                              (equal (alist-get 'word (overlay-get o 'pen-entry)) word))
                            existing-overlays)
                     (pen-add-overlay beg end note-type note entry))))))
            (t
             ;; TODO the match is not robust
             ;; first check string between beg and end
             (if (string-match-p (regexp-quote (s-trim (s-collapse-whitespace (buffer-substring-no-properties beg end)) ) ) (s-trim (s-collapse-whitespace real-word) ))
                 (unless (cl-find-if
                          (lambda (o)
                            (equal (alist-get 'word (overlay-get o 'pen-entry)) word))
                          (overlays-in (point-min) (point-max)))
                   (pen-add-overlay beg end note-type note entry))

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
                          (equal (alist-get 'word (overlay-get o 'pen-entry)) word))
                        (overlays-at (point)))
                 (pen-add-overlay beg end note-type note entry))))))))


(defun pen-previous-annotation ()
  (interactive)
  (cond ((eq major-mode 'pen-search-mode)
         (progn
           (pen-previous)
           (pen-find-origin)))
        ((bound-and-true-p focus-mode)
         (pen-focus-find-prev-thing-segment))
        (t (let* ((previous-overlays (reverse (-filter
                                               (lambda (o)
                                                 (overlay-get o 'pen-entry))
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


(defun pen-next-annotation ()
  (interactive)
  (cond ((eq major-mode 'pen-search-mode)
         (progn
           (pen-next)
           (pen-find-origin)) )
        ((bound-and-true-p focus-mode)
         (pen-focus-find-next-thing-segment))
        (t (let* ((overlay (cl-find-if
                            (lambda (o)
                              (overlay-get o 'pen-entry))
                            (overlays-in (point) (point-max))))
                  (beg (if overlay (overlay-start overlay)))
                  (end (if overlay (overlay-end overlay)))
                  (next-overlay (cl-find-if
                                 (lambda (o)
                                   (overlay-get o 'pen-entry))
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



(defun pen-copy-annotation ()
  (interactive)
  (let ((entry (get-char-property (point) 'pen-entry)))
    (if entry
      (let ((word (or (pen-get-real-word entry) "")))
        (kill-new word)
        (message "Copied \"%s\"" word)))))

(defun pen-delete-annotation (&optional en)
  (interactive)
  (let* ((entry (or en (get-char-property (point) 'pen-entry)))
         (content (alist-get 'content entry))
         (content-json (condition-case nil
                           (let ((output (json-read-from-string content)))
                             (if (and (not (eq output nil))
                                      (not (arrayp output))
                                      (not (numberp output)))
                                 output
                               nil))
                         (error nil)))
         (content-filename (alist-get 'filename content-json))
         (serverp (alist-get 'serverp entry))
         (origin_id (alist-get 'origin_id entry))
         (content-path (alist-get 'path content-json))
         (note-type (alist-get 'note_type entry))
         (word (alist-get 'word entry)))
    (when (yes-or-no-p (format "Delete word: %s" word))
      (progn
        (if (eq serverp 1)
            (pen-request-delete-words word origin_id)
          (pen-db-delete word))
        (if content-path
            (pcase (car note-type)
              ('image (let ((png (expand-file-name content-path pen-note-dir)))
                        (if (file-exists-p png)
                            (delete-file png)
                          (message "Image %s not exists." png))))
              ('attachment (let ((attachment (expand-file-name content-path pen-note-dir)))
                             (if (file-exists-p attachment)
                                 (delete-file attachment)
                               (message "Attachment %s not exists." attachment))))
              (_ nil)))
        ;; if the overlay is not in the current buffer, we need to delete it in other buffers
        (-map (lambda (b)
                (with-current-buffer b
                  (if (eq pen-annotation-mode t)
                      (let ((overlays-to-delete
                             (cl-remove-if-not
                              (lambda (o) (equal (alist-get 'word (overlay-get o 'pen-entry)) word))
                              (overlays-in (point-min) (point-max)))))
                        (dolist (o overlays-to-delete) ; delete all matching overlays
                          (delete-overlay o)))))) ; update the
              (buffer-list))))
    (if pen-search-entries (setq pen-search-entries (-remove (lambda (x) (equal word (alist-get 'word x))) pen-search-entries)) )
    (if pen-full-entries (setq pen-full-entries (-remove (lambda (x) (equal word (alist-get 'word x))) pen-full-entries)) )
    ;; update buffer
    (if (buffer-live-p (get-buffer "*pen*"))
        (pen t))))

(defun pen-clear-annotation-overlay ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((ovs (pen-get-all-overlays)))
      (dolist (ov ovs)
        (delete-overlay ov)))))

(defun pen-get-all-overlays()
  (-filter
   (lambda (o)
     (overlay-get o 'pen-entry))
   (overlays-in (point-min) (point-max))))

(defun pen-get-all-entries-from-overlays()
  (cl-remove-duplicates
   (mapcar
    (lambda (o)
      (overlay-get o 'pen-entry))
    (overlays-in (point-min) (point-max))) ))

(defun pen-list-default-action (x)
    (let* ((entry (get-text-property 0 'pen-entry x))
           (word (pen-get-real-word entry))
           (origin-point (alist-get 'origin_point entry))
           (note-type (alist-get 'note_type entry))
           (old-origin-path (alist-get 'origin_path entry))
           (new-origin-path (pen-get-origin-path)))
      (if entry
          (pcase (car note-type)
            ('attachment (pen-find-note entry))
            ('image (pen-find-note entry))
            ;; if same path, we just need to call `pen-goto-location'
            (_ (if (equal old-origin-path new-origin-path)
                   (pen-goto-location origin-point word)
                 (pen-find-origin entry))))
        (pen-add-attachment x))))

(defun pen-list-delete-action (x)
  (let* ((entry (get-text-property 0 'pen-entry x)))
    (if entry
        (pen-delete-annotation entry))))

;;;###autoload
(defun pen-list-annotations (whole-file)
  (interactive "P")
  (consult--read (pen-candidates-format nil whole-file t t)
                 :prompt "Annotations: "
                 :sort nil
                 :history nil
                 :lookup (lambda(cand candidates input-string _)
                           (let* ((entry (get-text-property 0 'pen-entry (cl-find-if
                                                                          (lambda (input)
                                                                            (string= input cand)) candidates) ))
                                  (word (pen-get-real-word entry))
                                  (origin-point (alist-get 'origin_point entry))
                                  (note-type (alist-get 'note_type entry))
                                  (old-origin-path (alist-get 'origin_path entry))
                                  (new-origin-path (pen-get-origin-path))
                                  (note-type (car (alist-get 'note_type entry))))
                             (pcase note-type
                               ('bookmark (if (equal old-origin-path new-origin-path)
                                              (pen-goto-location origin-point word)
                                            (pen-find-origin entry)))
                               (_ (pen-find-note entry)))))))

;;;###autoload
(defun pen-list-all-annotations ()
  (interactive)
  (consult--read (pen-candidates-format t)
                 :prompt "All Annotations: "
                 :sort nil
                 :history nil
                 :lookup (lambda(cand candidates input-string _)
                           (pen-list-default-action
                            (cl-find-if
                             (lambda (input)
                               (string= input cand)) candidates)))))


(defvar pen-annotation-links-source
  (list
   :name "Annotation Links"
   :narrow   ?l
   :items    (lambda()
               (pen-candidates-format nil nil nil nil t) )
   :action   (lambda(cand)
               (pen-list-default-action cand))))

;;;###autoload
(defun pen-list-all-links ()
  (interactive)
  (consult--read (pen-candidates-format nil nil nil nil t)
                 :prompt "All Links: "
                 :sort nil
                 :history nil
                 :lookup (lambda(cand candidates input-string _)
                           (pen-list-default-action
                            (cl-find-if
                             (lambda (input)
                               (string= input cand)) candidates)))))


(defun pen-change-annotation-note-type ()
  "Change the annotation note type"
  (interactive)
  (if (bound-and-true-p focus-mode)
      (pen-focus-find-current-thing-segment)
    (pen-add-annotation-overlay (get-char-property (point) 'pen-entry) t)
    ;; update buffer
    (if (buffer-live-p (get-buffer "*pen*"))
        (pen t)) ))


(defun pen-candidates-by-mode (&optional whole-file sort current-buffer)
  "Match major modes and return the list of candidates.
If WHOLE-FILE is t, always index the whole file."
  (if current-buffer
      (let* ((candidates (pen-get-all-entries-from-overlays))
             (len (length candidates)))
        (cons candidates len))
  (pcase major-mode
    ('nov-mode
     (let* ((candidates (if sort
                            (-sort (lambda (ex ey)
                                     (let ((x (alist-get 'origin_point ex))
                                           (y (alist-get 'origin_point ey)))
                                       (cond ((and (numberp x) (numberp y))
                                              (< x y))
                                             ((and (consp x) (consp y))
                                              (< (car x) (car y)))
                                             ((consp x)
                                              (< (car x) y))
                                             ((consp y)
                                              (< x (car y)))))) (pen-candidates-by-origin-path))
                            (pen-candidates-by-origin-path) ))
            (len (length candidates)))
       (cons (if whole-file
                 candidates
               ;; filter current index candidates
               (-filter (lambda (x)
                          (let ((origin-point (alist-get 'origin_point x)))
                            (cond ((listp origin-point)
                                   (eq nov-documents-index (car origin-point)))
                                  (t nil))))
                        candidates) ) len)))
    ('wallabag-mode
     (let* ((candidates (if sort
                            (-sort (lambda (ex ey)
                                     (let ((x (alist-get 'origin_point ex))
                                           (y (alist-get 'origin_point ey)))
                                       (cond ((and (numberp x) (numberp y))
                                              (< x y))
                                             ((and (consp x) (consp y))
                                              (< (car x) (car y)))
                                             ((consp x)
                                              (< (car x) y))
                                             ((consp y)
                                              (< x (car y)))))) (pen-candidates-by-origin-path))
                          (pen-candidates-by-origin-path) ))
            (len (length candidates)))
       (cons candidates len)))
    ('eww-mode
     (let* ((candidates (if sort
                            (-sort (lambda (ex ey)
                                     (let ((x (alist-get 'origin_point ex))
                                           (y (alist-get 'origin_point ey)))
                                       (cond ((and (numberp x) (numberp y))
                                              (< x y))
                                             ((and (consp x) (consp y))
                                              (< (car x) (car y)))
                                             ((consp x)
                                              (< (car x) y))
                                             ((consp y)
                                              (< x (car y)))))) (pen-candidates-by-origin-path))
                          (pen-candidates-by-origin-path) ))
            (len (length candidates)))
       (cons candidates len)))
    ('pdf-view-mode
     (let* ((candidates (if sort
                            (-sort (lambda (ex ey)
                                     (let ((x (alist-get 'origin_point ex))
                                           (y (alist-get 'origin_point ey)))
                                       (cond ((and (numberp x) (numberp y))
                                              (< x y))
                                             ((and (consp x) (consp y))
                                              (< (car x) (car y)))
                                             ((consp x)
                                              (< (car x) y))
                                             ((consp y)
                                              (< x (car y)))))) (pen-candidates-by-origin-path))
                          (pen-candidates-by-origin-path) ))
            (len (length candidates)))
       (cons candidates len)))
    (_
     (let* ((candidates (if sort
                            (-sort (lambda (ex ey)
                                     (let ((x (alist-get 'origin_point ex))
                                           (y (alist-get 'origin_point ey)))
                                       (cond ((and (numberp x) (numberp y))
                                              (< x y))
                                             ((and (consp x) (consp y))
                                              (< (car x) (car y)))
                                             ((consp x)
                                              (< (car x) y))
                                             ((consp y)
                                              (< x (car y)))))) (pen-candidates-by-origin-path))
                          (pen-candidates-by-origin-path) ))
            (len (length candidates)))
       (cons candidates len))))

  )
  )

(defun pen-candidates-format (&optional all whole-file sort current-buffer only-links)
  "Match major modes and return the list of formated candidates."
  (-map
   (lambda (entry)
     (concat
      (propertize (or (alist-get 'created_at entry) "") 'pen-entry entry)
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
         (pen-format-column
          (pen-format-icon note-type content serverp)
          2 :left)
         "  "
         (propertize (s-truncate 55 (pen-get-real-word word) ) 'face 'pen-offline-face)
         "  "
         (if (stringp origin-point)
             origin-point
           (if origin-path
               (pcase origin-type
                 ('wallabag-entry-mode
                  (propertize origin-path 'face 'pen-wallabag-face))
                 ('nov-mode
                  (propertize (file-name-nondirectory origin-path) 'face 'pen-nov-face))
                 ((or 'pdf-view-mode 'nov-mode "pdf-viewer")
                  (propertize (file-name-nondirectory origin-path) 'face 'pen-pdf-face))
                 ((or 'eaf-mode "browser")
                  (propertize origin-path 'face 'pen-link-face))
                 (_ (propertize (file-name-nondirectory origin-path ) 'face 'pen-file-face)))
             ""))

         ))
      "  "
      (s-collapse-whitespace (or (s-truncate 120 (alist-get 'note entry)) ""))) )
   (cond (all ;; if all is t, return all candidates which is serverp equals 2
          (-filter (lambda (entry)
                     (eq (alist-get 'serverp entry) 2)) (pen-all-candidates)))

         (only-links (pen-candidates-only-links))
         ((derived-mode-p 'eaf-mode)
          (car (pen-candidates-by-mode t)))
         (t (car (pen-candidates-by-mode whole-file sort current-buffer))))))

(defvar pen-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") 'pen-list-annotations)
    (define-key map (kbd "C-c C-a") 'pen-add-highlight)
    (define-key map (kbd "C-c C-,") 'pen-add-attachment)
    (define-key map (kbd "C-c C-n") 'pen-next-annotation)
    (define-key map (kbd "C-c C-p") 'pen-previous-annotation)
    (define-key map (kbd "s") 'pen-view-note)
    (define-key map (kbd "t") 'pen-view-note-translate)
    (define-key map (kbd "i") 'pen-add-highlight)
    (define-key map (kbd "s") 'pen-add-online-word)
    (define-key map (kbd "u") 'pen-scroll-down)
    (define-key map (kbd "d") 'pen-scroll-up)
    (define-key map (kbd "c") 'pen-view-note-current-thing)
    (define-key map (kbd "n") 'pen-view-note-next-thing)
    (define-key map (kbd "p") 'pen-view-note-prev-thing)
    (define-key map (kbd "f") 'focus-mode)
    (define-key map (kbd "r") 'pen-view-note-play)
    ;; (define-key map (kbd "q") 'pen-view-note-quit)
    (define-key map [mouse-1] 'pen-view-note-click)
    map)
  "Keymap for function `pen-annotation-mode'.")


(if (fboundp 'evil-define-key)
    (evil-define-key '(normal visual insert) pen-annotation-mode-map
      (kbd "s") 'pen-view-note
      (kbd "t") 'pen-view-note-transalate
      (kbd "i") 'pen-add-highlight
      (kbd "a") 'pen-add-online-word
      (kbd "u") 'pen-scroll-down
      (kbd "d") 'pen-scroll-up
      (kbd "c") 'pen-view-note-current-thing
      (kbd "n") 'pen-view-note-next-thing
      (kbd "p") 'pen-view-note-prev-thing
      (kbd "f") 'focus-mode
      (kbd "r") 'pen-view-note-play
      [mouse-1] 'pen-view-note-click
      ;; (kbd "q") 'pen-view-note-quit
      ) )

(defcustom pen-view-note-transalate-function 'pen-nov-translate
  "pen view note translate function"
  :group 'pen
  :type '(choice (function-item pen-view-note-transalate)
          function))

(defun pen-view-note-transalate ()
  (interactive)
  (funcall pen-view-note-transalate-function))


(defcustom pen-view-note-click-function 'pen-click-to-view-note-nov
  "pen view note click function"
  :group 'pen
  :type '(choice (function-item pen-click-to-view-note-nov)
          function))

(defun pen-view-note-click (event)
  (interactive "e")
  (funcall pen-view-note-click-function event))


(defun pen-click-to-view-note-nov (event)
  "Click to view note in nov mode
Argument EVENT mouse event."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No URL chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      ;; (goldendict-search-at-point)

      (require 'sdcv)
      (if (shr-link-at-point-p)
          (if (eq major-mode 'nov-mode)
              (nov-browse-url)
            (shr-browse-url))
        (pen-view-note)))))


(defcustom pen-view-note-current-thing-function 'pen-focus-find-current-thing-segment
  "pen view note current thing function"
  :group 'pen
  :type '(choice (function-item pen-focus-find-current-thing-segment)
          function))

(defun pen-view-note-current-thing ()
  (interactive)
  (funcall pen-view-note-current-thing-function))



(defcustom pen-view-note-next-thing-function 'pen-focus-find-next-thing-segment
  "pen view note next thing function"
  :group 'pen
  :type '(choice (function-item pen-focus-find-next-thing-segment)
          function))

(defun pen-view-note-next-thing ()
  (interactive)
  (funcall pen-view-note-next-thing-function))


(defcustom pen-view-note-prev-thing-function 'pen-focus-find-prev-thing-segment
  "pen view note previous thing function"
  :group 'pen
  :type '(choice (function-item pen-focus-find-prev-thing-segment)
          function))

(defun pen-view-note-prev-thing ()
  (interactive)
  (funcall pen-view-note-prev-thing-function))


(defvar-local pen-annotation-read-only nil
  "Buffer local variable to store the original read-only state of the buffer.")

;;;###autoload
(define-minor-mode pen-annotation-mode
  "Toggle pen-annotation-mode"
  :group 'pen
  :keymap pen-annotation-mode-map
  (if (cl-find-if (lambda (mode)
                    (eq major-mode mode))
                  pen-annotation-mode-supported-modes)
      (let ((mode-line-segment '(:eval (pen-annotation-mode-line-text))))
        (cond
         (pen-annotation-mode
          (if (cl-find-if (lambda (mode)
                            (eq major-mode mode))
                          pen-annotation-mode-supported-modes)
              (progn
               ;; show all annotations first
               (pen-show-all-annotations)
               ;; then update and show the mode line
               (pen-annotation-get-mode-line-text)
               (if (symbolp (car-safe mode-line-format))
                   (setq mode-line-format (list mode-line-segment mode-line-format))
                 (push mode-line-segment mode-line-format))
               ;; Save the original read-only state of the buffer
               (setq pen-annotation-read-only buffer-read-only)
               (if (bound-and-true-p flyspell-mode)
                   (flyspell-mode -1))
               (read-only-mode 1)
               (run-hooks 'pen-annotation-mode-hook))
           (message "Major mode %s is not supported by pen-annotation-mode." major-mode)))
         (t
          (if (cl-find-if (lambda (mode)
                            (eq major-mode mode))
                          pen-annotation-mode-supported-modes)
              (progn
                (setq pen-annotation-mode-map (make-sparse-keymap))
                (setq mode-line-format (delete mode-line-segment mode-line-format))
                ;; Restore the original read-only state of the buffer
                (setq buffer-read-only pen-annotation-read-only)
                (if (bound-and-true-p flyspell-mode)
                    (flyspell-mode +1))
                (pen-clear-annotation-overlay))
            (message "Major mode %s is not supported by pen-annotation-mode." major-mode))
          )))
    (message "Major mode %s is not supported by pen-annotation-mode." major-mode)))

(defvar pen-annotation--menu-contents
  '("Pen Annotation"
    ["Pen Dashboard" pen
     :help "Open Pen Dashboard"]
    ["Toggle pen annotation mode" pen-annotation-mode
     :help "Toggle pen annotation mode"]
    ["Go to Pen Dashboard" pen-goto-dashboard
     :help "Go to Pen Dashboard"
     :enable pen-annotation-mode]
    ["List annotations" pen-list-annotations
     :help "List annotations in the buffer"
     :enable pen-annotation-mode]
    ["List all annotations" pen-list-all-annotations
     :help "List all annotations in the buffer"]
    ["View note" pen-view-note
     :help "View the annotation"]
    ["View all notes in the buffer" pen-view-notes
     :help "View all annotations in the buffer"
     :enable pen-annotation-mode]
    ["Edit annotation" pen-find-note
     :help "View the annotation"
     :enable pen-annotation-mode]
    ["Change annotation type" pen-change-annotation-note-type
     :help "Change the annotation type"
     :enable pen-annotation-mode]
    ["Change note type" pen-change-note-type
     :help "Change the note type"
     :enable pen-annotation-mode]
    ["Copy annotation" pen-copy-annotation
     :help "Copy the annotation"
     :enable pen-annotation-mode]
    ["Next annotation" pen-next-annotation
     :help "Go to the next annotation"
     :enable pen-annotation-mode]
    ["Previous annotation" pen-previous-annotation
     :help "Go to the previous annotation"
     :enable pen-annotation-mode]
    ["Follow link" pen-follow-link
     :help "Follow the link"
     :enable pen-annotation-mode]
    ["Replay audio" pen-replay
     :help "Replay the audio"
     :enable pen-annotation-mode]
    "---"
    ["Add a word (online)" pen-add-online-word
     :help "Add a word to Eudic"]
    ["Add a word (offline)" pen-add-word
     :help "Add a word locally"]
    ["Add a highlight" pen-add-highlight
     :help "Add a highlight annotation"]
    ["Add a link" pen-add-link
     :help "Add a link annotation"]
    ["Add a question" pen-add-question
     :help "Add a question annotation"]
    ["Add a todo" pen-add-todo
     :help "Add a todo annotation"]
    ["Add a done" pen-add-done
     :help "Add a done annotation"]
    ["Add a cancel" pen-add-cancel
     :help "Add a cancel annotation"]
    ["Add a bookmark" pen-add-bookmark
     :help "Add a bookmark annotation"]
    ["Delete annotation" pen-delete-annotation
     :help "Delete the annotation"
     :enable pen-annotation-mode])
  "Contents of the Pen Annotation menu.")


(defun pen-annotation--menu-bar-enable ()
  "Enable pen-annotation menu bar."
  (define-key-after global-map [menu-bar pen-annotation]
    (easy-menu-binding
     (easy-menu-create-menu "Pen Annotation" pen-annotation--menu-contents) "Pen Annotation")
    "Tools"))


;; Enable Pen Annotation menu bar by default
(pen-annotation--menu-bar-enable)

;;;###autoload
(define-minor-mode pen-annotation-menu-bar-mode "Show Pen Annotation menu bar."
  :global t
  :init-value t
  (if pen-annotation-menu-bar-mode
      (pen-annotation--menu-bar-enable)
    (define-key global-map [menu-bar pen-annotation] nil)))


(defun pen-annotation-context-menu (menu _click)
  "Populate MENU with Pen Annotation commands at CLICK."
  (define-key menu [pen-annotation-separator] menu-bar-separator)
  (let ((easy-menu (make-sparse-keymap "Pen Annotation")))
    (easy-menu-define nil easy-menu nil
      pen-annotation--menu-contents)
    (dolist (item (reverse (lookup-key easy-menu [menu-bar])))
      (when (consp item)
        (define-key menu (vector (car item)) (cdr item)))))
  menu)

(defvar pen-annotation-mode-line-text "0 notes")

(defun pen-annotation-mode-line-text ()
  (if (and pen-db-update-p pen-annotation-mode)
      (progn
        (setq-local pen-db-update-p nil)
        (pen-annotation-get-mode-line-text))
    pen-annotation-mode-line-text))

(defun pen-annotation-get-mode-line-text ()
  (let* ((candidate-cons (pen-candidates-by-mode nil nil))
         (number-of-notes (or (length (car candidate-cons)) 0))
         (number-of-all-notes (or (cdr candidate-cons) 0))
         (no-of-overlays (length (pen-get-all-entries-from-overlays) )))
    (setq-local pen-annotation-mode-line-text
                (pcase major-mode
                  ('nov-mode
                   (cond ((= number-of-notes 0) (propertize (format " 0/%d (%d) notes " number-of-all-notes no-of-overlays) 'face 'pen-no-notes-exist-face))
                         ((= number-of-notes 1) (propertize (format " 1/%d (%d) note " number-of-all-notes no-of-overlays) 'face 'pen-notes-exist-face))
                         (t (propertize (format " %d/%d (%d) notes " number-of-notes number-of-all-notes no-of-overlays) 'face 'pen-notes-exist-face))))
                  (_
                   (cond ((= number-of-notes 0) (propertize (format " 0 (%d) notes " no-of-overlays) 'face 'pen-no-notes-exist-face))
                         ((= number-of-notes 1) (propertize (format " 1 (%d) note " no-of-overlays) 'face 'pen-notes-exist-face))
                         (t (propertize (format " %d (%d) notes " number-of-notes no-of-overlays) 'face 'pen-notes-exist-face))))) )))

;;; format
(defun pen-format-content (note-type word content content-path content-filename)
  (pcase (car note-type)
    ('attachment
     (s-pad-right 30 " "
                  (let* ((ext (downcase (file-name-extension content-path)))
                         (ext (if (string= ext "jpg") "jpeg" ext)))
                    (pcase ext
                      ((or "pbm" "xbm" "xpm" "gif" "jpeg" "tiff" "png" "svg" "jpg")
                       (propertize "IMAGS"
                                   'face 'pen-offline-face
                                   'display (create-image (expand-file-name content-path pen-note-dir) nil nil :width (if IS-LINUX 200 100) :height nil  :margin '(0 . 1))))
                      (_ (propertize (format "%s %s" (pen-attach-icon-for (expand-file-name content-filename)) (pen-format-column content-filename 40 :left) )
                                     'face 'pen-offline-face))) ) ))
    ('image
     (s-pad-right 30 " "
                  (propertize "IMAGS"
                              'face 'pen-offline-face
                              'display (create-image (expand-file-name content-path pen-note-dir) nil nil :width (if IS-LINUX 200 100) :height nil :margin '(0 . 1))) ))
    (_ (s-pad-right 40 " "
                    (propertize (s-truncate 36 (s-collapse-whitespace (or (if (equal content 0) word (if content content "")) (pen-get-real-word word))))
                                'face 'pen-offline-face)))))

(defun pen-format-icon (note-type content serverp)
  "Return the icon based on NOTE-TYPE and CONTENT.
CONTENT is useful for sub types, for example, link."
  (pcase (car note-type)
    ('word
     (propertize (cdr note-type) 'display (if (eq serverp 1)
                                              pen-star-face-icon
                                              pen-word-icon)))
    ('image
     (propertize (cdr note-type) 'display pen-image-icon))
    ('bookmark
     (propertize (cdr note-type) 'display pen-bookmark-icon))
    ('attachment
     (propertize (cdr note-type) 'display pen-attachment-icon))
    ('question
     (propertize (cdr note-type) 'display pen-question-icon))
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
          (propertize (cdr note-type) 'display pen-file-link-icon))
         ("url"
          (propertize (cdr note-type) 'display pen-url-link-icon))
         ("annotation"
          (propertize (cdr note-type) 'display pen-annotation-link-icon))
         ("org"
          (propertize (cdr note-type) 'display (create-image pen-org-link-file nil nil :width nil :height nil :ascent 'center))))))
    ('todo
     (propertize (cdr note-type) 'display pen-todo-icon))
    ('done
     (propertize (cdr note-type) 'display pen-done-icon))
    ('cancel
     (propertize (cdr note-type) 'display pen-cancel-icon))
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

(defun pen-add-overlay (beg end note-type note entry)
  "Add overlay between BEG and END based on NOTE-TYPE.
Add NOTE and ENTRY as overlay properties."
  (pcase (car note-type)
    ('word
     (let ((ov (make-overlay beg end)))
       (overlay-put ov 'before-string
                    (let ((serverp (alist-get 'serverp entry)))
                      (if (eq serverp 1)
                          (propertize (cdr note-type) 'display pen-star-face-icon)
                        (propertize (cdr note-type) 'display pen-word-icon))))
       (overlay-put ov 'face 'pen-word-face)
       ;; show studylist for online words
       (overlay-put ov 'help-echo (let ((serverp (alist-get 'serverp entry))
                                        (origin_path (alist-get 'origin_path entry)))
                                    (if (eq serverp 1)
                                        origin_path
                                        note)))
       (overlay-put ov 'keymap pen-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'pen-entry entry)
       (overlay-put ov 'mouse-face 'pen-word-hover-face)))
    ('question
     (let ((ov (make-overlay beg end)))
       (overlay-put ov 'before-string (propertize (cdr note-type) 'display pen-question-icon))
       (overlay-put ov 'face '(:underline t))
       (overlay-put ov 'help-echo note)
       (overlay-put ov 'keymap pen-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'pen-entry entry)
       (overlay-put ov 'mouse-face 'pen-mouse-face)))
    ('todo
     (let ((ov (make-overlay beg end)))
       (overlay-put ov 'before-string (propertize (cdr note-type) 'display pen-todo-icon))
       (overlay-put ov 'face 'pen-todo-face)
       (overlay-put ov 'help-echo note)
       (overlay-put ov 'keymap pen-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'pen-entry entry)
       (overlay-put ov 'mouse-face 'pen-mouse-face)))
    ('done
     (let ((ov (make-overlay beg end)))
       (overlay-put ov 'before-string (propertize (cdr note-type) 'display pen-done-icon))
       (overlay-put ov 'face 'pen-done-face)
       (overlay-put ov 'help-echo note)
       (overlay-put ov 'keymap pen-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'pen-entry entry)
       (overlay-put ov 'mouse-face 'pen-mouse-face)))
    ('cancel
     (let ((ov (make-overlay beg end)))
       (overlay-put ov 'before-string (propertize (cdr note-type) 'display pen-cancel-icon))
       (overlay-put ov 'face 'pen-cancel-face)
       (overlay-put ov 'help-echo note)
       (overlay-put ov 'keymap pen-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'pen-entry entry)
       (overlay-put ov 'mouse-face 'pen-mouse-face)))
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
            (overlay-put ov 'before-string (propertize (cdr note-type) 'display pen-file-link-icon)))
           ("url"
            (overlay-put ov 'before-string (propertize (cdr note-type) 'display pen-url-link-icon)))
           ("annotation"
            (overlay-put ov 'before-string (propertize (cdr note-type) 'display pen-annotation-link-icon)))
           ("org"
            (overlay-put ov 'before-string (propertize (cdr note-type) 'display (create-image pen-org-link-file nil nil :width nil :height nil :ascent 'center))))))
       (overlay-put ov 'face 'pen-link-face)
       (overlay-put ov 'help-echo (alist-get 'content entry))
       (overlay-put ov 'keymap pen-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'pen-entry entry)
       (overlay-put ov 'mouse-face 'pen-mouse-face)))
    ('stamp
     (let ((ov (make-overlay beg end)))
       (overlay-put ov 'before-string (propertize (cdr note-type) 'display (cdr note-type)))
       (overlay-put ov 'face '(:underline t))
       (overlay-put ov 'help-echo note)
       (overlay-put ov 'keymap pen-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'pen-entry entry)
       (overlay-put ov 'mouse-face 'pen-mouse-face)))
    (_
     (let ((ov (make-overlay beg end)))
       (overlay-put ov 'face (cdr note-type))
       (overlay-put ov 'help-echo note)
       (overlay-put ov 'keymap pen-annotation-map)
       (overlay-put ov 'cursor t)
       (overlay-put ov 'pen-entry entry)
       (overlay-put ov 'mouse-face 'pen-mouse-face)))))

(provide 'pen-annotation)
