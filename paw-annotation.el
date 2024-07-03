;;; paw-annotation.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'paw-db)
(require 'paw-util)
(require 'paw-faces)
(require 'paw-svg)
(require 'paw-gptel)
(require 'paw-request)
(require 'paw-focus)

(require 'org)
(require 's)
(require 'dash)
(require 'focus)
(require 'thingatpt)
(require 'evil-core nil t)

(defconst paw-note-type-alist
  '((word . "✎")
    (highlight-1 . paw-highlight-1-face)
    (highlight-2 . paw-highlight-2-face)
    (highlight-3 . paw-highlight-3-face)
    (highlight-4 . paw-highlight-4-face)
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

(defcustom paw-annotation-mode-supported-modes
  '(nov-mode org-mode paw-view-note-mode wallabag-entry-mode eww-mode)
  "Supported modes for paw-annotation-mode."
  :group 'paw
  :type 'list)

(defcustom paw-annotation-search-paths '()
  "Alternative pathes for paw-annotation-mode. The books pathes
 that are possibly used for paw-annotation-mode."
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
  "Space string for annotation. Currently only support Japanese."
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
    (define-key map "r" 'paw-replay)
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



(defun paw-add-general (word type location &optional gptel note path)
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
                                                                   (ivy-read "Please insert an annotation: " (paw-candidates-format t)
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
     :origin_type (if (derived-mode-p 'eaf-mode)
                      eaf--buffer-app-name
                    major-mode)
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
         (message (format "Added bookmark: %s -> %s" (paw-get-origin-path) location)))
        ('sdcv
         (message (format "Added sdcv: %s" word)))
        (_ (pcase major-mode
             ('pdf-view-mode nil)
             ('eaf-mode nil)
             ('paw-search-mode nil)
             (_ (paw-add-annotation-overlay (car candidates))))))
      ;; push to paw-search-entries and paw-full-entries, so that we donot need to refresh the database
      ;; only if paw-full-entries is not nil, we use push
      ;; since if paw-full-entries is nil, it maybe the first time to load
      (if paw-full-entries
          (progn
            (push (car candidates) paw-search-entries)
            (push (car candidates) paw-full-entries)
            ;; update *paw* buffer
            (if (buffer-live-p (get-buffer "*paw*"))
                (paw t)))
        (paw t)))

    ;; enable paw annotation mode after adding
    (unless paw-annotation-mode
      (paw-annotation-mode 1))

    ))

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
  (let* ((word (cond ((eq major-mode 'paw-search-mode) "")
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
                                      (paw-remove-spaces-based-on-ascii-rate (buffer-substring-no-properties (region-beginning) (region-end)))
                                    (buffer-substring-no-properties (region-beginning) (region-end))))
                     (t (substring-no-properties (or (thing-at-point 'symbol t) "")))))
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

(defun paw-get-location ()
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
             (cl-find-if (lambda (x) (equal link (alist-get 'word x))) paw-full-entries)))))))))

;;;###autoload
(defun paw-add-attachment (&optional word)
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
                     (require 'org-noter)
                     (org-noter--doc-approx-location
                      (org-noter--conv-page-scroll-percentage
                       (+ (window-vscroll)
                          (cdr (posn-col-row (event-start (read-event "Click the location"))))))))
                    ('eaf-mode
                     (string-to-number (eaf-call-sync "execute_function" eaf--buffer-id "current_page")))
                    (_ (point))))
        (type (assoc 'image paw-note-type-alist)))
    (paw-add-general word type location)))

(defun paw-show-all-annotations (&optional candidates)
  "when candidates, just check and show the candidates overlays, this is much faster"
  (interactive)
  (if (or candidates paw-annotation-mode)
      (let ((candidates (if candidates candidates (paw-candidates-by-origin-path-serverp) )))
        (save-excursion
          (cl-loop for entry in candidates do
                   (pcase (car (alist-get 'note_type entry))
                     ('attachment)
                     ('bookmark)
                     ('image)
                     (_ (paw-add-annotation-overlay entry))))))))

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
                              (equal (alist-get 'word (overlay-get o 'paw-entry)) word))
                            existing-overlays)
                     (paw-add-overlay beg end note-type note entry))))))
            (t
             ;; TODO the match is not robust
             ;; first check string between beg and end
             (if (string-match-p (regexp-quote (s-trim (s-collapse-whitespace (buffer-substring-no-properties beg end)) ) ) (s-trim (s-collapse-whitespace real-word) ))
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
  (interactive)
  (cond ((eq major-mode 'paw-search-mode)
         (progn
           (paw-previous)
           (paw-find-origin)))
        ((bound-and-true-p focus-mode)
         (paw-focus-find-prev-thing-segment))
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
  (interactive)
  (cond ((eq major-mode 'paw-search-mode)
         (progn
           (paw-next)
           (paw-find-origin)) )
        ((bound-and-true-p focus-mode)
         (paw-focus-find-next-thing-segment))
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

(defun paw-get-all-entries-from-overlays()
  (cl-remove-duplicates
   (mapcar
    (lambda (o)
      (overlay-get o 'paw-entry))
    (overlays-in (point-min) (point-max))) ))

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
(defun paw-list-annotations (whole-file)
  (interactive "P")
  (consult--read (paw-candidates-format nil whole-file t t)
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
                             (pcase note-type
                               ('bookmark (if (equal old-origin-path new-origin-path)
                                              (paw-goto-location origin-point word)
                                            (paw-find-origin entry)))
                               (_ (paw-find-note entry)))))))

;;;###autoload
(defun paw-list-all-annotations ()
  (interactive)
  (consult--read (paw-candidates-format t)
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
   :name "Annotation Links"
   :narrow   ?l
   :items    (lambda()
               (paw-candidates-format nil nil nil nil t) )
   :action   (lambda(cand)
               (paw-list-default-action cand))))

(defvar paw-list-add-links-history nil)

;;;###autoload
(defun paw-list-all-links ()
  "List all eaf/eww links in the buffer."
  (interactive)
  (consult--read (paw-candidates-format nil nil nil nil t)
                 :prompt "All Links: "
                 :sort t
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
    (if (eq major-mode 'paw-search-mode)
        (paw-search-refresh)
      (paw-search-refresh t))
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
    (if (eq major-mode 'paw-search-mode)
        (paw-search-refresh)
      (paw-search-refresh t))
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


(defun paw-candidates-by-mode (&optional whole-file sort current-buffer)
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

(defun paw-candidates-format (&optional all whole-file sort current-buffer only-links)
  "Match major modes and return the list of formated candidates."
  (-map
   (lambda (entry)
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
          (paw-format-icon note-type content serverp)
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
      (s-collapse-whitespace (or (s-truncate 120 (alist-get 'note entry)) ""))) )
   (cond (all ;; if all is t, return all candidates which is serverp equals 2
          (-filter (lambda (entry)
                     (eq (alist-get 'serverp entry) 2)) (paw-all-candidates)))

         (only-links (paw-candidates-only-links))
         ((derived-mode-p 'eaf-mode)
          (car (paw-candidates-by-mode t)))
         (t (car (paw-candidates-by-mode whole-file sort current-buffer))))))

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


(if (fboundp 'evil-define-key)
    (evil-define-key '(normal visual insert) paw-annotation-mode-map
      (kbd "s") 'paw-view-note-in-minibuffer
      (kbd "t") 'paw-view-note-transalate
      ;; (kbd "i") 'paw-add-highlight
      (kbd "a") 'paw-add-online-word
      (kbd "u") 'paw-scroll-down
      (kbd "d") 'paw-scroll-up
      (kbd "c") 'paw-view-note-current-thing
      (kbd "n") 'paw-view-note-next-thing
      (kbd "p") 'paw-view-note-prev-thing
      (kbd "f") 'focus-mode
      (kbd "r") 'paw-view-note-play
      [mouse-1] 'paw-view-note-click
      [mouse-2] 'paw-view-note-click
      ;; (kbd "q") 'paw-view-note-quit
      ) )

(defcustom paw-view-note-transalate-function 'paw-nov-translate
  "paw view note translate function"
  :group 'paw
  :type '(choice (function-item paw-view-note-transalate)
          function))

(defun paw-view-note-transalate ()
  (interactive)
  (funcall paw-view-note-transalate-function))


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
      ;; show all annotations first
      (paw-show-all-annotations)

      ;; show all unknown words
      (if paw-annotation-show-unknown-words-p
          (paw-focus-find-unknown-words))

      ;; then update and show the mode line
      (paw-annotation-get-mode-line-text)
      (if (symbolp (car-safe mode-line-format))
          (setq mode-line-format (list mode-line-segment mode-line-format))
        (push mode-line-segment mode-line-format))
      (when paw-annotation-read-only-enable
        ;; Save the original read-only state of the buffer
        (setq paw-annotation-read-only buffer-read-only)
        (read-only-mode 1))
      (run-hooks 'paw-annotation-mode-hook))
     (t
      (setq-local minor-mode-map-alist
                  (assq-delete-all 'paw-annotation-mode minor-mode-map-alist))
      (setq mode-line-format (delete mode-line-segment mode-line-format))
      (when paw-annotation-read-only-enable
        ;; Restore the original read-only state of the buffer
        (setq buffer-read-only paw-annotation-read-only))
      (if paw-click-overlay
          (delete-overlay paw-click-overlay))
      (paw-clear-annotation-overlay)))))

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
    ["Replay audio" paw-replay
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
  (let* ((candidate-cons (paw-candidates-by-mode nil nil))
         (number-of-notes (or (length (car candidate-cons)) 0))
         (number-of-all-notes (or (cdr candidate-cons) 0))
         (no-of-overlays (length (paw-get-all-entries-from-overlays) )))
    (setq-local paw-annotation-mode-line-text
                (pcase major-mode
                  ('nov-mode
                   (cond ((= number-of-notes 0) (propertize (format " 0/%d (%d) notes " number-of-all-notes no-of-overlays) 'face 'paw-no-notes-exist-face))
                         ((= number-of-notes 1) (propertize (format " 1/%d (%d) note " number-of-all-notes no-of-overlays) 'face 'paw-notes-exist-face))
                         (t (propertize (format " %d/%d (%d) notes " number-of-notes number-of-all-notes no-of-overlays) 'face 'paw-notes-exist-face))))
                  (_
                   (cond ((= number-of-notes 0) (propertize (format " 0 (%d) notes " no-of-overlays) 'face 'paw-no-notes-exist-face))
                         ((= number-of-notes 1) (propertize (format " 1 (%d) note " no-of-overlays) 'face 'paw-notes-exist-face))
                         (t (propertize (format " %d (%d) notes " number-of-notes no-of-overlays) 'face 'paw-notes-exist-face))))) )))

;;; format
(defun paw-format-content (note-type word content content-path content-filename)
  (pcase (car note-type)
    ('attachment
     (s-pad-right 30 " "
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
     (s-pad-right 30 " "
                  (propertize "IMAGS"
                              'face 'paw-offline-face
                              'display (create-image (expand-file-name content-path paw-note-dir) nil nil :width (if (eq system-type 'gnu/linux) 200 100) :height nil :margin '(0 . 1))) ))
    (_ (s-pad-right 40 " "
                    (propertize (s-truncate 36 (s-collapse-whitespace (or (if (equal content 0) word (if content content "")) (paw-get-real-word word))))
                                'face 'paw-offline-face)))))

(defun paw-format-icon (note-type content serverp)
  "Return the icon based on NOTE-TYPE and CONTENT.
CONTENT is useful for sub types, for example, link."
  (pcase (car note-type)
    ('word
     (propertize (cdr note-type) 'display (if (paw-online-p serverp)
                                              paw-star-face-icon
                                              paw-word-icon)))
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
            (when (string= (alist-get 'lang entry) "ja")
              ;; seperate the word with space
              (overlay-put ov 'after-string (propertize paw-annotation-after-string-space 'mouse-face nil))))
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
