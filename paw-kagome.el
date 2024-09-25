;;; paw-kagome.el -*- lexical-binding: t; -*-

(require 'json)

(defcustom paw-kagome-program
  (cond
   ((string-equal system-type "android")
    "/data/data/com.termux/files/home/go/bin/kagome")
   (t
    "kagome"))
  "Executable used to access the kagome."
  :type 'file
  :group 'kagome)

(defvar paw-kagome-running-process nil)

;;;###autoload
(defun paw-kagome-kill-process ()
  (interactive)
  (when (process-live-p paw-kagome-running-process )
    (kill-process paw-kagome-running-process)
    (setq paw-kagome-running-process nil)))


;;;###autoload
(defun paw-kagome-segment ()
  "Segments a STRING of Japanese text using Kagome and logs the result asynchronously."
  (interactive)
  (paw-kagome-kill-process)
  (let* ((original-output-buffer (get-buffer "*kagome-output*"))
         (output-buffer (if (buffer-live-p original-output-buffer)
                            (progn (kill-buffer original-output-buffer)
                                   (get-buffer-create "*kagome-output*") )
                          (get-buffer-create "*kagome-output*") ))
         (kagome-process (make-process
                          :name "Kagome"
                          :buffer output-buffer
                          :noquery t
                          :command `(,paw-kagome-program "-json")
                          :filter 'paw-kagome-process-filter
                          :sentinel 'paw-kagome-process-sentinel-whole-buffer)))
    (setq paw-kagome-running-process kagome-process)
    (process-send-string kagome-process (concat (buffer-substring-no-properties (point-min) (point-max)) "\n"))
    (process-send-eof kagome-process)))


;;;###autoload
(defun paw-kagome-segment-blocking ()
  "Synchronously segment Japanese text in the current buffer using Kagome."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "kagome -json" t t)
  (goto-char (point-min))
  (let* ((json-array-type 'list)
         (json-object-type 'plist)
         (json-responses (json-read))
         (segmented-text (mapconcat (lambda (resp) (plist-get resp :surface))
                                    json-responses
				    ;; " "
				    paw-non-ascii-word-separator)))
    (erase-buffer)
    (insert segmented-text))
  (message "Segmentation completed."))


(defun paw-kagome-process-filter (proc string)
  "Accumulates the strings received from the Kagome process."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (insert string))))



(defun paw-kagome-process-sentinel (proc _event)
  "Handles the Kagome process termination event."
  (when (eq (process-status proc) 'exit)
    (let* ((json-object-type 'plist)
           (json-array-type 'list)
           (buffer-content (with-current-buffer (process-buffer proc)
                             (buffer-string)))
           (json-responses (json-read-from-string buffer-content)))
      (dolist (resp json-responses)
        (let* ((start (plist-get resp :start))
               (end (plist-get resp :end))
               (surface (plist-get resp :surface))
               (cls (plist-get resp :class)) ;; 'class' is a built-in function
               (pos (plist-get resp :pos))
               (base-form (plist-get resp :base_form))
               (reading (plist-get resp :reading))
               (pronunciation (plist-get resp :pronunciation))
               (features (plist-get resp :features)))
          (message "start: %s, end: %s, surface: %s, class: %s, pos: %s, base_form: %s, reading: %s, pronunciation: %s, features: %s"
                   start end surface cls pos base-form reading pronunciation features)))
      (let ((segmented-text (mapconcat
                             (lambda (resp) (plist-get resp :surface))
                             json-responses
                             ;; " "
			     paw-non-ascii-word-separator)))
        (message "Segmented text: %s" segmented-text)))))


(defun paw-kagome-process-sentinel-whole-buffer (proc _event)
  "Handles the Kagome process termination event."
  (when (eq (process-status proc) 'exit)
    (let* ((json-object-type 'plist)
           (json-array-type 'list)
           (buffer-content (with-current-buffer (process-buffer proc)
                             (buffer-string)))
           (json-responses (json-read-from-string buffer-content)))
      ;; (dolist (resp json-responses)
      ;;   (let* ((start (plist-get resp :start))
      ;;          (end (plist-get resp :end))
      ;;          (surface (plist-get resp :surface))
      ;;          (cls (plist-get resp :class)) ;; 'class' is a built-in function
      ;;          (pos (plist-get resp :pos))
      ;;          (base-form (plist-get resp :base_form))
      ;;          (reading (plist-get resp :reading))
      ;;          (pronunciation (plist-get resp :pronunciation))
      ;;          (features (plist-get resp :features)))
      ;;     (message "start: %s, end: %s, surface: %s, class: %s, pos: %s, base_form: %s, reading: %s, pronunciation: %s, features: %s"
      ;;              start end surface cls pos base-form reading pronunciation features)))
      (let ((segmented-text (mapconcat
                             (lambda (resp) (plist-get resp :surface))
                             json-responses
                             ;; " "
			     paw-non-ascii-word-separator
			     )))
        (with-current-buffer (current-buffer)
          (erase-buffer)
          (insert segmented-text))))))


(defun paw-kagome-process-output (string)
  (let* ((json-array-type 'list)
         (json-object-type 'plist)
         (json (json-read-from-string string))
         (surface-strings (mapcar (lambda (j) (plist-get j 'surface)) json)))
    (mapconcat 'identity surface-strings paw-non-ascii-word-separator)))

(defvar kagome-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s g") 'paw-kagome-segment)
    map)

  "Keymap for Kagome minor mode.")

(defun paw-kagome-command (string &optional sentinel)
  "Segments a STRING of Japanese text using Kagome and logs the result asynchronously."
  (unless (executable-find paw-kagome-program)
    (error "kagome is not found, please install via 'go install github.com/ikawaha/kagome/v2@latest' first."))
  (paw-kagome-kill-process)
  (let* ((original-output-buffer (get-buffer "*kagome-output*"))
         (output-buffer (if (buffer-live-p original-output-buffer)
                            (progn (kill-buffer original-output-buffer)
                                   (get-buffer-create "*kagome-output*") )
                          (get-buffer-create "*kagome-output*") ))
         (kagome-process (make-process
                          :name "Kagome"
                          :buffer output-buffer
                          :noquery t
                          :command `(,paw-kagome-program "-json")
                          :filter 'paw-kagome-process-filter
                          :sentinel (if sentinel sentinel 'paw-kagome-process-sentinel))))
    (setq paw-kagome-running-process kagome-process)
    (process-send-string kagome-process (concat string "\n"))
    (process-send-eof kagome-process)))

;;;###autoload
(defun paw-kagome-segment-file-blocking ()
  "Segments Japanese text in current buffer using Kagome synchronously."
  (interactive)
  (let* ((pre-string (buffer-substring-no-properties (point-min) (point-max)))
         (input-temp-file-path (make-temp-file "kagome" nil ".org"))
         (file-code (with-temp-file input-temp-file-path
                      (insert pre-string)
                      42))
         (buffer-content (with-temp-buffer
                           (shell-command (format "kagome -file %s -json" input-temp-file-path) t t)
                           (buffer-string)))
         (post-string (with-temp-buffer
                        (let* ((json-object-type 'plist)
                               (json-array-type 'list)
                               (json-responses (paw-kagome-parse-json-sequence buffer-content))
                               (segmented-text (let (result)
                                                 (dolist (ele  json-responses)
                                                   (setq result (concat (mapconcat (lambda (el) (plist-get el :surface)) ele paw-non-ascii-word-separator) "\n\n" result ) ))
                                                 result)))
                          segmented-text))))
    (erase-buffer)
    (insert post-string)
    ;; (message post-string)
    (goto-char (point-min))
    (message "Segmentation finished.")))



(defun paw-kagome-parse-json-sequence (json-string)
  (with-temp-buffer
    (insert json-string)
    (goto-char (point-min))
    (let ((result nil)) ;; list to hold parsed JSON arrays
      ;; Search forward for '['
      (while (search-forward "[" nil t)
        (backward-char) ;; Move point back to beginning of [
        (let ((json-array (json-read)))
          (if json-array
              (push json-array result))))
      result))) ;; Return reversed list to maintain original order






(defun paw-kagome-command-blocking (string)
  "Segments a STRING of Japanese text using Kagome synchronously and return the result."
  (unless (executable-find paw-kagome-program)
    (error "kagome is not found, please install via 'go install github.com/ikawaha/kagome/v2@latest' first."))
  (let* ((buffer-content (with-temp-buffer
                           (insert string)
                           (shell-command-on-region (point-min) (point-max) "kagome -json" t t)
                           (buffer-string))))
    (with-temp-buffer
      (let* ((json-object-type 'plist)
             (json-array-type 'list)
             (json-responses (json-read-from-string buffer-content))
             (segmented-text (mapconcat
                              (lambda (resp) (plist-get resp :surface))
                              json-responses
			      paw-non-ascii-word-separator)))
        segmented-text))))

(defun paw-kagome-shr-insert (text)
  (when (and (not (bolp))
             (get-text-property (1- (point)) 'image-url))
    (insert "\n"))
  (cond
   ((eq shr-folding-mode 'none)
    (let ((start (point)))
      (insert text)
      (save-restriction
        (narrow-to-region start (point))
        (shr--translate-insertion-chars)
        (goto-char (point-max)))))
   (t
    (let ((font-start (point)))
      (when (and (string-match-p "\\`[ \t\n\r]" text)
                 (not (bolp))
                 (not (eq (char-after (1- (point))) ? )))
        (insert " "))
      (let ((start (point))
            (bolp (bolp)))
        (insert (paw-kagome-command-blocking text ))
        (save-restriction
          (narrow-to-region start (point))
          (goto-char start)
          (when (looking-at "[ \t\n\r]+")
            (replace-match "" t t))
          (while (re-search-forward "[\t\n\r]+" nil t)
            (replace-match " " t t))
          (goto-char start)
          (while (re-search-forward "  +" nil t)
            (replace-match " " t t))
          (shr--translate-insertion-chars)
          (goto-char (point-max)))
        ;; We may have removed everything we inserted if it was just
        ;; spaces.
        (unless (= font-start (point))
          ;; Mark all lines that should possibly be folded afterwards.
          (when bolp
            (shr-mark-fill start))
          (when shr-use-fonts
            (put-text-property font-start (point)
                               'face
                               (or shr-current-font 'shr-text)))))))))


(provide 'paw-kagome)
