;;; pen/pen-sdcv.el -*- lexical-binding: t; -*-

(require 'sdcv nil t)

;;;###autoload
(defun pen-sdcv-search-input ()
  "Translate current input WORD.
And show information in other buffer."
  (interactive)
  ;; Display details translate result.
  (let ((sdcv-say-word-p t))
    (sdcv-search-input)))


;;;###autoload
(defun pen-sdcv-search-at-point (&optional arg)
  "Get current word.
And display complete translations in other buffer."
  (interactive "P")
  (require 'sdcv)
  ;; Display details translate result.
  (if arg
      (pen-sdcv-search-input)
    (let* ((sdcv-say-word-p t)
           (fun 'pen-sdcv-search-detail))
      (pcase major-mode
        ('pdf-view-mode
         (funcall fun (if (pdf-view-active-region-p)
                          (mapconcat 'identity (pdf-view-active-region-text) ? )
                        "EMPTY ANNOTATION") ))
        ('xwidget-webkit-mode (xwidget-webkit-get-selection fun))
        (_ (funcall fun (sdcv-region-or-word))))

      ))
  ;; (if (y-or-n-p-with-timeout "Save to wordlist?" 0.5 nil)
  ;;     (save-to-wordlist global-wordlist))
  )
(defun pen-sdcv-search-detail (&optional word)
  "Search WORD in `sdcv-dictionary-complete-list'.
The result will be displayed in buffer named with
`sdcv-buffer-name' in `sdcv-mode'."
  (message "Searching...")
  (let ((buffer (get-buffer-create sdcv-buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq sdcv-current-translate-object word)
      (insert (sdcv-search-with-dictionary word sdcv-dictionary-complete-list))
      (pen-sdcv-hook)
      (sdcv-mode-reinit)
      (unless (eq major-mode 'sdcv-mode)
        (sdcv-mode)))
    (pop-to-buffer buffer)))


(defun pen-sdcv-hook()
  (setq header-line-format '(:eval (funcall sdcv-header-function)))
  )

(defvar sdcv-header-function #'sdcv-header)

(defun sdcv-header ()
  "Header function for *pen* buffer."
  (format "%s" (propertize sdcv-current-translate-object 'face 'font-lock-keyword-face)))

(defun pen-sdcv-call-process (&rest arguments)
  "Call `sdcv-program' with ARGUMENTS.
Result is parsed as json."
  (with-temp-buffer
    (save-excursion
      (let* ((lang-env (concat "LANG=" sdcv-env-lang))
             (process-environment (cons lang-env process-environment)))
        (apply #'call-process sdcv-program nil t nil
               (append (list "--non-interactive" "--json-output")
                       (when sdcv-only-data-dir
                         (list "--only-data-dir"))
                       (when sdcv-dictionary-data-dir
                         (list "--data-dir" sdcv-dictionary-data-dir))
                       arguments))))
    (buffer-substring-no-properties (goto-line 2) (point-max))
    (ignore-errors (json-read))))

(if (string-equal system-type "android")
    (advice-add 'sdcv-call-process :override #'pen-sdcv-call-process))


(defun pne-sdcv-search-with-dictionary-async (word dictionary-list)
  "Search some WORD with DICTIONARY-LIST.
Argument DICTIONARY-LIST the word that needs to be transformed."
  (let* ((word (or word (sdcv-region-or-word))))
    (pen-sdcv-translate-result-async word dictionary-list)))


(defun pen-sdcv-translate-result-async (word dictionary-list)
  "Call sdcv to search WORD in DICTIONARY-LIST.
Return filtered string of results."
  (let* ((arguments (cons word (mapcan (lambda (d) (list "-u" d)) dictionary-list))))
    (pen-sdcv-start-process arguments)))

(defun pen-sdcv-start-process (&rest arguments)
  "Call `sdcv-program' with ARGUMENTS.
Result is parsed as json."
  (let* ((original-output-buffer (get-buffer "*sdcv-output*"))
         (output-buffer (if (buffer-live-p original-output-buffer)
                            (progn (kill-buffer original-output-buffer)
                                   (get-buffer-create "*sdcv-output*") )
                          (get-buffer-create "*sdcv-output*") ))
         (filter 'pen-sdcv-process-filter)
         (sentinel 'pen-sdcv-process-sentinel)
         (lang-env (concat "LANG=" sdcv-env-lang))
         (process-environment (cons lang-env process-environment))
         (process (make-process
                   :name "sdcv"
                   :buffer output-buffer
                   :command (append (list sdcv-program "--non-interactive" "--json-output")
                                    (when sdcv-only-data-dir
                                      (list "--only-data-dir"))
                                    (when sdcv-dictionary-data-dir
                                      (list "--data-dir" sdcv-dictionary-data-dir))
                                    (car arguments ))
                   :filter filter
                   :sentinel sentinel)))
    (set-process-query-on-exit-flag process nil)))

(defun pen-sdcv-format-result (result)
  "Return a formatted string for RESULT."
  (let-alist result
    (format "-->%s\n-->%s%s\n\n" .dict .word .definition)))


(defun pen-sdcv-process-filter (proc string)
  "Accumulates the strings received from the Kagome process."
  (with-current-buffer (process-buffer proc)
    (insert string)))

(defun pen-sdcv-process-sentinel (proc _event)
  (when (eq (process-status proc) 'exit)
    (let* ((buffer-content (with-current-buffer (process-buffer proc)
                             (if (string-equal system-type "android")
                                 (buffer-substring-no-properties (goto-line 2) (point-max))
                               (buffer-string))))
           (json-responses (json-read-from-string buffer-content))
           (pen-view-note-buffer (get-buffer "*pen-view-note*"))
           (pen-sub-note-buffer (get-buffer "*pen-sub-note*")))
      ;; (pp json-responses)
      (save-excursion
        (with-current-buffer
            (if (buffer-live-p pen-view-note-buffer)
                pen-view-note-buffer
              (if (buffer-live-p pen-sub-note-buffer)
                  pen-sub-note-buffer
                (generate-new-buffer "*pen-view-note*")))

          (let* ((buffer-read-only nil)
                 (result (mapconcat
                          'pen-sdcv-format-result
                          json-responses
                          ""))
                 (result (if (string-empty-p result)
                             sdcv-fail-notify-string
                           (replace-regexp-in-string "^\\*" "-" result))))
            (goto-char (point-min))
            ;; TODO find the overlay and add transaction to it, but it is very complicated
            ;; (overlay-get (cl-find-if
            ;;               (lambda (o)
            ;;                (string-equal (overlay-get o 'pen-dictionary-word)  ))
            ;;               (overlays-in (point) (point-max))) 'pen-dictionary-word)
            (search-forward "** Dictionary" nil t)
            (org-mark-subtree)
            (forward-line)
            (delete-region (region-beginning) (region-end))
            (pen-insert-and-make-overlay "#+BEGIN_SRC sdcv\n" 'invisible t)
            (insert (format "%s" result))
            (pen-insert-and-make-overlay "#+END_SRC" 'invisible t)
            (insert "\n")
            ;; (message "Translation completed %s" translation)
            )
          ) ))
    (deactivate-mark))
  (other-window 1))

(defun pen-update-all-word ()
  (interactive)
  (let ((words (mapcar 'car (pen-db-sql [:select word :from items]))))
    (cl-loop for id in words do
             (pen-db-update-exp
              id
              (sdcv-translate-result (pen-get-real-word id) sdcv-dictionary-complete-list))))
  (pen-search-refresh))

(defun pen-view-definition ()
  (interactive)
  (let ((sdcv-say-word-p t))
    (sdcv-search-detail
     (pen-get-real-word (get-text-property (point) 'pen-entry))) ))

;; (pne-sdcv-search-with-dictionary-async "goo" sdcv-dictionary-simple-list)

(provide 'pen-sdcv)
