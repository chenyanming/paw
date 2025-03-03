;;; paw-sdcv.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'sdcv nil t)

(defcustom paw-sdcv-dictionary-list nil
  "The dictionary list for sdcv. If not defined, use `sdcv-dictionary-simple-list'."
  :type 'list
  :group 'paw)


;;;###autoload
(defun paw-sdcv-search-input ()
  "Translate curre input WORD.
And show information in other buffer."
  (interactive)
  ;; Display details translate result.
  (let ((sdcv-say-word-p t))
    (sdcv-search-input)))

(defun paw-sdcv-kill-process ()
  (interactive)
  (when (process-live-p paw-sdcv-running-process )
    (kill-process paw-sdcv-running-process)
    (setq paw-sdcv-running-process nil)))

;;;###autoload
(defun paw-sdcv-search-at-point (&optional arg)
  "Get current word.
And display complete translations in other buffer."
  (interactive "P")
  ;; Display details translate result.
  (if arg
      (paw-sdcv-search-input)
    (let* ((sdcv-say-word-p t)
           (fun 'paw-sdcv-search-detail))
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
(defun paw-sdcv-search-detail (&optional word)
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
      (paw-sdcv-hook)
      (sdcv-mode-reinit)
      (unless (eq major-mode 'sdcv-mode)
        (sdcv-mode)))
    (pop-to-buffer buffer)))


(defun paw-sdcv-hook()
  (setq header-line-format '(:eval (funcall sdcv-header-function)))
  )

(defvar sdcv-header-function #'sdcv-header)

(defun sdcv-header ()
  "Header function for *paw* buffer."
  (format "%s" (propertize sdcv-current-translate-object 'face 'font-lock-keyword-face)))

(defun paw-sdcv-search-with-dictionary-async (word buffer)
  "Search some WORD with DICTIONARY-LIST.
Argument DICTIONARY-LIST the word that needs to be transformed."
  (if (featurep 'sdcv)
      (progn
        (paw-sdcv-kill-process)
        (let* ((word (or word (sdcv-region-or-word)))
               (dictionary-list (if paw-sdcv-dictionary-list
                                    paw-sdcv-dictionary-list
                                  sdcv-dictionary-simple-list)))
          (paw-sdcv-translate-result-async word dictionary-list buffer)))
    (message "Please install sdcv first.")))


(defun paw-sdcv-translate-result-async (word dictionary-list buffer)
  "Call sdcv to search WORD in DICTIONARY-LIST.
Show results on BUFFER."
  (let* ((arguments (cons word (mapcan (lambda (d) (list "-u" d)) dictionary-list))))
    (paw-sdcv-start-process buffer arguments)))

(defun paw-sdcv-start-process (&optional buffer &rest arguments)
  "Call `sdcv-program' with ARGUMENTS.
Result is parsed as json."
  (let* ((original-output-buffer (get-buffer "*sdcv-output*"))
         (output-buffer (if (buffer-live-p original-output-buffer)
                            (progn (kill-buffer original-output-buffer)
                                   (get-buffer-create "*sdcv-output*") )
                          (get-buffer-create "*sdcv-output*") ))
         (filter 'paw-sdcv-process-filter)
         (lang-env (concat "LANG=" sdcv-env-lang))
         (process-environment (cons lang-env process-environment))
         (process (make-process
                   :name "sdcv"
                   :buffer output-buffer
                   :noquery t
                   :command (append (list sdcv-program "--non-interactive" "--json-output")
                                    (when sdcv-only-data-dir
                                      (list "--only-data-dir"))
                                    (when sdcv-dictionary-data-dir
                                      (list "--data-dir" sdcv-dictionary-data-dir))
                                    (car arguments ))
                   :filter filter
                   :sentinel (lambda (proc event)
			       (paw-sdcv-process-sentinel proc event buffer)))))
    (setq paw-sdcv-running-process process)
    (set-process-query-on-exit-flag process nil)))

(defun paw-sdcv-format-result (result)
  "Return a formatted string for RESULT."
  (if (string= paw-view-note-meaning-src-lang "org")
      (let-alist result
	(format "%s\n" .definition))
    (let-alist result
      (format "-->%s\n-->%s%s\n" .dict .word .definition))))


(defun paw-sdcv-process-filter (proc string)
  "Accumulates the strings received from the Kagome process."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (insert string)) ))

(defun paw-sdcv-process-sentinel (proc _event buffer)
  (when (eq (process-status proc) 'exit)
    (let* ((buffer-content (with-current-buffer (process-buffer proc)
                             (buffer-string)))
           (json-responses (json-read-from-string buffer-content)))
      ;; (pp json-responses)
      (save-excursion
        (if (buffer-live-p buffer)
            (with-current-buffer buffer
              (let* ((buffer-read-only nil)
                     (result (mapconcat
                              'paw-sdcv-format-result
                              json-responses
                              ""))
                     (result (if (string-empty-p result)
                                 sdcv-fail-notify-string
                               (replace-regexp-in-string "^\\*" "-" result))))
                (goto-char (point-min))
                (if (string= sdcv-fail-notify-string result) ;; if no result, goto Translation
                    (search-forward "** Translation" nil t)
                  ;; TODO find the overlay and add transaction to it, but it is very complicated
                  ;; (overlay-get (cl-find-if
                  ;;               (lambda (o)
                  ;;                (string-equal (overlay-get o 'paw-dictionary-word)  ))
                  ;;               (overlays-in (point) (point-max))) 'paw-dictionary-word)
                  (search-forward "** Meaning" nil t)
                  (org-mark-subtree)
                  (forward-line)
                  (delete-region (region-beginning) (region-end))
		  (if (string= paw-view-note-meaning-src-lang "org")
                      (paw-insert-and-make-overlay (format "%s" result) 'face `(:background ,paw-view-note-background-color :extend t))
		    (progn
		      (paw-insert-and-make-overlay "#+BEGIN_SRC sdcv\n" 'invisible t)
		      (insert (format "%s" result))
		      (paw-insert-and-make-overlay "#+END_SRC" 'invisible t)))
                  (insert "\n")
                  ;; (goto-char (point-min))
                  ;; (unless (search-forward "** Dictionaries" nil t)
                  ;;   (search-forward "** Translation" nil t))
                  ;; (beginning-of-line)
                  ;; (recenter 0)
                  ;; (message "Translation completed %s" translation)
                  ))
              (deactivate-mark)

              ) ) ))
    )
  ;; TODO back to original window, but unsafe
  ;; (other-window 1)
  )

(defcustom paw-view-note-meaning-src-lang "sdcv"
  "Language to be used for highlighting sdcv ouput in meaning section of paw-view-note buffer."
  :group 'paw
  :type 'string)

(defun paw-update-all-word ()
  (interactive)
  (let ((words (mapcar 'car (paw-db-sql [:select word :from items]))))
    (cl-loop for id in words do
             (paw-db-update-exp
              id
              (sdcv-translate-result (paw-get-real-word id) sdcv-dictionary-complete-list))))
  (paw-search-refresh))

(defun paw-view-definition ()
  (interactive)
  (let ((sdcv-say-word-p t))
    (sdcv-search-detail
     (paw-get-real-word (get-text-property (point) 'paw-entry))) ))

;; (paw-sdcv-search-with-dictionary-async "goo" sdcv-dictionary-simple-list)

(provide 'paw-sdcv)
