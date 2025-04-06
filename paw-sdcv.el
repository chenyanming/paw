;;; paw-sdcv.el -*- lexical-binding: t; -*-

(require 'paw-vars)

(defcustom paw-sdcv-dictionary-list nil
  "The dictionary list for sdcv. If not defined, use `sdcv-dictionary-simple-list'."
  :type 'list
  :group 'paw)


(defcustom paw-sdcv-program (if (boundp 'sdcv-program)
                                sdcv-program
                              (if (string-equal system-type "darwin") "/usr/local/bin/sdcv" "sdcv"))
  "Path to sdcv."
  :type 'file
  :group 'paw)


(defcustom paw-sdcv-env-lang "zh_CN.UTF-8"
  "Default LANG environment for sdcv program.

Default is zh_CN.UTF-8, maybe you need to change it to other
coding if your system is not zh_CN.UTF-8."
  :type 'string
  :group 'paw)

(defcustom paw-sdcv-only-data-dir t
  "Search is performed using only `paw-sdcv-dictionary-data-dir'."
  :type 'boolean
  :group 'paw)

(defcustom paw-sdcv-exact-match nil
  "Make SDCV return exact matches only."
  :type 'boolean
  :group 'paw)

(defcustom paw-sdcv-dictionary-data-dir nil
  "Default, sdcv search word in /usr/share/startdict/dict/.
If you customize this value with local dir, then you don't need
to copy dict data to /usr/share directory everytime when you
finish system installation."
  :type '(choice (const :tag "Default" nil) directory)
  :group 'paw)


(defcustom paw-view-note-meaning-src-lang "sdcv"
  "Language to be used for highlighting sdcv ouput in meaning section of paw-view-note buffer."
  :group 'paw
  :type 'string)

(defvar paw-sdcv-fail-notify-string "没有发现解释也... \n用更多的词典查询一下吧! ^_^"
  "User notification message on failed search.")


(defvar paw-sdcv-mode-font-lock-keywords
  '(;; Dictionary name
    ("^-->\\(.*\\)\n-" . (1 font-lock-type-face))
    ;; Search word
    ("^-->\\(.*\\)[ \t\n]*" . (1 font-lock-function-name-face))
    ;; Serial number
    ("\\(^[0-9] \\|[0-9]+:\\|[0-9]+\\.\\)" . (1 font-lock-constant-face))
    ;; Type name
    ("^<<\\([^>]*\\)>>$" . (1 font-lock-comment-face))
    ;; Phonetic symbol
    ("^/\\([^>]*\\)/$" . (1 font-lock-string-face))
    ("^\\[\\([^]]*\\)\\]$" . (1 font-lock-string-face)))
  "Expressions to highlight in `sdcv-mode'.")

(define-derived-mode paw-sdcv-mode nil "paw-sdcv"
  "Major mode to look up word through sdcv.
\\{paw-sdcv-mode-map}

Turning on Text mode runs the normal hook `paw-sdcv-mode-hook'."
  (setq font-lock-defaults '(paw-sdcv-mode-font-lock-keywords t))
  (setq buffer-read-only t)
  (set (make-local-variable 'outline-regexp) "^-->.*\n-->"))

(defun paw-sdcv-kill-process ()
  (interactive)
  (when (process-live-p paw-sdcv-running-process )
    (kill-process paw-sdcv-running-process)
    (setq paw-sdcv-running-process nil)))

(defun paw-sdcv-region-or-word ()
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word t)))

(defun paw-sdcv-search-with-dictionary-async (word buffer)
  "Search some WORD with `paw-sdcv-dictionary-list' and update BUFFER."
  (paw-sdcv-kill-process)
  (let* ((word (or word (paw-sdcv-region-or-word)))
         (dictionary-list (if paw-sdcv-dictionary-list
                              paw-sdcv-dictionary-list
                            (if (boundp 'sdcv-dictionary-simple-list)
                                sdcv-dictionary-simple-list
                              (error "Please define `paw-sdcv-dictionary-list' or `sdcv-dictionary-simple-list'")))))
    (paw-sdcv-translate-result-async word dictionary-list buffer)))


(defun paw-sdcv-translate-result-async (word dictionary-list buffer)
  "Call sdcv to search WORD in DICTIONARY-LIST.
Show results on BUFFER."
  (let* ((arguments (cons word (mapcan (lambda (d) (list "-u" d)) dictionary-list))))
    (paw-sdcv-start-process buffer arguments)))

(defun paw-sdcv-start-process (&optional buffer &rest arguments)
  "Call `paw-sdcv-program' with ARGUMENTS.
Result is parsed as json."
  (let* ((original-output-buffer (get-buffer "*sdcv-output*"))
         (output-buffer (if (buffer-live-p original-output-buffer)
                            (progn (kill-buffer original-output-buffer)
                                   (get-buffer-create "*sdcv-output*") )
                          (get-buffer-create "*sdcv-output*") ))
         (original-error-buffer (get-buffer "*sdcv-error*"))
         (error-buffer (if (buffer-live-p original-error-buffer)
                           (progn (kill-buffer original-error-buffer)
                                  (get-buffer-create "*sdcv-error*") )
                         (get-buffer-create "*sdcv-error*") )) ; Buffer for stderr
         (filter 'paw-sdcv-process-filter)
         (lang-env (concat "LANG=" paw-sdcv-env-lang))
         (process-environment (cons lang-env process-environment))
         (process (make-process
                   :name "sdcv"
                   :buffer output-buffer
                   :noquery t
                   :command (append (list paw-sdcv-program "--non-interactive" "--json-output" "--utf8-input")
				    (when paw-sdcv-exact-match
				      (list "--exact-search"))
                                    (when paw-sdcv-only-data-dir
                                      (list "--only-data-dir"))
                                    (when paw-sdcv-dictionary-data-dir
                                      (list "--data-dir" (expand-file-name paw-sdcv-dictionary-data-dir)))
                                    (car arguments))
                   :filter filter
                   :sentinel (lambda (proc event)
			       (paw-sdcv-process-sentinel proc event buffer))
                   :stderr error-buffer)))
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
      (if (buffer-live-p buffer)
          (with-current-buffer buffer
            (let* ((buffer-read-only nil)
                   (result (mapconcat
                            'paw-sdcv-format-result
                            json-responses
                            ""))
                   (result (if (string-empty-p result)
                               paw-sdcv-fail-notify-string
                             (replace-regexp-in-string "^\\*" "-" result))))
              (save-excursion
                (goto-char (point-min))
                (search-forward "** Meaning" nil t)
                (org-mark-subtree)
                (forward-line)
                (delete-region (region-beginning) (region-end))
                (if (string= paw-view-note-meaning-src-lang "org")
                    (paw-insert-and-make-overlay (format "%s" result) 'face `(:background ,paw-view-note-background-color :extend t))
                  (progn
                    (paw-insert-and-make-overlay "#+BEGIN_SRC paw-sdcv\n" 'invisible t)
                    (insert (format "%s" result))
                    (paw-insert-and-make-overlay "#+END_SRC" 'invisible t)))
                (insert "\n")))
            (deactivate-mark))))))

(provide 'paw-sdcv)
