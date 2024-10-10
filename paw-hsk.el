(defconst paw-hsk-wordlist-urls '(("hsk1" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List/HSK%201.txt")
				  ("hsk2" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List/HSK%202.txt")
				  ("hsk3" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List/HSK%203.txt")
				  ("hsk4" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List/HSK%204.txt")
				  ("hsk5" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List/HSK%205.txt")
				  ("hsk6" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List/HSK%206.txt")
				  ("hsk7-to-9" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List/HSK%207-9.txt")))

(defcustom paw-hsk-dir (expand-file-name "hsk/" org-directory)
  "Path to HSK wordlist files."
  :type 'string
  :group 'paw-hsk)

(defcustom paw-curl-options nil
  "Additional options for `curl' command."
  :type '(repeat (string :tag "String"))
  :group 'paw-hsk)

(defcustom paw-hsk-default-known-words-file nil
  "Default file for known words, when you delete unknown words, it will be save the here.")

(defcustom paw-hsk-levels-to-highlight "hsk4 hsk5 hsk6 hsk7-to-9"
  "HSK levels for which paw should highlight words.")

(defun paw-hsk-download-wordlist (level)
  "Download wordlist for HSK LEVEL. File will be saved to `paw-hsk-dir.'"
  (let* ((options (mapconcat #'identity paw-curl-options " "))
	 (link (alist-get level paw-hsk-wordlist-urls nil nil 'string-equal))
	 (output (expand-file-name (format "%s.txt" level) paw-hsk-dir)))
    (apply #'call-process
	   (executable-find "curl")
	   nil nil nil
	   (flatten-tree `(,paw-curl-options
			   ,link
			   "--output"
			   ,output)))))

(defun paw-hsk-download-check-wordlists ()
  "Check if all HSK level word lists can be found in `paw-hsk-dir'. If not download them."
  (interactive)
  (mapcar (lambda (level)
	    (let ((l (car level)))
	      (unless 
		  (file-exists-p (expand-file-name (format "%s.txt" l) paw-hsk-dir))
		(paw-hsk-download-wordlist l)))) paw-hsk-wordlist-urls)
  (format "All HSK Levels downloaded to %s" paw-hsk-dir))


;; Add check if file exists logic
(defun paw-hsk-make-big-word-list ()
  "Remove duplicate words from HSK word lists and combine into one list."
  (paw-hsk-download-check-wordlists)
  (setq paw-hsk-levels (mapcar (lambda (level)
				 (car level)) paw-hsk-wordlist-urls))
  (unless (file-exists-p (expand-file-name "hsk-all.txt" paw-hsk-dir))
    (with-temp-buffer
      (goto-char (point-max))
      (mapcar (lambda (level)
		(insert (format "\nhsk%s\n" level))
		(insert-file-contents 
		 (expand-file-name (format "%s.txt" level) paw-hsk-dir))
		(goto-char (point-max)))
	      paw-hsk-levels)
      (delete-duplicate-lines (point-min) (point-max))
      (goto-char (point-max))
      (write-file (expand-file-name (format "%s.txt" "hsk-all") paw-hsk-dir)))))

;; Add check if file exists logic
(defun paw-hsk-make-word-alist ()
  (with-temp-buffer
    (setq paw-hsk-all-words nil)
    (insert-file-contents
     (expand-file-name (format "%s.txt" "hsk-all") paw-hsk-dir))
    (let* ((l 
	    (string-split (buffer-substring-no-properties (point-min) (point-max)) "\nhsk"))
	   (level (car l)))
      (mapcar (lambda (level)
		(push (string-split level "\n") paw-hsk-all-words)) l))
    paw-hsk-all-words)
  (setq paw-hsk-all-words (butlast paw-hsk-all-words)))

(defun paw-hsk-make-easy-word-list ()
  (setq paw-hsk-levels (mapcar (lambda (level)
				 (car level)) paw-hsk-wordlist-urls))
  (let* ((highlight (string-split paw-hsk-levels-to-highlight " "))
	 (easy-levels (seq-difference paw-hsk-levels highlight)))
    (setq paw-hsk-easy-words
	  (flatten-list 
	   (mapcar (lambda (level) (alist-get level paw-hsk-all-words nil nil 'string-equal))
		   easy-levels)))))


;; Integrate `paw-hsk-download-check-wordlists'
(defun paw-hsk-update-word-lists ()
  "Update paw Chinese word lists if `hsk-all.txt' can be found in `paw-hsk-dir'."
  (interactive)
  (if (file-exists-p (expand-file-name "hsk-all.txt" paw-hsk-dir))
      (progn 
	(paw-hsk-make-word-alist)
	(paw-hsk-make-easy-word-list))
    (progn
      (paw-hsk-make-big-word-list)
      (paw-hsk-make-word-alist)
      (paw-hsk-make-easy-word-list))))

;; (paw-hsk-make-easy-word-list)

;; TODO very limited
(defconst paw-hsk-chinese-punctuation
  '("，" "⁣" "。"))

(provide 'paw-hsk)
