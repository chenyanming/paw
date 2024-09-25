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

(defun paw-hsk-download-wordlist (level)
  "Download wordlist for HSK LEVEL. File will be saved to `paw-hsk-dir.'"
  (let* ((options (mapconcat #'identity paw-curl-options " "))
	 (link (alist-get level paw-hsk-wordlist-urls nil nil 'string-equal))
	 (output (expand-file-name (format "%s.txt" level) paw-hsk-dir))
	 (command (format "curl %s \"%s\" -o %s" options link output)))
    (async-shell-command command)))

(defun paw-hsk-download-check-wordlists ()
  "Check if all HSK level word lists can be found in `paw-hsk-dir'. If not download them."
  (mapcar (lambda (level)
	    (let ((l (car level)))
	      (unless 
		  (file-exists-p (expand-file-name (format "%s.txt" l) paw-hsk-dir))
		(paw-hsk-download-wordlist l)))) paw-hsk-wordlist-urls))

(provide 'paw-hsk)
