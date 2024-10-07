(defconst paw-hsk-wordlist-urls '(("hsk1" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List/HSK%201.txt")
				  ("hsk2" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List/HSK%202.txt")
				  ("hsk3" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List/HSK%203.txt")
				  ("hsk4" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List/HSK%204.txt")
				  ("hsk5" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List/HSK%205.txt")
				  ("hsk6" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List/HSK%206.txt")
				  ("hsk7-to-9" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List/HSK%207-9.txt")))

;; (defconst paw-hsk-wordlist-urls '(("hsk1" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List%20(Meaning)/HSK%201.tsv")
;; 				  ("hsk2" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List%20(Meaning)/HSK%202.tsv")
;; 				  ("hsk3" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List%20(Meaning)/HSK%203.tsv")
;; 				  ("hsk4" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List%20(Meaning)/HSK%204.tsv")
;; 				  ("hsk5" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List%20(Meaning)/HSK%205.tsv")
;; 				  ("hsk6" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List%20(Meaning)/HSK%206.tsv")
;; 				  ("hsk7-to-9" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List%20(Meaning)/HSK%207-9.tsv")))

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

;; (with-temp-buffer
;;   (insert-file-contents
;;    (expand-file-name (format "%s.tsv" "hsk1") paw-hsk-dir))
;;   (setq hsk1-plist nil)
;;   (goto-char (point-min))
;;   (while (not (eobp))
;;     (let* ((line (thing-at-point 'line))
;; 	   (split (string-split line "\t"))
;; 	   (trad (nth 0 split))
;; 	   (simp (nth 1 split))
;; 	   (pinyin (nth 2 split))
;; 	   (meaning (substring  (nth 3 split) 0 -1))
;; 	   (entry `(:traditional ,trad :simplified ,simp :pinyin ,pinyin :meaning, meaning)))
;;       (push entry hsk1-plist)
;;       (forward-line 1)))
;;   hsk1-plist)

;; (defconst paw-hsk-wordlist-urls '(("hsk1" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List%20(Meaning)/HSK%201.tsv")
;; 				  ("hsk2" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List%20(Meaning)/HSK%202.tsv")
;; 				  ("hsk3" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List%20(Meaning)/HSK%203.tsv")
;; 				  ("hsk4" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List%20(Meaning)/HSK%204.tsv")
;; 				  ("hsk5" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List%20(Meaning)/HSK%205.tsv")
;; 				  ("hsk6" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List%20(Meaning)/HSK%206.tsv")
;; 				  ("hsk7-to-9" . "https://raw.githubusercontent.com/krmanik/HSK-3.0/refs/heads/main/HSK%20List%20(Meaning)/HSK%207-9.tsv")))

;; Add check if file exists logic
(defun paw-hsk-make-big-word-list ()
  "Remove duplicate words from HSK word lists and combine into one list."
  (setq paw-hsk-levels (mapcar (lambda (level)
				 (car level)) paw-hsk-wordlist-urls))
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
    (write-file (expand-file-name (format "%s.txt" "hsk-all") paw-hsk-dir))))

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

(paw-hsk-make-word-alist)

(setq hsk-db "~/notes/hsk/hsk.db")

(defcustom paw-hsk-default-known-words-file nil
  "Default file for known words, when you delete unknown words, it will be save the here.")

(defcustom paw-hsk-levels-to-highlight "hsk4 hsk5 hsk6 hsk7-to-9"
  "HSK levels for which paw should highlight words.")

(defun paw-hsk-make-easy-word-list ()
  (setq paw-hsk-levels (mapcar (lambda (level)
				 (car level)) paw-hsk-wordlist-urls))
  (let* ((highlight (string-split paw-hsk-levels-to-highlight " "))
	 (easy-levels (seq-difference paw-hsk-levels highlight)))
    (setq paw-hsk-easy-words
	  (flatten-list 
	   (mapcar (lambda (level) (alist-get level paw-hsk-all-words nil nil 'string-equal))
		   easy-levels)))))

(paw-hsk-make-easy-word-list)


(defconst paw-hsk-chinese-punctuation
  '("，" "⁣" "。"))

;; (let* ((sentence "画面上，天空是铁青色混合着火焰的颜色，唯一的一株巨树矗立着，已经枯死的树枝向着四面八方延伸，织成一张密网，支撑住皲裂的天空")
;;        (jb (jieba-cut sentence))
;;        (words (seq-difference 
;; 	       (seq-difference jb paw-hsk-easy-words)
;; 	       paw-hsk-chinese-punctuation)))
;;   (mapcar #'my/chinese-tonify-word words))

(provide 'paw-hsk)
