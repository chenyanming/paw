(require 'jieba nil t)

(require 'sdcv nil t)

(defun paw-jieba-segment-text (text)
  "Segment Chinese text into individual words with jieba-rs."
  (interactive)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let* ((seg (jieba-tokenize (buffer-substring-no-properties (point-min) (point-max))))
	   (segmented-text (mapconcat (lambda (resp) (car resp)) seg paw-non-ascii-word-separator)))
      (erase-buffer)
      (insert segmented-text))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun paw-jieba-segement-buffer (&optional buffer)
  "Segment Chinese text in BUFFER. If no buffer is given, use current buffer."
  (if buffer
      (with-current-buffer buffer
	(paw-jieba-segment-text (buffer-substring-no-properties (point-min) (point-max))))
    (paw-jieba-segment-text (buffer-substring-no-properties (point-min) (point-max)))))

(defun paw-jieba-segment-file (file)
  "Segment Chinese text in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (paw-jieba-segment-text (buffer-substring-no-properties (point-min) (point-max)))))

(defun paw-jieba-segment-and-replace-buffer ()
  (save-excursion
    (let ((seg (paw-jieba-segement-buffer)))
      (erase-buffer)
      (insert seg)))
  (message "Segmentation finished."))

(defcustom paw-chinese-hide-easy-words t
  "Should easy Chinese words be hidden in `paw-view-note'")

(defun paw-chinese-segment-and-remove-easy-words (string)
  (let* ((words (jieba-cut string))
	 (words-no-punct (seq-difference words paw-hsk-chinese-punctuation)))
    (if paw-chinese-hide-easy-words
	(seq-difference (jieba-cut string) paw-hsk-easy-words)
      words-no-punct)))

(defcustom paw-chinese-sdcv-exact-match nil
  "Make SDCV return exact matches only.")

(defun paw-chinese-sdcv-format-dictionary-list (&optional dictionary-list)
  (let* ((dictionary-list (if paw-sdcv-dictionary-list
                              paw-sdcv-dictionary-list
                            sdcv-dictionary-simple-list))
	 (dics (mapcan (lambda (d) (list "-u" d)) dictionary-list)))
    (if paw-chinese-sdcv-exact-match
	(cons "--exact-search" dics)
      dics)))

(defun paw-chinese-format-sdcv-definitions (string)
  (substring
   (mapconcat (lambda (word)
		(mapconcat (lambda (result)
			     (let-alist result
			       (if (string= paw-view-note-meaning-src-lang "org")
				   (format "%s\n" .definition)
				 (format "-->%s\n-->%s%s\n" .dict .word .definition))))
			   (apply #'sdcv-call-process
				  (cons word (paw-chinese-sdcv-format-dictionary-list)))))
	      (paw-chinese-segment-and-remove-easy-words string)
	      ) 1))

(defun paw-chinese-search-function (word buffer)
  (save-excursion
    (if (buffer-live-p buffer)
	(with-current-buffer buffer
          (let* ((buffer-read-only nil)
		 (result (paw-chinese-format-sdcv-definitions word))
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
              (goto-char (point-min))
              (unless (search-forward "** Dictionaries" nil t)
		(search-forward "** Translation" nil t))
              (beginning-of-line)
              ;; (recenter 0)
              ;; (message "Translation completed %s" translation)
              ))
          (deactivate-mark)

          ) ) )
  )

(provide 'paw-jieba)
