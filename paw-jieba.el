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

(provide 'paw-jieba)
