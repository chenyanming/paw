(require 'jieba)

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

(provide 'paw-jieba)
