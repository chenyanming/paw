;;; paw-koreader.el -*- lexical-binding: t; -*-

(require 'paw-vars)

(defvar paw-koreader-program (concat (file-name-directory load-file-name) "paw-koreader.py")
  "Path to koreader program.")

(defcustom paw-koreade-vocabulary-builder-db (concat (file-name-directory load-file-name) "vocabulary_builder.sqlite")
  "Path to jplt database. Made by
git clone https://github.com/chenyanming/yomichan-koreader-vocab
cd yomichan-koreader-vocab
python make_dictionary_db.py"
  :type 'string
  :group 'paw-koreader)

(defcustom paw-koreader-known-words-files nil
  "Path to the known words, known words will be skipped by koreader.
If csv, the first column is the word, and comma or tab seperated.
For other file types, one word one line."
  :type 'string
  :group 'paw-koreader)

(defvar paw-koreader-running-process nil)

(defun paw-koreader-kill-process ()
  (interactive)
  (when (process-live-p paw-koreader-running-process )
    (kill-process paw-koreader-running-process)
    (setq paw-koreader-running-process nil)))

(defun paw-koreader-process-filter (proc string)
  "Accumulates the strings received from the ECDICT process."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (insert string)) ))

(defun paw-koreader-db-command (string sentinel &optional search-type)
  "Segments a STRING of Japanese text using paw-koreader.py and logs the result asynchronously."
  (paw-koreader-kill-process)
  (let* ((original-output-buffer (get-buffer "*paw-koreader-output*"))
         (output-buffer (if (buffer-live-p original-output-buffer)
                            (progn (kill-buffer original-output-buffer)
                                   (get-buffer-create "*paw-koreader-output*") )
                          (get-buffer-create "*paw-koreader-output*") ))
         (paw-koreader-process (make-process
                          :name "koreader"
                          :buffer output-buffer
                          :noquery t
                          :command `(,paw-python-program
                                     ,paw-koreader-program
                                     ,paw-koreade-vocabulary-builder-db
                                     ,(if paw-koreader-known-words-files
                                          (mapconcat #'identity paw-koreader-known-words-files ",")
                                        ""))
                          :filter 'paw-koreader-process-filter
                          :sentinel sentinel)))
    (setq paw-koreader-running-process paw-koreader-process)
    (with-current-buffer output-buffer
      (setq-local original-string string))
    (process-send-eof paw-koreader-process)))


(provide 'paw-koreader)
