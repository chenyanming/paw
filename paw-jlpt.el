;;; paw-jlpt.el -*- lexical-binding: t; -*-
(require 'paw-vars)

(defvar paw-jlpt-program (concat (file-name-directory load-file-name) "paw-jlpt.py")
  "Path to jlpt program.")

(defcustom paw-jlpt-db (concat (file-name-directory load-file-name) "japanese.db")
  "Path to jplt database. Made by
git clone https://github.com/chenyanming/yomichan-jlpt-vocab
cd yomichan-jlpt-vocab
python make_dictionary_db.py"
  :type 'string
  :group 'paw-jlpt)

(defcustom paw-jlpt-default-known-words-file nil
  "Default file for known words, when you delete unknown words, it will be save the here.")

(defcustom paw-jlpt-known-words-files nil
  "Path to the known words, known words will be skipped by jlpt.
If csv, the first column is the word, and comma or tab seperated.
For other file types, one word one line."
  :type 'string
  :group 'paw-jlpt)

(defcustom paw-jlpt-tags "n5 n4 n3 n2 n1"
  "Tags for querying japanese words, set it part of: 'n5 n4 n3 n2 n1'.")

(defvar paw-jlpt-running-process nil)

(defun paw-jlpt-kill-process ()
  (interactive)
  (when (process-live-p paw-jlpt-running-process )
    (kill-process paw-jlpt-running-process)
    (setq paw-jlpt-running-process nil)))

(defun paw-jlpt-process-filter (proc string)
  "Accumulates the strings received from the ECDICT process."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (insert string)) ))

(defun paw-jlpt-command (string &optional sentinel search-type)
  "Segments a STRING of Japanese text using paw-jlpt.py and logs the result asynchronously."
  (paw-jlpt-kill-process)
  (let* ((original-output-buffer (get-buffer "*paw-jlpt-output*"))
         (output-buffer (if (buffer-live-p original-output-buffer)
                            (progn (kill-buffer original-output-buffer)
                                   (get-buffer-create "*paw-jlpt-output*") )
                          (get-buffer-create "*paw-jlpt-output*") ))
         (paw-jlpt-process (make-process
                          :name "JLPT"
                          :buffer output-buffer
                          :noquery t
                          :command `(,paw-python-program
                                     ,paw-jlpt-program
                                     ,paw-jlpt-db
                                     ,search-type
                                     ,string
                                     ,paw-jlpt-tags
                                     ,(if paw-jlpt-known-words-files
                                          (mapconcat #'identity paw-jlpt-known-words-files ",")
                                        ""))
                          :filter 'paw-jlpt-process-filter
                          :sentinel (if sentinel sentinel 'paw-jlpt-process-sentinel))))
    (setq paw-jlpt-running-process paw-jlpt-process)
    (with-current-buffer output-buffer
      (setq-local original-string string))
    (process-send-eof paw-jlpt-process)))

(provide 'paw-jlpt)
