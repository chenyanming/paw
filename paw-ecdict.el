;;; paw-ecdict.el -*- lexical-binding: t; -*-
(require 'paw-vars)

(defvar paw-ecdict-program (concat (file-name-directory load-file-name) "paw-ecdict.py")
  "Path to ecdict program.")

(defcustom paw-ecdict-db (concat (file-name-directory load-file-name) "ecdict.db")
  "Path to ECDICT database."
  :type 'string
  :group 'paw-ecdict)

(defcustom paw-ecdict-frq 3000
  "Minimal Frequency (frp 当代语料库词频顺序, from
https://github.com/skywind3000/ECDICT) threshold for querying
english words. Words tat less than it would not be queried."
  :type 'integer
  :group 'paw-ecdict)

(defvar paw-ecdict-running-process nil)

;;;###autoload
(defun paw-ecdict-kill-process ()
  (interactive)
  (when (process-live-p paw-ecdict-running-process )
    (kill-process paw-ecdict-running-process)
    (setq paw-ecdict-running-process nil)))



(defun paw-ecdict-process-filter (proc string)
  "Accumulates the strings received from the ECDICT process."
  (with-current-buffer (process-buffer proc)
    (insert string)))

(defun paw-ecdict-process-sentinel (proc _event)
  "Handles the ecdict process termination event."
  (when (eq (process-status proc) 'exit)
    (let* ((json-object-type 'plist)
           (json-array-type 'list)
           (buffer-content (with-current-buffer (process-buffer proc)
                             (buffer-string)))
           (json-responses (json-read-from-string buffer-content)))
      (dolist (resp json-responses)
        (let* ((id (plist-get resp :id))
               (word (plist-get resp :word))
               (sw (plist-get resp :sw))
               (phonetic (plist-get resp :phonetic))
               (definition (plist-get resp :definition))
               (translation (plist-get resp :translation))
               (pos (plist-get resp :pos))
               (collins (plist-get resp :collins))
               (oxford (plist-get resp :oxford))
               (tag (plist-get resp :tag))
               (bnc (plist-get resp :bnc))
               (frq (plist-get resp :frq))
               (exchange (plist-get resp :exchange))
               (detail (plist-get resp :detail))
               (audio (plist-get resp :audio)))
          (message "id: %s, word: %s, sw: %s, phonetic: %s, definition: %s, translation: %s, pos: %s, collins: %s, oxford: %s, tag: %s, bnc: %s, frq: %s, detail: %s, audio: %s"
                   id word sw phonetic definition translation pos collins oxford tag bnc frq detail audio)))
      ;; (let ((segmented-text (mapconcat
      ;;                        (lambda (resp) (plist-get resp :surface))
      ;;                        json-responses
      ;;                        " ")))
      ;;   (message "Segmented text: %s" segmented-text))
      ;; (pp buffer-content)
      ;; (pp json-responses)

      )))

(defun paw-ecdict-command (string &optional sentinel)
  "Segments a STRING of Japanese text using ECDICT and logs the result asynchronously."
  (paw-ecdict-kill-process)
  (let* ((original-output-buffer (get-buffer "*paw-ecdict-output*"))
         (output-buffer (if (buffer-live-p original-output-buffer)
                            (progn (kill-buffer original-output-buffer)
                                   (get-buffer-create "*paw-ecdict-output*") )
                          (get-buffer-create "*paw-ecdict-output*") ))
         (paw-ecdict-process (make-process
                          :name "ECDICT"
                          :buffer output-buffer
                          :command `(,paw-python-program ,paw-ecdict-program ,paw-ecdict-db ,string)
                          :filter 'paw-ecdict-process-filter
                          :sentinel (if sentinel sentinel 'paw-ecdict-process-sentinel))))
    (setq paw-ecdict-running-process paw-ecdict-process)
    (with-current-buffer output-buffer
      (setq-local original-string string))
    (process-send-eof paw-ecdict-process)))

(provide 'paw-ecdict)