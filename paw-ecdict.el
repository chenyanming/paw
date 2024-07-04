;;; paw-ecdict.el -*- lexical-binding: t; -*-
(require 'paw-vars)

(defvar paw-ecdict-program (concat (file-name-directory load-file-name) "paw-ecdict.py")
  "Path to ecdict program.")

(defcustom paw-ecdict-db (concat (file-name-directory load-file-name) "stardict.db")
  "Path to ECDICT database."
  :type 'string
  :group 'paw-ecdict)

(defcustom paw-ecdict-default-known-words-file nil
  "Default file for known words, when you delete unknown words, it will be save the here.")

(defcustom paw-ecdict-known-words-files nil
  "Path to the known words, known words will be skipped by ecdict.
If csv, the first column is the word, and comma or tab seperated.
For other file types, one word one line."
  :type 'string
  :group 'paw-ecdict)

(defcustom paw-ecdict-frq -1
  "Minimal Frequency (frp from
https://github.com/skywind3000/ECDICT) threshold for querying
english words. Words tat less than it would not be queried."
  :type 'integer
  :group 'paw-ecdict)

(defcustom paw-ecdict-bnc -1
  "Minimal Frequency (bnc from
https://github.com/skywind3000/ECDICT) threshold for querying
english words. Words tat less than it would not be queried."
  :type 'integer
  :group 'paw-ecdict)

(defcustom paw-ecdict-tags "cet4 cet6 ielts toefl gre"
  "Tags for querying english words, set it part of: 'zk gk ky cet4 cet6 ielts toefl gre empty'.")

(defcustom paw-ecdict-oxford 1
  "Whether within oxford 3000
0: not in oxford 3000
1: in oxford 3000 ")

(defcustom paw-ecdict-collins-max-level 5
  "The max collins level, if any")

(defcustom paw-ecdict-show-tags-p nil
  "Whether show tags in the result.")

(defcustom paw-ecdict-show-transaltion-p t
  "Whether show translation (Chinese) in the result.")

(defcustom paw-ecdict-show-definition-p t
  "Whether show definition (English) in the result.")

(defcustom paw-ecdict-show-exchange-p t
  "Whether show exchange in the result.")

(defvar paw-ecdict-running-process nil)

;;;###autoload
(defun paw-ecdict-kill-process ()
  (interactive)
  (when (process-live-p paw-ecdict-running-process )
    (kill-process paw-ecdict-running-process)
    (setq paw-ecdict-running-process nil)))



(defun paw-ecdict-process-filter (proc string)
  "Accumulates the strings received from the ECDICT process."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (insert string)) ))

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

(defun paw-ecdict-command (string &optional sentinel search-type)
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
                          :noquery t
                          :command `(,paw-python-program
                                     ,paw-ecdict-program
                                     ,paw-ecdict-db
                                     ,search-type
                                     ,string
                                     ,paw-ecdict-tags
                                     ,(number-to-string paw-ecdict-oxford)
                                     ,(number-to-string paw-ecdict-collins-max-level)
                                     ,(number-to-string paw-ecdict-bnc)
                                     ,(number-to-string paw-ecdict-frq)
                                     ,(if paw-ecdict-known-words-files
                                          (mapconcat #'identity paw-ecdict-known-words-files ",")
                                        ""))
                          :filter 'paw-ecdict-process-filter
                          :sentinel (if sentinel sentinel 'paw-ecdict-process-sentinel))))
    (setq paw-ecdict-running-process paw-ecdict-process)
    (with-current-buffer output-buffer
      (setq-local original-string string))
    (process-send-eof paw-ecdict-process)))

(defun paw-ecdict-format-string (phonetic translation definition collins oxford tag bnc frq exchange)
  (format "%s%s%s%s%s"
          (if (and (stringp phonetic) (not (string= phonetic "")))
              (format "[%s]\n\n" phonetic)
            "")
          (if paw-ecdict-show-transaltion-p
              (if (and (stringp translation) (not (string= translation "")))
                  (format "%s" translation)
                "")
            "")
          (if paw-ecdict-show-definition-p
              (if (and (stringp definition) (not (string= definition "")))
                  (format "\n\n%s" definition)
                "")
            "")
          (if paw-ecdict-show-tags-p
              (if (or collins oxford tag bnc frq)
                  (format "\n\n%s%s%s%s"
                          (if collins (format "collins: %s, " collins) "")
                          (if oxford (format "oxford: %s, " oxford) "")
                          (if (and (stringp tag) (not (string= tag ""))) (format "%s, " tag) "")
                          (if (or bnc frq) (format "%s/%s" bnc frq) ""))
                "")
            "")
          (if paw-ecdict-show-exchange-p
              (if (and (stringp exchange) (not (string= exchange "")))
                  (format "\n\n%s" exchange)
                "")
            ""))
  )

(provide 'paw-ecdict)
