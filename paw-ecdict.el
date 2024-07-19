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

(defcustom paw-ecdict-words-tags "a1 a2 b1 b2 c1"
  "Tags for querying english words, set it part of: 'zk gk ky cet4 cet6 ielts toefl gre empty'.")

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

(defcustom paw-ecdict-show-tags-p t
  "Whether show tags in the result.")

(defcustom paw-ecdict-show-translation-p t
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

(defun paw-ecdict-db-command (string &optional sentinel search-type)
  "Segments a STRING of text using ECDICT and logs the result asynchronously."
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


(defun paw-ecdict-csv-command (string &optional sentinel search-type)
  "Segments a STRING of text using ECDICT and logs the result asynchronously."
  (paw-ecdict-kill-process)
  (let* ((original-output-buffer (get-buffer "*paw-ecdict-csv-output*"))
         (output-buffer (if (buffer-live-p original-output-buffer)
                            (progn (kill-buffer original-output-buffer)
                                   (get-buffer-create "*paw-ecdict-csv-output*") )
                          (get-buffer-create "*paw-ecdict-csv-output*") ))
         (paw-ecdict-process (make-process
                          :name "ECDICT-CSV"
                          :buffer output-buffer
                          :noquery t
                          :command `(,paw-python-program
                                     ,paw-ecdict-program
                                     ,paw-default-wordlist-file
                                     ,search-type
                                     ,string
                                     ,paw-ecdict-words-tags
                                     ,(if paw-wordlist-files
                                          (mapconcat #'identity paw-wordlist-files ",")
                                        ""))
                          :filter 'paw-ecdict-process-filter
                          :sentinel (if sentinel sentinel 'paw-ecdict-process-sentinel))))
    (setq paw-ecdict-running-process paw-ecdict-process)
    (with-current-buffer output-buffer
      (setq-local original-string string))
    (process-send-eof paw-ecdict-process)))

(defun paw-ecdict-format-string (phonetic translation definition collins oxford tag bnc frq exchange seperator)
  (format "%s%s%s%s%s"
          (propertize
           (if (and (stringp phonetic) (not (string= phonetic "")))
               (format "[%s]%s" phonetic seperator)
             "")
           'face '(:inherit paw-image-face :height 0.9))
          (propertize
           (if paw-ecdict-show-translation-p
               (if (and (stringp translation) (not (string= translation "")))
                   (format "%s" translation)
                 "")
             "")
           'face '(:inherit default :height 0.9))
          (propertize
           (if paw-ecdict-show-definition-p
               (if (and (stringp definition) (not (string= definition "")))
                   (format "%s%s" seperator definition)
                 "")
             "")
           'face '(:inherit paw-path-face :height 0.9))
          (propertize
           (if paw-ecdict-show-tags-p
              (if (or collins oxford tag bnc frq)
                  (format "%s%s%s%s%s"
                          seperator
                          (if collins (format "collins: %s, " collins) "")
                          (if oxford (format "oxford: %s, " oxford) "")
                          (if (and (stringp tag) (not (string= tag ""))) (format "%s, " tag) "")
                          (if (or bnc frq) (format "%s/%s" bnc frq) ""))
                "")
             "") 'face '(:inherit paw-done-face :height 0.9) )
          (propertize
           (if paw-ecdict-show-exchange-p
              (if (and (stringp exchange) (not (string= exchange "")))
                  (format "%s%s" seperator (paw-ecdict-format-exchange exchange))
                "")
             "") 'face '(:inherit paw-pdf-face :height 0.9) ))
  )

(defun paw-ecdict-format-exchange (str)
  "Reformats an exchange string."
  (let* ((table-en '(("p" . "Past tense")
                     ("d" . "Past participle")
                     ("i" . "Present participle")
                     ("3" . "3rd person singular")
                     ("r" . "Comparative")
                     ("t" . "Superlative")
                     ("s" . "Plural noun form")
                     ("0" . "Lemma")
                     ("1" . "Form of lemma")))
         (table-zh '(("p" . "过去式")
                     ("d" . "过去分词")
                     ("i" . "现在分词")
                     ("3" . "第三人称单数")
                     ("r" . "比较级")
                     ("t" . "最高级")
                     ("s" . "复数形式")
                     ("0" . "基本形式")
                     ("1" . "变化形式")))
         (table (if paw-ecdict-show-translation-p table-zh table-en))
         (pieces (split-string str "/"))
         (results '())
         (first-item '()))
    (dolist (piece pieces)
      (let ((split (split-string piece ":"))
            (meaning '()))
        (if (equal (cl-first split) "1")
            (progn
              (dotimes (i (length (cl-second split)))
                (push (cdr (assoc (string (elt (cl-second split) i)) table)) meaning))
              (setq first-item (list (concat (mapconcat 'identity meaning " & ") "; "))))
          (setq results (append results (list (concat (cdr (assoc (cl-first split) table)) ": " (cl-second split))))))))
    (concat (car first-item) (mapconcat 'identity results ", "))))

(provide 'paw-ecdict)
