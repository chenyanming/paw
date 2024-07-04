;;; paw-dictionary.el -*- lexical-binding: t; -*-
(require 'paw-ecdict)

(defun paw-dictionary-search(&optional word)
  (interactive)
  (let* ((word (or word (thing-at-point 'word t) ))
         (lang (paw-check-language word)))
    (cond ((string= lang "en")
           (paw-ecdict-command word 'paw-dictionary-search-sentinel-english "WORD"))
          ((string= lang "ja")
           (paw-jlpt-command word 'paw-dictionary-search-sentinel-japanese "WORD"))
          (t (message "Unsupported language %s" lang)))))

(defun paw-dictionary-search-sentinel-english (proc _event)
  "Handles the english process termination event."
  (when (eq (process-status proc) 'exit)
    (let* ((json-object-type 'plist)
           (json-array-type 'list)
           (original-string (with-current-buffer (process-buffer proc)
                              original-string))
           (buffer-content (with-current-buffer (process-buffer proc)
                             (buffer-string)))
           (json-response (json-parse-string buffer-content :object-type 'plist :array-type 'list :null-object nil))
           (message-log-max nil))
      (let* ((id (plist-get json-response :id))
             (word (plist-get json-response :word))
             (sw (plist-get json-response :sw))
             (phonetic (plist-get json-response :phonetic))
             (definition (plist-get json-response :definition))
             (translation (plist-get json-response :translation))
             (pos (plist-get json-response :pos))
             (collins (plist-get json-response :collins))
             (oxford  (plist-get json-response :oxford))
             (tag (plist-get json-response :tag))
             (bnc (plist-get json-response :bnc))
             (frq (plist-get json-response :frq))
             (exchange (plist-get json-response :exchange))
             (detail (plist-get json-response :detail))
             (audio (plist-get json-response :audio))
             (output (paw-ecdict-format-string phonetic translation definition collins oxford tag bnc frq exchange))) ; features just a combination of other fields

             ;; skip the similar word in db
             ;; FIXME: this could be done in python as well
             ;; (unless (paw-check-word-exist-p word)
             ;;   (pp (paw-new-entry word :lang "en"
             ;;                  :exp (format "_collins_: %s, _oxford_: %s, _tag_: %s, _bnc_ %s, _frq_: %s, _exchange_: %s\n%s\n%s"
             ;;                               collins oxford tag bnc frq exchange translation definition )
             ;;                  ;; FIXME: use created-at to store the order,
             ;;                  ;; because new words are not in db, can not
             ;;                  ;; compare the time with the words in db
             ;;                  :created-at (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))
             ;;                  :add-to-known-words t ;; so that it could be added into default known file
             ;;                  ) ))

             ;; (insert (replace-regexp-in-string "[ \n]+" " " (replace-regexp-in-string "^[ \n]+" "" (format "%s/%s" translation definition ))) )
             (if word
                 (message (paw-remove-spaces output "en"))
               (message "No definition"))
           )


      ;; (with-current-buffer (current-buffer)
      ;;   (paw-show-all-annotations candidates))

      )))


(defun paw-dictionary-search-sentinel-japanese (proc _event)
  "Handles the japanese process termination event."
  (when (eq (process-status proc) 'exit)
    (let* ((json-object-type 'plist)
           (json-array-type 'list)
           (original-string (with-current-buffer (process-buffer proc)
                              original-string))
           (buffer-content (with-current-buffer (process-buffer proc)
                             (buffer-string)))
           (json-responses (json-parse-string buffer-content :object-type 'plist :array-type 'list))
           (message-log-max nil)
           candidates
           order)
      ;; find by kanji
      (dolist (resp json-responses candidates)
        (when-let* ((word (plist-get resp :kanji))
                    (kana (plist-get resp :kana))
                    (origin (plist-get resp :origin))
                    (level (plist-get resp :level))
                    (wordp (not (string= word "")))
                    (waller_definition (plist-get resp :waller_definition))
                   ;; (entry (paw-candidate-by-word word)) ;; check in db, if KNOWN words, would not push
                   ) ; features just a combination of other fields

          ;; skip the similar word in db
          ;; FIXME: this could be done in python as well
          (unless (paw-check-word-exist-p word)
            (if (string= (alist-get 'word (car candidates)) word)
                (progn
                  ;; (message "Found multiple meanings %s" word)
                  (setf (alist-get 'exp (car candidates))
                        (format "%s\n%s[%s](%s)\n%s" (alist-get 'exp (car candidates)) kana level origin waller_definition)) )
              (push (format "%s[%s](%s)\n%s" kana level origin waller_definition) candidates)))))
      ;; find by kana
      (dolist (resp json-responses candidates)
        (when-let* ((word (plist-get resp :kana))
                    (kanji (plist-get resp :kanji))
                    (origin (plist-get resp :origin))
                    (level (plist-get resp :level))
                    (wordp (not (string= word "")))
                    (waller_definition (plist-get resp :waller_definition))
                   ;; (entry (paw-candidate-by-word word)) ;; check in db, if KNOWN words, would not push
                   ) ; features just a combination of other fields

          ;; skip the similar word in db
          ;; FIXME: this could be done in python as well
          (unless (paw-check-word-exist-p word)
            (if (string= (alist-get 'word (car candidates)) word)
                (progn
                  ;; (message "Found multiple meanings %s" word)
                  (setf (alist-get 'exp (car candidates))
                        (format "%s\n%s[%s](%s)\n%s" (alist-get 'exp (car candidates)) kanji level origin waller_definition)) )
              (push (format "%s[%s](%s)\n%s" kanji level origin waller_definition) candidates)))))
      (message candidates))))



(provide 'paw-dictionary)
