;;; paw-dictionary.el -*- lexical-binding: t; -*-
(require 'paw-ecdict)

(defun paw-dictionary-search(&optional word)
  (interactive)
  (let* ((word (or word (thing-at-point 'word t) ))
         (lang (paw-check-language word)))
    (cond ((string= lang "en")
           (paw-ecdict-command word 'paw-dictionary-search-sentinel-english "WORD"))
          ((string= lang "ja")
           (paw-jlpt-command word 'paw-focus-find-unknown-words-sentinel-japanese))
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
           (json-response (json-parse-string buffer-content :object-type 'plist :array-type 'list))
           (message-log-max nil))
      (if-let* ((id (plist-get json-response :id))
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
                (audio (plist-get json-response :audio))) ; features just a combination of other fields

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
             (message
              (paw-remove-spaces
               (format "%s%s%s"
                       (if paw-ecdict-show-tags-p
                           (format "_collins_: %s, _oxford_: %s, _tag_: %s, _bnc_ %s, _frq_: %s, _exchange_: %s\n"
                                   collins oxford tag bnc frq exchange)
                         "")
                       (if paw-ecdict-show-transaltion-p
                           (format "%s\n"
                                   translation )
                         ""
                         )
                       (if paw-ecdict-show-definition-p
                           (format "%s\n"
                                   definition )
                         "")) "en"))
           (message "No Definition"))


      ;; (with-current-buffer (current-buffer)
      ;;   (paw-show-all-annotations candidates))

      )))


(provide 'paw-dictionary)