;;; paw-dictionary.el -*- lexical-binding: t; -*-
(require 'paw-ecdict)
(require 'eldoc-box nil t)

(defun paw-dictionary-search(&optional word lang)
  "Query the word using wordlists as dictionaries:
`paw-ecdict-wordlist-files' (en) or
`paw-jlpt-wordlist-files'(ja)."
  (interactive)
  (let* ((word (or word (thing-at-point 'word t) ))
         (lang (or lang (paw-check-language word) )))
    (cond ((string= lang "en")
           (paw-ecdict-csv-command word 'paw-dictionary-search-sentinel-english "WORDLIST")
           ;; (paw-ecdict-db-command word 'paw-dictionary-search-sentinel-english "WORD")
           )
          ((string= lang "ja")
           (paw-jlpt-csv-command word 'paw-dictionary-search-sentinel-japanese "WORDLIST")
           ;; (paw-jlpt-db-command word 'paw-dictionary-search-sentinel-japanese "WORD")
           )
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
           (json-response (car (json-parse-string buffer-content :object-type 'plist :array-type 'list :null-object nil) ))
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
             (output (paw-ecdict-format-string phonetic translation definition collins oxford tag bnc frq exchange "\n"))) ; features just a combination of other fields

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

        ;; use eldoc-box to display the definition if available
             (if word
                 (if (eq last-command 'paw-view-note-in-minibuffer)
                     (if (boundp 'eldoc--doc-buffer)
                         (progn
                           (let ((eldoc-box-max-pixel-width 512)
                                 (eldoc-box-max-pixel-height 1024)
                                 (eldoc-box-position-function
                                  #'eldoc-box--default-at-point-position-function))
                             (eldoc-box--display
                              (with-current-buffer eldoc--doc-buffer
                                output)))
                           (setq eldoc-box--help-at-point-last-point (point))
                           (run-with-timer 0.1 nil #'eldoc-box--help-at-point-cleanup) )
                       (message (paw-remove-spaces output "en")))
                   (if (boundp 'eldoc--doc-buffer)
                       (progn
                         (let ((eldoc-box-max-pixel-width 512)
                               (eldoc-box-max-pixel-height 1024)
                               (eldoc-box-position-function
                                (lambda (width _)
                                    (cons (pcase (eldoc-box--window-side) ; x position + a little padding (16)
                                            ;; display doc on right
                                            ('left (- (frame-outer-width (selected-frame)) width 10))
                                            ;; display doc on left
                                            ('right 10))
                                          ;; y position + a little padding (16)
                                          32))))
                           (eldoc-box--display
                            (with-current-buffer eldoc--doc-buffer
                              output)))
                         (setq eldoc-box--help-at-point-last-point (point))
                         (run-with-timer 0.1 nil #'eldoc-box--help-at-point-cleanup) )
                     (message (paw-remove-spaces output "en"))))
               (message "No definition")))

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
           output
           order)
      ;; find by kanji
      (setq order 1)
      (dolist (resp json-responses candidates)
        (setq order (+ order 1))
        (let* ((word (plist-get resp :kanji))
               (kana (plist-get resp :kana))
               (origin (plist-get resp :origin))
               (level (plist-get resp :level))
               (waller_definition (plist-get resp :waller_definition)))
          (if (string= (alist-get 'word (car candidates)) word)
              (setf (alist-get 'exp (car candidates))
                    (format "%s\n%s[%s](%s)\n%s" (alist-get 'exp (car candidates)) kana level origin waller_definition))
            (push (paw-new-entry word :lang "ja"
                                 :exp (if kana
                                          (format "%s[%s](%s)\n%s" kana level origin waller_definition)
                                        waller_definition)
                                 :created-at (format-time-string "%Y-%m-%d %H:%M:%S" (time-add (current-time) (seconds-to-time order)))
                                 :add-to-known-words t ;; so that it could be added into default known file
                                 ) candidates))))

      ;; find by kana
      (dolist (resp json-responses candidates)
        (setq order (+ order 1))
        (let* ((word (plist-get resp :kana))
               (kanji (plist-get resp :kanji))
               (origin (plist-get resp :origin))
               (level (plist-get resp :level))
               (wordp (not (string= word "")))
               (waller_definition (plist-get resp :waller_definition)))
          (if (string= (alist-get 'word (car candidates)) word)
              (setf (alist-get 'exp (car candidates))
                    (format "%s\n%s[%s](%s)\n%s" (alist-get 'exp (car candidates)) kanji level origin waller_definition))
            (push (paw-new-entry word :lang "ja"
                                 :exp (if kanji
                                          (format "%s[%s](%s)\n%s" kanji level origin waller_definition)
                                        waller_definition)
                                 :created-at (format-time-string "%Y-%m-%d %H:%M:%S" (time-add (current-time) (seconds-to-time order)))
                                 :add-to-known-words t ;; so that it could be added into default known file
                                 ) candidates))))

      (dolist (cand candidates output)
        (push (alist-get 'exp cand) output))

      (if (eq last-command 'paw-view-note-in-minibuffer)
          (if (boundp 'eldoc--doc-buffer)
              (progn
                (let ((eldoc-box-max-pixel-width 512)
                      (eldoc-box-max-pixel-height 1024)
                      (eldoc-box-position-function
                       #'eldoc-box--default-at-point-position-function))
                  (eldoc-box--display
                   (with-current-buffer eldoc--doc-buffer
                     (string-join output " | \n"))))
                (setq eldoc-box--help-at-point-last-point (point))
                (run-with-timer 0.1 nil #'eldoc-box--help-at-point-cleanup) )
            (message (paw-remove-spaces output "en")))
        (if (boundp 'eldoc--doc-buffer)
            (progn
              (let ((eldoc-box-max-pixel-width 512)
                    (eldoc-box-max-pixel-height 1024)
                    (eldoc-box-position-function
                     (lambda (width _)
                       (cons (pcase (eldoc-box--window-side) ; x position + a little padding (16)
                               ;; display doc on right
                               ('left (- (frame-outer-width (selected-frame)) width 10))
                               ;; display doc on left
                               ('right 10))
                             ;; y position + a little padding (16)
                             32))))
                (eldoc-box--display
                 (with-current-buffer eldoc--doc-buffer
                   (string-join output " | \n"))))
              (setq eldoc-box--help-at-point-last-point (point))
              (run-with-timer 0.1 nil #'eldoc-box--help-at-point-cleanup) )
          (message (paw-remove-spaces (string-join output " | \n") "en")))))))



(provide 'paw-dictionary)
