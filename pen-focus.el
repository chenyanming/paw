;;; pen/pen-focus.el -*- lexical-binding: t; -*-

(require 'focus nil t)
(require 'kagome nil t)
(require 'svg-lib nil t)
(require 'ecdict nil t)

(defun pen-focus-find-current-thing()
  (interactive)
  (cond ((get-char-property (point) 'pen-entry)
         (pen-view-note))
        (t (let* ((thing (or pen-note-word
                             (if mark-active
                                 (buffer-substring-no-properties (region-beginning) (region-end))
                               (if focus-mode
                                   (buffer-substring-no-properties (car (focus-bounds)) (cdr (focus-bounds)))
                                 (pen-get-sentence-or-line))) ))
                  (lang_word (pen-remove-spaces-based-on-ascii-rate-return-cons thing))
                  (lang (car lang_word))
                  (new-thing (cdr lang_word)))
             (cond ((string= lang "en") (pen-view-note (pen-new-entry new-thing)))
                   ((string= lang "ja") (pen-view-note (pen-new-entry new-thing))))))))

(defun pen-focus-find-current-thing-with-mouse(event)
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No URL chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (pen-focus-find-current-thing))))

(defun pen-focus-find-current-thing-segment-with-mouse(event)
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (if (not (windowp window))
        (error "No URL chosen"))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (pen-focus-find-current-thing-segment))))


(defun pen-focus-find-current-thing-segment()
  (interactive)
  (let* ((thing (or pen-note-word
                    (if mark-active
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (if focus-mode
                          (buffer-substring-no-properties (car (focus-bounds)) (cdr (focus-bounds)))
                        (pen-get-sentence-or-line)))))
         (lang_word (pen-remove-spaces-based-on-ascii-rate-return-cons thing))
         (lang (car lang_word))
         (new-thing (cdr lang_word)))
    (message "Analysing %s..." new-thing)
    (cond ((string= lang "en") (ecdict-command new-thing
                                               'pen-focus-view-note-process-sentinel-english))
          ((string= lang "ja") (kagome-command new-thing 'pen-focus-view-note-process-sentinel-japanese)))))

(defun pen-focus-find-next-thing-segment()
  (interactive)
  (call-interactively 'focus-next-thing)
  (call-interactively  'pen-focus-find-current-thing-segment))

(defun pen-focus-find-prev-thing-segment()
  (interactive)
  (call-interactively 'focus-prev-thing)
  (call-interactively  'pen-focus-find-current-thing-segment))



(defun pen-focus-find-current-thing-segment-japanese()
  (interactive)
  (if (get-char-property (point) 'pen-entry)
      (pen-view-note)
    (kagome-command
     (if mark-active
         (pen-remove-spaces-based-on-ascii-rate (buffer-substring-no-properties (region-beginning) (region-end)) )
       (if focus-mode
           (pen-remove-spaces-based-on-ascii-rate (buffer-substring-no-properties (car (focus-bounds)) (cdr (focus-bounds))) )
         (pen-remove-spaces-based-on-ascii-rate (thing-at-point 'sentence t) )))
     'pen-focus-view-note-process-sentinel-japanese) ))

(defun pen-focus-find-next-thing-segment-japanese()
  (interactive)
  (call-interactively 'focus-next-thing)
  (kagome-command
   (if mark-active
       (pen-remove-spaces-based-on-ascii-rate (buffer-substring-no-properties (region-beginning) (region-end)) )
     (if focus-mode
         (pen-remove-spaces-based-on-ascii-rate (buffer-substring-no-properties (car (focus-bounds)) (cdr (focus-bounds))) )
       (pen-remove-spaces-based-on-ascii-rate (thing-at-point 'sentence t) )))
   'pen-focus-view-note-process-sentinel-japanese))

(defun pen-focus-find-prev-thing-segment-japanese()
  (interactive)
  (call-interactively 'focus-prev-thing)
  (kagome-command
   (if mark-active
       (pen-remove-spaces-based-on-ascii-rate (buffer-substring-no-properties (region-beginning) (region-end)) )
     (if focus-mode
         (pen-remove-spaces-based-on-ascii-rate (buffer-substring-no-properties (car (focus-bounds)) (cdr (focus-bounds))) )
       (pen-remove-spaces-based-on-ascii-rate (thing-at-point 'sentence t) )))
   'pen-focus-view-note-process-sentinel-japanese))


(defun pen-focus-view-note-process-sentinel-japanese (proc _event)
  "Handles the Kagome process termination event."
  (when (eq (process-status proc) 'exit)
    (let* ((json-object-type 'plist)
           (json-array-type 'list)
           (buffer-content (with-current-buffer (process-buffer proc)
                             (buffer-string)))
           (json-responses (json-read-from-string buffer-content))
           (segmented-text (mapconcat
                            (lambda (resp) (plist-get resp :surface))
                            json-responses
                            " "))
           candidates
           (kagome-output (with-temp-buffer
                            (dolist (resp json-responses candidates)
                              (let* ((start (plist-get resp :start))
                                     (end (plist-get resp :end))
                                     (surface (plist-get resp :surface))
                                     (cls (plist-get resp :class)) ;; 'class' is a built-in function
                                     (pos (plist-get resp :pos))
                                     (base-form (plist-get resp :base_form))
                                     (reading (plist-get resp :reading))
                                     (pronunciation (plist-get resp :pronunciation))
                                     (features (plist-get resp :features))
                                     (entry (pen-candidate-by-word surface))) ; features just a combination of other fields
                                (when (string= cls "KNOWN")
                                    (insert (format "*** ~%s~ %s " surface pos))
                                    (insert (pen-play-youdao-button
                                                            (lambda ()
                                                              (interactive)
                                                              (funcall pen-read-function-1 surface))) " ")
                                    (insert (pen-play-button
                                                            (lambda ()
                                                              (interactive)
                                                              (funcall pen-read-function-2 surface))) " ")
                                    (if entry
                                        (progn
                                          (insert (pen-edit-button
                                                             (lambda ()
                                                               (interactive)
                                                               (funcall 'pen-find-note (car (pen-candidate-by-word surface) )))) " ")
                                          (insert (pen-delete-button
                                                             (lambda ()
                                                               (interactive)
                                                               (funcall 'pen-delete-word (car (pen-candidate-by-word surface) )))) " ")
                                          )
                                        (insert (pen-add-button
                                                                (lambda ()
                                                                  (interactive)
                                                                  (funcall-interactively 'pen-add-online-word surface segmented-text))) " ")
                                      )
                                    (insert (pen-goldendict-button (lambda ()
                                                                   (interactive)
                                                                   (funcall pen-external-dictionary-function surface))) "\n")
                                    ;; (insert "#+BEGIN_SRC\n")
                                    (insert (format "base_form: %s, reading: %s, pronunciation: %s\n"
                                                    base-form reading pronunciation) )
                                    ;; (insert "#+END_SRC\n\n")
                                    )
                                (if entry (push (car entry) candidates) )))
                            (buffer-string)) ))
      (pen-view-note (pen-new-entry segmented-text kagome-output) nil t)
      (with-current-buffer (get-buffer "*pen-view-note*")
        (pen-show-all-annotations candidates)
        )
      (other-window 1)

      )))

(defun pen-focus-find-current-thing-segment-english()
  (interactive)
  (if (get-char-property (point) 'pen-entry)
      (pen-view-note)
    (ecdict-command
     (if mark-active
         (pen-remove-spaces-based-on-ascii-rate (buffer-substring-no-properties (region-beginning) (region-end)) )
       (if focus-mode
           (pen-remove-spaces-based-on-ascii-rate (buffer-substring-no-properties (car (focus-bounds)) (cdr (focus-bounds))) )
         (pen-remove-spaces-based-on-ascii-rate (thing-at-point 'sentence t) )))
     'pen-focus-view-note-process-sentinel-english) ))


(defun pen-focus-view-note-process-sentinel-english (proc _event)
  "Handles the english process termination event."
  (when (eq (process-status proc) 'exit)
    (let* ((json-object-type 'plist)
           (json-array-type 'list)
           (original-string (with-current-buffer (process-buffer proc)
                              original-string))
           (buffer-content (with-current-buffer (process-buffer proc)
                             (buffer-string)))
           (json-responses (json-read-from-string buffer-content))
           (segmented-text (mapconcat
                            (lambda (resp) (plist-get resp :word))
                            json-responses
                            " "))
           candidates
           (kagome-output (with-temp-buffer
                            (dolist (resp json-responses candidates)
                              (let* ((id (plist-get resp :id))
                                     (word (plist-get resp :word))
                                     (sw (plist-get resp :sw))
                                     (phonetic (plist-get resp :phonetic))
                                     (definition (plist-get resp :definition))
                                     (translation (plist-get resp :translation))
                                     (pos (plist-get resp :pos))
                                     (collins (plist-get resp :collins))
                                     (oxford  (plist-get resp :oxford))
                                     (tag (plist-get resp :tag))
                                     (bnc (plist-get resp :bnc))
                                     (frq (plist-get resp :frq))
                                     (exchange (plist-get resp :exchange))
                                     (detail (plist-get resp :detail))
                                     (audio (plist-get resp :audio))
                                     (entry (pen-candidate-by-word word))) ; features just a combination of other fields
                                (when (and (not (string= word "nil")) (> frq 3000) )
                                    (insert (format "*** ~%s~ [%s] " word phonetic))
                                    (insert (pen-play-youdao-button
                                                            (lambda ()
                                                              (interactive)
                                                              (funcall pen-read-function-1 word))) " ")
                                    (insert (pen-play-button
                                                            (lambda ()
                                                              (interactive)
                                                              (funcall pen-read-function-2 word))) " ")
                                    (if entry
                                        (progn
                                          (insert (pen-edit-button (lambda ()
                                                                     (interactive)
                                                                     (funcall 'pen-find-note (car (pen-candidate-by-word word) )))) " ")
                                          (insert (pen-delete-button (lambda ()
                                                                     (interactive)
                                                                     (funcall 'pen-delete-word (car (pen-candidate-by-word word) )))) " "))
                                      (insert (pen-add-button (lambda ()
                                                                 (interactive)
                                                                 (funcall-interactively 'pen-add-online-word word original-string))) " "))
                                    (insert (pen-goldendict-button (lambda ()
                                                                   (interactive)
                                                                   (funcall pen-external-dictionary-function word))) "\n")

                                    ;; (insert "#+BEGIN_SRC\n")
                                    (insert (format "_collins_: %s, _oxford_: %s, _tag_: %s, _bnc_ %s, _frq_: %s, _exchange_: %s\n%s\n%s\n"
                                                    collins oxford tag bnc frq exchange translation definition ))
                                    ;; (insert "#+END_SRC\n\n")
                                    (if entry (push (car entry) candidates) ))))
                            (buffer-string)) ))
      (pen-view-note (pen-new-entry original-string kagome-output) nil t)
      (with-current-buffer (get-buffer "*pen-view-note*")
        (pen-show-all-annotations candidates))
      (other-window 1)

      )))

(provide 'pen-focus)
