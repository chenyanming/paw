;;; paw-gptel.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'gptel)
(require 'paw-db)

;;; Code:

(defcustom paw-gptel-backend gptel-backend
  "The GPT backend to use for paw-gptel."
  :type `(choice
          (const :tag "ChatGPT" ,gptel--openai)
          (restricted-sexp :match-alternatives (gptel-backend-p 'nil)
                           :tag "Other backend"))
  :group 'paw)

(defcustom paw-gptel-model gptel-model
  "The model to use for paw-gptel.  If nil, use `paw-gptel-backend'."
  :type `(choice
          (symbol :tag "Specify model name")
          ,@(mapcar (lambda (model)
                      (list 'const :tag (symbol-name (car model))
                            (car model)))
                    gptel--openai-models))
  :group 'paw)

(defun paw-gptel-update-note (id word type &optional callback)
  (let ((gptel-backend paw-gptel-backend)
        (gptel-model paw-gptel-model))
    (gptel-request
        (pcase type
          ('word (format "You are an English Expert, you carefully research the word, give me the English explanations/sentences from Famous Dictionaries, and the related Chinese explanation: %s, return your answer in Emacs Org mode format, heading starts from **, no need to tell me 'Sure'"
                         word))
          (_ (format "Explain: %s directly, return your answer in Emacs Org Mode format, heading starts from **, no need to tell me 'Sure'"
                     word )))
      :callback
      (lambda (response info)
        (if (not response)
            (message "gptel-quick failed with message: %s" (plist-get info :status))
          (paw-update-note id response)
          (message "%s" response))
        (if callback
            (funcall callback)))) ))

(defun paw-gptel-update-exp (id word type &optional callback)
  (let ((gptel-backend paw-gptel-backend)
        (gptel-model paw-gptel-model))
    (gptel-request
        (pcase type
          ('word (format "You are an English Expert, you carefully research the word, give me the English explanations/sentences from Famous Dictionaries, and the related Chinese explanation: %s, return your answer in Emacs Org mode format, heading starts from **, no need to tell me 'Sure'"
                         word))
          (_ (format "Explain: %s directly, return your answer in Emacs Org Mode format, heading starts from **, no need to tell me 'Sure'"
                     word )))
      :callback
      (lambda (response info)
        (if (not response)
            (message "gptel-quick failed with message: %s" (plist-get info :status))
          (paw-update-exp id response)
          (message "%s" response))
        (if callback
            (funcall callback)))) ))

(defun paw-gptel-translate (word &optional prompt callback buffer section)
  "Translate WORD with PROMPT, and insert the result into BUFFER under SECTION."
  ;; Cancel any ongoing translation request
  (when paw-gptel-translate-fsm
    (gptel-abort (current-buffer))
    (setq paw-gptel-translate-fsm nil)
    (message "Cancelled previous translation request"))

  (let ((buffer (or buffer (current-buffer)))
        (gptel-backend paw-gptel-backend)
        (gptel-model paw-gptel-model)) ;; the button is pressed on current-buffer
    (message "[%s:%s] Translating..."
             (gptel-backend-name gptel-backend)
             (gptel--model-name gptel-model))
    ;; Store the FSM for potential cancellation
    (setq paw-gptel-translate-fsm
          (gptel-request prompt
    :callback
    (lambda (response info)
      ;; Clear the FSM since this request is complete
      (setq paw-gptel-translate-fsm nil)
      (if (not response)
          (message "paw-gptel-translate failed with message: %s" (plist-get info :status))
        ;; (message "%s" response)
        )
      (if callback
          (funcall callback)
        (with-current-buffer buffer
          (save-excursion
            (when-let* ((translation response))
              (setq buffer-read-only nil)
              ;; (message translation)
              (unless (string-match-p section (org-no-properties (org-get-heading t t t t)))
                (goto-char (point-min))
                (search-forward (format "** %s" section) nil t))
              (org-end-of-subtree t t)
              ;; (forward-line)
              ;; (delete-region (region-beginning) (region-end))
              (let ((bg-color paw-view-note-background-color))
                (paw-insert-and-make-overlay
                 translation
                 'face `(:background ,bg-color :extend t))
                (insert "\n"))
              (if (or paw-ask-ai-p paw-ai-translate-p paw-ai-translate-context-p)
                  (insert "\n"))
              (goto-char (point-min))
              (search-forward "** Dictionaries" nil t)
              (beginning-of-line)
              (setq buffer-read-only t)
              ;; (message "Translation completed")
              ;; (message "Translation completed %s" translation)
              ) )
          (deactivate-mark))))) )))


(defvar paw-gptel-chat-buffer nil)

;; Variable to track ongoing translation requests
(defvar paw-gptel-translate-fsm nil
  "Current ongoing translation request FSM.")


;; TODO this could probably be replaced with something already in gptel
(defun paw-gptel-parse-user-query (buffer)
  "Parse and extract the most recent user query from BUFFER.
The query is expected to be after the last '* ' (org-mode) or
 '### ' (markdown-mode) heading.  Returns nil if no query is found."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (let ((case-fold-search t)
            (heading-regex (if (derived-mode-p 'org-mode)
                               "^\\*\\*\\* "
                             "^### ")))
        (when (re-search-backward heading-regex nil t)
          (let ((query-text (buffer-substring-no-properties (point) (point-max))))
            (string-trim query-text)))))))


(defcustom paw-gptel-window-size 0.33
  "Size of the paw chat window as a fraction of the frame.
Must be a number between 0 and 1, exclusive."
  :type 'float
  :group 'paw
  :set (lambda (symbol value)
         (if (and (numberp value)
                  (< 0 value 1))
             (set-default symbol value)
           (user-error "paw-window-size must be a number between 0 and 1, exclusive"))))

(defcustom paw-gptel-window-style 'horizontal
  "Specify the orientation.  It can be \='horizontal, '\=vertical, or nil."
  :type '(choice (const :tag "Horizontal" horizontal)
          (const :tag "Vertical" vertical)
          (const :tag "None" nil)))

(defun paw-gptel-setup-windows (&optional buffer-name)
  "Set up the coding assistant layout with the chat window."
  (setq paw-gptel-chat-buffer
        (gptel (or buffer-name "*paw-gptel*")))

  (if (equal (buffer-name) paw-view-note-buffer-name)
      ;; switch to gptel buffer if ask in *paw-view-note*
      (set-window-buffer (selected-window) paw-gptel-chat-buffer)
    ;; split window if ask in other buffer
    (when paw-gptel-window-style
      (delete-other-windows)

      (let* ((main-buffer (current-buffer))
             (main-window (selected-window))
             (split-size (floor (* (if (eq paw-gptel-window-style 'vertical)
                                       (window-width)
                                     (window-height))
                                   (- 1 paw-gptel-window-size)))))
        (with-current-buffer paw-gptel-chat-buffer)
        (if (eq paw-gptel-window-style 'vertical)
            (split-window-right split-size)
          (split-window-below split-size))
        (set-window-buffer main-window main-buffer)
        (other-window 1)
        (set-window-buffer (selected-window) paw-gptel-chat-buffer)))))

(defun paw-gptel-get-buffer-name ()
  "Return the gtpel buffer name for the current context."
  (format "*paw: %s*"
          (if paw-note-target-buffer
              (buffer-name paw-note-target-buffer)
            (buffer-name))))

(defun paw-gptel-query (&optional user-query &rest args)
  "Send USER-QUERY.
If USER-QUERY is nil, prompt the user for a query
ARGS can include:
 :buffer BUFFER-NAME to specify the chat buffer name
 :no-request if you want to skip sending the request to gptel"
  (interactive)

  (let* ((buffer-name (or (plist-get args :buffer) (paw-gptel-get-buffer-name)))
         (no-request (plist-get args :no-request)))
    (unless user-query
      (setq user-query (read-string (format "Ask AI (%s): " buffer-name) (paw-get-word))))

    (paw-gptel-setup-windows buffer-name)

    (let ((chat-buffer paw-gptel-chat-buffer))
      (deactivate-mark)
      (save-excursion
        (with-current-buffer chat-buffer
          (goto-char (point-max))
          ;; TODO if the whole current line is equal "*** "
          (if (string= "*** " (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
              (insert user-query)
            (insert "\n\n")
            (insert "*** " user-query))
          (insert "\n\n")))

      (unless no-request
        (let ((gptel-backend paw-gptel-backend)
              (gptel-model paw-gptel-model))
          (message "[%s:%s] Querying..."
                   (gptel-backend-name gptel-backend)
                   (gptel--model-name gptel-model))
          (gptel-request user-query
          :buffer chat-buffer
          :callback #'paw-gptel-handle-response))))))


(defun paw-gptel-handle-response (response info)
  "Handle the RESPONSE from gptel.
The changes will be applied to CODE-BUFFER in a git merge format.
INFO is passed into this function from the `gptel-request' function."
  (when response
    ;; Insert explanations into chat buffer
    (with-current-buffer paw-gptel-chat-buffer
      (let ((explanation-info (list :buffer (plist-get info :buffer)
                                    :position (point-max-marker)
                                    :in-place t)))
        (gptel--insert-response response explanation-info))

      (gptel--sanitize-model)
      (gptel--update-status " Ready" 'success))))

(provide 'paw-gptel)
