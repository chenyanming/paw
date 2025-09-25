;;; paw-org.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'ol)
(require 'cl-seq)

(defcustom paw-org-protocol-hook nil
  "A hook called after `paw-org-protocol' was called."
  :group 'paw
  :type 'hook)

(org-link-set-parameters
 "paw"
 :follow #'paw-org-link-find-origin
 :face 'paw-link-face)

(org-link-set-parameters
 "paw-view-note"
 :follow #'paw-org-link-view-note
 :face 'paw-view-note-link-face)

(defun paw-org-link-copy ()
  "Copy the marked items as paw org links."
  (interactive)
  (let* ((marked-entries (paw-find-marked-candidates))
         (entries (or marked-entries (list (get-char-property (point) 'paw-entry)))))
    (kill-new
     (with-temp-buffer
       (dolist (entry entries)
         (let* ((origin-word (alist-get 'word entry))
                (word (paw-get-real-word origin-word))
                (id (paw-get-id origin-word)))
           (insert (format "[[paw:%s][%s]]\n" id word))
           (message "Copied: \"%s\" as paw org link." word)))
       (buffer-string)))
    ;; remove overlays and text properties
    (paw-clear-marks)))

;;;###autoload
(defun paw-org-link-find-note (word arg)
  "Follow paw org links."
  (let ((entry (paw-candidate-by-id word)))
    (if entry
        (if arg
            ;; (paw-goto-dashboard (car entry))
            (paw-find-origin (car entry))
          (paw-find-note (car entry) t))
      (message "No this entry."))))

;;;###autoload
(defun paw-org-link-find-origin (word _)
  "Follow paw org link."
  (let ((entry (car (paw-candidate-by-id (paw-get-id word)) ))
        (paw-view-note-show-type 'buffer))
    (pcase major-mode
      ('paw-note-mode
       ;; TODO go to the location seems more useful
       (paw-find-origin entry t))
      ('paw-view-note-mode
       (let* ((current-location (org-entry-get nil paw-file-property-current-location))
              (context (if current-location
                           (with-current-buffer (or paw-note-return-buffer paw-note-target-buffer)
                             (goto-char (string-to-number current-location))
                             (paw-get-note)) "")))
         (if entry
             (progn
               (setf (alist-get 'context entry) context)
               (paw-view-note entry))
           (paw-view-note
            (paw-new-entry
             word
             :context context)
            :no-pushp t :buffer-name paw-view-note-sub-buffer-name))))
      (_
       (if entry
           (paw-find-origin entry t)
         (paw-view-note (paw-new-entry word) :no-pushp t :buffer-name paw-view-note-sub-buffer-name))))))

;;;###autoload
(defun paw-org-link-view-note (word _)
  "Follow paw org link."
  (let ((entry (car (paw-candidate-by-id (paw-get-id word))))
        (paw-view-note-show-type 'buffer))
    (pcase major-mode
      ('paw-note-mode
       (paw-view-note entry))
      ('paw-view-note-mode
       (let* ((current-location (org-entry-get nil paw-file-property-current-location))
              (context (if current-location
                           (with-current-buffer (or paw-note-return-buffer paw-note-target-buffer)
                             (goto-char (string-to-number current-location))
                             (paw-get-note)) "")))
         (if entry
             (progn
               (setf (alist-get 'context entry) context)
               (paw-view-note entry))
           (paw-view-note
            (paw-new-entry
             word
             :context context)
            :no-pushp t :buffer-name paw-view-note-sub-buffer-name))))
      (_
       (if entry
           (paw-view-note entry)
         (paw-view-note (paw-new-entry word) :no-pushp t :buffer-name paw-view-note-sub-buffer-name))))))

(defcustom paw-org-protocol-display-function 'switch-to-buffer
  "The function to display the note buffer."
  :type 'function
  :group 'paw)

(defun paw-org-protocol (data)
  (let* ((protocol (plist-get data :protocol)) ;; this is coming from paw-server, not part of org-protocol
         (note (plist-get data :note))
         (url (plist-get data :url))
         (title (plist-get data :title))
         (word (plist-get data :body))
         (entry (or (car (paw-candidate-by-word word))
                    (car (paw-candidate-by-word (downcase word)))))
         (entry (if entry (append `((context . ,note)) entry) nil))
         (paw-note-target-buffer (get-buffer paw-view-note-buffer-name)))
    (if (and protocol (not (string= protocol "paw"))) ;; can not call itself, avoid recursive calling
        (funcall (plist-get (cdr (cl-find-if (lambda (e)
                                        (string= (plist-get (cdr e) :protocol) protocol))
                                      org-protocol-protocol-alist)) :function) data)
      (if (and (string-empty-p word)
               (string-empty-p note))
        (paw-view-note (or entry (paw-new-entry word
                                                :origin_type "browser"
                                                :serverp 3
                                                :content (json-encode data)
                                                :origin_path url
                                                :origin_point title
                                                :lang (paw-check-language word)
                                                :context note ) )
                       ;; :buffer-name (format "*Paw: %s*" title)
                       :buffer-name paw-view-note-buffer-name
                       :display-func paw-org-protocol-display-function)))
    (run-hook-with-args 'paw-org-protocol-hook data)
    nil))


(defun paw-org-setup-org-protocol()
  (require 'org-protocol)
  (add-to-list 'org-protocol-protocol-alist '("paw"
                                              :protocol "paw"
                                              :function paw-org-protocol
                                              :kill-client t)))

;; setup org protocol for user
(paw-org-setup-org-protocol)

(provide 'paw-org)
