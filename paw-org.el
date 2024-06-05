;;; paw/paw-org.el -*- lexical-binding: t; -*-

(require 'ol)

(org-link-set-parameters
 "paw"
 :follow #'paw-org-link-view
 :face 'paw-link-face)

(defun paw-org-link-copy ()
  "Copy the marked items as paw org links."
  (interactive)
  (let* ((marked-entries (paw-find-marked-candidates))
         (entries (or marked-entries (list (get-text-property (point) 'paw-entry)))))
    (kill-new
     (with-temp-buffer
       (dolist (entry entries)
         (let* ((origin-word (alist-get 'word entry))
                (word (if (string-match ":id:\\(.*\\)" origin-word)
                          (match-string 1 origin-word)
                        origin-word))
                (content (s-collapse-whitespace (s-truncate 100 (alist-get 'content entry) ) ))
                (origin-path (alist-get 'origin_path entry)))
           (insert (format "[[paw:%s][%s: %s]]\n" word origin-path content))
           (message "Copied: %s - \"%s\" as paw org link." word content)))
       (buffer-string)))
    ;; remove overlays and text properties
    (paw-clear-marks)))

;;;###autoload
(defun paw-org-link-view (word arg)
  "Follow paw org links."
  (let ((entry (paw-candidate-by-id word)))
    (if entry
        (if arg
            ;; (paw-goto-dashboard (car entry))
            (paw-find-origin (car entry))
          (paw-find-note (car entry) t))
      (message "No this entry."))))

(org-link-set-parameters
 "paw-path"
 :follow #'paw-path-org-link-view
 :face 'paw-link-face)

;;;###autoload
(defun paw-path-org-link-view (path _)
  "Follow paw-path org links."
  (paw-view-notes path t))

(provide 'paw-org)
