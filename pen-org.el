;;; pen/pen-org.el -*- lexical-binding: t; -*-

(require 'ol)

(org-link-set-parameters
 "pen"
 :follow #'pen-org-link-view
 :face 'pen-link-face)

(defun pen-org-link-copy ()
  "Copy the marked items as pen org links."
  (interactive)
  (let* ((marked-entries (pen-find-marked-candidates))
         (entries (or marked-entries (list (get-text-property (point) 'pen-entry)))))
    (kill-new
     (with-temp-buffer
       (dolist (entry entries)
         (let* ((origin-word (alist-get 'word entry))
                (word (if (string-match ":id:\\(.*\\)" origin-word)
                          (match-string 1 origin-word)
                        origin-word))
                (content (s-collapse-whitespace (s-truncate 100 (alist-get 'content entry) ) ))
                (origin-path (alist-get 'origin_path entry)))
           (insert (format "[[pen:%s][%s: %s]]\n" word origin-path content))
           (message "Copied: %s - \"%s\" as pen org link." word content)))
       (buffer-string)))
    ;; remove overlays and text properties
    (pen-clear-marks)))

;;;###autoload
(defun pen-org-link-view (word arg)
  "Follow pen org links."
  (let ((entry (pen-candidate-by-id word)))
    (if entry
        (if arg
            ;; (pen-goto-dashboard (car entry))
            (pen-find-origin (car entry))
          (pen-find-note (car entry) t))
      (message "No this entry."))))

(org-link-set-parameters
 "pen-path"
 :follow #'pen-path-org-link-view
 :face 'pen-link-face)

;;;###autoload
(defun pen-path-org-link-view (path _)
  "Follow pen-path org links."
  (pen-view-notes path t))

(provide 'pen-org)
