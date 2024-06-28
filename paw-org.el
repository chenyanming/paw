;;; paw-org.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'ol)

(org-link-set-parameters
 "paw"
 :follow #'paw-org-link-view-note
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
                (word (paw-get-real-word origin-word)))
           (insert (format "[[paw:%s][%s]]\n" origin-word word))
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
(defun paw-org-link-view-note (word _)
  "Follow paw org link."
  (let ((entry (car (paw-candidate-by-word word) ))
        (paw-view-note-show-type 'buffer))
    (if entry
        (paw-view-note entry)
      (paw-view-note (paw-new-entry word) :no-pushp t :buffer-name paw-view-note-sub-buffer-name))))

(provide 'paw-org)
