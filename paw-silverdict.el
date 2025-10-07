;;; paw-silverdict.el -*- lexical-binding: t; -*-

(defcustom paw-silverdict-host "localhost"
  "Host where SilverDict server is running."
  :type 'string
  :group 'paw)

(defcustom paw-silverdict-port "2628"
  "Port where SilverDict server is running."
  :type 'string
  :group 'paw)

(defcustom paw-silverdict-query-path "/api/query"
  "Path for SilverDict query API endpoint."
  :type 'string
  :group 'paw)

;;;###autoload
(defun paw-silverdict-search-details (&optional word en)
  "Search word with SilverDict web server.
The SilverDict server must be running on paw-silverdict-host:paw-silverdict-port."
  (interactive)
  (let* ((word (or word (paw-get-word)))
         (url (format "http://%s:%s%s?word=%s"
                      paw-silverdict-host
                      paw-silverdict-port
                      paw-silverdict-query-path
                      (url-hexify-string word))))
    (browse-url url)))

(provide 'paw-silverdict)

