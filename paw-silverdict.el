;;; paw-silverdict.el -*- lexical-binding: t; -*-

(defcustom paw-silverdict-host "localhost"
  "Host where SilverDict server is running."
  :type 'string
  :group 'paw)

(defcustom paw-silverdict-port "2628"
  "Port where SilverDict server is running.
Default port is 2628 (standard DICT protocol port).
Check your SilverDict server configuration for the actual port."
  :type 'string
  :group 'paw)

(defcustom paw-silverdict-query-path "/api/query/Default%20Group/"
  "Path for SilverDict query API endpoint.
The word will be appended directly to this path.
Common values:
  - \"/api/query/Default%20Group/\" (default)
  - \"/api/query/Chinese/\"
  - \"/api/query/YourGroup/\"
Note: The path should end with a slash."
  :type 'string
  :group 'paw)

;;;###autoload
(defun paw-silverdict-search-details (&optional word en)
  "Search WORD with SilverDict web server.
The SilverDict server must be running on paw-silverdict-host:paw-silverdict-port.
If WORD is not provided, uses the word at point via `paw-get-word'.
EN parameter is ignored but kept for API compatibility."
  (interactive)
  (let* ((word (or word (paw-get-word)))
         (url (format "http://%s:%s%s%s"
                      paw-silverdict-host
                      paw-silverdict-port
                      paw-silverdict-query-path
                      (url-hexify-string word))))
    (browse-url url)))

(provide 'paw-silverdict)

