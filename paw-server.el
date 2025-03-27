;;; paw-server.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'paw-db)

(defcustom paw-server-port "5001"
  "The port of paw server."
  :type 'integer
  :group 'paw)

(defcustom paw-server-program (or (executable-find "paw")
                                  (concat (file-name-directory load-file-name) "paw/cli.py") )
  "The python file of paw server."
  :type 'string
  :group 'paw)

(defcustom paw-server-html-file (expand-file-name "paw_server_html_file.html" temporary-file-directory)
  "The port of paw server."
  :type 'integer
  :group 'paw)

;;;###autoload
(defun paw-server()
  "Open paw server."
  (interactive)
  (if (process-status "paw-server")
      (message "paw-server is already running")
    (if (featurep 'wallabag)
        (progn
          (message "Starting paw-server with wallabag features...")
          (require 'wallabag)
          (let ((cmd (if (executable-find "paw")
                         (list paw-server-program)
                       (list paw-python-program paw-server-program)))
                (credentials (wallabag-credentials)))
            (apply #'start-process
                   "paw-server"
                   "*paw-server*"
                   (append cmd
                           (list "run_server"
                                 "--database" paw-db-file
                                 "--save-dir" paw-server-html-file
                                 "--port" paw-server-port
                                 "--wallabag-host" (plist-get credentials :host)
                                 "--wallabag-username" (plist-get credentials :username)
                                 "--wallabag-password" (plist-get credentials :password)
                                 "--wallabag-clientid" (plist-get credentials :clientid)
                                 "--wallabag-secret" (plist-get credentials :secret))))))
      (message "Starting paw-server...")
      (let ((cmd (if (executable-find "paw")
                     (list paw-server-program)
                   (list paw-python-program paw-server-program))))
        (apply #'start-process
               "paw-server"
               "*paw-server*"
               (append cmd
                       (list "run_server"
                             "--database" paw-db-file
                             "--save-dir" paw-server-html-file
                             "--port" paw-server-port)))))))

(provide 'paw-server)
