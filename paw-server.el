;;; paw-server.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'paw-db)

(defcustom paw-server-port "5001"
  "The port of paw server."
  :type 'integer
  :group 'paw)

(defcustom paw-server-program (concat (file-name-directory load-file-name) "cli.py")
  "The python file of paw server."
  :type 'string
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
          (start-process "paw-server"
                         "*paw-server*"
                         paw-python-program
                         paw-server-program
                         "run_server"
                         "--database"
                         paw-db-file
                         "--save-dir"
                         temporary-file-directory
                         "--port"
                         paw-server-port
                         "--wallabag-host"
                         wallabag-host
                         "--wallabag-username"
                         wallabag-username
                         "--wallabag-password"
                         wallabag-password
                         "--wallabag-clientid"
                         wallabag-clientid
                         "--wallabag-secret"
                         wallabag-secret))
      (message "Starting paw-server...")
      (start-process "paw-server"
                     "*paw-server*"
                     paw-python-program
                     paw-server-program
                     "run_server"
                     "--database"
                     paw-db-file
                     "--save-dir"
                     temporary-file-directory
                     "--port"
                     paw-server-port))))

(provide 'paw-server)
