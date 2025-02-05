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
                       (list paw-python-program paw-server-program))))
            (if (and wallabag-host
                     wallabag-username
                     wallabag-password
                     wallabag-clientid
                     wallabag-secret)
                (apply #'start-process
                       "paw-server"
                       "*paw-server*"
                       (append cmd
                               (list "run_server"
                                     "--database" paw-db-file
                                     "--save-dir" temporary-file-directory
                                     "--port" paw-server-port
                                     "--wallabag-host" wallabag-host
                                     "--wallabag-username" wallabag-username
                                     "--wallabag-password" wallabag-password
                                     "--wallabag-clientid" wallabag-clientid
                                     "--wallabag-secret" wallabag-secret)))
              (error "Paw-server can not start wallabag without empty secrets, please set wallabag-host, wallabag-password, wallabag-clientid, and wallabag-secret first"))))
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
                             "--save-dir" temporary-file-directory
                             "--port" paw-server-port)))))))

(provide 'paw-server)
