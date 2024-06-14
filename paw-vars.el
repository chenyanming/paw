;;; paw-vars.el --- Settings and variables -*- lexical-binding: t; -*-

(defvar paw-view-note-buffer-name "*paw-view-note*")

(defvar paw-view-note-sub-buffer-name "*paw-sub-note*")

(defvar paw-sdcv-running-process nil)

(defvar paw-go-translate-running-p nil
  "TODO Workaournd to detect the translation is running.")

(defcustom paw-detect-language-p nil
  "use python pycld2 to detect language, install pycld2
 via 'pip install pycld2' before enabling it"
  :group 'paw
  :type 'boolean)

(defcustom paw-python-program (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python program used."
  :type 'string)

(provide 'paw-vars)
