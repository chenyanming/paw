;;; paw-vars.el --- Settings and variables -*- lexical-binding: t; -*-

(defvar paw-view-note-buffer-name "*paw-view-note*")

(defvar paw-view-note-sub-buffer-name "*paw-sub-note*")

(defvar paw-sdcv-running-process nil)

(defvar paw-go-translate-running-p nil
  "TODO Workaournd to detect the translation is running.")

(defcustom paw-detect-language-p nil
  "use `paw-detect-language-program' to detect language, install
 `paw-detect-language-program'
before enabling it"
  :group 'paw
  :type 'boolean)

(defcustom paw-detect-language-program 'gcld3
  "The program used to detect language.
1. pycld2: pip install pycld2
2. glcd3: pip install gcld3
3. other"
  :group 'paw
  :type '(choice (const :tag "pycld2" pycld2)
                 (const :tag "gcld3" gcld3)
                 (symbol :tag "other")))

(defcustom paw-python-program (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python program used."
  :type 'string)

(defcustom paw-gptel-language "chinese"
  "The language to use in gptel."
  :type 'string
  :group 'paw)

(defcustom paw-gptel-ai-translate-prompt nil
  "The prompt to use ai translate in gptel. If not set, will use the default prompt.
You can get the word with function `paw-note-word', and compose the prompt with it."
  :type 'string
  :group 'paw)

(defcustom paw-gptel-ask-ai-prompt nil
  "The prompt to ask ai in gptel. If not set, will use the default prompt.
You can get the word with function `paw-note-word', and compose the prompt with it."
  :type 'string
  :group 'paw)



(provide 'paw-vars)
