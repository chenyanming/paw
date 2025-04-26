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
3. lingua: pip install lingua-language-detector
4. other"
  :group 'paw
  :type '(choice (const :tag "pycld2" pycld2)
                 (const :tag "gcld3" gcld3)
                 (const :tag "lingua" lingua)
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

(defcustom paw-annotation-search-paths '()
  "Alternative pathes for paw-annotation-mode. The books pathes
 that are possibly used for paw-annotation-mode."
  :group 'paw
  :type 'list)


(defconst paw-note-type-alist
  '((word . "✎")
    (highlight-1 . paw-highlight-1-face)
    (highlight-2 . paw-highlight-2-face)
    (highlight-3 . paw-highlight-3-face)
    (highlight-4 . paw-highlight-4-face)
    (underline-1 . paw-underline-1-face)
    (underline-2 . paw-underline-2-face)
    (underline-3 . paw-underline-3-face)
    (underline-4 . paw-underline-4-face)
    (underline-5 . paw-underline-5-face)
    (underline-6 . paw-underline-6-face)
    (comment . paw-comment-face)
    (attachment . "📝")
    (question . "❓")
    (image . "📷")
    (bookmark . "🔖")
    (todo . "☐")
    (done . "☑")
    (cancel . "☒")
    (link . "🔗")
    (sdcv . "✎"))
  "Const annotation types and their characters or faces.")

(defcustom paw-ask-ai-p nil
  "ask ai automatically"
  :group 'paw
  :type 'boolean)

(defcustom paw-ai-translate-p nil
  "Transalate the word with ai automatically"
  :group 'paw
  :type 'boolean)

(defcustom paw-ai-translate-context-p nil
  "Translate context with ai automatically"
  :group 'paw
  :type 'boolean)

(defvar paw-cli-program (or (executable-find "paw")
                             (concat (file-name-directory load-file-name) "paw/cli.py") )
  "Path to emacs paw command line program.")


(defcustom paw-ask-ai-prompt '(("拆解分析" . "用简体中文拆解分析以下文字:{content}")
                               ("文章总结" . "用简体中文总结以下文字:{content}")
                               ("根据上下文解释" . "我正在阅读{context}，以下是高亮的文字:{content}。用简体中文解释它的意思。")
                               ("回答问题" . "用简体中文回答以下问题: {content}")
                               ("头脑风暴" . "用简体中文为{content}进行头脑风暴")
                               ("草拟大纲" . "用简体中文为{content}草拟大纲")
                               ("草拟任何内容" . "用简体中文为{content}草拟任何内容")
                               ("草拟电子邮件" . "用简体中文为{content}草拟电子邮件")
                               ("草拟日记条目" . "用简体中文为{content}草拟日记条目")
                               ("草拟会议议程" . "用简体中文为{content}草拟会议议程")
                               ("12个字以内解释" . "用简体中文为{content}用12个字以内解释")
                               ("48个字以内解释" . "用简体中文为{content}用48个字以内解释")
                               ("100个字以内解释" . "用简体中文为{content}用100个字以内解释")
                               ("200个字以内解释" . "用简体中文为{content}用200个字以内解释")
                               ("总结一下" . "用简体中文总结一下{content}")
                               ("翻译成中文" . "将{content}翻译成中文")
                               ("写任何内容" . "用简体中文为{content}写任何内容")
                               ("Explain by context" . "I'm reading {context}, with the following highlighted text:{content}. Explain its meaning.")
                               ("Answer it" . "Answer the following question: {content}")
                               ("Brainstorm ideas" . "Brainstorm ideas for {content}")
                               ("Draft an outline" . "Draft an outline for {content}")
                               ("Draft anything" . "Draft anything for {content}")
                               ("Draft an email" . "Draft an email for {content}")
                               ("Draft a journal entry" . "Draft a journal entry for {content}")
                               ("Draft a meeting agenda" . "Draft a meeting agenda for {content}")
                               ("Explain in 12 words or less" . "Explain in 12 words or less for {content}")
                               ("Explain in 48 words or less" . "Explain in 48 words or less for {content}")
                               ("Explain in 100 words or less" . "Explain in 100 words or less for {content}")
                               ("Explain in 200 words or less" . "Explain in 200 words or less for {content}")
                               ("Summarize it" . "Summarize it for {content}")
                               ("Translate it to Chinese" . "Translate it to Chinese for {content}")
                               ("Write anything" . "Write anything for {content}"))
  "The default question to ask AI."
  :group 'paw
  :type 'alist)

(defcustom paw-ask-ai-defualt-prompt "Explain in 48 words or less"
  "The default question to ask AI."
  :group 'paw
  :type 'string)

(provide 'paw-vars)
