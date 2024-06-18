;;; paw-util.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'paw-gptel)
(require 'paw-sdcv)
(require 'paw-goldendict)
(require 'paw-go-translate)
(require 'paw-android)
(require 'compile)

(require 'thingatpt)

(eval-when-compile (defvar paw-current-entry))

(defvar paw-provider-url "")

(defvar paw-provider-english-url-alist
  (append '(("TIO"       "https://tio.freemdict.com/api?br=1&key=%s")
            ("有道" "https://www.youdao.com/result?word=%s&lang=en")
            ("欧陆" "https://dict.eudic.net/mdicts/en/%s")
            ("牛津" "https://www.oxfordlearnersdictionaries.com/definition/english/%s")
            ("朗文" "https://www.ldoceonline.com/dictionary/%s")
            ("韦氏" "https://www.merriam-webster.com/dictionary/%s")
            ("剑桥" "https://dictionary.cambridge.org/dictionary/english-chinese-simplified/%s")
            ("美国传统" "https://www.ahdictionary.com/word/search.html?q=%s")
            ("柯林斯" "https://www.collinsdictionary.com/zh/dictionary/english/%s")
            ("Dictcn" "https://dict.cn/search?q=%s")
            ("Wikipedia"       "https://en.wikipedia.org/wiki/%s")
            ("Wiktionary" "https://en.wiktionary.org/wiki/%s")
            ;; ("Onelook" "https://www.onelook.com/?w=%s")
            ("vocabulary.com" "https://www.vocabulary.com/dictionary/%s")
            ;; ("Google maps"       "https://maps.google.com/maps?q=%s")
            ;; ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
            ;; ("DuckDuckGo"        +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
            ;; ("DevDocs.io"        "https://devdocs.io/#q=%s")
            ;; ("StackOverflow"     "https://stackoverflow.com/search?q=%s")
            ;; ("Github"            "https://github.com/search?ref=simplesearch&q=%s")
            ;; ("Wolfram alpha"     "https://wolframalpha.com/input/?i=%s")
            ;; ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
            ;; ("MDN"               "https://developer.mozilla.org/en-US/search?q=%s")
            ;; ("Internet archive"  "https://web.archive.org/web/*/%s")
            ;; ("Sourcegraph"       "https://sourcegraph.com/search?q=context:global+%s&patternType=literal")
            ;; ("Rust Docs" "https://doc.rust-lang.org/std/?search=%s")
            )))


(defvar paw-provider-japanese-url-alist
  (append '(
            ("TIO"      "https://tio.freemdict.com/japi?key=%s")
            ("Jisho"      "https://jisho.org/search/%s")
            ("Forvo"      "https://forvo.com/search/%s")
            ("Weblio(日中)" "https://cjjc.weblio.jp/content/+%s")
            ("Weblio" "https://www.weblio.jp/content/%s")
            ("Lorenzi's Jisho" "https://jisho.hlorenzi.com/search/%s")
            ("Nihongomaster" "https://www.nihongomaster.com/japanese/dictionary?query=%s")
            ("Kanshudo" "https://www.kanshudo.com/searchw?q=%s")
            ("Goo" "https://dictionary.goo.ne.jp/srch/all/%s/m0u/")
            ("Mazii" "https://mazii.net/zh-CN/search/word/jacn/%s")
            ("OJAD" "https://www.gavo.t.u-tokyo.ac.jp/ojad/search/index/word:%s")
            ("Moji" "https://www.mojidict.com/searchText/%s")
            ("Wikipedia"       "https://ja.wikipedia.org/wiki/%s")
            ;; ("Wiktionary(中)" "https://zh.wiktionary.org/wiki/%s")
            ;; ("Google maps"       "https://maps.google.com/maps?q=%s")
            ;; ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
            ;; ("DuckDuckGo"        +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
            ;; ("DevDocs.io"        "https://devdocs.io/#q=%s")
            ;; ("StackOverflow"     "https://stackoverflow.com/search?q=%s")
            ;; ("Github"            "https://github.com/search?ref=simplesearch&q=%s")
            ;; ("Wolfram alpha"     "https://wolframalpha.com/input/?i=%s")
            ;; ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
            ;; ("MDN"               "https://developer.mozilla.org/en-US/search?q=%s")
            ;; ("Internet archive"  "https://web.archive.org/web/*/%s")
            ;; ("Sourcegraph"       "https://sourcegraph.com/search?q=context:global+%s&patternType=literal")
            ;; ("Rust Docs" "https://doc.rust-lang.org/std/?search=%s")
            )))

(defvar paw-provider-general-url-alist
  (append '(("Google"            "https://google.com/search?q=%s")
            ("Google Translate"            "https://translate.google.com/#auto/zh-CN/%s")
            ;; ("Google(EN-CN)"            "https://translate.google.com/#en/zh-CN/%s")
            ("Images"     "https://www.google.com/images?q=%s")
            ;; ("Google maps"       "https://maps.google.com/maps?q=%s")
            ;; ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
            ;; ("DuckDuckGo"        +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
            ;; ("DevDocs.io"        "https://devdocs.io/#q=%s")
            ;; ("StackOverflow"     "https://stackoverflow.com/search?q=%s")
            ;; ("Github"            "https://github.com/search?ref=simplesearch&q=%s")
            ("Youtube"           "https://youtube.com/results?aq=f&oq=&search_query=%s")
            ;; ("Wolfram alpha"     "https://wolframalpha.com/input/?i=%s")
            ;; ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
            ;; ("MDN"               "https://developer.mozilla.org/en-US/search?q=%s")
            ;; ("Internet archive"  "https://web.archive.org/web/*/%s")
            ;; ("Sourcegraph"       "https://sourcegraph.com/search?q=context:global+%s&patternType=literal")
            ;; ("Rust Docs" "https://doc.rust-lang.org/std/?search=%s")
            )))

(defcustom paw-say-word-p t
  "paw say word automatically"
  :group 'paw
  :type 'boolean)


(defcustom paw-tts-japanese-voice "ja-JP-NanamiNeural"
  "Japanese tts voice."
  :group 'paw
  :type 'string)

(defcustom paw-tts-zh-cn-voice "zh-CN-XiaoxiaoNeural"
  "ZH-CN tts voice."
  :group 'paw
  :type 'string)


(defcustom paw-tts-zh-tw-voice "zh-TW-HsiaoChenNeural"
  "ZH-TW tts voice."
  :group 'paw
  :type 'string)

(defcustom paw-tts-korean-voice "ko-KR-SunHiNeural"
  "Korean tts voice."
  :group 'paw
  :type 'string)

(defcustom paw-tts-english-voice "en-US-AvaNeural"
  "English tts voice."
  :group 'paw
  :type 'string)

(defcustom paw-tts-multilingual-voice "en-US-AvaMultilingualNeural"
  "Multilingual tts voice."
  :group 'paw
  :type 'string)


(defcustom paw-posframe-p nil
  "show paw-view-note in posframe"
  :group 'paw
  :type 'boolean)

(defcustom paw-transalte-p t
  "transalate automatically"
  :group 'paw
  :type 'boolean)

(defcustom paw-default-say-word-function 'paw-say-word ;; paw-resay-word to regenerate the pronunciation
  "paw read function"
  :group 'paw
  :type '(choice (function-item paw-say-word)
          (function-item paw-android-say-word)
          (function-item paw-youdao-say-word)
          function))


(defcustom paw-share-word-function
  (cond
   ((eq system-type 'android)
    'paw-android-search-details)
   (t
    'paw-goldendict-search-details))
  "paw share the word to system tool"
  :group 'paw
  :type '(choice (function-item paw-android-search-details)
          (function-item paw-moji-search-details)
          (function-item paw-eudic-search-details)
          (function-item paw-chatgpt-search-details)
          function))

(defcustom paw-dictionary-browse-function 'browse-url
  "paw external dictionary browse function"
  :group 'paw
  :type '(choice (function-item browse-url)
          (function-item popweb-url-input)
          function))

(defcustom paw-translate-function 'paw-go-translate-insert
  "paw transalte function"
  :group 'paw
  :type '(choice (function-item paw-go-translate-insert)
          function))

(defcustom paw-ai-translate-function 'paw-gptel-translate
  "paw ai translate (gptel) function"
  :group 'paw
  :type '(choice (function-item paw-gptel-translate)
          function))

(defcustom paw-stardict-function 'paw-sdcv-search-detail
  "paw internal (sdcv) dictionary function"
  :group 'paw
  :type '(choice (function-item paw-sdcv-search-detail)
          function))

(defcustom paw-external-dictionary-function
  (cond
   ((eq system-type 'android)
    'paw-eudic-search-details)
   (t
    'paw-goldendict-search-details))
  "paw external dictionary function"
  :group 'paw
  :type '(choice (function-item paw-goldendict-search-details)
          (function-item paw-eudic-search-details)
          function))

(defcustom paw-mdict-dictionary-function 'browse-url
  "paw mdict dictionary function"
  :group 'paw
  :type '(choice (function-item browse-url)
          function))


(defun paw-parse-json (json)
  (append (alist-get 'data json ) nil))

(defun paw-buffer ()
  "Create buffer *paw*."
  (get-buffer-create "*paw*"))

(defun paw-format-column (string width &optional align)
  "Return STRING truncated or padded to WIDTH following ALIGNment.
Align should be a keyword :left or :right."
  (if (<= width 0)
      ""
    (format (format "%%%s%d.%ds" (if (eq align :left) "-" "") width width)
            string)))

(defun paw-clamp (min value max)
  "Clamp a value between two values."
  (min max (max min value)))

(defun paw-attach-icon-for (path)
  (char-to-string
   (pcase (downcase (file-name-extension path))
     ((or "jpg" "jpeg" "png" "gif") ?)
     ("pdf" ?)
     ((or "ppt" "pptx") ?)
     ((or "xls" "xlsx") ?)
     ((or "doc" "docx") ?)
     ((or "ogg" "mp3" "wav" "aiff" "flac") ?)
     ((or "mp4" "mov" "avi") ?)
     ((or "zip" "gz" "tar" "7z" "rar") ?)
     (_ ?))))

(defun paw-get-real-word (entry)
  "Get the word excluded the id."
  (if (stringp entry)
      (replace-regexp-in-string ":id:.*" "" entry)
    (if entry
        (replace-regexp-in-string ":id:.*" "" (alist-get 'word entry))
      "")))

(defun paw-entry-p (entry)
  "Check if the entry is a valid entry."
  (alist-get 'word entry))

(defun paw-new-entry(word &optional kagome lang)
  ;; create new entry
  ;; example ((word . "major") (exp . "adj. 主要的；主修的；重要的；较多的; n. 成年人；主修科目；陆军少校; vi. 主修<br>...") (content . 0) (serverp . 1) (note . "") (note_type word . "✎") (origin_type) (origin_path . "PN") (origin_id . "1709212272") (origin_point) (created_at . "2024-04-24 19:11:00"))
  ;; kagome: NOT the database field
  `((word . ,word)
    (exp . "")
    (content . ,word) ;; sam as other annotations which has id, currently it only saves the real content of the word, or json string for internal usage
    (serverp . 3)
    (note . ,(paw-get-note))
    (note_type word . "✎")
    (origin_type . ,(if (boundp 'eaf--buffer-app-name)
                        eaf--buffer-app-name
                      major-mode))
    (origin_path . ,(paw-get-origin-path))
    (origin_id . "")
    (origin_point)
    (created_at . ,(format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
    (kagome . ,kagome)
    (lang . ,(if lang lang (paw-check-language word)))))


(defvar paw-youdao-say-word-running-process nil)

(defun paw-youdao-say-word (word)
  "Listen to WORD pronunciation."
  (when (process-live-p paw-youdao-say-word-running-process )
    (kill-process paw-youdao-say-word-running-process)
    (setq paw-youdao-say-word-running-process nil))
  (if (featurep 'cocoa)
      (call-process-shell-command
       (format "say %s" word) nil 0)
    (let ((player (or (executable-find "mpv")
                      (executable-find "mplayer")
                      (executable-find "mpg123"))))
      (if player
          (setq paw-youdao-say-word-running-process
                (start-process
                 player
                 nil
                 player
                 (format "http://dict.youdao.com/dictvoice?type=2&audio=%s" (url-hexify-string word))) )
        (message "mpv, mplayer or mpg123 is needed to play word voice")))))

(defcustom paw-tts-cache-dir
  (expand-file-name (concat user-emacs-directory "edge-tts"))
  "paw say word cache directory."
  :group 'paw
  :type 'directory)

(defvar paw-say-word-running-process nil)

(defcustom paw-tts-program "edge-tts"
  "The tts program to use."
  :group 'paw
  :type 'string)

(defun paw-say-word (word &optional lang refresh)
  "Listen to WORD pronunciation using edge-tts, with LANG. If
 REFRESH is t, regenerate the pronunciation."
  (unless (executable-find paw-tts-program)
    (error "edge-tts is not found, please install via 'pip install edge-tts' first."))
  (when (process-live-p paw-say-word-running-process)
    (kill-process paw-say-word-running-process)
    (setq paw-say-word-running-process nil))
  (let* ((word-hash (md5 word))
         (mp3-file (concat (expand-file-name word-hash paw-tts-cache-dir) ".mp3"))
         (subtitle-file (concat (expand-file-name word-hash paw-tts-cache-dir) ".vtt")))
    (make-directory paw-tts-cache-dir t) ;; ensure cache directory exists
    (when (and refresh (file-exists-p mp3-file))
        (delete-file mp3-file)
        (delete-file subtitle-file))
    (if (file-exists-p mp3-file)
        (setq paw-say-word-running-process
              (start-process "*paw say word*" nil "mpv" mp3-file))
      (let ((proc (start-process "*paw-tts*" "*paw-tts*" paw-tts-program
                                 "--text" word
                                 "--write-media" mp3-file
                                 "--write-subtitles" subtitle-file
                                 "--voice" (pcase (if lang lang (paw-check-language word))
                                             ("en" paw-tts-english-voice)
                                             ("ja" paw-tts-japanese-voice)
                                             ("zh" paw-tts-zh-cn-voice)
                                             ("zh-Hant" paw-tts-zh-tw-voice)
                                             ("ko" paw-tts-korean-voice)
                                             (_ paw-tts-multilingual-voice)))))
        (setq paw-say-word-running-process proc)
        ;; Define sentinel
        (set-process-sentinel
         proc
         (lambda (process event)
           ;; When process "finished", then begin playback
           (when (string= event "finished\n")
             (start-process "*paw say word*" nil "mpv" mp3-file))))))))

;;;###autoload
(defun paw-tts-cache-clear ()
  "Clear tts cache."
  (interactive)
  (delete-directory paw-tts-cache-dir t)
  (make-directory paw-tts-cache-dir t))

(defun paw-resay-word (word &optional lang)
  "Delete the mp3 and subtitle then regenerate."
  (paw-say-word word lang t))


(defun paw-get-note ()
  (pcase major-mode
    ;; disable nov get header-line-format, since it is annoying, so that notes are totally control by myself
    ('nov-mode
     (paw-remove-spaces-based-on-ascii-rate (thing-at-point 'sentence t)))
    ('pdf-view-mode "")
    ('paw-search-mode "")
    ('paw-view-note-mode (alist-get 'note paw-current-entry))
    ('eaf-mode "")
    (_ (or (paw-get-sentence-or-line t) ""))))

(defvar paw-get-sentence-max-length 300)

(defun paw-get-sentence-or-line(&optional no-click-show)
  "Get the sentence or line at point. If the line is too long (>
`paw-get-sentence-max-length'), then use the current line. Remove
org link in the sentence."
  (let ((current-thing (thing-at-point 'sentence t)))
    (if current-thing
        (-let* ((length-of-thing (length current-thing))
                ((beg . end) (bounds-of-thing-at-point 'sentence)))
          (cond ((or (> length-of-thing paw-get-sentence-max-length) (= length-of-thing 0))  ;; if the sentence is too long, like detect failed, then use the current line
                 (-let ((line (thing-at-point 'line t))
                        ((beg . end) (bounds-of-thing-at-point 'line)))
                   ;; remove org links
                   (when (string-match "\\[\\[.*?\\]\\[.*?\\]\\]" line)
                     (setq line (replace-match "" nil nil line)))
                   (unless no-click-show (paw-click-show beg end 'paw-focus-face) )
                   line))
                ;; remove org links
                (t (when (string-match "\\[\\[.*?\\]\\[.*?\\]\\]" current-thing)
                     (setq current-thing (replace-match "" nil nil current-thing)))
                   (unless no-click-show (paw-click-show beg end 'paw-focus-face) )
                   current-thing)))
      "")))

(defcustom paw-ascii-rate 0.5
  "The rate of ascii characters in the text.

If `paw-ascii-rate' < 1, use the ascii rate to determine the language of the text:
- if matched, use `paw-default-language'.
- if umatched, and `paw-detect-language-p' is t, use `paw-detect-language-program' to determine the language.
- if umatched, and if `paw-detect-language-p' is nil, use `paw-non-ascii-language' directly.

If `paw-ascii-rate' >= 1, it will turn of ascii rate pre-checking, then ignore `paw-default-language', and:
- if `paw-detect-language-p' is t, use `paw-detect-language-program' to determine the language
- if `paw-detect-language-p' is nil, use `paw-non-ascii-language' directly.

The external language detection tools are known to be bad on
short words, so it is here to use `paw-ascii-rate' to determine
the language first.

If you always work on two languages, I recommend you find the
comfortable `paw-ascii-rate' (< 1), and turn off
`paw-detect-language-p'. This works well on ascii language +
non-ascii language, such as English + Japanese/Korean.

If you only want to use language detection tool to determine the
language. I recommend you set `paw-ascii-rate' to 1, and
`paw-default-language' to t, in this case, both
`paw-default-language' and `paw-non-ascii-language' will be
ingored."
  :group 'paw
  :type 'float)


(defcustom paw-default-language "en"
  "The default language for ascii characters."
  :group 'paw
  :type 'string)

(defcustom paw-non-ascii-language "ja"
  "The default language for non-ascii characters. It only works when
`paw-detect-language-program' is nil"
  :group 'paw
  :type 'string)


(defun paw-check-language (text)
  "Use simple ascii rate: `paw-ascii-rate' to detect the language,
if the rate is greater than `paw-ascii-rate', then it is
considered as `paw-default-language', otherwise use
`paw-detect-language-program' to detect the language of the TEXT,
if `paw-detect-language-p' is t, or return as `paw-non-ascii-language' if
`paw-detect-language-p' is nil."
  (let* ((strs (split-string text "")) ;; after spliting, it has two redundant elements, that's why minus 2 below
         (number (cl-count-if (lambda (str) (string-match-p "[[:ascii:]]+" str)) strs))
         (rate paw-ascii-rate)
         (lang (if (> (/ number (float (- (length strs) 2))) rate) ;; it is impossible to > 1, if 1 or larger, will ignore `paw-default-language'
                   paw-default-language
                 (if paw-detect-language-p
                     (with-temp-buffer
                       (pcase paw-detect-language-program
                         ('gcld3 (call-process
                                  paw-python-program
                                  nil ;; Infile
                                  t ;; Buffer
                                  nil ;; Display
                                  "-c"
                                  "import sys, gcld3; detector = gcld3.NNetLanguageIdentifier(min_num_bytes=0, max_num_bytes=2000); result = detector.FindLanguage(text=sys.argv[1]); print(result.language)"
                                  text))
                         ('pycld2 (call-process
                                   paw-python-program
                                   nil
                                   t
                                   nil
                                   "-c"
                                   "import sys; import pycld2 as cld2; reliable, _, detections = cld2.detect(sys.argv[1]); print(detections[0][1])"
                                   text))
                         (_ (call-process paw-detect-language-program
                                          nil
                                          t
                                          nil
                                          text)))
                       (goto-char (point-min))
                       (let ((detected-lang (string-trim (buffer-string))))
                         (if (string-equal "un" detected-lang) "en" detected-lang)))
                   paw-non-ascii-language))))
    ;; (message "Text: %s, Language: %s" text lang)
    lang))

(defun paw-remove-spaces (text lang)
  "TODO Refomat the TEXT based on the LANG."
  (cond ((string= lang "en") (replace-regexp-in-string "[ \n]+" " " (replace-regexp-in-string "^[ \n]+" "" text)))
        ((string= lang "ja") (replace-regexp-in-string "\\(^[ \t\n\r]+\\|[ \t\n\r]+\\)" "" text))
        (t text)))


(defun paw-remove-spaces-based-on-ascii-rate-return-cons (text)
  "TODO Refomat the text based on the language."
  (let ((lang (paw-check-language text)))
    (cons lang (paw-remove-spaces text lang))))

(defun paw-remove-spaces-based-on-ascii-rate (text)
  "TODO Refomat the text based on the language."
  (let ((lang (paw-check-language text)))
    (paw-remove-spaces text lang)))

(defun paw-provider-lookup (word provider alist)
  (let* ((provider-alist alist)
         (url-template (cadr (assoc provider provider-alist))))
    (format url-template (paw-get-real-word word ))))

(defun paw-get-id ()
  (pcase major-mode
    ('wallabag-entry-mode
     (alist-get 'id (get-text-property 1 'wallabag-entry)))
    ('eaf-mode
     nil)
    (_ nil)))


;;; mark/unmark

(defun paw-previous ()
  (let ((location (get-text-property (point) 'paw-id)) previous)
    (cond
     ;; check the current point headline number first
     ((numberp location)
      (setq previous (text-property-any (point-min) (point-max) 'paw-id (1- location)))
      (if (numberp previous)
          (goto-char previous)
        (goto-char (point-min))))
     ;; check the current point if >= the first header (no matter level), keep (point) if no headlines
     ((>= (or (text-property-not-all (point-min) (point-max) 'paw-id nil) (point)) (point))
      (message "Beginning of buffer")
      (goto-char (point-min)))
     (t
      (let ((current (point-min)) (start (1+ (point))) point number)
        ;; Scan from point-min to (1+ (point)) to find the current headline.
        ;; (1+ (point)) to include under current point headline into the scan range.
        (if (<= start (point-max))
            (while (setq point (text-property-not-all
                                current start 'paw-id nil))
              (setq current (1+ point))) ; not at (point-max)
          (while (setq point (text-property-not-all
                              current (point-max) 'paw-id nil))
            (setq current (1+ point)))) ; at the (point-max)
        (setq number (1- (get-text-property (1- current) 'paw-id)))
        (goto-char (text-property-any (point-min) (point-max) 'paw-id (1+ number))))))))

(defun paw-next ()
  (let* ((header-in-line (text-property-not-all (line-beginning-position) (line-end-position) 'paw-id nil))
         (location (get-text-property (or header-in-line (point)) 'paw-id))
         next)
    (cond
     ;; check the current line headline number first, since if use org-cycle, cursor will go to the begining of line
     ((numberp location)
      (setq next (text-property-any (point-min) (point-max) 'paw-id (1+ location)))
      (if (numberp next)
          (goto-char next)
        (goto-char (point-max))))
     ;; check the current point if >= the first header (no matter level), keep (point) if no headlines
     ((>= (setq next (or (text-property-not-all (point-min) (point-max) 'paw-id nil) (point))) (point))
      (if (equal next (point))
          (progn
            (message "End of buffer")
            (goto-char (point-max)) )
        (goto-char next)))
     (t
      (let ((current (point-min)) (start (1+ (point))) point number)
        ;; Scan from point-min to (1+ (point)) to find the current headline.
        ;; (1+ (point)) to include under current point headline into the scan range.
        (unless (> start (point-max))
          (while (setq point (text-property-not-all
                              current start 'paw-id nil))
            (setq current (1+ point))))
        (cond ((equal (point) 1) (setq number 0))
              ((equal (point) 2) (setq number 0))
              ((equal (point) (point-max)) (setq number (point-max)) (message "End of buffer"))
              (t
               (setq number (1- (get-text-property (1- current) 'paw-id)))))
        (goto-char (or (text-property-any (point-min) (point-max) 'paw-id (+ 2 number)) (point-max))))))))

;;;###autoload
(defun paw-mark-at-point ()
  "Mark the current line."
  (interactive)
  (remove-overlays (line-beginning-position) (line-end-position))
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (inhibit-read-only t)
         (overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'paw-mark-face)
    (put-text-property beg end 'paw-mark ?>)))

;;;###autoload
(defun paw-unmark-at-point ()
  "Unmark the current line."
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (inhibit-read-only t))
    (remove-overlays (line-beginning-position) (line-end-position))
    (remove-text-properties beg end '(paw-mark nil))))

;;;###autoload
(defun paw-mark-and-forward ()
  "Mark the current line and forward."
  (interactive)
  (paw-mark-at-point)
  (paw-next))

;;;###autoload
(defun paw-unmark-and-forward ()
  "Unmark the current line and forward."
  (interactive)
  (paw-unmark-at-point)
  (paw-next))

;;;###autoload
(defun paw-unmark-and-backward ()
  "Unmark the current line and backward."
  (interactive)
  (paw-previous)
  (paw-unmark-at-point))

(defun paw-clear-marks ()
  (if (eq (current-buffer) (get-buffer "*paw*"))
      (let* ((beg (point-min))
             (end (point-max))
             (inhibit-read-only t))
        (remove-overlays beg end)
        (remove-text-properties beg end '(paw-mark nil)))))

;;;###autoload
(defun paw-find-candidate-at-point ()
  "Find candidate at point and return the list."
  (interactive)
  (get-text-property (point) 'paw-entry))

;;;###autoload
(defun paw-find-marked-candidates ()
  "Find marked candidates and return the alist."
  (interactive)
  (save-excursion
    (let (candidate beg end cand-list)
      (when (text-property-not-all (point-min) (point-max) 'paw-mark nil)
        (setq end (text-property-any (point-min) (point-max) 'paw-mark ?>))
        (while (setq beg (text-property-any end (point-max) 'paw-mark ?>) )
          (goto-char beg)
          (setq candidate (paw-find-candidate-at-point))
          (push candidate cand-list)
          ;; (message (number-to-string beg))
          (forward-line 1)
          (setq end (point)))
        cand-list))))


(defun paw-insert-and-make-overlay (str prop val &optional export)
  "when `export' is t, use insert directly, otherwise use overlay"
  (if export
      (insert str)
    (let* ((start (point))
           (end (+ start (length str)))
           overlay)
      (insert str)
      (setq overlay (make-overlay start end))
      (overlay-put overlay prop val)
      overlay)))

;;;###autoload
(defun paw-scroll-up(arg)
  (interactive "p")
  (cond ((eq major-mode 'paw-view-note-mode)
         (call-interactively 'org-forward-element)
         (recenter 0))
        ((eq major-mode 'nov-mode)
         (call-interactively 'nov-scroll-up))
        (t (call-interactively 'scroll-up))))

;;;###autoload
(defun paw-scroll-down(arg)
  (interactive "P")
  (cond ((eq major-mode 'paw-view-note-mode)
         (call-interactively 'org-backward-element)
         (recenter 0))
        ((eq major-mode 'nov-mode)
         (call-interactively 'nov-scroll-down))
        (t (call-interactively 'scroll-down))))

;;;###autoload
(defun paw-goto-toc()
  (interactive)
  (pcase major-mode
    ('nov-mode (nov-goto-toc))
    ('wallabag-entry-mode (wallabag))
    (_ (consult-notes))))

;;;###autoload
(defun paw-step-backward()
  (interactive)
  (pcase major-mode
    ('nov-mode (nov-history-back))
    ('wallabag-search-mode
     (if (buffer-live-p (get-buffer "*wallabag-entry*"))
         (switch-to-buffer "*wallabag-entry*")
       (message "No wallabag entry buffer found")))
    ('wallabag-entry-mode
     (if (buffer-live-p (get-buffer "*wallabag-search*"))
         (switch-to-buffer "*wallabag-search*")
       (message "No wallabag search buffer found")))
    (_ (better-jumper-jump-backward))))

;;;###autoload
(defun paw-step-forward()
  (interactive)
  (pcase major-mode
    ('nov-mode (nov-history-back))
    (_ (better-jumper-jump-forward))))


;;;###autoload
(defun paw-view-current-thing()
  (interactive)
  (if (bound-and-true-p focus-mode)
      (funcall-interactively 'paw-view-note-current-thing)
    (funcall-interactively 'paw-view-note)))

;;;###autoload
(defun paw-occur()
  (interactive)
  (pcase major-mode
    ('nov-mode (shrface-occur))
    (_ (call-interactively 'occur))))


;;;###autoload
(defun paw-replay ()
  (interactive)
  (let* ((entry (alist-get 'word (get-char-property (point) 'paw-entry)))
         (word (if entry
                   (s-collapse-whitespace
                    (paw-get-real-word entry) )
                 (if mark-active
                     (s-collapse-whitespace
                      (buffer-substring-no-properties (region-beginning) (region-end)) )
                   (thing-at-point 'word t))))
         (englishp (if (string-match-p "[a-z-A-Z]" word) t nil)))
    (message "Playing...")
    (if englishp
        (let ((player (or (executable-find "mpv")
                          (executable-find "mplayer")
                          (executable-find "mpg123"))))
          (if player
              (start-process
               player
               nil
               player
               (format "https://dict.youdao.com/dictvoice?type=2&audio=%s" (url-hexify-string word)))
            (message "mpv, mplayer or mpg123 is needed to play word voice")))
      (if (eq system-type 'darwin)
          (call-process-shell-command
           (format "say -v Kyoko %s" word) nil 0)
        (let ((player (or (executable-find "mpv")
                          (executable-find "mplayer")
                          (executable-find "mpg123"))))
          (if player
              (start-process
               player
               nil
               player
               (format "https://dict.youdao.com/dictvoice?type=2&audio=%s" (url-hexify-string word)))
            (message "mpv, mplayer or mpg123 is needed to play word voice")))))))

(defcustom paw-click-overlay-enable nil
  "Enable click overlay when paw-annotation-mode is enabled. If t,
show the overlay when the item is clicked."
  :group 'paw
  :type 'boolean)

(defvar paw-click-overlay nil
  "Overlay used to show the item was clicked.")

(defun paw-click-show (pos end-pos face)
  "Show the thing user click to help the user find it.
POS start position

END-POS end position, flash the characters between the two
points

FACE the flash face used

DELAY the flash delay"
  (when (and paw-click-overlay-enable (bound-and-true-p paw-annotation-mode))
    (if paw-click-overlay
        (delete-overlay paw-click-overlay) )
    (setq paw-click-overlay (or paw-click-overlay (make-overlay (point-min) (point-min))))
    (overlay-put paw-click-overlay 'face face)
    (move-overlay paw-click-overlay pos end-pos)))

(defun paw-flash-show (pos end-pos face delay)
  "Flash a temporary highlight to help the user find something.
POS start position

END-POS end position, flash the characters between the two
points

FACE the flash face used

DELAY the flash delay"
  (when (and (numberp delay)
             (> delay 0))
    ;; else
    (when (timerp next-error-highlight-timer)
      (cancel-timer next-error-highlight-timer))
    (setq compilation-highlight-overlay (or compilation-highlight-overlay
                                            (make-overlay (point-min) (point-min))))
    (overlay-put compilation-highlight-overlay 'face face)
    (overlay-put compilation-highlight-overlay 'priority 10000)
    (move-overlay compilation-highlight-overlay pos end-pos)
    (add-hook 'pre-command-hook #'compilation-goto-locus-delete-o)
    (setq next-error-highlight-timer
          (run-at-time delay nil #'compilation-goto-locus-delete-o))))


(defun paw-find-origin (&optional entry switch)
  "Go to the original location of the entry."
  (interactive)
  (let* ((entry (or entry
                    (get-text-property (point) 'paw-entry)
                    (car (paw-candidate-by-word (paw-note-word)))))
         (origin-type (alist-get 'origin_type entry))
         (origin-path (alist-get 'origin_path entry))
         (origin-path-file (file-name-nondirectory origin-path))
         (origin-path (if (string-match-p "^http\\(s\\)?://[^ \n]*$" origin-path)
                          origin-path
                          (if (file-exists-p origin-path) ;; check the file in
                              ;; origin-path first
                              origin-path
                            (let ((new-path (-first (lambda (dir) ;; if not exist,
                                                      ;; check in
                                                      ;; paw-annotation-search-paths
                                                      (file-exists-p (expand-file-name origin-path-file dir)))
                                                    paw-annotation-search-paths)))
                              (if new-path
                                  (expand-file-name origin-path-file new-path)
                                origin-path))) ))
         (origin-id (alist-get 'origin_id entry))
         (origin-point (alist-get 'origin_point entry))
         (word (paw-get-real-word entry)))
    (pcase origin-type
      ('wallabag-entry-mode
       (require 'wallabag)
       (let ((entry (wallabag-db-select origin-id)))
         (if entry
             (progn
               (let ((wallabag-show-entry-switch 'switch-to-buffer-other-window))
                 (wallabag-show-entry (car entry)))
               (paw-goto-location origin-point word))
           (message "No this entry."))))
      ('nov-mode
       (require 'nov)
       (if (file-exists-p origin-path)
           (progn
             (if (buffer-live-p (get-buffer (file-name-nondirectory origin-path)))
                 (if switch
                     (switch-to-buffer-other-window (get-buffer (file-name-nondirectory origin-path)))
                   (switch-to-buffer (get-buffer (file-name-nondirectory origin-path))))
               (if switch
                   (find-file-other-window origin-path)
                 (find-file origin-path)))
             (paw-goto-location origin-point word))
         (message "File %s not exists." origin-path)))
      ('pdf-view-mode
       (require 'pdf-tools)
       (if (file-exists-p origin-path)
           (with-current-buffer
               (if (buffer-live-p (get-buffer (file-name-nondirectory origin-path)))
                   (if switch
                       (switch-to-buffer-other-window (get-buffer (file-name-nondirectory origin-path)))
                     (switch-to-buffer (get-buffer (file-name-nondirectory origin-path))))
                 (if switch
                     (find-file-other-window origin-path)
                   (find-file origin-path)))
             (paw-goto-location origin-point word)
             (unless switch
               (other-window 1)
               (if (buffer-live-p (get-buffer "*paw*"))
                   (switch-to-buffer "*paw*"))))
         (message "File %s not exists." origin-path)))
      ('eww-mode
       (lexical-let ((origin-point origin-point)
                     (word word))
         (defun paw-goto-location-eww-callback ()
           (paw-goto-location origin-point word)
           ;; Remove the hook after running
           (remove-hook 'eww-after-render-hook 'paw-goto-location-eww-callback))

         ;; Call EWW
         (if (not (get-buffer "*eww*"))
             (progn
               (eww-browse-url origin-path)
               (add-hook 'eww-after-render-hook 'paw-goto-location-eww-callback))
           (switch-to-buffer "*eww*")
           (let ((eww-buffer-url (plist-get eww-data :url)))
             (if (string= eww-buffer-url origin-path)
                 (paw-goto-location origin-point word)
               (eww-browse-url origin-path)
               (add-hook 'eww-after-render-hook 'paw-goto-location-eww-callback))))))
      ('eaf-mode
       (if (eq system-type 'android) ; if android, use browser
           (paw-android-browse-url origin-path)
         (require 'eaf)
         (let* ((buffer (eaf-interleave--find-buffer (expand-file-name origin-path))))
           (if buffer
               (progn
                 (switch-to-buffer-other-window buffer)
                 (eaf-interleave--display-buffer buffer)
                 (when origin-point
                   (with-current-buffer buffer
                     (pcase origin-id
                       ("pdf-viewer"
                        (eaf-interleave--pdf-viewer-goto-page (expand-file-name origin-path) origin-point))
                       (_ nil)))))
             (pcase origin-id ;; online word origin-id is studylist id
               ("pdf-viewer"
                (eaf-interleave--open-pdf (expand-file-name origin-path))
                (eaf-interleave--pdf-viewer-goto-page (expand-file-name origin-path) origin-point))
               ("browser"
                (eaf-interleave--open-web-url origin-path))
               (_ (eaf-interleave--open-web-url origin-path))))) ))
      ("browser"
       (if (eq system-type 'android)
           (browse-url origin-path)
         (require 'eaf)
         (let* ((buffer (eaf-interleave--find-buffer (expand-file-name origin-path))))
           (if buffer
               (progn
                 (switch-to-buffer-other-window buffer)
                 (eaf-interleave--display-buffer buffer))
             (eaf-interleave--open-web-url origin-path)))))
      ("pdf-viewer"
       (require 'eaf)
       (let* ((buffer (eaf-interleave--find-buffer (expand-file-name origin-path))))
         (if buffer
             (progn
               (switch-to-buffer-other-window buffer)
               (eaf-interleave--display-buffer buffer)
               (eaf-interleave--pdf-viewer-goto-page (expand-file-name origin-path) origin-point))
           (eaf-interleave--open-pdf (expand-file-name origin-path))
           (eaf-interleave--pdf-viewer-goto-page (expand-file-name origin-path) origin-point))))
      (_
       (if (stringp origin-path)
           (if (file-exists-p origin-path)
               (progn
                 (if switch
                     (find-file-other-window origin-path)
                   (find-file origin-path))
                 (paw-goto-location origin-point word))
             (message "File %s not exists." origin-path))
         (message "Can not find the origin.")))))
  ;; back to paw
  ;; (let ((buffer (get-buffer "*paw*")))
  ;;   (if (buffer-live-p buffer)
  ;;       (let ((window (get-buffer-window buffer)))
  ;;         (if window
  ;;             (select-window window)
  ;;           (switch-to-buffer buffer)))))
  )

(defun paw-goto-location-eww-callback (location real-word)
  (paw-goto-location location real-word)
  ;; Remove the hook after running
  (remove-hook 'eww-after-render-hook 'paw-goto-location-eww-callback))

(defun paw-goto-location (location real-word)
  "Go to location specified by LOCATION."
  (let ((window (get-buffer-window (current-buffer)))
        (mode major-mode))
    (with-selected-window window
      (cond
       ;; ((run-hook-with-args-until-success 'org-noter--doc-goto-location-hook mode location))
       ((memq mode '(doc-view-mode pdf-view-mode))
        (require 'org-noter)
        (let ((page (if (numberp location) location (car location)))
              (pos (if (numberp location) nil (cdr location))))
          (if (eq mode 'doc-view-mode)
              (doc-view-goto-page page)
            (pdf-view-goto-page page)
            ;; NOTE(nox): This timer is needed because the tooltip may introduce a delay,
            ;; so syncing multiple pages was slow
            (if pos
                (when (>= org-noter-arrow-delay 0)
                  (when org-noter--arrow-location (cancel-timer (aref org-noter--arrow-location 0)))
                  (setq org-noter--arrow-location
                        (vector (run-with-idle-timer org-noter-arrow-delay nil 'org-noter--show-arrow)
                                window
                                pos)))))
          (if pos
              (image-scroll-up (- (org-noter--conv-page-percentage-scroll pos)
                                  (window-vscroll))))))
       ((eq mode 'eaf-mode)
        (eaf-interleave--pdf-viewer-goto-page eaf--buffer-url location))
       ((eq mode 'nov-mode)
        (cond ((listp location)
               (let ((pos (cdr location)))
                 (if (eq nov-documents-index (car location))
                     (paw-search-location pos real-word)
                   (nov-goto-document (car location))
                   (paw-search-location pos real-word))
                 ;; (paw-show-all-annotations)     ; TODO performance issue
                 ))
              (t (paw-search-location location real-word)))
        (recenter))
       ((eq mode 'wallabag-entry-mode)
        (paw-search-location location real-word)
        (recenter))
       ((eq mode 'eww-mode)
        (paw-search-location location real-word)
        (paw-show-all-annotations)
        (recenter))
       (t
        (paw-search-location location real-word)
        (paw-show-all-annotations))
       (recenter))
      (unless paw-annotation-mode
        (paw-annotation-mode 1))
      ;; NOTE(nox): This needs to be here, because it would be issued anyway after
      ;; everything and would run org-noter--nov-scroll-handler.
      ;; (redisplay)
      )))

(defun paw-search-location (location real-word)
  "Search LOCATION, and verify REAL-WORD.
Finally goto the location that was tuned."
  (let (beg end)
    (cond ((numberp location)
           (progn
             (goto-char location)
             (if (string-match-p (regexp-quote (or (thing-at-point 'word t) "")) real-word)
                 (progn
                   (goto-char location)
                   (forward-thing 'word 1)
                   (setq end (point))
                   (forward-thing 'word -1)
                   (setq beg (point))
                   (paw-flash-show beg end 'highlight 1))
               ;; first search -50 ~ 50
               (goto-char (- location 50))
               (cond ((re-search-forward (regexp-quote real-word) (+ location 50) t)
                      (setq beg (match-beginning 0))
                      (setq end (match-end 0)))
                     (t
                      ;; second search (point-min) (point-max)
                      (goto-char (point-min))
                      (when (re-search-forward (regexp-quote real-word) (point-max) t)
                        (setq beg (match-beginning 0))
                        (setq end (match-end 0)))))) ))
          ((listp location)
           (setq beg (car location))
           (setq end (cdr location))
           (if (string-match-p (regexp-quote (s-trim (s-collapse-whitespace (buffer-substring-no-properties beg end)))) (s-trim (s-collapse-whitespace real-word) ))
               (progn
                 (goto-char (car location))
                 (paw-flash-show beg end 'highlight 1))
             (goto-char (- beg 500))
             (if (re-search-forward (regexp-quote real-word) (+ end 500) t)
                 (progn
                   (setq beg (match-beginning 0))
                   (setq end (match-end 0)))
               (goto-char (point-min))
               ;; second search 0 (point-max)
               (if (re-search-forward (regexp-quote real-word) (point-max) t)
                   (progn
                     (setq beg (match-beginning 0))
                     (setq end (match-end 0)))
                 (goto-char (car location))
                 (paw-flash-show beg end 'highlight 1)))))
          (t
           (goto-char (point-min))
           (let ((case-fold-search t))  ; or nil for case-sensitive
             (if (if (string-match-p "[[:ascii:]]+" real-word)
                     ;; english
                     (re-search-forward (concat "\\b" real-word "\\b") nil t)
                   ;; non-english
                   (re-search-forward real-word nil t))
                 (progn
                   (setq beg (match-beginning 0))
                   (setq end (match-end 0)))
               (message "Can not find \"%s\", maybe the location is changed, or it is an online word added from another location."
                        (s-truncate 40 (s-collapse-whitespace real-word))))))
          ;; goto the beg of tuned location
          (goto-char beg))))


(provide 'paw-util)
