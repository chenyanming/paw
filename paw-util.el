;;; paw-util.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'paw-gptel)
(require 'paw-sdcv)
(require 'paw-goldendict)
(require 'paw-go-translate)
(require 'paw-mac)
(require 'paw-android)
(require 'paw-dictionary)
(require 'compile)
(require 'esxml-query)

(require 'thingatpt)
(require 'esxml-query)
(require 'esxml)

(eval-when-compile (defvar paw-current-entry))

(defcustom paw-player-program
  (cond
   ((eq system-type 'darwin)
    (or (executable-find "afplay")
        (executable-find "mpv")
        (executable-find "mplayer")
        (executable-find "mpg123")))
   (t
    (or (executable-find "mpv")
        (executable-find "mplayer")
        (executable-find "mpg123"))))
  "paw player program"
  :group 'paw
  :type 'boolean)

(defvar paw-provider-url "")

(defcustom paw-say-word-p t
  "paw say word automatically"
  :group 'paw
  :type 'boolean)

(defcustom paw-tts-english-voice "en-US-BrianMultilingualNeural"
  "English tts voice."
  :group 'paw
  :type '(choice (const :tag "Female en-US-AvaNeural" "en-US-AvaNeural")
                 (const :tag "Male en-AU-WilliamNeural" "en-AU-WilliamNeural")
                 (const :tag "Female en-CA-ClaraNeural" "en-CA-ClaraNeural")
                 (const :tag "Male en-CA-LiamNeural" "en-CA-LiamNeural")
                 (const :tag "Female en-GB-LibbyNeural" "en-GB-LibbyNeural")
                 (const :tag "Female en-GB-MaisieNeural" "en-GB-MaisieNeural")
                 (const :tag "Male en-GB-RyanNeural" "en-GB-RyanNeural")
                 (const :tag "Female en-GB-SoniaNeural" "en-GB-SoniaNeural")
                 (const :tag "Male en-GB-ThomasNeural" "en-GB-ThomasNeural")
                 (const :tag "Male en-HK-SamNeural" "en-HK-SamNeural")
                 (const :tag "Female en-HK-YanNeural" "en-HK-YanNeural")
                 (const :tag "Male en-IE-ConnorNeural" "en-IE-ConnorNeural")
                 (const :tag "Female en-IE-EmilyNeural" "en-IE-EmilyNeural")
                 (const :tag "Female en-IN-NeerjaExpressiveNeural" "en-IN-NeerjaExpressiveNeural")
                 (const :tag "Female en-IN-NeerjaNeural" "en-IN-NeerjaNeural")
                 (const :tag "Male en-IN-PrabhatNeural" "en-IN-PrabhatNeural")
                 (const :tag "Female en-KE-AsiliaNeural" "en-KE-AsiliaNeural")
                 (const :tag "Male en-KE-ChilembaNeural" "en-KE-ChilembaNeural")
                 (const :tag "Male en-NG-AbeoNeural" "en-NG-AbeoNeural")
                 (const :tag "Female en-NG-EzinneNeural" "en-NG-EzinneNeural")
                 (const :tag "Male en-NZ-MitchellNeural" "en-NZ-MitchellNeural")
                 (const :tag "Female en-NZ-MollyNeural" "en-NZ-MollyNeural")
                 (const :tag "Male en-PH-JamesNeural" "en-PH-JamesNeural")
                 (const :tag "Female en-PH-RosaNeural" "en-PH-RosaNeural")
                 (const :tag "Female en-SG-LunaNeural" "en-SG-LunaNeural")
                 (const :tag "Male en-SG-WayneNeural" "en-SG-WayneNeural")
                 (const :tag "Male en-TZ-ElimuNeural" "en-TZ-ElimuNeural")
                 (const :tag "Female en-TZ-ImaniNeural" "en-TZ-ImaniNeural")
                 (const :tag "Female en-US-AnaNeural" "en-US-AnaNeural")
                 (const :tag "Male en-US-AndrewMultilingualNeural" "en-US-AndrewMultilingualNeural")
                 (const :tag "Male en-US-AndrewNeural" "en-US-AndrewNeural")
                 (const :tag "Female en-US-AriaNeural" "en-US-AriaNeural")
                 (const :tag "Female en-US-AvaMultilingualNeural" "en-US-AvaMultilingualNeural")
                 (const :tag "Female en-US-AvaNeural" "en-US-AvaNeural")
                 (const :tag "Male en-US-BrianMultilingualNeural" "en-US-BrianMultilingualNeural")
                 (const :tag "Male en-US-BrianNeural" "en-US-BrianNeural")
                 (const :tag "Male en-US-ChristopherNeural" "en-US-ChristopherNeural")
                 (const :tag "Female en-US-EmmaMultilingualNeural" "en-US-EmmaMultilingualNeural")
                 (const :tag "Female en-US-EmmaNeural" "en-US-EmmaNeural")
                 (const :tag "Male en-US-EricNeural" "en-US-EricNeural")
                 (const :tag "Male en-US-GuyNeural" "en-US-GuyNeural")
                 (const :tag "Female en-US-JennyNeural" "en-US-JennyNeural")
                 (const :tag "Female en-US-MichelleNeural" "en-US-MichelleNeural")
                 (const :tag "Male en-US-RogerNeural" "en-US-RogerNeural")
                 (const :tag "Male en-US-SteffanNeural" "en-US-SteffanNeural")
                 (const :tag "Female en-ZA-LeahNeural" "en-ZA-LeahNeural")
                 (const :tag "Male en-ZA-LukeNeural" "en-ZA-LukeNeural")))

(defun paw-tts-select-english-voice ()
  "Select English TTS Voice."
  (interactive)
  (setq paw-tts-english-voice
        (completing-read "Select English TTS Voice: "
                         '("en-US-AvaNeural"
                           "en-AU-WilliamNeural"
                           "en-CA-ClaraNeural"
                           "en-CA-LiamNeural"
                           "en-GB-LibbyNeural"
                           "en-GB-MaisieNeural"
                           "en-GB-RyanNeural"
                           "en-GB-SoniaNeural"
                           "en-GB-ThomasNeural"
                           "en-HK-SamNeural"
                           "en-HK-YanNeural"
                           "en-IE-ConnorNeural"
                           "en-IE-EmilyNeural"
                           "en-IN-NeerjaExpressiveNeural"
                           "en-IN-NeerjaNeural"
                           "en-IN-PrabhatNeural"
                           "en-KE-AsiliaNeural"
                           "en-KE-ChilembaNeural"
                           "en-NG-AbeoNeural"
                           "en-NG-EzinneNeural"
                           "en-NZ-MitchellNeural"
                           "en-NZ-MollyNeural"
                           "en-PH-JamesNeural"
                           "en-PH-RosaNeural"
                           "en-SG-LunaNeural"
                           "en-SG-WayneNeural"
                           "en-TZ-ElimuNeural"
                           "en-TZ-ImaniNeural"
                           "en-US-AnaNeural"
                           "en-US-AndrewMultilingualNeural"
                           "en-US-AndrewNeural"
                           "en-US-AriaNeural"
                           "en-US-AvaMultilingualNeural"
                           "en-US-AvaNeural"
                           "en-US-BrianMultilingualNeural"
                           "en-US-BrianNeural"
                           "en-US-ChristopherNeural"
                           "en-US-EmmaMultilingualNeural"
                           "en-US-EmmaNeural"
                           "en-US-EricNeural"
                           "en-US-GuyNeural"
                           "en-US-JennyNeural"
                           "en-US-MichelleNeural"
                           "en-US-RogerNeural"
                           "en-US-SteffanNeural"
                           "en-ZA-LeahNeural"
                           "en-ZA-LukeNeural"))))

(defcustom paw-tts-japanese-voice "ja-JP-KeitaNeural"
  "Japanese tts voice."
  :group 'paw
  :type '(choice (const :tag "Female ja-JP-NanamiNeural" "ja-JP-NanamiNeural")
                 (const :tag "Male ja-JP-KeitaNeural" "ja-JP-KeitaNeural")))

(defun paw-tts-select-japanese-voice ()
  "Select Japanese TTS Voice."
  (interactive)
  (setq paw-tts-japanese-voice
        (completing-read "Select Japanese TTS Voice: "
                         '("ja-JP-NanamiNeural"
                           "ja-JP-KeitaNeural"))))

(defcustom paw-tts-korean-voice "ko-KR-SunHiNeural"
  "Korean tts voice."
  :group 'paw
  :type '(choice (const :tag "Female ko-KR-SunHiNeural" "ko-KR-SunHiNeural")
          (const :tag "Male ko-KR-HyunsuNeural" "ko-KR-HyunsuNeural")
          (const :tag "Male ko-KR-InJoonNeural"  "ko-KR-InJoonNeural")))

(defun paw-tts-select-korean-voice ()
  "Select Korean TTS Voice."
  (interactive)
  (setq paw-tts-korean-voice
        (completing-read "Select Korean TTS Voice: "
                         '("ko-KR-SunHiNeural"
                           "ko-KR-HyunsuNeural"
                           "ko-KR-InJoonNeural"))))

(defcustom paw-tts-zh-cn-voice "zh-CN-YunyangNeural"
  "Chinese tts voice, if detected as zh."
  :group 'paw
  :type '(choice (const :tag "Female zh-CN-XiaoxiaoNeural" "zh-CN-XiaoxiaoNeural")
          (const :tag "Female zh-CN-XiaoyiNeural" "zh-CN-XiaoyiNeural")
          (const :tag "Male zh-CN-YunjianNeural" "zh-CN-YunjianNeural")
          (const :tag "Male zh-CN-YunxiNeural" "zh-CN-YunxiNeural")
          (const :tag "Male zh-CN-YunxiaNeural" "zh-CN-YunxiaNeural")
          (const :tag "Male zh-CN-YunyangNeural" "zh-CN-YunyangNeural")
          (const :tag "Female zh-CN-liaoning-XiaobeiNeural" "zh-CN-liaoning-XiaobeiNeural")
          (const :tag "Female zh-CN-shaanxi-XiaoniNeural" "zh-CN-shaanxi-XiaoniNeural")
          (const :tag "Female zh-TW-HsiaoChenNeural" "zh-TW-HsiaoChenNeural")
          (const :tag "Female zh-HK-HiuGaaiNeural" "zh-HK-HiuGaaiNeural")
          (const :tag "Female zh-HK-HiuMaanNeural" "zh-HK-HiuMaanNeural")
          (const :tag "Male zh-HK-WanLungNeural" "zh-HK-WanLungNeural")
          (const :tag "Female zh-TW-HsiaoYuNeural" "zh-TW-HsiaoYuNeural")
          (const :tag "Male zh-TW-YunJheNeural" "zh-TW-YunJheNeural")))

(defun paw-tts-select-zh-cn-voice ()
  "Select Chinese TTS Voice."
  (interactive)
  (setq paw-tts-zh-cn-voice
        (completing-read "Select Chinese TTS Voice: "
                         '("zh-CN-XiaoxiaoNeural"
                           "zh-CN-XiaoyiNeural"
                           "zh-CN-YunjianNeural"
                           "zh-CN-YunxiNeural"
                           "zh-CN-YunxiaNeural"
                           "zh-CN-YunyangNeural"
                           "zh-CN-liaoning-XiaobeiNeural"
                           "zh-CN-shaanxi-XiaoniNeural"
                           "zh-TW-HsiaoChenNeural"
                           "zh-HK-HiuGaaiNeural"
                           "zh-HK-HiuMaanNeural"
                           "zh-HK-WanLungNeural"
                           "zh-TW-HsiaoYuNeural"
                           "zh-TW-YunJheNeural"))))

(defcustom paw-tts-zh-tw-voice "zh-TW-HsiaoChenNeural"
  "Chinese tts voice, if detected as zh-Hant."
  :group 'paw
  :type '(choice (const :tag "Female zh-CN-XiaoxiaoNeural" "zh-CN-XiaoxiaoNeural")
                 (const :tag "Male zh-CN-YunJheNeural" "zh-CN-YunJheNeural")
                 (const :tag "Female zh-TW-HsiaoChenNeural" "zh-TW-HsiaoChenNeural")
                 (const :tag "Female zh-HK-HiuGaaiNeural" "zh-HK-HiuGaaiNeural")
                 (const :tag "Female zh-CN-XiaoyiNeural" "zh-CN-XiaoyiNeural")
                 (const :tag "Male zh-CN-YunjianNeural" "zh-CN-YunjianNeural")
                 (const :tag "Male zh-CN-YunxiNeural" "zh-CN-YunxiNeural")
                 (const :tag "Male zh-CN-YunxiaNeural" "zh-CN-YunxiaNeural")
                 (const :tag "Male zh-CN-YunyangNeural" "zh-CN-YunyangNeural")
                 (const :tag "Female zh-CN-liaoning-XiaobeiNeural" "zh-CN-liaoning-XiaobeiNeural")
                 (const :tag "Female zh-CN-shaanxi-XiaoniNeural" "zh-CN-shaanxi-XiaoniNeural")
                 (const :tag "Female zh-HK-HiuMaanNeural" "zh-HK-HiuMaanNeural")
                 (const :tag "Male zh-HK-WanLungNeural" "zh-HK-WanLungNeural")
                 (const :tag "Female zh-TW-HsiaoYuNeural" "zh-TW-HsiaoYuNeural")
                 (const :tag "Male zh-TW-YunJheNeural" "zh-TW-YunJheNeural")))

(defun paw-tts-select-zh-tw-voice ()
  "Select Chinese TTS Voice."
  (interactive)
  (setq paw-tts-zh-tw-voice
        (completing-read "Select Chinese TTS Voice: "
                         '("zh-CN-XiaoxiaoNeural"
                           "zh-CN-XiaoyiNeural"
                           "zh-CN-YunjianNeural"
                           "zh-CN-YunxiNeural"
                           "zh-CN-YunxiaNeural"
                           "zh-CN-YunyangNeural"
                           "zh-CN-liaoning-XiaobeiNeural"
                           "zh-CN-shaanxi-XiaoniNeural"
                           "zh-TW-HsiaoChenNeural"
                           "zh-HK-HiuGaaiNeural"
                           "zh-HK-HiuMaanNeural"
                           "zh-HK-WanLungNeural"
                           "zh-TW-HsiaoYuNeural"
                           "zh-TW-YunJheNeural"))))

(defcustom paw-tts-multilingual-voice "en-US-BrianMultilingualNeural"
  "Multilingual tts voice."
  :group 'paw
  :type '(choice (const :tag "Female en-US-AvaMultilingualNeural" "en-US-AvaMultilingualNeural")
          (const :tag "Male de-DE-ConradNeural" "de-DE-ConradNeural")
          (const :tag "Female de-DE-SeraphinaMultilingualNeural" "de-DE-SeraphinaMultilingualNeural")
          (const :tag "Male en-US-AndrewMultilingualNeural" "en-US-AndrewMultilingualNeural")
          (const :tag "Male fr-FR-RemyMultilingualNeural" "fr-FR-RemyMultilingualNeural")
          (const :tag "Female fr-FR-VivienneMultilingualNeural" "fr-FR-VivienneMultilingualNeural")))

(defun paw-tts-select-multilingual-voice ()
  "Select Multilingual TTS Voice."
  (interactive)
  (setq paw-tts-multilingual-voice
        (completing-read "Select Multilingual TTS Voice: "
                         '("en-US-AvaMultilingualNeural"
                           "en-US-BrianMultilingualNeural"
                           "en-US-EmmaMultilingualNeural"
                           "de-DE-ConradNeural"
                           "de-DE-SeraphinaMultilingualNeural"
                           "en-US-AndrewMultilingualNeural"
                           "fr-FR-RemyMultilingualNeural"
                           "fr-FR-VivienneMultilingualNeural"))))


(defcustom paw-posframe-p nil
  "show paw-view-note in posframe"
  :group 'paw
  :type 'boolean)

(defcustom paw-translate-p t
  "translate automatically"
  :group 'paw
  :type 'boolean)

(define-obsolete-variable-alias 'paw-transalte-p
  'paw-translate-p "paw 1.1.1")

(defcustom paw-translate-context-p t
  "translate context automatically"
  :group 'paw
  :type 'boolean)

(define-obsolete-variable-alias 'paw-transalte-context-p
  'paw-translate-context-p "paw 1.1.1")

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
   ((eq system-type 'darwin)
    'paw-mac-dictionary-search-details)
   (t
    'paw-goldendict-search-details))
  "paw share the word to system tool"
  :group 'paw
  :type '(choice (function-item paw-android-search-details)
          (function-item paw-mac-dictionary-search-details)
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

(defcustom paw-dictionary-function
  (cond
   ((eq system-type 'android)
    'paw-eudic-search-details)
   ((eq system-type 'darwin)
    'paw-mac-dictionary-search-details)
   (t
    'paw-goldendict-search-details))
  "paw dictionary function, Default dictionary function for querying
the WORD."
  :group 'paw
  :type '(choice (function-item paw-dictionary-search)
          (function-item paw-mac-dictionary-search-details)
          (function-item paw-eudic-search-details)
          (function-item paw-goldendict-search-details)
          function))

(defcustom paw-search-function #'paw-sdcv-search-with-dictionary-async
  "Default search function for querying the WORD. Its purpose is to
search the WORD, and replace the content under Meaning section
inside BUFFER.

By deafult, we use sdcv to search the WORD, you can change it
other search function. We may change it in the future.

Don't change it if you are unsure.

Be careful, the following behavior may be changed in the future:
1. It is required to be able to insert into the Meaning section direcly
2. This function can be overridden by the `:kagome' property in paw-view-note."
  :group 'paw
  :type 'function)

(defcustom paw-translate-function 'paw-go-translate-insert
  "paw translate function. Its purpose is to tranlate the WORD with
LANG, and replace the content under Translation section inside
BUFFER.

By deafult, we use go-translate to search the word, you can change it
other search function. We may change it in the future.

Don't change it if you are unsure.

Be careful, the following behavior may be changed in the future.
1. It is required to be able to insert into the Translation section direcly."
  :group 'paw
  :type '(choice (function-item paw-go-translate-insert)
          function))

(defcustom paw-ai-translate-function 'paw-gptel-translate
  "paw ai translate (gptel) function"
  :group 'paw
  :type '(choice (function-item paw-gptel-translate)
          function))

(defcustom paw-ask-ai-function 'paw-gptel-query
  "paw ask ai (gptel) function"
  :group 'paw
  :type '(choice (function-item paw-gptel-query)
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
   ((eq system-type 'darwin)
    'paw-mac-dictionary-search-details)
   (t
    'paw-goldendict-search-details))
  "paw external dictionary function"
  :group 'paw
  :type '(choice (function-item paw-goldendict-search-details)
          (function-item paw-mac-dictionary-search-details)
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

(defun paw-new-entry(word &rest properties)
  ;; create new entry
  ;; example ((word . "major") (exp . "adj. 主要的；主修的；重要的；较多的; n. 成年人；主修科目；陆军少校; vi. 主修<br>...") (content . 0) (serverp . 1) (note . "") (note_type word . "✎") (origin_type) (origin_path . "PN") (origin_id . "1709212272") (origin_point) (created_at . "2024-04-24 19:11:00"))
  ;; kagome: NOT the database field
  ;; lang: NOT the database field
  `((word . ,word)
    (exp . ,(plist-get properties :exp))
    (content . ,(plist-get properties :content)) ;; sam as other annotations which has id, currently it only saves the real content of the word, or json string for internal usage
    (serverp . ,(or (plist-get properties :serverp) 3))
    (note . ,(plist-get properties :note))
    (note_type word . "✎")
    (origin_type . ,(or (plist-get properties :origin_type)
                        (if (boundp 'eaf--buffer-app-name)
                            eaf--buffer-app-name
                          major-mode) ))
    (origin_path . ,(or (plist-get properties :origin_path) (paw-get-origin-path) ))
    (origin_id . "")
    (origin_point . ,(plist-get properties :origin_point))
    (created_at . ,(plist-get properties :created-at))
    (kagome . ,(plist-get properties :kagome))
    (sound . ,(plist-get properties :sound))
    (lang . ,(or (plist-get properties :lang) (paw-check-language word)))
    (add-to-known-words . ,(plist-get properties :add-to-known-words))
    (context . ,(plist-get properties :context))))



(defun paw-edge-tts-say-word (word &rest args)
  "Listen to WORD pronunciation."
  (let ((lang (plist-get args :lang))
        (lambda (plist-get args :lambda))
        (download-only (plist-get args :download-only)))
    (paw-download-and-say-word
     :source-name "edge-tts"
     :word word
     :lambda lambda
     :extension "mp3"
     :edge-tts t
     :edge-tts-lang (or lang (paw-check-language word))
     :download-only download-only) )
)

;; (paw-edge-tts-say-word "hello" "en")
;; (paw-edge-tts-say-word "hello" "ja" nil t)


(defvar paw-youdao-say-word-running-process nil)

(defun paw-youdao-say-word (word &rest args)
  "Listen to WORD pronunciation."
  (let ((lambda (plist-get args :lambda))
        (download-only (plist-get args :download-only)))
    (paw-download-and-say-word
     :source-name "youdao"
     :word word
     :extension "mp3"
     :audio-url (format "http://dict.youdao.com/dictvoice?type=2&audio=%s" (url-hexify-string word))
     :lambda lambda
     :download-only download-only) ))

;; (paw-youdao-say-word "hello")
;; (paw-youdao-say-word "hello" nil t)

(defun paw-download-and-say-word (&rest args)
  "Use curl to download AUDIO-URL, finally play the sound file.
If LAMBDA is non-nil, call it after creating the download process."
  (let* ((source-name (plist-get args :source-name))
         (word (plist-get args :word))
         (audio-url (plist-get args :audio-url))
         (lambda (plist-get args :lambda))
         (extension (plist-get args :extension))
         (edge-tts (plist-get args :edge-tts))
         (edge-tts-lang (plist-get args :edge-tts-lang))
         (edge-tts-lang (plist-get args :edge-tts-lang))
         (download-only (plist-get args :download-only))
         (word-hash (md5 word))
         (default-mp3-file (concat (expand-file-name word-hash paw-tts-cache-dir) "." (if extension extension (file-name-extension audio-url))))
         (mp3-file (concat (expand-file-name (concat word-hash "+" source-name) paw-tts-cache-dir) "." (if extension extension (file-name-extension audio-url))))
         (proc (if edge-tts
                   (start-process "*paw-tts*" "*paw-tts*" paw-tts-program
                                  "--text" word
                                  "--write-media" mp3-file
                                  ;; "--write-subtitles" subtitle-file
                                  "--voice" (pcase (if edge-tts-lang
                                                       edge-tts-lang
                                                     (completing-read (format "Select TTS Sound Engine (%s): " word) '("en" "ja" "zh" "zh-Hant" "ko" "Multilingual") nil t))
                                              ("en" paw-tts-english-voice)
                                              ("ja" paw-tts-japanese-voice)
                                              ("zh" paw-tts-zh-cn-voice)
                                              ("zh-Hant" paw-tts-zh-tw-voice)
                                              ("ko" paw-tts-korean-voice)
                                              (_ paw-tts-multilingual-voice)))
                   (start-process
                    (executable-find "curl")
                    "*paw-audio-downloder*"
                    (executable-find "curl")
                    "-L"
                    "--user-agent"
                    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"
                    audio-url
                    "--output"
                    mp3-file) )))
    ;; (message mp3-file)
    (if audio-url (message "%s" audio-url) )
    (if download-only
        (set-process-sentinel
         proc
         (lambda (process event)
           (when (string= event "finished\n")
             (if lambda (funcall lambda mp3-file)))))
      (setq paw-say-word-running-process proc)
      (set-process-sentinel
       proc
       (lambda (process event)
         (paw-play-mp3-process-sentiel process event mp3-file)
         (when (string= event "finished\n")
           (if (file-exists-p mp3-file) (copy-file mp3-file default-mp3-file t) )
           (if lambda (funcall lambda mp3-file))))))))

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


(defvar paw-say-word-english-functions '(paw-say-word-cambridge paw-say-word-oxford paw-youdao-say-word))
(defvar paw-say-word-japanese-functions '(paw-say-word-jpod101-alternate))

(defcustom paw-say-word-functions '(paw-say-word-cambridge paw-say-word-oxford paw-say-word-jpod101-alternate paw-edge-tts-say-word paw-youdao-say-word paw-say-word-forvo)
  "The functions to download and play sound file one by one, used in `paw-say-word' if arg is nil. If any one success, it will break."
  :type 'list
  :group 'paw)

(defun paw-say-word-function (say-word-fns-list word finished)
  ;; delete the function from the list if the language is not matched
  (pcase (paw-check-language word)
    ("en" (dolist (fn paw-say-word-japanese-functions)
            (setq say-word-fns-list (cl-remove fn say-word-fns-list))))
    (_ (dolist (fn paw-say-word-english-functions)
            (setq say-word-fns-list (cl-remove fn say-word-fns-list)))))
  (when say-word-fns-list
    (let ((say-word-function (car say-word-fns-list))
          (remaining-functions (cdr say-word-fns-list)))
      (funcall say-word-function word
               :always-first t
               :lambda (lambda (file)
                         (if (and file (file-exists-p file) (> (file-attribute-size (file-attributes file)) 0))
                             (funcall finished file)
                           (paw-say-word-function remaining-functions word finished)))))))


(defun paw-say-word (word &rest args)
  "Listen to WORD pronunciation with multiple SOURCEs.
If arg LANG is non-nil, use it as the edge-tts language.
If arg REFRESH is t, regenerate the pronunciation.
If arg SOURCE is t, select from source, regenerate the pronunciation.

1. Default will use edge-tts to download the pronunciation with
auto-decteded langauge.

2. If SOURCE is t, you can force edge-tts to use a specific sound
engine. After the audio file is downloaded and prounced, it will
become the default audio file for this WORD and cached in
`paw-tts-cache-dir'.

3. For Jisho/Jpod101/Jpod101Alternative, it will ask you to input
the reading of the word before downloading. It will try to get
the marked words or word at point as the reading. Reading is very
important for download Japanese audio correctly, that's why it
will prompt you every first time when download the audio file. "
  (unless (executable-find paw-tts-program)
    (error "edge-tts is not found, please install via 'pip install edge-tts' first."))
  (when (process-live-p paw-say-word-running-process)
    (kill-process paw-say-word-running-process)
    (setq paw-say-word-running-process nil))
  (let* ((lang (plist-get args :lang))
         (refresh (plist-get args :refresh))
         (source (plist-get args :source))
         (word-hash (md5 word))
         (mp3-file (concat (expand-file-name word-hash paw-tts-cache-dir) ".mp3"))
         (audio-url)
         (lambda (lambda (file)
                   (if (and file (file-exists-p file) (> (file-attribute-size (file-attributes file)) 0) )
                       (copy-file file mp3-file t)))))
    (make-directory paw-tts-cache-dir t) ;; ensure cache directory exists
    (when refresh
        (message "Re-Downloading the audio file...")
        (setq paw-say-word-jpod101-alternate-audio-list nil)
        (setq pay-say-word-cambridge-audio-list nil)
        (setq pay-say-word-oxford-audio-list nil)
        (setq paw-say-word-forvo-audio-list nil))
    (paw-say-word-delete-mp3-file (concat word-hash "+edge-tts") refresh)
    (paw-say-word-delete-mp3-file (concat word-hash "+youdao") refresh)
    (paw-say-word-delete-mp3-file (concat word-hash "+jisho") refresh)
    (paw-say-word-delete-mp3-file (concat word-hash "+jpod101") refresh)
    (paw-say-word-delete-mp3-file (concat word-hash "+jpod101-alternate") refresh)
    (paw-say-word-delete-mp3-file (concat word-hash "+frovo") refresh)
    (paw-say-word-delete-mp3-file (concat word-hash "+cambridge") refresh)
    (paw-say-word-delete-mp3-file (concat word-hash "+oxford") refresh)
    (let ((source (if source (completing-read (format "Select Audio Playback Source (%s): " word) '("edge-tts" "forvo" "youdao" "cambridge" "oxford" "jisho" "jpod101" "jpod101-alternate") nil t) "default")))
      (pcase source
        ("default"
         (if (file-exists-p mp3-file)
             (setq audio-url mp3-file)
           (setq audio-url mp3-file)
           (paw-say-word-function paw-say-word-functions word lambda)))
        ("edge-tts"
         (paw-edge-tts-say-word word
                                :lang (completing-read (format "Select TTS Sound Engine (%s): " word) '("en" "ja" "zh" "zh-Hant" "ko" "Multilingual") nil t)
                                :lambda lambda))
        ("forvo"
         (paw-say-word-forvo word
                             :lang (completing-read (format "Select TTS Sound Engine (%s): " word) '("en" "ja" "zh" "ko") nil t)
                             :lambda lambda))
        ("youdao" (paw-youdao-say-word word :lambda lambda))
        ("cambridge" (paw-say-word-cambridge word :lambda lambda))
        ("oxford" (paw-say-word-oxford word :lambda lambda))
        ("jisho"
         (paw-say-word-jisho word
                             ;; have to provide a reading
                             (read-string (format "Reading for '%s': " word)
                                          (let ((input (if mark-active
                                                           (buffer-substring-no-properties (region-beginning) (region-end))
                                                         (thing-at-point 'word t))))
                                            (if (string= input paw-play-source-button)
                                                word
                                              input)))
                             :lambda lambda))
        ("jpod101"
         (paw-say-word-jpod101 word
                               ;; have to provide a reading
                               (read-string (format "Reading for '%s': " word)
                                            (let ((input (if mark-active
                                                             (buffer-substring-no-properties (region-beginning) (region-end))
                                                           (thing-at-point 'word t))))
                                              (if (string= input paw-play-source-button)
                                                  word
                                                input)))
                               :lambda lambda))
        ("jpod101-alternate" (paw-say-word-jpod101-alternate word :lambda lambda))))
    (if (and audio-url (file-exists-p audio-url) )
        (setq paw-say-word-running-process (start-process "*paw say word*" nil paw-player-program audio-url)))
    (if (and audio-url (file-exists-p audio-url) ) audio-url )))

(defun paw-say-word-delete-mp3-file (hash refresh)
  (let* ((mp3-file (concat (expand-file-name hash paw-tts-cache-dir) ".mp3")))
    (when (or (and refresh (file-exists-p mp3-file))
              (and (file-exists-p mp3-file) (= (file-attribute-size (file-attributes mp3-file)) 0) ))
      (delete-file mp3-file))))

(defun paw-play-mp3-process-sentiel(process event mp3-file)
  ;; When process "finished", then begin playback
  (when (string= event "finished\n")
    (start-process "*paw say word*" nil paw-player-program mp3-file)))

;;;###autoload
(defun paw-tts-cache-clear ()
  "Clear tts cache. This will delete all the audio cache, not only tts but other sounces."
  (interactive)
  (delete-directory paw-tts-cache-dir t)
  (make-directory paw-tts-cache-dir t))

(defun paw-resay-word (word &optional lang)
  "Delete the mp3 and subtitle then regenerate."
  (paw-say-word word :lang lang :refresh t))

(defun paw-resay-word-with-source (&optional word lang)
  "Play with soruce."
  (interactive)
  (paw-say-word (or word (or (paw-note-word) (thing-at-point 'word t) ))
                :lang lang :refresh t :source t))

(defun paw-say-word-with-source (&optional word lang)
  "Play with soruce."
  (interactive)
  (paw-say-word (or word (or (paw-note-word) (thing-at-point 'word t) ))
                :lang lang :source t))


(defun paw-get-note ()
  (pcase major-mode
    ;; disable nov get header-line-format, since it is annoying, so that notes are totally control by myself
    ('nov-mode
     (paw-remove-spaces-based-on-ascii-rate (or (thing-at-point 'sentence t) "")))
    ('pdf-view-mode "")
    ('paw-search-mode "")
    ('paw-view-note-mode (alist-get 'context paw-current-entry))
    ('eaf-mode "")
    (_ (or (paw-get-sentence-or-line t) ""))))

(defvar paw-get-sentence-max-length 300)

(defun paw-get-sentence-or-line(&optional no-click-show)
  "Get the sentence or line at point. If the line is too long (>
`paw-get-sentence-max-length'), then use the current line. Remove
org link in the sentence."
  (let ((current-thing (thing-at-point 'sentence t)))
    (if current-thing
        (let* ((length-of-thing (length current-thing))
                (bounds (bounds-of-thing-at-point 'sentence))
                (beg (car bounds))
                (end (cdr bounds)))
          (cond ((or (> length-of-thing paw-get-sentence-max-length) (= length-of-thing 0))  ;; if the sentence is too long, like detect failed, then use the current line
                 (let* ((line (thing-at-point 'line t))
                        (bounds (bounds-of-thing-at-point 'line))
                        (beg (car bounds))
                        (end (cdr bounds)))
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

(defcustom paw-non-ascii-word-separator " "
  "The default separator to be placed between words in non-ascii languages."
  :group 'paw
  :type 'string)

(defun paw-check-language (text)
  "If provide a filename as TEXT, it will use the file content to detect the
language, if `paw-detect-language-p' is t, or return as
`paw-default-language' if `paw-detect-language-p' is nil.

For an org file, setup the Local Variables like so:
# Local Variables:
# eval: (setq-local paw-detect-language-p nil)
# eval: (setq-local paw-default-language \"ja\")
# eval: (paw-annotation-mode +1)
# End:
Could always use the specified langauge, and avoid false/redundant detection.

If provide a string TEXT, use simple ascii rate: `paw-ascii-rate' to detect the language,
if the rate is greater than `paw-ascii-rate', then it is
considered as `paw-default-language', otherwise use
`paw-detect-language-program' to detect the language of the TEXT,
if `paw-detect-language-p' is t, or return as `paw-non-ascii-language' if
`paw-detect-language-p' is nil."

  (cond ((string-empty-p text)
         "en")
        ((file-exists-p text)
         (if paw-detect-language-p
             (with-temp-buffer
               (pcase paw-detect-language-program
                 ('gcld3 (call-process paw-python-program nil t nil "-c"
                                       "import sys, gcld3; detector = gcld3.NNetLanguageIdentifier(min_num_bytes=0, max_num_bytes=2000); result = detector.FindLanguage(text=open(sys.argv[1], 'r').read()); print(result.language)"
                                       text))
                 ('pycld2 (call-process paw-python-program nil t nil "-c"
                                        "import sys; import pycld2 as cld2; reliable, _, detections = cld2.detect(open(sys.argv[1], 'r').read()); print(detections[0][1])"
                                        text))
                 (_ (call-process paw-detect-language-program nil t nil text)))
               (goto-char (point-min))
               (let ((detected-lang (string-trim (buffer-string))))
                 (if (string-equal "un" detected-lang) "en" detected-lang)))
           paw-default-language))
        (t (let* ((strs (split-string text "")) ;; after spliting, it has two redundant elements, that's why minus 2 below
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

        )

  )

(defun paw-remove-spaces (text lang)
  "TODO Refomat the TEXT based on the LANG."
  (cond ((string= lang "en") (replace-regexp-in-string "[ \n]+" " " (replace-regexp-in-string "^[ \n]+" "" text)))
        ((or (string= lang "ja")
	     (string= lang "zh"))
	 (replace-regexp-in-string "\\(^[ \t\n\r]+\\|[ \t\n\r]+\\)" "" text))
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
  (interactive)
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
  (interactive)
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
  "Customized scroll-up function."
  (interactive "p")
  (cond ((eq major-mode 'paw-view-note-mode)
         (call-interactively 'org-forward-element)
         (recenter 0))
        ((eq major-mode 'paw-search-mode)
         (call-interactively 'paw-next))
        ;; ((eq major-mode 'wallabag-search-mode)
        ;;  (call-interactively 'wallabag-next-entry))
        ((eq major-mode 'nov-mode)
         (if (>= (window-end) (point-max))
             (progn
               ;; (nov-next-document)
               (message "End of this chapter")
               (goto-char (point-max)))
           (if (fboundp 'pixel-scroll-interpolate-down)
               (if pixel-scroll-precision-interpolate-page
                   (pixel-scroll-precision-interpolate (- (window-text-height nil t))
                                                       ;; use an interpolation factor,
                                                       ;; since we if visual line mode is applied, the last line may be cut off.
                                                       nil 0.99)
                 (cua-scroll-up))
             (scroll-up arg))))
        (t (if (fboundp 'pixel-scroll-interpolate-down)
               (if pixel-scroll-precision-interpolate-page
                   (pixel-scroll-precision-interpolate (- (window-text-height nil t))
                                                       nil 1)
                 (cua-scroll-up))
             (scroll-up arg)))))

;;;###autoload
(defun paw-scroll-down(arg)
  "Customized scroll-down function."
  (interactive "P")
  (cond ((eq major-mode 'paw-view-note-mode)
         (call-interactively 'org-backward-element)
         (recenter 0))
        ((eq major-mode 'paw-search-mode)
         (call-interactively 'paw-previous))
        ;; ((eq major-mode 'wallabag-search-mode)
        ;;  (call-interactively 'wallabag-previous-entry))
        ((eq major-mode 'nov-mode)
         (if (and (<= (window-start) (point-min))
                  (> nov-documents-index 0))
             (progn
               ;; (nov-previous-document)
               (message "Beginning of this chapter")
               (goto-char (point-min)))
           (if (fboundp 'pixel-scroll-interpolate-up)
               (if pixel-scroll-precision-interpolate-page
                   (pixel-scroll-precision-interpolate (window-text-height nil t)
                                                       ;; use an interpolation factor,
                                                       ;; since we if visual line mode is applied, the last line may be cut off.
                                                       nil 0.99)
                 (cua-scroll-down))
             (scroll-down arg))))
        (t (if (fboundp 'pixel-scroll-interpolate-up)
               (if pixel-scroll-precision-interpolate-page
                   (pixel-scroll-precision-interpolate (window-text-height nil t)
                                                       nil 1)
                 (cua-scroll-down))
             (scroll-down arg)))))

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
        (let ((player paw-player-program))
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
        (let ((player paw-player-program))
          (if player
              (start-process
               player
               nil
               player
               (format "https://dict.youdao.com/dictvoice?type=2&audio=%s" (url-hexify-string word)))
            (message "mpv, mplayer or mpg123 is needed to play word voice")))))))


(defun paw-say-word-jpod101 (term reading &rest args)
  (let ((lambda (plist-get args :lambda))
        (download-only (plist-get args :download-only)))
    (if (and (equal term reading) (and (stringp term) (stringp reading))) ; Assuming `isStringEntirelyKana` checks whether the term is a string
        (setq term ""))
    (let ((params ()))
      (when (> (length term) 0)
        (push (list "kanji" term) params))
      (when (> (length reading) 0)
        (push (list "kana" reading) params))
      (let* ((query-string (url-build-query-string params))
             (audio-url (concat "https://assets.languagepod101.com/dictionary/japanese/audiomp3.php?" query-string)))
        (paw-download-and-say-word
         :source-name "jpod101"
         :word term
         :audio-url audio-url
         :lambda lambda
         :extension "mp3"
         :download-only download-only)))))

;; (paw-say-word-jpod101 "日本" "日本" nil :download-only t)
;; (paw-say-word-jpod101 "日本" "にほん")
;; (paw-say-word-jpod101 "日本" "にっぽん")

(defvar paw-say-word-jpod101-alternate-audio-list nil)

(defun paw-say-word-jpod101-alternate (term &rest args)
  (let* ((reading (or (plist-get args :reading) ""))
         (lambda (plist-get args :lambda))
         (download-only (plist-get args :download-only))
         (always-first (plist-get args :always-first))
         (select-func (lambda (items)
                        (if-let* ((choice (if (> (length items) 1)
                                              (completing-read "Select sound: " items)
                                            (caar items)))
                                  (audio-url (car (assoc-default choice items) )))
                            (paw-download-and-say-word
                             :source-name "jpod101-alternate"
                             :word term
                             :audio-url audio-url
                             :lambda lambda
                             :download-only download-only)
                          (message "No valid audio url")))))
    (if (and (stringp (caar paw-say-word-jpod101-alternate-audio-list ))
             (string= (car (string-split (caar paw-say-word-jpod101-alternate-audio-list ) " ")) term ) )
        (if always-first
            (funcall select-func (list (car paw-say-word-jpod101-alternate-audio-list ) ))
          (funcall select-func paw-say-word-jpod101-alternate-audio-list))

      (request "https://www.japanesepod101.com/learningcenter/reference/dictionary_post"
        :parser 'buffer-string
        :type "POST"
        :data (url-build-query-string `(("post" "dictionary_reference")
                                        ("match_type" "exact")
                                        ("search_query" ,term)
                                        ("vulgar" "true")) )
        :headers '(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                   ("Content-Type" . "application/x-www-form-urlencoded"))
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    ;; Parse HTML
                    (let* ((parsed-html (with-temp-buffer
                                          (insert data)
                                          (libxml-parse-html-region (point-min) (point-max))))
                           ;; Get all 'dc-result-row' elements
                           (dc-result-rows (dom-by-class parsed-html "dc-result-row"))
                           (items))
                      ;; (with-temp-file "~/test.html"
                      ;;   (insert data))
                      ;; (pp dc-result-rows)
                      (dolist (row dc-result-rows items)
                        ;; Get 'audio' and 'src' elements
                        (when-let* ((audio-elem (dom-by-tag row 'audio))
                                    (source-elem (dom-by-tag audio-elem 'source))
                                    (audio-url (dom-attr source-elem 'src))
                                    (vocab-kana-elems (dom-by-class row "dc-vocab_kana"))
                                    (vocab-romanization-elems (dom-by-class row "dc-vocab_romanization")))
                          ;; Get all 'dc-vocab_kana' elements and the first reading
                          (when-let ((vocab-kana (dom-text vocab-kana-elems))
                                     (vocab-romanization (dom-text vocab-romanization-elems)))
                            ;; ;; Matching the reading
                            ;; (when (or (string= term reading)
                            ;;           (string= reading vocab-kana))
                            ;;   (push (list (format "%s %s %s %s" term vocab-kana vocab-romanization audio-url)
                            ;;               audio-url) items))
                            (push (list (format "%s %s %s" (propertize term 'face 'paw-file-face) vocab-kana vocab-romanization)
                                        audio-url) items)
                            )))
                      (if items
                          (progn
                            (setq items (cl-remove-duplicates items :test #'equal))
                            (setq paw-say-word-jpod101-alternate-audio-list items)
                            (if always-first
                                (funcall select-func (list (car items)))
                              (funcall select-func items)) )
                        (if lambda (funcall lambda nil) ))

                      )))
        :error
        (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                       (if lambda (funcall lambda nil) )
                       (message "Failed to get audio on jpod101")))) )))

;; (paw-say-word-jpod101-alternate "日本" "") ;; all sounds
;; (paw-say-word-jpod101-alternate "日本" "日本") ;; all sounds
;; (paw-say-word-jpod101-alternate "日本人" "日本人") ;; all sounds
;; (paw-say-word-jpod101-alternate "日本" "にほん") ;; one specified sound
;; (paw-say-word-jpod101-alternate "にほん" "にほん") ;; one specified sound
;; (paw-say-word-jpod101-alternate "日本" "にっぽん") ;; one specified sound
;; (paw-say-word-jpod101-alternate "日本" "にほん" :download-only t) ;; one specified sound


;; (paw-say-word-cambridge "hello")
;; (paw-say-word-cambridge "日本")
;; (paw-say-word-cambridge "epicondylitis")


(defvar pay-say-word-cambridge-audio-list nil)

(defcustom paw-say-word-cambridge-voice "us"
  "The voice of the cambridge dictionary, either us or uk."
  :group 'paw
  :type 'string)

(defun paw-say-word-cambridge (term &rest args)
  (let* ((reading (or (plist-get args :reading) ""))
         (lambda (plist-get args :lambda))
         (download-only (plist-get args :download-only))
         (select-func (lambda (items)
                        (if-let* ((choice (if (> (length items) 1)
                                              (completing-read "Select sound: " items)
                                            (caar items)))
                                  (audio-url (car (assoc-default choice items) )))
                            (paw-download-and-say-word
                             :source-name "cambridge"
                             :word term
                             :audio-url audio-url
                             :lambda lambda
                             :download-only download-only)
                          (message "No valid audio url")))))
    (if (and (stringp (caar pay-say-word-cambridge-audio-list ))
             (string-match-p term (caar pay-say-word-cambridge-audio-list )) )

        (pcase paw-say-word-cambridge-voice
          ("us" (funcall select-func (list (cl-find-if (lambda (item)
                                                         (string-match-p "\\[us\\]" (car item)))
                                                       pay-say-word-cambridge-audio-list))))
          ("uk" (funcall select-func (list (cl-find-if (lambda (item)
                                                         (string-match-p "\\[uk\\]" (car item)))
                                                       pay-say-word-cambridge-audio-list))))
          (_ (funcall select-func pay-say-word-cambridge-audio-list)))

      (request (format "https://dictionary.cambridge.org/de/worterbuch/englisch/%s" term)
        :parser 'buffer-string
        :headers '(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                   ("Content-Type" . "application/x-www-form-urlencoded"))
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    ;; Parse HTML
                    (let* ((parsed-html (with-temp-buffer
                                          (insert data)
                                          (libxml-parse-html-region (point-min) (point-max))))
                           ;; Get all 'dc-result-row' elements
                           (us-voice (dom-by-class parsed-html "us dpron-i "))
                           (uk-voice (dom-by-class parsed-html "uk dpron-i "))
                           (items)
                           (us-voice-url)
                           (uk-voice-url))
                      ;; (with-temp-file "~/test.html"
                      ;;   (insert data))
                      ;; (pp us-voice)
                      (when-let* ((audio-elem (dom-by-tag us-voice 'audio))
                                  (source-elems (dom-by-tag audio-elem 'source))
                                  (audio-url (dom-attr (seq-filter (lambda (source)
                                                                     (string= (dom-attr source 'type) "audio/mpeg"))
                                                                   source-elems) 'src)))
                        (setq us-voice-url (list (format "%s [us]" (propertize term 'face 'paw-file-face)) (concat "https://dictionary.cambridge.org" audio-url)))

                        (push us-voice-url items))

                      (when-let* ((audio-elem (dom-by-tag uk-voice 'audio))
                                  (source-elems (dom-by-tag audio-elem 'source))
                                  (audio-url (dom-attr (seq-filter (lambda (source)
                                                                     (string= (dom-attr source 'type) "audio/mpeg"))
                                                                   source-elems) 'src)))
                        (setq uk-voice-url (list (format "%s [uk]" (propertize term 'face 'paw-file-face)) (concat "https://dictionary.cambridge.org" audio-url)))
                        (push uk-voice-url items))

                      (if items
                          (progn
                            (setq pay-say-word-cambridge-audio-list items)
                            (pcase paw-say-word-cambridge-voice
                              ("us" (funcall select-func (list us-voice-url)))
                              ("uk" (funcall select-func (list uk-voice-url)))
                              (_ (funcall select-func items))) )
                        (if lambda (funcall lambda nil) )))))
        :error
        (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                       (if lambda (funcall lambda nil) )
                       (message "Failed to get audio on cambridge")))) )))


(defvar pay-say-word-oxford-audio-list nil)


(defcustom paw-say-word-oxford-voice "us"
  "The voice of the oxford dictionary, either us or uk."
  :group 'paw
  :type 'string)

(defun paw-say-word-oxford (term &rest args)
  (let* ((reading (or (plist-get args :reading) ""))
         (lambda (plist-get args :lambda))
         (download-only (plist-get args :download-only))
         (select-func (lambda (items)
                        (if-let* ((choice (if (> (length items) 1)
                                              (completing-read "Select sound: " items)
                                            (caar items)))
                                  (audio-url (car (assoc-default choice items) )))
                            (paw-download-and-say-word
                             :source-name "oxford"
                             :word term
                             :audio-url audio-url
                             :lambda lambda
                             :download-only download-only)
                          (message "No valid audio url")))))
    (if (and (stringp (caar pay-say-word-oxford-audio-list ))
             (string-match-p term (caar pay-say-word-oxford-audio-list )) )

        (pcase paw-say-word-oxford-voice
          ("us" (funcall select-func (list (cl-find-if (lambda (item)
                                                         (string-match-p "\\[us\\]" (car item)))
                                                       pay-say-word-oxford-audio-list))))
          ("uk" (funcall select-func (list (cl-find-if (lambda (item)
                                                         (string-match-p "\\[uk\\]" (car item)))
                                                       pay-say-word-oxford-audio-list))))
          (_ (funcall select-func pay-say-word-oxford-audio-list)))

      (request (format "https://www.oxfordlearnersdictionaries.com/definition/english/%s" (downcase term))
        :parser 'buffer-string
        :headers '(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                   ("Content-Type" . "application/x-www-form-urlencoded"))
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    ;; Parse HTML

                    ;; (with-temp-file "~/test.html"
                    ;;   (insert data))
                    (let* ((parsed-html (with-temp-buffer
                                          (insert data)
                                          (libxml-parse-html-region (point-min) (point-max))))
                           ;; Get all 'dc-result-row' elements
                           (uk-voice (dom-by-class parsed-html "phons_br"))
                           (us-voice (dom-by-class parsed-html "phons_n_am"))
                           (items)
                           (us-voice-url)
                           (uk-voice-url))

                      ;; (with-temp-file "~/test.html"
                      ;;   (insert data))
                      (when-let* ((audio-elem (dom-by-class us-voice "pron-us"))
                                  (audio-url (dom-attr audio-elem 'data-src-mp3)))
                        (setq us-voice-url (list (format "%s [us]" (propertize term 'face 'paw-file-face)) audio-url))
                        (push us-voice-url items))

                      ;; (pp us-voice-url)

                      (when-let* ((audio-elem (dom-by-class uk-voice "pron-uk"))
                                  (audio-url (dom-attr audio-elem 'data-src-mp3)))
                        (setq uk-voice-url (list (format "%s [uk]" (propertize term 'face 'paw-file-face)) audio-url))
                        (push uk-voice-url items))

                      ;; (pp uk-voice-url)
                      (if items
                          (progn
                            (setq pay-say-word-oxford-audio-list items)
                            (pcase paw-say-word-oxford-voice
                              ("us" (funcall select-func (list us-voice-url)))
                              ("uk" (funcall select-func (list uk-voice-url)))
                              (_ (funcall select-func items))) )
                        (if lambda (funcall lambda nil) ))
                      )


                    ))

        :error
        (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                       (if lambda (funcall lambda nil) )
                       (message "Failed to get audio on oxford")))))))


;; (paw-say-word-oxford "hello")
;; (paw-say-word-oxford "world")



(defun paw-request-oxford-5000 (term &rest args)
  (request "https://www.oxfordlearnersdictionaries.com/wordlists/oxford3000-5000"
    :parser 'buffer-string
    :headers '(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
               ("Content-Type" . "application/x-www-form-urlencoded"))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                ;; Parse HTML
                (let* ((parsed-html (with-temp-buffer
                                      (insert data)
                                      (libxml-parse-html-region (point-min) (point-max))))
                       ;; Get all 'dc-result-row' elements
                       (top-g (dom-by-class parsed-html "top-g"))
                       (word-list (dom-by-tag top-g 'li))
                       (items)
                       (us-voice-url)
                       (uk-voice-url))
                  ;; (with-temp-file "~/test.html"
                  ;;   (insert data))
                  (with-temp-file (expand-file-name "5000.csv" org-directory)
                    (insert "word,tag,uk-voice,us-voice\n")
                    (cl-loop for item in word-list collect
                               (let* ((data-hw (dom-attr item 'data-hw))
                                      (a (dom-by-tag item 'a))
                                      (href (concat "https://www.oxfordlearnersdictionaries.com" (dom-attr a 'href)))
                                      (data-ox3000 (dom-attr item 'data-ox3000))
                                      (data-ox5000 (dom-attr item 'data-ox5000))
                                      (us-audio-elem (dom-by-class item "pron-us"))
                                      (us-audio-url (dom-attr us-audio-elem 'data-src-mp3))
                                      (ukaudio-elem (dom-by-class item "pron-uk"))
                                      (uk-audio-url (dom-attr ukaudio-elem 'data-src-mp3)))
                                 ;; `(,data-hw ,data-ox3000 ,data-ox5000)
                                 ;; Oxford 5000 excluding Oxford 3000
                                 (unless (or (string= data-ox5000 "a1")
                                             (string= data-ox5000 "a2")
                                             (string= data-ox5000 "b1"))
                                   (insert (format "%s,%s,%s,%s,%s\n" data-hw data-ox5000 href
                                                   (concat "https://www.oxfordlearnersdictionaries.com" uk-audio-url)
                                                   (concat "https://www.oxfordlearnersdictionaries.com" us-audio-url)) ) )))))))))


(defun paw-request-oxford-phrase-list (term &rest args)
  (request "https://www.oxfordlearnersdictionaries.com/wordlists/oxford-phrase-list"
    :parser 'buffer-string
    :headers '(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
               ("Content-Type" . "application/x-www-form-urlencoded"))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                ;; Parse HTML
                (let* ((parsed-html (with-temp-buffer
                                      (insert data)
                                      (libxml-parse-html-region (point-min) (point-max))))
                       ;; Get all 'dc-result-row' elements
                       (top-g (dom-by-class parsed-html "top-g"))
                       (word-list (dom-by-tag top-g 'li))
                       (items)
                       (us-voice-url)
                       (uk-voice-url))
                  ;; (with-temp-file "~/test.html"
                  ;;   (insert data))
                  (with-temp-file (expand-file-name "phrase-list.csv" org-directory)
                    (insert "word,tag,uk-voice,us-voice\n")
                    (cl-loop for item in word-list collect
                               (let* ((data-hw (dom-attr item 'data-hw))
                                      (a (dom-by-tag item 'a))
                                      (href (concat "https://www.oxfordlearnersdictionaries.com" (dom-attr a 'href)))
                                      (data-oxford_phrase_list (dom-attr item 'data-oxford_phrase_list))
                                      (us-audio-elem (dom-by-class item "pron-us"))
                                      (us-audio-url (dom-attr us-audio-elem 'data-src-mp3))
                                      (ukaudio-elem (dom-by-class item "pron-uk"))
                                      (uk-audio-url (dom-attr ukaudio-elem 'data-src-mp3)))
                                 (insert (format "%s,%s,%s,%s,%s\n" data-hw data-oxford_phrase_list href
                                                 (concat "https://www.oxfordlearnersdictionaries.com" us-audio-url)
                                                 (concat "https://www.oxfordlearnersdictionaries.com" uk-audio-url)) ))) ))))))

(defun paw-request-oxford-opal (term &rest args)
  (request "https://www.oxfordlearnersdictionaries.com/wordlists/opal"
    :parser 'buffer-string
    :headers '(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
               ("Content-Type" . "application/x-www-form-urlencoded"))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                ;; Parse HTML
                (let* ((parsed-html (with-temp-buffer
                                      (insert data)
                                      (libxml-parse-html-region (point-min) (point-max))))
                       ;; Get all 'dc-result-row' elements
                       (top-g (dom-by-class parsed-html "top-g"))
                       (word-list (dom-by-tag top-g 'li))
                       (items)
                       (us-voice-url)
                       (uk-voice-url))
                  ;; (with-temp-file "~/test.html"
                  ;;   (insert data))
                  ;;
                  (with-temp-file (expand-file-name "opal.csv" org-directory)
                    (insert "word,tag,uk-voice,us-voice\n")
                    (cl-loop for item in word-list do
                             (let* ((data-hw (dom-attr item 'data-hw))
                                    (a (dom-by-tag item 'a))
                                    (href (concat "https://www.oxfordlearnersdictionaries.com" (dom-attr a 'href)))
                                    (data-opal_written (or (dom-attr item 'data-opal_written)
                                                           (dom-attr item 'data-opal_written_phrases) ))
                                    (data-opal_spoken (or (dom-attr item 'data-opal_spoken)
                                                          (dom-attr item 'data-opal_spoken_phrases)))
                                    (us-audio-elem (dom-by-class item "pron-us"))
                                    (us-audio-url (dom-attr us-audio-elem 'data-src-mp3))
                                    (ukaudio-elem (dom-by-class item "pron-uk"))
                                    (uk-audio-url (dom-attr ukaudio-elem 'data-src-mp3)))
                               (insert (format "%s,%s,%s,%s,%s\n" data-hw (if data-opal_written
                                                                                    (concat "written:" data-opal_written)
                                                                                  (concat "spoken:" data-opal_spoken))
                                               href
                                               (concat "https://www.oxfordlearnersdictionaries.com" uk-audio-url)
                                               (concat "https://www.oxfordlearnersdictionaries.com" us-audio-url)))))))))))

(defun paw-request-cambridge-all ()
  "Login https://dictionary.cambridge.org/plus/cambridgeWordlists and get the cookie in DevTools."
  (interactive)
  (let ((cookie (read-string "Cookie: ")))
    (paw-request-cambridge "Cambridge_word_lists_-_Advanced" cookie)
    (paw-request-cambridge "Cambridge_word_lists_-_Intermediate" cookie)
    (paw-request-cambridge "Cambridge_word_lists_-_Beginner" cookie)
    (paw-request-cambridge "Caring_about_sustainability" cookie)
    (paw-request-cambridge "IELTS_word_lists" cookie) )
  )

(defun paw-request-cambridge (sub-url cookie)
  (request (format "https://dictionary.cambridge.org/plus/cambridgeWordlists/%s/getWordlists?page=1" sub-url)
    :parser 'json-read
    :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
               ("Content-Type" . "application/x-www-form-urlencoded")
               ("Cookie" . ,cookie))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                ;; (pp (append data nil))
                (with-temp-file (expand-file-name (expand-file-name (format "%s.csv" sub-url) org-directory) org-directory)
                  (insert "headword,name,definition,translation,pos,cefrLevel,domain,usage,gram,entryUrl,soundUKMp3,soundUSMp3\n"))

                (cl-loop for item in (append data nil) do
                         (let* ((id (alist-get 'id item))
                                (count (alist-get 'count item))
                                (shared (alist-get 'shared item))
                                (creationDate (alist-get 'creationDate item))
                                (modificationDate (alist-get 'modificationDate item))
                                (name (alist-get 'name item))
                                (userId (alist-get 'userId item))
                                (wordlistEntries (alist-get 'wordlistEntries item))
                                (metadatas (alist-get 'metadatas item))
                                (userMetadatas (alist-get 'userMetadatas item)))
                           (request (format "https://dictionary.cambridge.org/plus/wordlist/%s/entries/1/" id)
                             :parser 'json-read
                             :headers `(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
                                        ("Content-Type" . "application/x-www-form-urlencoded")
                                        ("Cookie" . ,cookie))
                             :success (cl-function
                                       (lambda (&key data &allow-other-keys)
                                         ;; (pp (append data nil))
                                         (cl-loop for item in (append data nil) do
                                                  (let* ((id (alist-get 'id item))
                                                         (entryId (alist-get 'entryId item))
                                                         (headword (alist-get 'headword item))
                                                         (senseId (alist-get 'senseId item))
                                                         (dictCode (alist-get 'dictCode item))
                                                         (definition (alist-get 'definition item))
                                                         (metaData (alist-get 'metaData item))
                                                         (pos (alist-get 'pos item))
                                                         (soundUK (alist-get 'soundUK item))
                                                         (soundUS (alist-get 'soundUS item))
                                                         (soundUKMp3 (alist-get 'soundUKMp3 item))
                                                         (soundUKOgg (alist-get 'soundUKOgg item))
                                                         (soundUSMp3 (alist-get 'soundUSMp3 item))
                                                         (soundUSOgg (alist-get 'soundUSOgg item))
                                                         (translation (alist-get 'translation item))
                                                         (wordlistId (alist-get 'wordlistId item))
                                                         (entryUrl (alist-get 'entryUrl item))
                                                         (cefrLevel (alist-get 'cefrLevel item))
                                                         (domain (alist-get 'domain item))
                                                         (gram (alist-get 'gram item))
                                                         (region (alist-get 'region item))
                                                         (usage (alist-get 'usage item)))
                                                    (with-temp-buffer
                                                      (insert-file-contents (expand-file-name (format "%s.csv" sub-url) org-directory))
                                                      (write-region (format "%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n"
                                                                            headword
                                                                            name
                                                                            definition
                                                                            (or translation "")
                                                                            (or pos "")
                                                                            (or cefrLevel "")
                                                                            (or domain "")
                                                                            (or usage "")
                                                                            (or gram "")
                                                                            (or entryUrl "")
                                                                            (or soundUKMp3 "")
                                                                            (or soundUSMp3 "")) nil (expand-file-name (format "%s.csv" sub-url) org-directory) 'append))
                                                    ;; (message (format "%s %s %s %s %s %s %s %s %s\n" headword dictCode definition pos translation cefrLevel entryUrl soundUKMp3 soundUSMp3) )

                                                    ))))
                             ;; (message "%s %s" id name)
                             ))))))
  )




(defun paw-request-mawl (term &rest args)
  (request "https://www.eapfoundation.com/vocab/academic/other/mawl/"
    :parser 'buffer-string
    :headers '(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36")
               ("Content-Type" . "application/x-www-form-urlencoded"))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                ;; Parse HTML
                (let* ((parsed-html (with-temp-buffer
                                      (insert data)
                                      (libxml-parse-html-region (point-min) (point-max))))
                       ;; Get all 'dc-result-row' elements
                       (offset (dom-by-class parsed-html "offset"))
                       (word-list (dom-by-tag offset 'tr))
                       (items))
                  ;; (pp (nth 1 word-list) )
                  ;; (with-temp-file "~/test.html"
                  ;;   (insert data))
                  (with-temp-file (expand-file-name "mawl.csv" org-directory)
                    (insert "word,phonetic,number,definition,word_forms\n")
                    (cl-loop for item in word-list collect
                             (let* ((tds (dom-by-tag item 'td))
                                    (headword (dom-text (dom-by-tag (nth 0 tds) 'b) ))
                                    (phonetic (dom-text (nth 0 tds) ))
                                    (number (dom-text (nth 1 tds)))
                                    (definition (mapconcat #'dom-texts (dom-by-tag (nth 2 tds) 'div) "\\n"))
                                    (word_forms (mapconcat #'dom-text (dom-by-tag (nth 3 tds) 'a) " "))
                                    ;; (message "\\n")
                                    )
                               (insert (format "%s,%s,%s,%s,%s\n" headword phonetic number definition word_forms) ))) ))))))


(defvar paw-say-word-forvo-audio-list nil)

(defun paw-say-word-forvo (term &rest args)
  (let* ((lang (plist-get args :lang))
         (lambda (plist-get args :lambda))
         (download-only (plist-get args :download-only))
         (select-func (lambda (list-play)
                        (let ((choice (if (> (length list-play) 1)
                                          (if (fboundp 'consult--read)
                                              (consult--read list-play :prompt "Select sound: " :sort nil)
                                            (completing-read "Select sound: " list-play))
                                        (caar list-play))))
                          (paw-download-and-say-word
                           :source-name "forvo"
                           :word term
                           :audio-url (car (assoc-default choice list-play))
                           :lambda lambda
                           :download-only download-only)))))
    (if (and (stringp (caar paw-say-word-forvo-audio-list ))
             (string= (car (string-split (caar paw-say-word-forvo-audio-list ) " ")) term ) )
        (funcall select-func paw-say-word-forvo-audio-list)
      (request (concat "https://forvo.com/word/" term "/")
        :parser 'buffer-string
        :headers '(("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64; rv:60.0) Gecko/20100101 Firefox/60.0"))
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    ;; (with-temp-file "~/test.html"
                    ;;   (insert data))
                    (let* ((dom (with-temp-buffer
                                  (insert data)
                                  (libxml-parse-html-region (point-min) (point-max))))
                           (results (esxml-query-all (format "#language-container-%s>article>ul.pronunciations-list>li"
                                                             (or lang (paw-check-language term)))
                                                     dom))
                           (list-play))
                      (dolist (res results list-play)
                        (if-let* ((play (dom-attr (dom-by-tag res 'div) 'onclick))
                                  (play-args (let* ((str play)
                                                    (match (string-match "Play\\((.*)\\);return false;" str))
                                                    (args-str (match-string 1 str))
                                                    (args (split-string (replace-regexp-in-string "'" "" args-str) ",")))
                                               args))
                                  (file (base64-decode-string (nth 4 play-args)))
                                  (audio-url (if (string= file "")
                                                 (concat "https://audio12.forvo.com" "/mp3/" (base64-decode-string (nth 1 play-args)))
                                               (concat "https://audio12.forvo.com" "/audios/mp3/" file)))
                                  (username (dom-texts (dom-by-class res "from")))
                                  (info (string-trim (dom-texts (dom-by-class res "ofLink")) )))
                            ;; (message "%s %s %s" term username audio-url)
                            (push (list (format "%s Pronunciation by %s %s" (propertize term 'face 'paw-file-face) info username) audio-url) list-play)))
                      (if list-play
                          (progn
                            (setq list-play (nreverse list-play))
                            (setq paw-say-word-forvo-audio-list list-play)
                            (funcall select-func list-play) )
                        (if lambda (funcall lambda nil) )))))
        :error
        (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                       (if lambda (funcall lambda nil) )
                       (message "Failed to get audio on forvo")))))))

;; (paw-say-word-forvo "彼ら")

;; (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
;;        (results (esxml-query-all (format "#language-container-%s>article>ul.pronunciations-list>li"
;;                                          "ja")
;;                                  dom))
;;        (list-play))
;;   (dolist (res results list-play)
;;     (let* ((play (dom-attr (dom-by-tag res 'div) 'onclick))
;;            (play-args (let* ((str play)
;;                              (match (string-match "Play\\((.*)\\);return false;" str))
;;                              (args-str (match-string 1 str))
;;                              (args (split-string (replace-regexp-in-string "'" "" args-str) ",")))
;;                         args))
;;            (file (base64-decode-string (nth 4 play-args)))
;;            (audio-url (if (string= file "")
;;                           (concat "https://audio12.forvo.com" "/mp3/" (base64-decode-string (nth 1 play-args)))
;;                           (concat "https://audio12.forvo.com" "/audios/mp3/" file)))
;;            (username (dom-texts (dom-by-class res "from")))
;;            (info (string-trim (dom-texts (dom-by-class res "ofLink")) ))
;;            )
;;       ;; (pp play-args)
;;       (push (list (format "%s %s %s %s" "hello" info username audio-url) audio-url) list-play)
;;       ;; (message "%s %s %s" term username audio-url)

;;       ))

;;   )

(defun paw-say-word-jisho (term reading &rest args)
  (let ((lambda (plist-get args :lambda))
        (download-only (plist-get args :download-only))
        (fetch-url (format "https://jisho.org/search/%s" (url-hexify-string term))))
    (request fetch-url
     :parser 'buffer-string
     :headers '(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        ;; (with-temp-file "~/test.html"
        ;;   (insert data))
        (when-let* ((parsed-html (with-temp-buffer
                                   (insert data)
                                   (libxml-parse-html-region (point-min) (point-max))))
                    (audio-element (dom-by-id parsed-html (format "audio_%s:%s" term reading)))
                    (source-element (dom-by-tag audio-element 'source))
                    (audio-url (dom-attr source-element 'src)))
          (paw-download-and-say-word
           :source-name "jisho"
           :word term
           :audio-url (concat "https:" audio-url)
           :lambda lambda
           :download-only))))
     :error
     (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                    (message "Failed to get audio on jisho"))))))

;; cloudflare need proper tls support
;; (paw-say-word-jisho "日本" "")
;; (paw-say-word-jisho "日本人" "")
;; (paw-say-word-jisho "日本" "にほん")
;; (paw-say-word-jisho "日本" "にほん" :download-only t)


(defun paw-say-word-lingua-libre (term language-summary)
  "https://commons.wikimedia.org/wiki/Category:Lingua_Libre_pronunciation"
  (when (not (and (listp language-summary) language-summary))
    (error "Invalid Arguments"))
  (let*  ((iso639_3 (alist-get 'iso639_3 language-summary))
          (search-category  (format "incategory:Lingua_Libre_pronunciation-%s" iso639_3))
          (search-string  (format "-%s.wav" (url-encode-url term)))
          (fetch-url  (format "https://commons.wikimedia.org/w/api.php?action=query&format=json&list=search&srsearch=intitle:/%s/i+%s&srnamespace=6&origin=*" search-string search-category)))
    (paw-say-word-wikimedia-commons fetch-url)))


;; (paw-say-word-lingua-libre "あい" '((iso639_3 . "jpn")))
;; (paw-say-word-lingua-libre "ああ言えばこう言う" '((iso639_3 . "jpn")))

(defun paw-say-word-wiktionary (term language-summary)
  (when (not (and (listp language-summary) language-summary))
    (error "Invalid Arguments"))
  (let*  ((iso  (alist-get 'iso language-summary))
          (search-string  (url-hexify-string (format "%s(-[a-zA-Z]{2})?-%s[0123456789]*.ogg" iso term) ))
          (fetch-url (format "https://commons.wikimedia.org/w/api.php?action=query&format=json&list=search&srsearch=intitle:/%s/i&srnamespace=6&origin=*" search-string)))
    (paw-say-word-wikimedia-commons fetch-url)))

;; (paw-say-word-wiktionary "モンゴル" '((iso . "Ja")))
;; (paw-say-word-wiktionary "Osaka" '((iso . "Ja")))
;; (paw-say-word-wiktionary "uneath" '((iso . "En")))


(defun paw-say-word-wikimedia-commons (fetch-url)
  (request fetch-url
    :parser 'json-read
    :headers '(("User-Agent" . "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.182 Safari/537.36"))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let* ((lookup-results (alist-get 'search (alist-get 'query data))))
                  (cl-loop for lookup-result in (append lookup-results nil) do
                           (when-let* ((title (alist-get 'title lookup-result))
                                  (file-info-url (format "https://commons.wikimedia.org/w/api.php?action=query&format=json&titles=%s&prop=imageinfo&iiprop=user|url&origin=*" (url-encode-url title))))
                             (request file-info-url
                               :parser 'json-read
                               :success (cl-function
                                         (lambda (&key data &allow-other-keys)
                                           (when-let ((file-results (alist-get 'pages (alist-get 'query data)))
                                                      (page (cdar file-results))
                                                      (file-infos (alist-get 'imageinfo page)))
                                             (cl-loop for file-info in (append file-infos nil) do
                                                      (let* ((file-url (alist-get 'url file-info))
                                                             (file-user (alist-get 'user file-info))
                                                             )
                                                        (pp file-url)
                                                        (start-process
                                                         paw-player-program
                                                         nil
                                                         paw-player-program
                                                         file-url))))))))))))))

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
         (origin-path-file (if (stringp origin-path)
                               (file-name-nondirectory origin-path)
                             nil))
         (origin-path (if (stringp origin-path)
                          (if (string-match-p "^http\\(s\\)?://[^ \n]*$" origin-path)
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
                                  origin-path))) ) ))
         (origin-id (alist-get 'origin_id entry))
         (origin-point (alist-get 'origin_point entry))
         (word (paw-get-real-word entry)))

    (pcase origin-type
      ('wallabag-entry-mode
       (require 'wallabag)
       (let ((entry (or (wallabag-db-select :id origin-id)
                        (wallabag-db-select :title (alist-get 'origin_path entry)))))
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
             (if (eq major-mode 'eaf-mode)
                 (eaf-interleave--pdf-viewer-goto-page
                  (expand-file-name origin-path)
                  (car origin-point))
               (paw-goto-location origin-point word)))
         (message "File %s not exists." origin-path))
       ;; pdf tools can not highlight inline, print it instead
       (message "%s" word))
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
                 (switch-to-buffer buffer)
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
      ('elfeed-show-mode
       ;; not sure how to jump to elfeed atm, and jump to elfeed seems not very
       ;; useful, since the entry may be lost
       (browse-url origin-path))
      ("browser"
       (if-let* ((_ (fboundp 'wallabag-db-select))
                 (entry (wallabag-db-select :url origin-path))
                 (wallabag-show-entry-switch 'switch-to-buffer-other-window))
           (if (yes-or-no-p "Found the article in wallabag, jump to wallabag? ")
               (progn
                 (wallabag-show-entry (car entry))
                 (paw-goto-location origin-point word))
             (browse-url origin-path))
         ;; (if (eq system-type 'android)
         ;;     (browse-url origin-path)
         ;;   (require 'eaf)
         ;;   (let* ((buffer (eaf-interleave--find-buffer (expand-file-name origin-path))))
         ;;     (if buffer
         ;;         (progn
         ;;           (switch-to-buffer-other-window buffer)
         ;;           (eaf-interleave--display-buffer buffer))
         ;;       (eaf-interleave--open-web-url origin-path))))
         (browse-url origin-path)))
      ("pdf-viewer"
       (require 'eaf)
       (let* ((buffer (eaf-interleave--find-buffer (expand-file-name origin-path))))
         (if buffer
             (progn
               (switch-to-buffer buffer)
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
       ((memq mode '(doc-view-mode pdf-view-mode))
        (let ((page (if (numberp location) location (car location)))
              (pos (if (numberp location) nil (cdr location))))
          (if (eq mode 'doc-view-mode)
              (doc-view-goto-page page)
            (pdf-view-goto-page page))))
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
           (if (and
                (< beg (point-max))
                (< end (point-max))
                (string-match-p (regexp-quote (s-trim (s-collapse-whitespace (buffer-substring-no-properties beg end)))) (s-trim (s-collapse-whitespace real-word) )) )
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
               ;; fallback to normal search
               (unless (search-forward real-word nil t)
                   (message "Can not find \"%s\", maybe the location is changed, or it is an online word added from another location."
                            (s-truncate 40 (s-collapse-whitespace real-word)))))))
          ;; goto the beg of tuned location
          (goto-char beg))))

(defun paw-get-word (&optional overlay)
  "Get the word at point or marked region."
  (cond ((eq major-mode 'paw-search-mode) (read-string "Add word: "))
        ((eq major-mode 'paw-view-note-mode) (paw-note-word))
        ((eq major-mode 'pdf-view-mode)
         (if (pdf-view-active-region-p)
             (replace-regexp-in-string "[ \n]+" " " (mapconcat 'identity (pdf-view-active-region-text) ? ))
           ""))
        ((eq major-mode 'eaf-mode)
         (pcase eaf--buffer-app-name
           ("browser"
            (eaf-execute-app-cmd 'eaf-py-proxy-copy_text)
            (sleep-for 0.1) ;; TODO small delay to wait for the clipboard
            (car kill-ring))
           ("pdf-viewer"
            (eaf-execute-app-cmd 'eaf-py-proxy-copy_select)
            (sleep-for 0.1) ;; TODO small delay to wait for the clipboard
            (car kill-ring))))
        (t (if mark-active
               (let ((result (if (eq major-mode 'nov-mode)
                                (paw-remove-spaces-based-on-ascii-rate (buffer-substring-no-properties (region-beginning) (region-end)))
                              (buffer-substring-no-properties (region-beginning) (region-end)))))
                 (if overlay
                     (let ((beg (region-beginning))
                           (end (region-end)))
                       (paw-click-show beg end 'paw-click-face)))
                 result)
             (if overlay
                 (-let (((beg . end) (bounds-of-thing-at-point 'symbol)))
                   (if (and beg end) (paw-click-show beg end 'paw-click-face))))
             (substring-no-properties (or (thing-at-point 'symbol t) ""))))))

(defun paw-view-note-in-eaf (note url title word)
  ;; TODO Don't use it, incomplete, need to work with customized eaf
  (let* ((entry (or (car (paw-candidate-by-word word))
                    (car (paw-candidate-by-word (downcase word))))))
    (if entry (setf (alist-get 'context entry) note)) ;; Set the context
    (paw-view-note (or entry (paw-new-entry word
                                            :origin_type eaf--buffer-app-name
                                            :serverp 3
                                            :content (json-encode `((word . ,word)
                                                                    (url . ,url)
                                                                    (title . ,title)
                                                                    (note . ,note)))
                                            :origin_path url
                                            :origin_point title
                                            :lang (paw-check-language word)
                                            :context note ) )
                   :buffer-name paw-view-note-buffer-name
                   ;; :buffer-name (format "*Paw: %s*" title)
                   ;; :display-func 'pop-to-buffer
                   )
    nil))

(defun paw-get-location ()
  "Get location at point or marked region."
  (pcase major-mode
    ('nov-mode
     (if mark-active
         (cons nov-documents-index (cons (region-beginning) (region-end)))
       (cons nov-documents-index (point))))
    ('pdf-view-mode
     (cons (image-mode-window-get 'page) 0))
    ('eaf-mode
     (pcase eaf--buffer-app-name
       ("pdf-viewer"
        (string-to-number (eaf-call-sync "execute_function" eaf--buffer-id "current_page")))
       ("browser" 0)
       (_ 0)))
    (_ (if mark-active (cons (region-beginning) (region-end))
         (point)))))

(provide 'paw-util)
