;;; paw-util.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'paw-gptel)
(require 'paw-sdcv)
(require 'paw-goldendict)
(require 'paw-go-translate)
(require 'paw-android)
(require 'paw-dictionary)
(require 'compile)

(require 'thingatpt)

(eval-when-compile (defvar paw-current-entry))

(defvar paw-provider-url "")

(defcustom paw-say-word-p t
  "paw say word automatically"
  :group 'paw
  :type 'boolean)

(defcustom paw-tts-english-voice "en-US-AvaNeural"
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

(defcustom paw-tts-japanese-voice "ja-JP-NanamiNeural"
  "Japanese tts voice."
  :group 'paw
  :type '(choice (const :tag "Female ja-JP-NanamiNeural" "ja-JP-NanamiNeural")
                 (const :tag "Male ja-JP-KeitaNeural" "ja-JP-KeitaNeural")))

(defcustom paw-tts-korean-voice "ko-KR-SunHiNeural"
  "Korean tts voice."
  :group 'paw
  :type '(choice (const :tag "Female ko-KR-SunHiNeural" "ko-KR-SunHiNeural")
          (const :tag "Male ko-KR-HyunsuNeural" "ko-KR-HyunsuNeural")
          (const :tag "Male ko-KR-InJoonNeural"  "ko-KR-InJoonNeural")))

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

(defcustom paw-tts-multilingual-voice "en-US-AvaMultilingualNeural"
  "Multilingual tts voice."
  :group 'paw
  :type '(choice (const :tag "Female en-US-AvaMultilingualNeural" "en-US-AvaMultilingualNeural")
          (const :tag "Male de-DE-ConradNeural" "de-DE-ConradNeural")
          (const :tag "Female de-DE-SeraphinaMultilingualNeural" "de-DE-SeraphinaMultilingualNeural")
          (const :tag "Male en-US-AndrewMultilingualNeural" "en-US-AndrewMultilingualNeural")
          (const :tag "Male fr-FR-RemyMultilingualNeural" "fr-FR-RemyMultilingualNeural")
          (const :tag "Female fr-FR-VivienneMultilingualNeural" "fr-FR-VivienneMultilingualNeural")))


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

(defcustom paw-dictionary-function #'paw-dictionary-search
  "paw dictionary function, Default dictionary function for querying
the WORD."
  :group 'paw
  :type 'function)

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
  "paw transalte function. Its purpose is to tranlate the WORD with
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

(defun paw-new-entry(word &rest properties)
  ;; create new entry
  ;; example ((word . "major") (exp . "adj. 主要的；主修的；重要的；较多的; n. 成年人；主修科目；陆军少校; vi. 主修<br>...") (content . 0) (serverp . 1) (note . "") (note_type word . "✎") (origin_type) (origin_path . "PN") (origin_id . "1709212272") (origin_point) (created_at . "2024-04-24 19:11:00"))
  ;; kagome: NOT the database field
  ;; lang: NOT the database field
  `((word . ,word)
    (exp . ,(plist-get properties :exp))
    (content . nil) ;; sam as other annotations which has id, currently it only saves the real content of the word, or json string for internal usage
    (serverp . 3)
    (note . ,(or (plist-get properties :note) ""))
    (note_type word . "✎")
    (origin_type . ,(if (boundp 'eaf--buffer-app-name)
                        eaf--buffer-app-name
                      major-mode))
    (origin_path . ,(or (plist-get properties :origin_path) (paw-get-origin-path) ))
    (origin_id . "")
    (origin_point . ,(plist-get properties :origin_point))
    (created_at . ,(plist-get properties :created-at))
    (kagome . ,(plist-get properties :kagome))
    (sound . ,(plist-get properties :sound))
    (lang . ,(or (plist-get properties :lang) (paw-check-language word)))
    (add-to-known-words . ,(plist-get properties :add-to-known-words))))



(defun paw-edge-tts-say-word (word lang &optional lambda)
  "Listen to WORD pronunciation."
  (paw-download-and-say-word
   :source-name "edge-tts"
   :word word
   :lambda lambda
   :extension "mp3"
   :edge-tts t
   :edge-tts-lang lang))

;; (paw-edge-tts-say-word "hello" "en")


(defvar paw-youdao-say-word-running-process nil)

(defun paw-youdao-say-word (word &optional lambda)
  "Listen to WORD pronunciation."
  (paw-download-and-say-word
   :source-name "youdao"
   :word word
   :audio-url (format "http://dict.youdao.com/dictvoice?type=2&audio=%s" (url-hexify-string word))
   :lambda lambda))

;; (paw-youdao-say-word "hello")

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
         (word-hash (md5 word))
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
                    audio-url
                    "--output"
                    mp3-file) )))
    (message mp3-file)
    (message "%s" audio-url)
    (setq paw-say-word-running-process proc)
    (set-process-sentinel
     proc
     (lambda (process event)
       (paw-play-mp3-process-sentiel process event mp3-file)
       (if lambda (funcall lambda mp3-file)))))
  )

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

(defun paw-say-word (word &optional lang refresh source)
  "Listen to WORD pronunciation with multiple SOURCEs.
If LANG is non-nil, use it as the edge-tts language.
If REFRESH is t, regenerate the pronunciation.
If mark-active, regenerate the pronunciation.
If SOURCE is t, select from source.

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
will prompt you every first time when download the audio file.

4. If no prompt, that means the audio file is already cached. You
can mark something to trigger it to redownload the audio file."
  (unless (executable-find paw-tts-program)
    (error "edge-tts is not found, please install via 'pip install edge-tts' first."))
  (when (process-live-p paw-say-word-running-process)
    (kill-process paw-say-word-running-process)
    (setq paw-say-word-running-process nil))
  (let* ((word-hash (md5 word))
         (mp3-file (concat (expand-file-name word-hash paw-tts-cache-dir) ".mp3"))
         (mp3-file-edge-tts (concat (expand-file-name (concat word-hash "+edge-tts") paw-tts-cache-dir) ".mp3"))
         (mp3-file-youdao (concat (expand-file-name (concat word-hash "+youdao") paw-tts-cache-dir) ".mp3"))
         (mp3-file-jisho (concat (expand-file-name (concat word-hash "+jisho") paw-tts-cache-dir) ".mp3"))
         (mp3-file-jpod101 (concat (expand-file-name (concat word-hash "+jpod101") paw-tts-cache-dir) ".mp3"))
         (mp3-file-jpod101-alternate (concat (expand-file-name (concat word-hash "+jpod101-alternate") paw-tts-cache-dir) ".mp3"))
         (subtitle-file (concat (expand-file-name word-hash paw-tts-cache-dir) ".vtt"))
         (audio-url))
    (make-directory paw-tts-cache-dir t) ;; ensure cache directory exists
    (if (or refresh mark-active)
        (message "Re-Downloading the audio file..."))
    (when (or (and refresh (file-exists-p mp3-file)) mark-active)
        (delete-file mp3-file)
        (delete-file subtitle-file))
    (when (or (and refresh (file-exists-p mp3-file-edge-tts)) mark-active)
      (delete-file mp3-file-edge-tts))
    (when (or (and refresh (file-exists-p mp3-file-jisho)) mark-active)
      (delete-file mp3-file-jisho))
    (when (or (and refresh (file-exists-p mp3-file-youdao)) mark-active)
      (delete-file mp3-file-youdao))
    (when (or (and refresh (file-exists-p mp3-file-jpod101)) mark-active)
      (delete-file mp3-file-jpod101))
    (when (or (and refresh (file-exists-p mp3-file-jpod101-alternate)) mark-active)
      (delete-file mp3-file-jpod101-alternate))
    (let ((source (if source (completing-read (format "Select Audio Playback Source (%s): " word) '("edge-tts" "youdao" "jisho" "jpod101" "jpod101-alternate") nil t) "default")))
      (pcase source
        ("default"
         (if (file-exists-p mp3-file)
             (setq audio-url mp3-file)
           (setq audio-url mp3-file)
           (paw-edge-tts-say-word word (paw-check-language word)
                                  (lambda (file) (copy-file file mp3-file t)))))

        ("edge-tts"
         (if (file-exists-p mp3-file-edge-tts)
             (progn
               (copy-file mp3-file-edge-tts mp3-file t) ;; repalce the default audio file
               (setq audio-url mp3-file-edge-tts) )
           (setq audio-url mp3-file-edge-tts)
           (paw-edge-tts-say-word word (completing-read (format "Select TTS Sound Engine (%s): " word) '("en" "ja" "zh" "zh-Hant" "ko" "Multilingual") nil t)
                                  (lambda (file) (copy-file file mp3-file t)))))

        ("youdao"
         (if (file-exists-p mp3-file-youdao)
             (progn
               (copy-file mp3-file-youdao mp3-file t) ;; repalce the default audio file
               (setq audio-url mp3-file-youdao) )
           (setq audio-url mp3-file-youdao)
           (paw-youdao-say-word word (lambda (file) (copy-file file mp3-file t)))))

        ("jisho"
         (if (file-exists-p mp3-file-jisho)
             (progn
               (copy-file mp3-file-jisho mp3-file t) ;; repalce the default audio file
               (setq audio-url mp3-file-jisho) )
           (setq audio-url mp3-file-jisho)
           (paw-say-word-jisho word
                               (read-string (format "Reading for '%s': " word)
                                            (let ((input (if mark-active
                                                             (buffer-substring-no-properties (region-beginning) (region-end))
                                                           (thing-at-point 'word t))))
                                              (if (string= input paw-play-source-button)
                                                  word
                                                input)))
                               (lambda (file) (copy-file file mp3-file t)))))

        ("jpod101"
         (if (file-exists-p mp3-file-jpod101)
             (progn
               (copy-file mp3-file-jpod101 mp3-file t) ;; repalce the default audio file
               (setq audio-url mp3-file-jpod101) )
           (setq audio-url mp3-file-jpod101)
           (paw-say-word-jpod101 word
                                 (read-string (format "Reading for '%s': " word)
                                              (let ((input (if mark-active
                                                               (buffer-substring-no-properties (region-beginning) (region-end))
                                                             (thing-at-point 'word t))))
                                                (if (string= input paw-play-source-button)
                                                    word
                                                  input)))
                                 (lambda (file) (copy-file file mp3-file t)))))

        ("jpod101-alternate"
         (if (file-exists-p mp3-file-jpod101-alternate)
             (progn
               (copy-file mp3-file-jpod101-alternate mp3-file t) ;; repalce the default audio file
               (setq audio-url mp3-file-jpod101-alternate) )
           (setq audio-url mp3-file-jpod101-alternate)
           (paw-say-word-jpod101-alternate word
                                           (read-string (format "Reading for '%s': " word)
                                                        (let ((input (if mark-active
                                                                         (buffer-substring-no-properties (region-beginning) (region-end))
                                                                       (thing-at-point 'word t))))
                                                          (if (string= input paw-play-source-button)
                                                              word
                                                            input)))
                                           (lambda (file) (copy-file file mp3-file t)))))))

    (if (file-exists-p audio-url)
        (setq paw-say-word-running-process (start-process "*paw say word*" nil "mpv" audio-url)))
    (if mark-active (deactivate-mark))
    (if (file-exists-p audio-url) audio-url )))

(defun paw-play-mp3-process-sentiel(process event mp3-file)
  ;; When process "finished", then begin playback
  (when (string= event "finished\n")
    (start-process "*paw say word*" nil "mpv" mp3-file)))

;;;###autoload
(defun paw-tts-cache-clear ()
  "Clear tts cache."
  (interactive)
  (delete-directory paw-tts-cache-dir t)
  (make-directory paw-tts-cache-dir t))

(defun paw-resay-word (word &optional lang)
  "Delete the mp3 and subtitle then regenerate."
  (paw-say-word word lang t))

(defun paw-resay-word-with-source (&optional word lang)
  "Play with soruce."
  (interactive)
  (paw-say-word (or word (or (paw-note-word) (thing-at-point 'word t) )) lang t t))

(defun paw-say-word-with-source (&optional word lang)
  "Play with soruce."
  (interactive)
  (paw-say-word (or word (or (paw-note-word) (thing-at-point 'word t) )) lang nil t))


(defun paw-get-note ()
  (pcase major-mode
    ;; disable nov get header-line-format, since it is annoying, so that notes are totally control by myself
    ('nov-mode
     (paw-remove-spaces-based-on-ascii-rate (or (thing-at-point 'sentence t) "")))
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

  (cond ((file-exists-p text)
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
        ((eq major-mode 'paw-search-mode)
         (call-interactively 'paw-search-next-page))
        ((eq major-mode 'nov-mode)
         (call-interactively 'nov-scroll-up))
        (t (call-interactively 'scroll-up))))

;;;###autoload
(defun paw-scroll-down(arg)
  (interactive "P")
  (cond ((eq major-mode 'paw-view-note-mode)
         (call-interactively 'org-backward-element)
         (recenter 0))
        ((eq major-mode 'paw-search-mode)
         (call-interactively 'paw-search-previous-page))
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


(defun paw-say-word-jpod101 (term reading &optional lambda)
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
       :extension "mp3"))))

;; (paw-say-word-jpod101 "日本" "日本")
;; (paw-say-word-jpod101 "日本" "にほん")
;; (paw-say-word-jpod101 "日本" "にっぽん")

(defun paw-say-word-jpod101-alternate (term reading &optional lambda)
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
                       (dc-result-rows (dom-by-class parsed-html "dc-result-row")))
                  ;; (with-temp-file "~/test.html"
                  ;;   (insert data))
                  (pp dc-result-rows)
                  (dolist (row dc-result-rows)
                      ;; Get 'audio' and 'src' elements
                      (when-let* ((audio-elem (dom-by-tag row 'audio))
                                  (source-elem (dom-by-tag audio-elem 'source))
                                  (audio-url (dom-attr source-elem 'src))
                                  (vocab-kana-elems (dom-by-class row "dc-vocab_kana"))
                                  (vocab-romanization-elems (dom-by-class row "dc-vocab_romanization")))
                        ;; Get all 'dc-vocab_kana' elements and the first reading
                        (when-let ((vocab-kana (dom-text vocab-kana-elems))
                                   (vocab-romanization (dom-text vocab-romanization-elems)))
                            ;; Matching the reading
                            (when (or (string= term reading)
                                      (string= reading vocab-kana))
                              (message "%s %s %s %s" term vocab-kana vocab-romanization audio-url)
                              (paw-download-and-say-word
                               :source-name "jpod101-alternate"
                               :word term
                               :audio-url audio-url
                               :lambda lambda))))))))))

;; (paw-say-word-jpod101-alternate "日本" "日本") ;; all sounds
;; (paw-say-word-jpod101-alternate "日本" "にほん") ;; one specified sound
;; (paw-say-word-jpod101-alternate "にほん" "にほん") ;; one specified sound
;; (paw-say-word-jpod101-alternate "日本" "にっぽん") ;; one specified sound



(defun paw-say-word-jisho (term reading &optional lambda)
  (let ((fetch-url (format "https://jisho.org/search/%s" (url-hexify-string term))))
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
           :lambda lambda))))
     :error
     (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                    (error "Failed to find audio URL, %s" error-thrown))))))

;; cloudflare need proper tls support
;; (paw-say-word-jisho "日本" "")
;; (paw-say-word-jisho "日本" "にほん")


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
                                                         (executable-find "mpv")
                                                         nil
                                                         (executable-find "mpv")
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
       (let ((entry (wallabag-db-select :id origin-id)))
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

(defun paw-get-word ()
  "Get the word at point or marked region."
  (cond ((eq major-mode 'paw-search-mode) (read-string "Add word: "))
        ((eq major-mode 'paw-view-note-mode) (paw-note-word))
        ((eq major-mode 'pdf-view-mode)
         (if (pdf-view-active-region-p)
             (mapconcat 'identity (pdf-view-active-region-text) ? )
           "EMPTY ANNOTATION"))
        ((eq major-mode 'eaf-mode)
         (pcase eaf--buffer-app-name
           ("browser"
            (eaf-execute-app-cmd 'eaf-py-proxy-copy_text)
            (sleep-for 0.01) ;; TODO small delay to wait for the clipboard
            (eaf-call-sync "execute_function" eaf--buffer-id "get_clipboard_text"))
           ("pdf-viewer"
            (eaf-execute-app-cmd 'eaf-py-proxy-copy_select)
            (sleep-for 0.01) ;; TODO small delay to wait for the clipboard
            (eaf-call-sync "execute_function" eaf--buffer-id "get_clipboard_text"))))
        (mark-active (buffer-substring-no-properties (region-beginning) (region-end)))
        (t (substring-no-properties (or (thing-at-point 'word t) "")))))


(provide 'paw-util)
