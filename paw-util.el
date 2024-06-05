;;; pen/pen-util.el -*- lexical-binding: t; -*-

(require 'pen-gptel)
(require 'pen-sdcv)
(require 'pen-goldendict)
(require 'pen-go-translate)

(defvar pen-provider-url "")

(defvar pen-provider-english-url-alist
  (append '(("牛津" "https://www.oxfordlearnersdictionaries.com/definition/english/%s")
            ("朗文" "https://www.ldoceonline.com/dictionary/%s")
            ("韦氏" "https://www.merriam-webster.com/dictionary/%s")
            ("剑桥" "https://dictionary.cambridge.org/dictionary/english-chinese-simplified/%s")
            ("美国传统" "https://www.ahdictionary.com/word/search.html?q=%s")
            ("有道" "https://www.youdao.com/result?word=%s&lang=en")
            ("欧陆" "https://dict.eudic.net/mdicts/en/%s")
            ("科林斯" "https://www.collinsdictionary.com/zh/dictionary/english/%s")
            ("Dictcn" "https://dict.cn/search?q=%s")
            ("TIO(英)"       "https://tio.freemdict.com/api?br=1&key=%s")
            ("Wikipedia(EN)"       "https://en.wikipedia.org/wiki/%s")
            ("Wiktionary(EN)" "https://en.wiktionary.org/wiki/%s")
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


(defvar pen-provider-japanese-url-alist
  (append '(
            ("TIO(日)"      "https://tio.freemdict.com/japi?key=%s")
            ("Jisho"      "https://jisho.org/search/%s")
            ("Forvo"      "https://forvo.com/search/%s")
            ("Weblio(JA-CN)" "https://cjjc.weblio.jp/content/%s")
            ("Weblio" "https://www.weblio.jp/content/%s")
            ("Lorenzi's Jisho" "https://jisho.hlorenzi.com/search/%s")
            ("Nihongomaster" "https://www.nihongomaster.com/japanese/dictionary?query=%s")
            ("Kanshudo" "https://www.kanshudo.com/searchw?q=%s")
            ("Goo" "https://dictionary.goo.ne.jp/srch/all/%s/m0u/")
            ("Mazii" "https://mazii.net/zh-CN/search/word/jacn/%s")
            ("OJAD" "https://www.gavo.t.u-tokyo.ac.jp/ojad/search/index/word:%s")
            ("Moji" "https://www.mojidict.com/searchText/%s")
            ("Wikipedia(JA)"       "https://ja.wikipedia.org/wiki/%s")
            ("Wiktionary(CN)" "https://zh.wiktionary.org/wiki/%s")
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

(defvar pen-provider-general-url-alist
  (append '(("Google"            "https://google.com/search?q=%s")
            ("Google(AUTO)"            "https://translate.google.com/#auto/zh-CN/%s")
            ("Google(EN-CN)"            "https://translate.google.com/#en/zh-CN/%s")
            ("Google images"     "https://www.google.com/images?q=%s")
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

(defcustom pen-say-word-p t
  "pen say word automatically"
  :group 'pen
  :type 'boolean)

(defcustom pen-posframe-p nil
  "show pen-view-note in posframe"
  :group 'pen
  :type 'boolean)

(defcustom pen-transalte-p t
  "transalate automatically"
  :group 'pen
  :type 'boolean)

(defcustom pen-default-say-word-function 'pen-say-word ;; pen-resay-word to regenerate the pronunciation
  "pen read function"
  :group 'pen
  :type '(choice (function-item pen-youdao-say-word)
          function))

(defcustom pen-read-function-1 'pen-youdao-say-word
  "pen read function"
  :group 'pen
  :type '(choice (function-item pen-youdao-say-word)
          function))

(defcustom pen-read-function-2 'pen-say-word
  "pen read function"
  :group 'pen
  :type '(choice (function-item pen-say-word)
          function))


(defcustom pen-dictionary-browse-function 'browse-url
  "pen dictionary function"
  :group 'pen
  :type '(choice (function-item eaf-open-browser)
          function))

(defcustom pen-translate-function 'pen-go-translate-insert
  "pen dictionary function"
  :group 'pen
  :type '(choice (function-item pen-go-translate-insert)
          function))

(defcustom pen-ai-translate-function 'pen-gptel-translate
  "pen dictionary function"
  :group 'pen
  :type '(choice (function-item pen-gptel-translate)
          function))

(defcustom pen-stardict-function 'pen-sdcv-search-detail
  "pen dictionary function"
  :group 'pen
  :type '(choice (function-item pen-go-translate-insert)
          function))

(defcustom pen-external-dictionary-function 'pen-goldendict-search-details
  "pen dictionary function"
  :group 'pen
  :type '(choice (function-item pen-goldendict-search-details)
          function))

(defcustom pen-mdict-dictionary-function 'browse-url
  "pen dictionary function"
  :group 'pen
  :type '(choice (function-item pen-go-translate-insert)
          function))


(defun pen-parse-json (json)
  (append (alist-get 'data json ) nil))

(defun pen-buffer ()
  "Create buffer *pen*."
  (get-buffer-create "*pen*"))

(defun pen-format-column (string width &optional align)
  "Return STRING truncated or padded to WIDTH following ALIGNment.
Align should be a keyword :left or :right."
  (if (<= width 0)
      ""
    (format (format "%%%s%d.%ds" (if (eq align :left) "-" "") width width)
            string)))

(defun pen-clamp (min value max)
  "Clamp a value between two values."
  (min max (max min value)))

(defun pen-flash-show (pos end-pos face delay)
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


(defun pen-attach-icon-for (path)
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

(defun pen-get-real-word (entry)
  "Get the word excluded the id."
  (if (stringp entry)
      (replace-regexp-in-string ":id:.*" "" entry)
    (if entry
        (replace-regexp-in-string ":id:.*" "" (alist-get 'word entry))
      "")))

(defun pen-entry-p (entry)
  "Check if the entry is a valid entry."
  (alist-get 'word entry))

(defun pen-new-entry(word &optional kagome)
  ;; create new entry
  ;; example ((word . "major") (exp . "adj. 主要的；主修的；重要的；较多的; n. 成年人；主修科目；陆军少校; vi. 主修<br>...") (content . 0) (serverp . 1) (note . "") (note_type word . "✎") (origin_type) (origin_path . "PN") (origin_id . "1709212272") (origin_point) (created_at . "2024-04-24 19:11:00"))
  ;; kagome: NOT the database field
  `((word . ,word)
    (exp . "")
    (content . ,word) ;; sam as other annotations which has id, currently it only saves the real content of the word, or json string for internal usage
    (serverp . 3)
    (note . ,(pen-get-note))
    (note_type word . "✎")
    (origin_type . ,(if (derived-mode-p 'eaf-mode)
                        eaf--buffer-app-name
                      major-mode))
    (origin_path . ,(pen-get-origin-path))
    (origin_id . "")
    (origin_point)
    (created_at . ,(format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
    (kagome . ,kagome)))


(defvar pen-youdao-say-word-running-process nil)

(defun pen-youdao-say-word (word)
  "Listen to WORD pronunciation."
  (when (process-live-p pen-youdao-say-word-running-process )
    (kill-process pen-youdao-say-word-running-process)
    (setq pen-youdao-say-word-running-process nil))
  (if (featurep 'cocoa)
      (call-process-shell-command
       (format "say %s" word) nil 0)
    (let ((player (or (executable-find "mpv")
                      (executable-find "mplayer")
                      (executable-find "mpg123"))))
      (if player
          (setq pen-youdao-say-word-running-process
                (start-process
                 player
                 nil
                 player
                 (format "http://dict.youdao.com/dictvoice?type=2&audio=%s" (url-hexify-string word))) )
        (message "mpv, mplayer or mpg123 is needed to play word voice")))))

(defcustom pen-tts-cache-dir
  (expand-file-name (concat user-emacs-directory "edge-tts"))
  "pen say word cache directory."
  :group 'pen
  :type 'directory)

(defvar pen-say-word-running-process nil)

(defun pen-say-word (word)
  "Listen to WORD pronunciation using edge-tts"
  (when (process-live-p pen-say-word-running-process)
    (kill-process pen-say-word-running-process)
    (setq pen-say-word-running-process nil))
  (let* ((lang_word (pen-remove-spaces-based-on-ascii-rate-return-cons word))
         (lang (car lang_word))
         (word (cdr lang_word))
         (word-hash (md5 word))
         (mp3-file (concat (expand-file-name word-hash pen-tts-cache-dir) ".mp3"))
         (subtitle-file (concat (expand-file-name word-hash pen-tts-cache-dir) ".vtt")))
    (make-directory pen-tts-cache-dir t) ;; ensure cache directory exists
    (if (file-exists-p mp3-file)
        (setq pen-say-word-running-process
              (start-process "*pen say word*" nil "mpv" mp3-file))
      (let ((proc (start-process "*pen-tts*" "*pen-tts*" "edge-tts"
                                 "--text" word
                                 "--write-media" mp3-file
                                 "--write-subtitles" subtitle-file
                                 "--voice" (pcase lang
                                             ("en" "en-US-AvaNeural")
                                             ("ja" "ja-JP-NanamiNeural")))))
        (setq pen-say-word-running-process proc)
        ;; Define sentinel
        (set-process-sentinel
         proc
         (lambda (process event)
           ;; When process "finished", then begin playback
           (when (string= event "finished\n")
             (start-process "*pen say word*" nil "mpv" mp3-file))))))))

;;;###autoload
(defun pen-tts-cache-clear ()
  "Clear tts cache."
  (interactive)
  (delete-directory pen-tts-cache-dir t)
  (make-directory pen-tts-cache-dir t))


(defun pen-resay-word (word)
  "Delete the pronunciation and regenerate."
  (when (process-live-p pen-say-word-running-process)
    (kill-process pen-say-word-running-process)
    (setq pen-say-word-running-process nil))
  (let* ((lang_word (pen-remove-spaces-based-on-ascii-rate-return-cons word))
         (lang (car lang_word))
         (word (cdr lang_word))
         (word-hash (md5 word))
         (mp3-file (concat (expand-file-name word-hash pen-tts-cache-dir) ".mp3"))
         (subtitle-file (concat (expand-file-name word-hash pen-tts-cache-dir) ".vtt")))
    (make-directory pen-tts-cache-dir t) ;; ensure cache directory exists
    (when (file-exists-p mp3-file)
      (delete-file mp3-file)
      (let ((proc (start-process "*pen-tts*" nil "edge-tts"
                                 "--text" word
                                 "--write-media" mp3-file
                                 "--write-subtitles" subtitle-file
                                 "--voice" (pcase lang
                                             ("en" "en-US-AvaNeural")
                                             ("ja" "ja-JP-NanamiNeural")))))
        (setq pen-say-word-running-process proc)
        ;; Define sentinel
        (set-process-sentinel
         proc
         (lambda (process event)
           ;; When process "finished", then begin playback
           (when (string= event "finished\n")
             (start-process "*pen say word*" nil "mpv" mp3-file))))))))


(defun pen-get-note ()
  (pcase major-mode
    ;; disable nov get header-line-format, since it is annoying, so that notes are totally control by myself
    ('nov-mode
     (pen-remove-spaces-based-on-ascii-rate (thing-at-point 'sentence t)))
    ('pdf-view-mode "")
    ('pen-search-mode "")
    ('pen-view-note-mode (alist-get 'note pen-current-entry))
    ('eaf-mode "")
    (_ (or (pen-get-sentence-or-line) ""))))

(defun pen-get-sentence-or-line()
  (let* ((current-thing (thing-at-point 'sentence t))
         (length-of-thing (length current-thing)))
    (cond ((or (> length-of-thing 100) (= length-of-thing 0))  ;; if the sentence is too long, like detect failed, then use the current line
           (let ((line (thing-at-point 'line t)))
             (when (string-match "\\[\\[.*?\\]\\[.*?\\]\\]" line)
               (setq line (replace-match "" nil nil line)))
             line))
          (t current-thing))))

;; TODO: it should be able to detect more languages
(defun pen-check-language(text)
  (let ((strs (split-string text ""))
        (number 0)
        (rate 0.5)) ;; the rate of ascii characters in the text
    (dolist
        (str strs)
      ;; check ascii or not
      (if (string-match-p "[[:ascii:]]+" str)
          (setq number (+ number 1))))
    (if (>= (/ number (float (length strs))) rate)
        "en"
      "ja")))

(defun pen-remove-spaces-based-on-ascii-rate-return-cons (text)
  (let ((lang (pen-check-language text)))
    (cons lang (cond ((string= lang "en") (replace-regexp-in-string "[ \n]+" " " (replace-regexp-in-string "^[ \n]+" "" text)))
          ((string= lang "ja") (replace-regexp-in-string "\\(^[ \t\n\r]+\\|[ \t\n\r]+\\)" "" text))) )))

(defun pen-remove-spaces-based-on-ascii-rate (text)
  (let ((lang (pen-check-language text)))
    (cond ((string= lang "en") (replace-regexp-in-string "[ \n]+" " " (replace-regexp-in-string "^[ \n]+" "" text)))
          ((string= lang "ja") (replace-regexp-in-string "\\(^[ \t\n\r]+\\|[ \t\n\r]+\\)" "" text)))))

(defun pen-provider-lookup (word provider)
  (let* ((provider-alist (cl-remove-duplicates (append pen-provider-english-url-alist pen-provider-japanese-url-alist pen-provider-general-url-alist) :test 'equal))
         (url-template (cadr (assoc provider provider-alist))))
    (format url-template (pen-get-real-word word ))))

;;;###autoload
(defun pen-translate-function (word)
  (interactive "sWord to lookup: ")
  (pen-provider-lookup "hello" "TIO English"))

(defun pen-get-id ()
  (pcase major-mode
    ('wallabag-entry-mode
     (alist-get 'id (get-text-property 1 'wallabag-entry)))
    ('eaf-mode
     nil)
    (_ nil)))


;;; mark/unmark

(defun pen-previous ()
  (let ((location (get-text-property (point) 'pen-id)) previous)
    (cond
     ;; check the current point headline number first
     ((numberp location)
      (setq previous (text-property-any (point-min) (point-max) 'pen-id (1- location)))
      (if (numberp previous)
          (goto-char previous)
        (goto-char (point-min))))
     ;; check the current point if >= the first header (no matter level), keep (point) if no headlines
     ((>= (or (text-property-not-all (point-min) (point-max) 'pen-id nil) (point)) (point))
      (message "Beginning of buffer")
      (goto-char (point-min)))
     (t
      (let ((current (point-min)) (start (1+ (point))) point number)
        ;; Scan from point-min to (1+ (point)) to find the current headline.
        ;; (1+ (point)) to include under current point headline into the scan range.
        (if (<= start (point-max))
            (while (setq point (text-property-not-all
                                current start 'pen-id nil))
              (setq current (1+ point))) ; not at (point-max)
          (while (setq point (text-property-not-all
                              current (point-max) 'pen-id nil))
            (setq current (1+ point)))) ; at the (point-max)
        (setq number (1- (get-text-property (1- current) 'pen-id)))
        (goto-char (text-property-any (point-min) (point-max) 'pen-id (1+ number))))))))

(defun pen-next ()
  (let* ((header-in-line (text-property-not-all (line-beginning-position) (line-end-position) 'pen-id nil))
         (location (get-text-property (or header-in-line (point)) 'pen-id))
         next)
    (cond
     ;; check the current line headline number first, since if use org-cycle, cursor will go to the begining of line
     ((numberp location)
      (setq next (text-property-any (point-min) (point-max) 'pen-id (1+ location)))
      (if (numberp next)
          (goto-char next)
        (goto-char (point-max))))
     ;; check the current point if >= the first header (no matter level), keep (point) if no headlines
     ((>= (setq next (or (text-property-not-all (point-min) (point-max) 'pen-id nil) (point))) (point))
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
                              current start 'pen-id nil))
            (setq current (1+ point))))
        (cond ((equal (point) 1) (setq number 0))
              ((equal (point) 2) (setq number 0))
              ((equal (point) (point-max)) (setq number (point-max)) (message "End of buffer"))
              (t
               (setq number (1- (get-text-property (1- current) 'pen-id)))))
        (goto-char (or (text-property-any (point-min) (point-max) 'pen-id (+ 2 number)) (point-max))))))))

;;;###autoload
(defun pen-mark-at-point ()
  "Mark the current line."
  (interactive)
  (remove-overlays (line-beginning-position) (line-end-position))
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (inhibit-read-only t)
         (overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'pen-mark-face)
    (put-text-property beg end 'pen-mark ?>)))

;;;###autoload
(defun pen-unmark-at-point ()
  "Unmark the current line."
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (inhibit-read-only t))
    (remove-overlays (line-beginning-position) (line-end-position))
    (remove-text-properties beg end '(pen-mark nil))))

;;;###autoload
(defun pen-mark-and-forward ()
  "Mark the current line and forward."
  (interactive)
  (pen-mark-at-point)
  (pen-next))

;;;###autoload
(defun pen-unmark-and-forward ()
  "Unmark the current line and forward."
  (interactive)
  (pen-unmark-at-point)
  (pen-next))

;;;###autoload
(defun pen-unmark-and-backward ()
  "Unmark the current line and backward."
  (interactive)
  (pen-previous)
  (pen-unmark-at-point))

(defun pen-clear-marks ()
  (if (eq (current-buffer) (get-buffer "*pen*"))
      (let* ((beg (point-min))
             (end (point-max))
             (inhibit-read-only t))
        (remove-overlays beg end)
        (remove-text-properties beg end '(pen-mark nil)))))

;;;###autoload
(defun pen-find-candidate-at-point ()
  "Find candidate at point and return the list."
  (interactive)
  (get-text-property (point) 'pen-entry))

;;;###autoload
(defun pen-find-marked-candidates ()
  "Find marked candidates and return the alist."
  (interactive)
  (save-excursion
    (let (candidate beg end cand-list)
      (when (text-property-not-all (point-min) (point-max) 'pen-mark nil)
        (setq end (text-property-any (point-min) (point-max) 'pen-mark ?>))
        (while (setq beg (text-property-any end (point-max) 'pen-mark ?>) )
          (goto-char beg)
          (setq candidate (pen-find-candidate-at-point))
          (push candidate cand-list)
          ;; (message (number-to-string beg))
          (forward-line 1)
          (setq end (point)))
        cand-list))))


(defun pen-insert-and-make-overlay (str prop val &optional export)
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
(defun pen-scroll-up(arg)
  (interactive "p")
  (cond ((bound-and-true-p focus-mode)
         (call-interactively 'focus-next-thing))
        ((eq major-mode 'nov-mode)
         (call-interactively 'nov-scroll-up))
        (t (call-interactively 'scroll-up))))

;;;###autoload
(defun pen-scroll-down(arg)
  (interactive "P")
  (cond ((bound-and-true-p focus-mode)
         (call-interactively 'focus-prev-thing))
        ((eq major-mode 'nov-mode)
         (call-interactively 'nov-scroll-down))
        (t (call-interactively 'scroll-down))))

;;;###autoload
(defun pen-goto-toc()
  (interactive)
  (pcase major-mode
    ('nov-mode (nov-goto-toc))
    ('wallabag-entry-mode (wallabag))
    (_ (consult-notes))))

;;;###autoload
(defun pen-step-backward()
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
(defun pen-step-forward()
  (interactive)
  (pcase major-mode
    ('nov-mode (nov-history-back))
    (_ (better-jumper-jump-forward))))


;;;###autoload
(defun pen-view-current-thing()
  (interactive)
  (if (bound-and-true-p focus-mode)
      (funcall-interactively 'pen-view-note-current-thing)
    (funcall-interactively 'pen-view-note)))

;;;###autoload
(defun pen-occur()
  (interactive)
  (pcase major-mode
    ('nov-mode (shrface-occur))
    (_ (call-interactively 'occur))))


;;;###autoload
(defun pen-replay ()
  (interactive)
  (let* ((entry (alist-get 'word (get-char-property (point) 'pen-entry)))
         (word (if entry
                   (s-collapse-whitespace
                    (pen-get-real-word entry) )
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

(provide 'pen-util)
