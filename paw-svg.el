;;; paw-svg.el -*- lexical-binding: t; -*-

(require 'paw-vars)
(require 'svg-lib)
(require 'dash)
(require 'all-the-icons nil t)
(require 'nerd-icons nil t)

(defcustom paw-svg-enable nil
  "Enable SVG image buttons. If nil, will try to use PBM image buttons."
  :type 'boolean
  :group 'paw)

(defcustom paw-pbm-enable nil
  "Enable PBM image buttons If nil, will try to use all-the-icons buttons."
  :type 'boolean
  :group 'paw)

(defcustom paw-all-the-icons-icon-enable nil
  "Enable all the icons icon. If nil, will use text icon, used on dashboard."
  :type 'boolean
  :group 'paw)


(defcustom paw-all-the-icons-button-enable nil
  "Enable all the icon buttons. If nil, will use text buttons, used on paw-view-note-mode."
  :type 'boolean
  :group 'paw)


(defcustom paw-nerd-icons-icon-enable nil
  "Enable nerd icons icon. If nil, will use text icon, used on dashboard."
  :type 'boolean
  :group 'paw)


(defcustom paw-nerd-icons-button-enable nil
  "Enable nerd icon buttons. If nil, will use text buttons, used on paw-view-note-mode."
  :type 'boolean
  :group 'paw)


(defcustom paw-add-button-online-p t
  "If t, add button will add word to online studylist."
  :type 'boolean
  :group 'paw)

(defvar paw-pbm-path (concat (file-name-directory load-file-name) "images")
  "Path to pbm images.")


(defvar paw-provider-english-url-alist
  (append `(("TIO"       "https://tio.freemdict.com/api?br=1&key=%s")
            ("有道" "https://www.youdao.com/result?word=%s&lang=en")
            ("欧陆" ,(if (eq system-type 'android)
                        "https://dict.eudic.net/mdicts/en/%s"
                      "https://dict.eudic.net/dicts/en/%s"))
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

(defvar paw-provider-chinese-url-alist
  (append '(("Wiktionary(中)" "https://zh.wiktionary.org/wiki/%s")
            )))

(defcustom paw-provide-general-urls-p t
  "Provide general Search URLS in `paw-view-note' buffer."
  :group 'paw
  :type 'boolean)

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



(defun paw-star-face-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "star-face" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0 :radius 0 ))
         ('dark
          (svg-lib-icon "star-face" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0 :radius 0 :foreground "yellow" :background (face-attribute 'default :background)))))
   (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-icon-enable)
       (all-the-icons-material "star"))
   (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
       (nerd-icons-mdicon "nf-md-star") "*")))



(defun paw-word-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "star" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0 :radius 0 ))
         ('dark
          (svg-lib-icon "star" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0 :radius 0 :foreground "yellow" :background (face-attribute 'default :background)))))
   (if (and (fboundp 'all-the-icons-faicon) paw-all-the-icons-icon-enable)
       (all-the-icons-faicon "book"))
   (if (and (fboundp 'nerd-icons-faicon) paw-nerd-icons-icon-enable)
       (nerd-icons-faicon "nf-fa-book") "+")))


(defun paw-question-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "help" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0 :foreground "red"))
         ('dark
          (svg-lib-icon "help" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0 :foreground "red" :background (face-attribute 'default :background)))))
   (if (and (fboundp 'all-the-icons-faicon) paw-all-the-icons-icon-enable)
       (all-the-icons-faicon "question"))
   (if (and (fboundp 'nerd-icons-faicon) paw-nerd-icons-icon-enable)
       (nerd-icons-faicon "nf-fa-question") "?")))


(defun paw-todo-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "checkbox-blank-outline" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "checkbox-blank-outline" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-icon-enable)
       (all-the-icons-material "check_box_outline_blank"))
   (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
       (nerd-icons-mdicon "nf-md-checkbox_blank_outline") "□")))


(defun paw-done-icon ()
    (or
     (if paw-svg-enable
         (pcase (frame-parameter nil 'background-mode)
           ('light
            (svg-lib-icon "checkbox-outline" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0))
           ('dark
            (svg-lib-icon "checkbox-outline" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
     (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-icon-enable)
         (all-the-icons-material "done"))
     (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
         (nerd-icons-mdicon "nf-md-checkbox_outline") "✓")))

(defun paw-cancel-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "close-box-outline" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "close-box-outline" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-icon-enable)
       (all-the-icons-material "cancel"))
   (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
       (nerd-icons-mdicon "nf-md-close_box_outline") "✗")))

(defun paw-bookmark-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "bookmark" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "bookmark-outline" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-icon-enable)
       (all-the-icons-material "bookmark"))
   (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
       (nerd-icons-mdicon "nf-md-bookmark_outline") "↪")))

(defun paw-file-link-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "file" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "file-outline" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-icon-enable)
       (all-the-icons-material "link"))
   (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
       (nerd-icons-mdicon "nf-md-file_outline") "⟷")))

(defun paw-url-link-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "link" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "link" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   (if (and (fboundp 'all-the-icons-faicon) paw-all-the-icons-icon-enable)
       (all-the-icons-faicon "link"))
   (if (and (fboundp 'nerd-icons-faicon) paw-nerd-icons-icon-enable)
       (nerd-icons-faicon "nf-fa-link") "➔")))

(defun paw-annotation-link-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "open-in-new" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "open-in-new" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   (if (and (fboundp 'all-the-icons-octicon) paw-all-the-icons-icon-enable)
       (all-the-icons-octicon "link"))
   (if (and (fboundp 'nerd-icons-octicon) paw-nerd-icons-icon-enable)
       (nerd-icons-octicon "nf-oct-link") "❰")))

(defun paw-attachment-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "paperclip" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "paperclip" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-icon-enable)
       (all-the-icons-material "attachment"))
   (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
       (nerd-icons-mdicon "nf-md-paperclip") "❱")))

(defun paw-image-icon ()
  (or
   (if paw-svg-enable
       (pcase (frame-parameter nil 'background-mode)
         ('light
          (svg-lib-icon "image" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0))
         ('dark
          (svg-lib-icon "image-outline" nil :scale 1 :height (if (eq system-type 'windows-nt) 0.5 0.9) :margin (if (eq system-type 'windows-nt) -1 0) :padding 0 :stroke 0 :foreground "white" :background (face-attribute 'default :background)))) )
   (if (and (fboundp 'all-the-icons-faicon) paw-all-the-icons-icon-enable)
       (all-the-icons-faicon "picture-o"))
   (if (and (fboundp 'nerd-icons-faicon) paw-nerd-icons-icon-enable)
       (nerd-icons-faicon "nf-fa-picture_o") "⟨")))


(defun paw-play-source-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[play]" (or callback 'paw-play-source-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "playlist-play.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-play-source-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-play-source-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "[▶]"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "playlist_play")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-playlist_play"))
            callback
            'paw-play-source-button-function))))

(defvar paw-all-the-icons-button-v-adjust -0.11)
(defvar paw-nerd-icons-button-v-adjust 0.11)

(defun paw-play-source-button-function (&optional arg)
  (interactive)
  (funcall paw-default-say-word-function (paw-get-real-word (paw-note-word)) :source t))

(defun paw-play-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[play]" (or callback 'paw-play-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "play.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-play-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-play-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "[▶]"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "play_arrow")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-play"))

            callback
            'paw-play-button-function))))

(defun paw-play-button-function (&optional arg)
  (interactive)
  (funcall paw-default-say-word-function (paw-get-real-word (paw-note-word))))


(defun paw-return-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[keyboard-return]" (or callback 'paw-return-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "keyboard-return.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-return-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-return-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "[BACK]"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "keyboard_return")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-keyboard_return"))
            callback
            'paw-return-button-function))))

(defun paw-return-button-function (&optional arg)
  (interactive)
  (if (car (paw-candidate-by-word (paw-note-word)))
      (funcall-interactively 'paw-find-origin)
    (let* ((name (org-entry-get nil paw-file-property-doc-file))
           (location (org-entry-get nil paw-file-property-note-location))
           (buffer (cl-find-if (lambda (b)
                                 (with-current-buffer b
                                   (string= buffer-file-truename name)))
                               (buffer-list))))

      (switch-to-buffer-other-window buffer)
      (with-current-buffer buffer
        (goto-char (1- (string-to-number location) ))))))


(defun paw-level-1-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[numeric-1-circle-outline]" (or callback 'paw-change-word-learning-level)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "numeric-1-circle-outline.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-change-word-learning-level))
                          (define-key map (kbd "<return>") (or callback 'paw-change-word-learning-level))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "[1]"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "looks_one")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-numeric_1"))
            callback
            'paw-change-word-learning-level))))

(defun paw-level-2-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[numeric-2-circle-outline]" (or callback 'paw-change-word-learning-level)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "numeric-2-circle-outline.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-change-word-learning-level))
                          (define-key map (kbd "<return>") (or callback 'paw-change-word-learning-level))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "[2]"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "looks_two")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-numeric_2"))
            callback
            'paw-change-word-learning-level))))

(defun paw-level-3-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[numeric-3-circle-outline]" (or callback 'paw-change-word-learning-level)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "numeric-3-circle-outline.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-change-word-learning-level))
                          (define-key map (kbd "<return>") (or callback 'paw-change-word-learning-level))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "[3]"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "looks_3")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-numeric_3"))
            callback
            'paw-change-word-learning-level))))

(defun paw-level-4-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[numeric-4-circle-outline]" (or callback 'paw-change-word-learning-level)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "numeric-4-circle-outline.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-change-word-learning-level))
                          (define-key map (kbd "<return>") (or callback 'paw-change-word-learning-level))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "[4]"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "looks_4")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-numeric_4"))
            callback
            'paw-change-word-learning-level))))

(defun paw-level-5-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[check-circle-outline]" (or callback 'paw-change-word-learning-level)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "check-circle-outline.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-change-word-learning-level))
                          (define-key map (kbd "<return>") (or callback 'paw-change-word-learning-level))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "[✓]"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "done")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-check"))
            callback
            'paw-change-word-learning-level))))

(defun paw-share-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[share]" (or callback 'paw-share-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "share.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-share-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-share-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "[S]"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "share")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-share"))
            callback
            'paw-share-button-function))))

(defun paw-share-button-function (&optional arg)
  (interactive)
  (funcall paw-share-word-function (paw-get-real-word (paw-note-word))))

(defun paw-prev-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[arrow-up-thick]" (or callback 'paw-prev-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "arrow-up-thick.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-prev-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-prev-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "[Up]"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "arrow_upward")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-arrow_up"))
            callback
            'paw-prev-button-function))))

(defun paw-prev-button-function (&optional arg)
  (interactive)
  (funcall-interactively 'paw-view-note-prev-thing))


(defun paw-next-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[arrow-down-thick]" (or callback 'paw-next-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "arrow-down-thick.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-next-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-next-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "[Down]"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "arrow_downward")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-arrow_down"))
            callback
            'paw-next-button-function))))

(defun paw-next-button-function (&optional arg)
  (interactive)
  (funcall-interactively 'paw-view-note-next-thing))


(defun paw-add-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[plus]" (or callback 'paw-add-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "plus.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-add-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-add-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "[+]"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "add")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-plus"))
            callback
            'paw-add-button-function))))

(defvar paw-add-button-action-list '("word"
                                     "word note"
                                     "highlight"
                                     "todo"
                                     "done"
                                     "cancel"
                                     "question"
                                     "link"
                                     "bookmark"))

(defun paw-add-button-function (&optional arg)
  (interactive)
  (let ((action (completing-read "Add this word/sentence/phrase as: " paw-add-button-action-list)))
    (pcase action
      ("word" (if paw-add-button-online-p
                  (let ((paw-add-online-word-without-asking nil)) ;; provide a way for user to input meaning, even if it is t
                    (funcall-interactively 'paw-add-online-word (paw-note-word)))
                (let ((paw-add-offline-word-without-asking nil))
                  (funcall-interactively 'paw-add-offline-word (paw-note-word)))))
      ("word note" (paw-add-general
                    (paw-note-word)
                    (assoc 'word paw-note-type-alist)
                    (alist-get 'origin_point paw-note-entry)
                    arg
                    (alist-get 'note paw-note-entry)
                    (alist-get 'origin_path paw-note-entry)
                    (alist-get 'origin_type paw-note-entry)))
      ("highlight" (paw-add-general
                    (paw-note-word)
                    paw-annotation-current-highlight-type
                    (alist-get 'origin_point paw-note-entry)
                    arg
                    (alist-get 'note paw-note-entry)
                    (alist-get 'origin_path paw-note-entry)
                    (alist-get 'origin_type paw-note-entry)))
      ("todo" (paw-add-general
               (paw-note-word)
               (assoc 'todo paw-note-type-alist)
               (alist-get 'origin_point paw-note-entry)
               arg
               (or (alist-get 'note paw-note-entry)
                   (alist-get 'context paw-note-entry))
               (alist-get 'origin_path paw-note-entry)
               (alist-get 'origin_type paw-note-entry)))
      ("done" (paw-add-general
               (paw-note-word)
               (assoc 'done paw-note-type-alist)
               (alist-get 'origin_point paw-note-entry)
               arg
               (or (alist-get 'note paw-note-entry)
                   (alist-get 'context paw-note-entry))
               (alist-get 'origin_path paw-note-entry)
               (alist-get 'origin_type paw-note-entry)))
      ("cancel" (paw-add-general
                 (paw-note-word)
                 (assoc 'cancel paw-note-type-alist)
                 (alist-get 'origin_point paw-note-entry)
                 arg
                 (or (alist-get 'note paw-note-entry)
                     (alist-get 'context paw-note-entry))
                 (alist-get 'origin_path paw-note-entry)
                 (alist-get 'origin_type paw-note-entry)))
      ("question" (paw-add-general
                   (paw-note-word)
                   (assoc 'question paw-note-type-alist)
                   (alist-get 'origin_point paw-note-entry)
                   arg
                   (or (alist-get 'note paw-note-entry)
                       (alist-get 'context paw-note-entry))
                   (alist-get 'origin_path paw-note-entry)
                   (alist-get 'origin_type paw-note-entry)))
      ("link" (paw-add-general
               (paw-note-word)
               (assoc 'link paw-note-type-alist)
               (alist-get 'origin_point paw-note-entry)
               arg
               (or (alist-get 'note paw-note-entry)
                   (alist-get 'context paw-note-entry))
               (alist-get 'origin_path paw-note-entry)
               (alist-get 'origin_type paw-note-entry)))
      ("bookmark" (paw-add-general
                   (paw-note-word)
                   (assoc 'bookmark paw-note-type-alist)
                   (alist-get 'origin_point paw-note-entry)
                   arg
                   (or (alist-get 'note paw-note-entry)
                       (alist-get 'context paw-note-entry))
                   (alist-get 'origin_path paw-note-entry)
                   (alist-get 'origin_type paw-note-entry)))
      (_ (message "No action taken")))
    ;; if action is inside paw-add-button-action-list
    (when (member action paw-add-button-action-list)
      (message "Added %s" action))))

(defun paw-edit-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[pencil]" (or callback 'paw-edit-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "file-edit-outline.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-edit-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-edit-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "[E]"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "create")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-note_edit_outline"))
            callback
            'paw-edit-button-function))))

(defun paw-edit-button-function(&optional arg)
  (interactive)
  (let ((title (org-no-properties (org-get-heading t t t t))))
    (cond ((string-prefix-p "Saved Meanings" title)
           (funcall 'paw-find-saved-meanings (car (paw-candidate-by-word (paw-note-word)))))
          ((string-prefix-p "Meaning" title)
           (funcall 'paw-change-studylist (car (paw-candidate-by-word (paw-note-word)))))
          ((string-prefix-p "Notes" title)
           (funcall 'paw-find-note (car (paw-candidate-by-word (paw-note-word)) )))
          (t (message "No note found"))) ))

(defun paw-delete-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[delete]" (or callback 'paw-delete-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "delete-outline.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-delete-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-delete-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "[-]"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "delete")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-delete"))
            callback
            'paw-delete-button-function))))

(defun paw-delete-button-function(&optional arg)
  (interactive)
  (let ((entry (car (paw-candidate-by-word (paw-note-word)))))
    (if entry
        ;; delete the word in db
        (funcall 'paw-delete-word entry)
      ;; add to known file instead of deleting it
      (funcall 'paw-delete-word (paw-new-entry (paw-note-word)
                                               :lang (paw-note-lang)
                                               :add-to-known-words t))))
  ;; TODO optional quit the window after deleting word, but user may want to keep the window
  ;; (when (get-buffer paw-view-note-buffer-name)
  ;;   (paw-view-note-quit))
  )

(defun paw-stardict-button ()
  (if paw-svg-enable
      (svg-lib-button "[text-search] Sdcv" 'paw-stardict-button-function)
    (format "%s" (buttonize "<Sdcv>" 'paw-stardict-button-function) )))

(defun paw-stardict-button-function (&optional arg)
  (interactive)
  (funcall paw-stardict-function (paw-get-real-word (paw-note-word))))

(defun paw-goldendict-button (&optional callback)
  (cond (paw-svg-enable (svg-lib-button "[text-search] Goldendict" (or callback 'paw-goldendict-button-function)))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "open-in-new.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") (or callback 'paw-goldendict-button-function))
                          (define-key map (kbd "<return>") (or callback 'paw-goldendict-button-function))
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "<Goldendict>"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "open_in_new")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-open_in_new"))
            callback
            'paw-goldendict-button-function))))

(defun paw-goldendict-button-function (&optional arg)
  (interactive)
  (funcall paw-external-dictionary-function (paw-get-real-word (paw-note-word))))


(defun paw-mdict-button ()
  (if paw-svg-enable
      (svg-lib-button "[text-search] Mdict" 'paw-mdict-button-function)
    (format "%s" (buttonize "<M>" 'paw-mdict-button-function) )))

(defun paw-mdict-button-function (&optional arg max-attempts)
  (interactive)
  (require 'mdx-dictionary)

  (let ((attempts 0)
        (check-interval 2) ; sets time interval between checks
        (max-attempts (or max-attempts 3))) ; set max attempts, default to 3

    ;; Start server if not already running
    (unless (process-live-p mdx-dictionary-server-process)
      (mdx-dictionary-start-server))
    ;; Check if server is running
    (while (and (< attempts max-attempts)
                (not (process-live-p mdx-dictionary-server-process)))

      ;; If not, wait a bit and try again
      (message "Server not running, attempt %d of %d..."
               (1+ attempts) max-attempts)
      (sit-for check-interval)

      ;; Increment attempts
      (setq attempts (1+ attempts)))

    ;; After max attempts, if still not running, message error
    (if (process-live-p mdx-dictionary-server-process)
        (funcall paw-mdict-dictionary-function
                 (format "http://localhost:8000/%s" (paw-note-word)))
      (error "Failed to start server after %d attempts" max-attempts))))

(defun paw-translate-button ()
  (cond (paw-svg-enable (svg-lib-button "[ideogram-cjk-variant] 译" 'paw-translate-button-function))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "translate.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") 'paw-translate-button-function)
                          (define-key map (kbd "<return>") 'paw-translate-button-function)
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "<译>"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "translate")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-google_translate"))
            nil
            'paw-translate-button-function))))

(defun paw-translate-button-function (&optional arg)
  (interactive)
  (let* ((section (org-no-properties (org-get-heading t t t t)))
         (to-translate (cond ((string-prefix-p "Translation" section)
                              (paw-get-real-word (paw-note-word)))
                             ((string-prefix-p "Context" section)
                              paw-note-context)
                             ((string-prefix-p "Notes" section)
                              (or paw-note-note (alist-get 'note (car (paw-candidate-by-word (paw-note-word)) ) )))
                             (t (paw-get-real-word (paw-note-word))))))
    (funcall paw-translate-function
             to-translate
             nil
             (current-buffer)
             section)))

(defun paw-ai-translate-button ()
  (cond (paw-svg-enable (svg-lib-button "[ideogram-cjk-variant] AI译" 'paw-ai-translate-button-function))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "translate-variant.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") 'paw-ai-translate-button-function)
                          (define-key map (kbd "<return>") 'paw-ai-translate-button-function)
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "<译>"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "language")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-translate"))
            nil
            'paw-ai-translate-button-function))))

(defun paw-ai-translate-button-function (&optional arg)
  (interactive)
  (let* ((section (org-no-properties (org-get-heading t t t t)))
         (to-translate (cond ((string-prefix-p "Translation" section)
                              (replace-regexp-in-string "^[ \n]+" "" (paw-get-real-word (paw-note-word))))
                             ((string-prefix-p "Context" section)
                              paw-note-context)
                             ((string-prefix-p "Notes" section)
                              paw-note-note)
                             (t (paw-get-real-word (paw-note-word)))))
         (prompt (or paw-gptel-ai-translate-prompt
                     (cond ((string-prefix-p "Translation" section)
                            (format "Translate this word/sentence/phrase into %s: %s. It is used in: %s"
                                    paw-gptel-language
                                    to-translate
                                    (or paw-note-note paw-note-context)))
                           (t (format "Translate this word/sentence/phrase into %s: %s"
                                      paw-gptel-language
                                      to-translate))))))
    (funcall paw-ai-translate-function
             to-translate
             prompt
             nil
             nil
             section)))

(defun paw-ask-ai-button ()
  (cond (paw-svg-enable (svg-lib-button "[chat-question] Ask AI" 'paw-ask-ai-button-function))
        (paw-pbm-enable (let* ((image (create-image (expand-file-name "chat-question-outline.pbm" paw-pbm-path)
                                                    nil nil :ascent 'center))
                               (map (make-sparse-keymap)))
                          (define-key map (kbd "<mouse-1>") 'paw-ask-ai-button-function)
                          (define-key map (kbd "<return>") 'paw-ask-ai-button-function)
                          (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                            image-string)))
        (t (paw-make-text-button
            "<Ask AI>"
            (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                    "mic")
                (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                    "nf-md-chat"))
            nil
            'paw-ask-ai-button-function))))

(defcustom paw-ask-ai-prompt "I'm reading %s, we have the following highlighted text: %s, %s"
  "The initial prompt for AI translation.
The first %s is the context
The second %s is the word
The final %s is the question."
  :group 'paw
  :type 'string)

(defcustom paw-ask-ai-question '("Answer it"
                                 "Brainstorm ideas"
                                 "Draft an outline"
                                 "Draft anything"
                                 "Draft an email"
                                 "Draft a journal entry"
                                 "Draft a meeting agenda"
                                 "Explain in 12 words or less"
                                 "Explain in 48 words or less"
                                 "Explain in 100 words or less"
                                 "Explain in 200 words or less"
                                 "Summarize it"
                                 "Translate it to Chinese"
                                 "Write anything")
  "The default question to ask AI."
  :group 'paw
  :type '(repeat string))

(defcustom paw-ask-ai-defualt-question "Explain in 48 words or less"
  "The default question to ask AI."
  :group 'paw
  :type 'string)

(defun paw-ask-ai-button-function (&optional arg)
  (interactive)
  (let* ((word (paw-get-real-word (paw-note-word)))
         (word (replace-regexp-in-string "^[ \n]+" "" word)))
    (funcall paw-ask-ai-function
             (format paw-ask-ai-prompt
                     (if (buffer-live-p paw-note-target-buffer)
                         (with-current-buffer paw-note-target-buffer
                           (pcase major-mode
                             ('nov-mode
                              (format "in this book, author: %s, title: %s, published at %s"
                                      (alist-get 'creator nov-metadata)
                                      (alist-get 'title nov-metadata)
                                      (alist-get 'date nov-metadata)))
                             ;; TODO support other modes
                             (_ (paw-get-note))))
                       "")
                     word
                     (if paw-ask-ai-p paw-ask-ai-defualt-question
                       (completing-read "Ask AI: " paw-ask-ai-question))))))



(defvar paw-provider-english-urls nil)
(defun paw-provider-english-urls()
  (setq paw-provider-english-urls
        (cl-loop for paw-provider in paw-provider-english-url-alist collect
                 (let* ((name (car paw-provider))
                        (url (paw-provider-lookup (paw-note-word) (car paw-provider) paw-provider-english-url-alist)))
                   (list name url) )) ))

(define-button-type 'paw-english-web-button-type
  'action 'paw-english-web-buttons-function
  'face 'paw-button-active-face
  'follow-link t)

(defun paw-english-web-buttons ()
  (cl-loop for url in paw-provider-english-url-alist collect
           (if paw-svg-enable
               (svg-lib-button
                (format "[web] %s" (car url))
                'paw-english-web-buttons-function)
             (make-text-button (car url) nil 'type 'paw-english-web-button-type))))

(defun paw-english-web-buttons-function (&optional arg)
  (interactive)
  (funcall paw-dictionary-browse-function
           (if paw-svg-enable
               (car (assoc-default (let* ((mouse-point (save-excursion
                                                         (mouse-set-point last-input-event)
                                                         (point)))
                                          (props (cdr (get-text-property mouse-point 'display))))
                                     (when (eq (plist-get props :type) 'svg)
                                       (let* ((data (plist-get props :data))
                                              (buf (with-temp-buffer
                                                     (insert data)
                                                     (xml-parse-region (point-min) (point-max))))
                                              (text-node (car (xml-get-children (car buf) 'text))))
                                         (s-trim (car (last text-node)) )))) (paw-provider-english-urls)))
             (car (assoc-default (button-label (button-at (point))) (paw-provider-english-urls))))))


(defvar paw-provider-japanese-urls nil)
(defun paw-provider-japanese-urls()
  (setq paw-provider-japanese-urls
        (cl-loop for paw-provider in paw-provider-japanese-url-alist collect
                 (let* ((name (car paw-provider))
                        (url (paw-provider-lookup (paw-note-word) (car paw-provider) paw-provider-japanese-url-alist)))
                   (list name url) )) ))


(define-button-type 'paw-japanese-web-button-type
  'action 'paw-japanese-web-buttons-function
  'face 'paw-button-active-face
  'follow-link t)


(defun paw-japanese-web-buttons ()
  (cl-loop for url in paw-provider-japanese-url-alist collect
           (if paw-svg-enable
               (svg-lib-button
                (format "[web] %s" (car url))
                'paw-japanese-web-buttons-function)
             (make-text-button (car url) nil 'type 'paw-japanese-web-button-type))))

(defun paw-japanese-web-buttons-function (&optional arg)
  (interactive)
  (funcall paw-dictionary-browse-function
           (if paw-svg-enable
               (car (assoc-default (let* ((mouse-point (save-excursion
							 (mouse-set-point last-input-event)
							 (point)))
					  (props (cdr (get-text-property mouse-point 'display)))
					  svg-data)
                                     (when (eq (plist-get props :type) 'svg)
                                       (setq svg-data (plist-get props :data))
                                       (with-temp-buffer
					 (insert svg-data)
					 (goto-char (point-min))
					 (if (re-search-forward "<text.*?>\\s-*\\(.*?\\)</text>" nil t)
                                             (match-string-no-properties 1)
					   "No text found in SVG data")))) (paw-provider-japanese-urls)))
             (car (assoc-default (button-label (button-at (point))) (paw-provider-japanese-urls))))))


(defvar paw-provider-chinese-urls nil)
(defun paw-provider-chinese-urls()
  (setq paw-provider-chinese-urls
        (cl-loop for paw-provider in paw-provider-chinese-url-alist collect
                 (let* ((name (car paw-provider))
                        (url (paw-provider-lookup (paw-note-word) (car paw-provider) paw-provider-chinese-url-alist)))
                   (list name url) )) ))


(define-button-type 'paw-chinese-web-button-type
  'action 'paw-chinese-web-buttons-function
  'face 'paw-button-active-face
  'follow-link t)


(defun paw-chinese-web-buttons ()
  (cl-loop for url in paw-provider-chinese-url-alist collect
           (if paw-svg-enable
               (svg-lib-button
                (format "[web] %s" (car url))
                'paw-chinese-web-buttons-function)
             (make-text-button (car url) nil 'type 'paw-chinese-web-button-type))))

(defun paw-chinese-web-buttons-function (&optional arg)
  (interactive)
  (funcall paw-dictionary-browse-function
           (if paw-svg-enable
               (car (assoc-default (let* ((mouse-point (save-excursion
							 (mouse-set-point last-input-event)
							 (point)))
					  (props (cdr (get-text-property mouse-point 'display)))
					  svg-data)
                                     (when (eq (plist-get props :type) 'svg)
                                       (setq svg-data (plist-get props :data))
                                       (with-temp-buffer
					 (insert svg-data)
					 (goto-char (point-min))
					 (if (re-search-forward "<text.*?>\\s-*\\(.*?\\)</text>" nil t)
                                             (match-string-no-properties 1)
					   "No text found in SVG data")))) (paw-provider-chinese-urls)))
             (car (assoc-default (button-label (button-at (point))) (paw-provider-chinese-urls))))))

(defvar paw-provider-general-urls nil)
(defun paw-provider-general-urls()
  (setq paw-provider-general-urls
        (cl-loop for paw-provider in paw-provider-general-url-alist collect
                 (let* ((name (car paw-provider))
                        (url (paw-provider-lookup (paw-note-word) (car paw-provider) paw-provider-general-url-alist)))
                   (list name url) )) ))

(define-button-type 'paw-general-web-button-type
  'action 'paw-general-web-buttons-function
  'face 'paw-button-active-face
  'follow-link t)

(defun paw-general-web-buttons ()
  (cl-loop for url in paw-provider-general-url-alist collect
           (if paw-svg-enable
               (svg-lib-button
                (format "[web] %s" (car url))
                'paw-general-web-buttons-function)
             (make-text-button (car url) nil 'type 'paw-general-web-button-type))))

(defun paw-general-web-buttons-function (&optional arg)
  (interactive)
  (funcall paw-dictionary-browse-function
           (if paw-svg-enable
               (car (assoc-default (let* ((mouse-point (save-excursion
                                                         (mouse-set-point last-input-event)
                                                         (point)))
                                          (props (cdr (get-text-property mouse-point 'display)))
                                          svg-data)
                                     (when (eq (plist-get props :type) 'svg)
                                       (setq svg-data (plist-get props :data))
                                       (with-temp-buffer
                                         (insert svg-data)
                                         (goto-char (point-min))
                                         (if (re-search-forward "<text.*?>\\s-*\\(.*?\\)</text>" nil t)
                                             (match-string-no-properties 1)
                                           "No text found in SVG data")))) (paw-provider-general-urls)))
             (car (assoc-default (button-label (button-at (point))) (paw-provider-general-urls))))))

(defun paw-note-word ()
  "Get the word of the current note."
  (cond
   ;; get the word inside "*paw-view-note*", invoked by `paw-view-note'
   (paw-note-word paw-note-word)
   ;; get the word via char property
   ((get-char-property (point) 'paw-entry)
    (alist-get 'word (get-char-property (point) 'paw-entry)))
   ;; get the word via overlay
   ((cl-find-if
     (lambda (o)
       (overlay-get o 'paw-entry))
     (overlays-at (point)))
    (alist-get 'word (overlay-get (cl-find-if
                                   (lambda (o)
                                     (overlay-get o 'paw-entry))
                                   (overlays-at (point))) 'paw-entry)))
   ;; get the word inside "*paw-view-note", invoked by `paw-view-notes'
   (t (save-excursion
        (org-up-heading-safe)
        (org-entry-get nil "id")))))


(defun paw-note-lang ()
  "Get the lang of the current note."
  (cond
   ;; get the word inside "*paw-view-note*", invoked by `paw-view-note'
   (paw-note-lang paw-note-lang)
   ;; get the word via char property
   ((get-char-property (point) 'paw-entry)
    (alist-get 'lang (get-char-property (point) 'paw-entry)))
   ;; get the word via overlay
   ((cl-find-if
     (lambda (o)
       (overlay-get o 'paw-entry))
     (overlays-at (point)))
    (alist-get 'lang (overlay-get (cl-find-if
                                   (lambda (o)
                                     (overlay-get o 'paw-entry))
                                   (overlays-at (point))) 'paw-entry)))
   ;; get the word inside "*paw-view-note", invoked by `paw-view-notes'
   (t (save-excursion
        (org-up-heading-safe)
        (org-entry-get nil "LANGUAGE")))))


(defmacro paw-web-buttons (language)
  `(progn
    (defvar ,(intern (format "paw-%s-web-section-index" language)) 0)
    (defcustom ,(intern (format "paw-%s-web-button-number" language)) 4
      "Define the number of buttons in a section."
      :type 'integer
      :group 'paw)
    (defvar ,(intern (format "paw-%s-web-buttons-sections-beg" language)) nil)
    (defvar ,(intern (format "paw-%s-web-buttons-sections-end" language)) nil)
    (defvar ,(intern (format "paw-%s-web-buttons-sections" language)) nil)

    (defun ,(intern (format "paw-%s-web-buttons-sections" language)) ()
       (setq ,(intern (format "paw-%s-web-buttons-sections" language))
             (-partition-all ,(intern (format "paw-%s-web-button-number" language))
                         ,(intern (format "paw-%s-web-buttons" language)))))


    (defun ,(intern (format "paw-%s-web-left-button" language)) (&optional callback)
       (cond (paw-svg-enable (svg-lib-button "[arrow-left-thick]" (or callback ',(intern (format "paw-%s-web-left-button-function" language)))))
             (paw-pbm-enable (let* ((image (create-image (expand-file-name "arrow-left-thick.pbm" paw-pbm-path)
                                                         nil nil :ascent 'center))
                                    (map (make-sparse-keymap)))
                               (define-key map (kbd "<mouse-1>") (or callback ',(intern (format "paw-%s-web-left-button-function" language))))
                               (define-key map (kbd "<return>") (or callback ',(intern (format "paw-%s-web-left-button-function" language))))
                               (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                                 image-string)))
             (t (paw-make-text-button
                 "[Left]"
                 (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                         "arrow_back")
                     (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                         "nf-md-arrow_left"))
                 callback
                 ',(intern (format "paw-%s-web-left-button-function" language))))))

    (defun ,(intern (format "paw-%s-web-left-button-function" language)) (&optional arg)
       (interactive)
       (when (> ,(intern (format "paw-%s-web-section-index" language)) 0)
         (setq ,(intern (format "paw-%s-web-section-index" language)) (1- ,(intern (format "paw-%s-web-section-index" language)))))
       (save-excursion
         (with-current-buffer (current-buffer)
           (let ((inhibit-read-only t))
             (goto-char ,(intern (format "paw-%s-web-buttons-sections-beg" language)))
             (delete-region ,(intern (format "paw-%s-web-buttons-sections-beg" language)) ,(intern (format "paw-%s-web-buttons-sections-end" language)))
             (cl-loop for button in (nth ,(intern (format "paw-%s-web-section-index" language)) ,(intern (format "paw-%s-web-buttons-sections" language))) do
                      (insert button " "))
             (setq ,(intern (format "paw-%s-web-buttons-sections-end" language)) (point)))) ))

    (defun ,(intern (format "paw-%s-web-right-button" language)) (&optional callback)
       (cond (paw-svg-enable (svg-lib-button "[arrow-right-thick]" (or callback ',(intern (format "paw-%s-web-right-button-function" language)))))
             (paw-pbm-enable (let* ((image (create-image (expand-file-name "arrow-right-thick.pbm" paw-pbm-path)
                                                         nil nil :ascent 'center))
                                    (map (make-sparse-keymap)))
                               (define-key map (kbd "<mouse-1>") (or callback ',(intern (format "paw-%s-web-right-button-function" language))))
                               (define-key map (kbd "<return>") (or callback ',(intern (format "paw-%s-web-right-button-function" language))))
                               (let ((image-string (propertize " " 'display image 'keymap map 'mouse-face 'highlight)))
                                 image-string)))
             (t (paw-make-text-button
                 "[Right]"
                 (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                         "arrow_forward")
                     (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                         "nf-md-arrow_right"))
                 callback
                 ',(intern (format "paw-%s-web-right-button-function" language))))))

    (defun ,(intern (format "paw-%s-web-right-button-function" language)) (&optional arg)
       (interactive)
       (when (< ,(intern (format "paw-%s-web-section-index" language)) (1- (length ,(intern (format "paw-%s-web-buttons-sections" language)))))
         (setq ,(intern (format "paw-%s-web-section-index" language)) (1+ ,(intern (format "paw-%s-web-section-index" language)))))
       (with-current-buffer (current-buffer)
         (let ((inhibit-read-only t))
           (goto-char ,(intern (format "paw-%s-web-buttons-sections-beg" language)))
           (delete-region ,(intern (format "paw-%s-web-buttons-sections-beg" language)) ,(intern (format "paw-%s-web-buttons-sections-end" language)))
           (cl-loop for button in (nth ,(intern (format "paw-%s-web-section-index" language)) ,(intern (format "paw-%s-web-buttons-sections" language))) do
                    (insert button " "))
           (setq ,(intern (format "paw-%s-web-buttons-sections-end" language)) (point)))))
    (defvar ,(intern (format "paw-%s-web-left-button" language)) (,(intern (format "paw-%s-web-left-button" language))))
    (defvar ,(intern (format "paw-%s-web-right-button" language)) (,(intern (format "paw-%s-web-right-button" language))))
)
  )

(defun paw-make-text-button (text icon-name callback alternative)
  (make-text-button (propertize text 'display
                                (or (if (and (fboundp 'all-the-icons-material) paw-all-the-icons-button-enable)
                                        (all-the-icons-material icon-name :v-adjust paw-all-the-icons-button-v-adjust))
                                    (if (and (fboundp 'nerd-icons-mdicon) paw-nerd-icons-icon-enable)
                                        (nerd-icons-mdicon icon-name :v-adjust paw-nerd-icons-button-v-adjust) text)))
                    nil
                    'type
                    (define-button-type (intern (format "paw-%s-type" icon-name))
                      'action (lambda (arg) (funcall (or callback alternative)))
                      'face 'default
                      'follow-link t)))

(paw-web-buttons "english")
(paw-web-buttons "japanese")
(paw-web-buttons "chinese")
(paw-web-buttons "general")



(defvar paw-star-face-icon (paw-star-face-icon))
(defvar paw-word-icon (paw-word-icon))
(defvar paw-question-icon (paw-question-icon))
(defvar paw-todo-icon (paw-todo-icon))
(defvar paw-done-icon (paw-done-icon))
(defvar paw-cancel-icon (paw-cancel-icon))
(defvar paw-bookmark-icon (paw-bookmark-icon))
(defvar paw-file-link-icon (paw-file-link-icon))
(defvar paw-url-link-icon (paw-url-link-icon))
(defvar paw-annotation-link-icon (paw-annotation-link-icon))
(defvar paw-org-link-icon nil)
(defvar paw-attachment-icon (paw-attachment-icon))
(defvar paw-image-icon (paw-image-icon))

(defvar paw-play-source-button (paw-play-source-button))
(defvar paw-play-button (paw-play-button))
(defvar paw-prev-button (paw-prev-button))
(defvar paw-share-button (paw-share-button))
(defvar paw-next-button (paw-next-button))
(defvar paw-return-button (paw-return-button))
(defvar paw-level-1-button (paw-level-1-button))
(defvar paw-level-2-button (paw-level-2-button))
(defvar paw-level-3-button (paw-level-3-button))
(defvar paw-level-4-button (paw-level-4-button))
(defvar paw-level-5-button (paw-level-5-button))
(defvar paw-default-play-button paw-play-button)
(defvar paw-add-button (paw-add-button))
(defvar paw-edit-button (paw-edit-button))
(defvar paw-delete-button (paw-delete-button))
(defvar paw-stardict-button (paw-stardict-button))
(defvar paw-goldendict-button (paw-goldendict-button))
(defvar paw-mdict-button (paw-mdict-button))
(defvar paw-translate-button (paw-translate-button))
(defvar paw-ai-translate-button (paw-ai-translate-button))
(defvar paw-ask-ai-button (paw-ask-ai-button))
(defvar paw-english-web-buttons (paw-english-web-buttons))
(defvar paw-japanese-web-buttons (paw-japanese-web-buttons))
(defvar paw-chinese-web-buttons (paw-chinese-web-buttons))
(defvar paw-general-web-buttons (paw-general-web-buttons))


;;;###autoload
(defun paw-get-icons ()
  (interactive)
  (setq paw-star-face-icon (paw-star-face-icon))
  (setq paw-word-icon (paw-word-icon))
  (setq paw-question-icon (paw-question-icon))
  (setq paw-todo-icon (paw-todo-icon))
  (setq paw-done-icon (paw-done-icon))
  (setq paw-cancel-icon (paw-cancel-icon))
  (setq paw-bookmark-icon (paw-bookmark-icon))
  (setq paw-file-link-icon (paw-file-link-icon))
  (setq paw-url-link-icon (paw-url-link-icon))
  (setq paw-annotation-link-icon (paw-annotation-link-icon))
  (setq paw-org-link-icon nil)
  (setq paw-attachment-icon (paw-attachment-icon))
  (setq paw-image-icon (paw-image-icon)))

(defvar paw-get-buttons-p nil
  "If t, all buttons are already loaded.")

;;;###autoload
(defun paw-get-buttons ()
  (interactive)
  (unless paw-get-buttons-p
    (setq paw-play-source-button (paw-play-source-button))
    (setq paw-play-button (paw-play-button))
    (setq paw-default-play-button paw-play-button)
    (setq paw-prev-button (paw-prev-button))
    (setq paw-next-button (paw-next-button))
    (setq paw-share-button (paw-share-button))
    (setq paw-return-button (paw-return-button))
    (setq paw-level-1-button (paw-level-1-button))
    (setq paw-level-2-button (paw-level-2-button))
    (setq paw-level-3-button (paw-level-3-button))
    (setq paw-level-4-button (paw-level-4-button))
    (setq paw-level-5-button (paw-level-5-button))
    (setq paw-add-button (paw-add-button))
    (setq paw-edit-button (paw-edit-button))
    (setq paw-delete-button (paw-delete-button))
    (setq paw-stardict-button (paw-stardict-button))
    (setq paw-goldendict-button (paw-goldendict-button))
    (setq paw-mdict-button (paw-mdict-button))
    (setq paw-translate-button (paw-translate-button))
    (setq paw-ai-translate-button (paw-ai-translate-button))
    (setq paw-ask-ai-button (paw-ask-ai-button))
    (setq paw-english-web-buttons (paw-english-web-buttons))
    (setq paw-japanese-web-buttons (paw-japanese-web-buttons))
    (setq paw-general-web-buttons (paw-general-web-buttons))
    (setq paw-english-web-left-button (paw-english-web-left-button))
    (setq paw-english-web-right-button (paw-english-web-right-button))
    (setq paw-japanese-web-left-button (paw-japanese-web-left-button))
    (setq paw-japanese-web-right-button (paw-japanese-web-right-button))
    (setq paw-general-web-left-button (paw-general-web-left-button))
    (setq paw-general-web-right-button (paw-general-web-right-button))
    (setq paw-get-buttons-p t)))

(provide 'paw-svg)
