;;; paw/paw-eink-faces.el -*- lexical-binding: t; -*-

(defface paw-button-active-face
  '((((class color) (background light))
     :box (:line-width 2  :style released-button :style nil) :height 0.8)
    (((class color) (background dark))
     :box (:line-width 2  :style released-button :style nil) :height 0.8)
    (t :inherit default))
  "Default face for active button"
  :group 'paw-faces)

(defface paw-click-face
  '((((class color) (background light))
     :box t :height 1.0)
    (((class color) (background dark))
     :box t :height 1.0)
    (t :inherit default))
  "Face used for click overlay."
  :group 'paw-faces)

(defface paw-focus-face
  '((((class color) (background light))
     :underline t)
    (((class color) (background dark))
     :underline t)
    (t :inherit default))
  "Face used for find current thing overlay. If focus-mode, no need
to show."
  :group 'paw-faces)


(defface paw-unknown-word-face
  '((((class color) (background light))
     :foreground "#091B32" :background "light sky blue" :height 1.0)
    (((class color) (background dark))
     :foreground "#091B32" :background "light sky blue" :height 1.0)
    (t :inherit default))
  "Face used for unknown word overlay."
  :group 'paw-faces)


(defface paw-unknown-word-hover-face
  '((((class color) (background light))
     :foreground "#091B32" :background "#c6dfff" :height 1.0 :box (:line-width 2 :color "#424242"))
    (((class color) (background dark))
     :foreground "#64ABFF" :background "#253952" :height 1.0 :box (:line-width 2 :color "#E4E4E4"))
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-search-highlight-face
  '((t :underline t))
  "Face for the header at point."
  :group 'paw-faces)

(defface paw-note-header-title-face
  '((((class color) (background light))
     :foreground "#4F894C"
     :weight bold
     :height 1.1)
    (((class color) (background dark))
     :foreground "#A3BE8C"
     :weight bold
     :height 1.1)
    (t :inherit default))
  "Face used for *paw-note* header tilte face"
  :group 'paw-faces)

(defface paw-note-header-title-path-face
  '((((class color) (background light))
     :foreground "#3B6EA8"
     :height 0.7)
    (((class color) (background dark))
     :foreground "#d9c6d6"
     :height 0.7)
    (t :inherit default))
  "Face used for author."
  :group 'paw-faces)


(defface paw-view-note-title-face '((t :inherit default :height 2.0))
  "Face used for title on compact view."
  :group 'paw-faces)

(defface paw-date-face
  '((((class color) (background light))
     :foreground "#8b94a5")
    (((class color) (background dark))
     :foreground "#EBCB8B")
    (t :inherit default))
  "Face for the date (last_modified)."
  :group 'paw-faces)

(defface paw-path-face
  '((((class color) (background light))
     :foreground "#3B6EA8")
    (((class color) (background dark))
     :foreground "#d9c6d6")
    (t :inherit default))
  "Face used for author."
  :group 'paw-faces)

(defface paw-type-face
  '((((class color) (background light))
     :foreground "#29838D")
    (((class color) (background dark))
     :foreground "#8FBCBB")
    (t :inherit default))
  "Face for the file path."
  :group 'paw-faces)

(defface paw-wallabag-face
  '((((class color) (background light))
     :foreground "#4F894C")
    (((class color) (background dark))
     :foreground "#A3BE8C")
    (t :inherit default))
  "Face used for format."
  :group 'paw-faces)

(defface paw-exp-face
  '((t :inherit default))
  "Face used for exp."
  :group 'paw-faces)

(defface paw-origin-point-face
  '((((class color) (background light))
     :foreground "#4F894C")
    (((class color) (background dark))
     :foreground "#A3BE8C")
    (t :inherit default))
  "Face used for origin-point."
  :group 'paw-faces)

(defface paw-nov-face
  '((((class color) (background light))
     :foreground "MediumSlateBlue")
    (((class color) (background dark))
     :foreground "cyan")
    (t :inherit default))
  "Face used for hightlight."
  :group 'paw-faces)

(defface paw-pdf-face
  '((((class color) (background light))
     :foreground "#8b94a5")
    (((class color) (background dark))
     :foreground "#EBCB8B")
    (t :inherit default))
  "Face used for tag."
  :group 'paw-faces)

(defface paw-file-face
  '((((class color) (background light))
     :foreground "#29838D")
    (((class color) (background dark))
     :foreground "#8FBCBB")
    (t :inherit default))
  "Face for the date (last_modified)."
  :group 'paw-faces)

(defface paw-offline-face
  '((((class color) (background light))
     :foreground "#4F894C")
    (((class color) (background dark))
     :foreground "#A3BE8C")
    (t :inherit default))
  "Face used for format."
  :group 'paw-faces)


(defface paw-level-1-offline-word-face
  '((((class color) (background light))
     :foreground "#1b180f" :background "yellow1" :height 1.0)
    (((class color) (background dark))
     :foreground "#1b180f" :background "#423C2C" :height 1.0)
    (t :inherit default))
  "Face used for offline word overlay."
  :group 'paw-faces)

(defface paw-level-2-offline-word-face
  '((((class color) (background light))
     :foreground "#1b180f" :background "gold1" :height 1.0)
    (((class color) (background dark))
     :foreground "#1b180f" :background "gold1" :height 1.0)
    (t :inherit default))
  "Face used for online word overlay."
  :group 'paw-faces)


(defface paw-level-3-offline-word-face
  '((((class color) (background light))
     :foreground "#1b180f" :background "yellow3" :height 1.0)
    (((class color) (background dark))
     :foreground "#1b180f" :background "yellow3" :height 1.0)
    (t :inherit default))
  "Face used for online word overlay."
  :group 'paw-faces)


(defface paw-level-4-offline-word-face
  '((((class color) (background light))
     :foreground "#1b180f" :background "yellow2" :height 1.0)
    (((class color) (background dark))
     :foreground "#1b180f" :background "gold1" :height 1.0)
    (t :inherit default))
  "Face used for online word overlay."
  :group 'paw-faces)

(defface paw-level-1-word-face
  '((((class color) (background light))
     :foreground "#1b180f" :background "gold2" :height 1.0)
    (((class color) (background dark))
     :foreground "#1b180f" :background "gold2" :height 1.0)
    (t :inherit default))
  "Face used for online word overlay."
  :group 'paw-faces)

(defface paw-level-2-word-face
  '((((class color) (background light))
     :foreground "#1b180f" :background "gold1" :height 1.0)
    (((class color) (background dark))
     :foreground "#1b180f" :background "gold1" :height 1.0)
    (t :inherit default))
  "Face used for online word overlay."
  :group 'paw-faces)


(defface paw-level-3-word-face
  '((((class color) (background light))
     :foreground "#1b180f" :background "yellow3" :height 1.0)
    (((class color) (background dark))
     :foreground "#1b180f" :background "yellow3" :height 1.0)
    (t :inherit default))
  "Face used for online word overlay."
  :group 'paw-faces)


(defface paw-level-4-word-face
  '((((class color) (background light))
     :foreground "#1b180f" :background "yellow2" :height 1.0)
    (((class color) (background dark))
     :foreground "#1b180f" :background "yellow2" :height 1.0)
    (t :inherit default))
  "Face used for online word overlay."
  :group 'paw-faces)

(defface paw-word-face
  '((((class color) (background light))
     :foreground "#1b180f" :background "yellow1" :height 1.0)
    (((class color) (background dark))
     :foreground "#1b180f" :background "yellow1" :height 1.0)
    (t :inherit default))
  "Face used for word highlight overlay."
  :group 'paw-faces)

(defface paw-word-hover-face
  '((((class color) (background light))
     :foreground "#1b180f" :background "gold2" :height 1.0 :box (:line-width 2 :color "#424242"))
    (((class color) (background dark))
     :foreground "#e9bb43" :background "gold2" :height 1.0 :box (:line-width 2 :color "#E4E4E4"))
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-image-face
  '((((class color) (background light))
     :foreground "blue" :height 1.0)
    (((class color) (background dark))
     :foreground "LightBlue" :height 1.0)
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-bookmark-face
  '((((class color) (background light))
     :foreground "blue" :height 1.0)
    (((class color) (background dark))
     :foreground "LightBlue" :height 1.0)
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-question-face
  '((((class color) (background light))
     :foreground "blue" :height 1.0)
    (((class color) (background dark))
     :foreground "LightBlue" :height 1.0)
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-todo-face
  '((t :inherit org-todo))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-done-face
  '((t :inherit org-done))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-cancel-face
  '((t :inherit org-done :strike-through t))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-link-face
  '((t :inherit link :foreground "#2a9d8f"))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-attachment-face
  '((((class color) (background light))
     :foreground "blue" :height 1.0)
    (((class color) (background dark))
     :foreground "LightBlue" :height 1.0)
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-highlight-1-face
  '((((class color) (background light))
     :background "pale goldenrod")
    (((class color) (background dark))
     :background "lightgoldenrodyellow" :foreground "#191919")
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-highlight-2-face
  '((((class color) (background light))
     :background "MediumSpringGreen")
    (((class color) (background dark))
     :background "MediumSpringGreen" :foreground "#191919")
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-highlight-3-face
  '((((class color) (background light))
     :background "gray")
    (((class color) (background dark))
     :background "gray" :foreground "#191919")
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-highlight-4-face
  '((((class color) (background light))
     :background "LightSkyBlue")
    (((class color) (background dark))
     :background "LightSkyBlue" :foreground "#191919")
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-underline-1-face
  '((((class color) (background light))
     :underline "Blue1")
    (((class color) (background dark))
     :underline "LightSkyBlue")
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-underline-2-face
  '((((class color) (background light))
     :underline "Green3")
    (((class color) (background dark))
     :underline "Green")
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-underline-3-face
  '((((class color) (background light))
     :underline "red")
    (((class color) (background dark))
     :underline "red")
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-underline-4-face
  '((((supports :underline (:style wave)) (background light))
     :underline (:style wave :color "Black"))
    (((supports :underline (:style wave)) (background dark))
     :underline (:style wave :color "LightSkyBlue"))
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-underline-5-face
  '((((supports :underline (:style wave)) (background light))
     :underline (:style dots :color "Black"))
    (((supports :underline (:style wave)) (background dark))
     :underline (:style dots :color "LightSkyBlue"))
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-underline-6-face
  '((((supports :underline (:style line)) (background light))
     :underline (:style dashes :color "Black"))
    (((supports :underline (:style line)) (background dark))
     :underline (:style dashes :color "LightSkyBlue"))
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)

(defface paw-comment-face
  '((((class color) (background light) (supports :underline (:style line)))
     :background "pale goldenrod"
     :underline (:style double-line :color "#ffe895"))
    (((class color) (background dark) (supports :underline (:style line)))
     :background "lightgoldenrodyellow" :foreground "#191919"
     :underline (:style line :color "#6c572b"))
    (t :inherit default))
  "Face used for overlay."
  :group 'paw-faces)


(define-obsolete-face-alias 'paw-overlay-face-1
  'paw-highlight-1-face "paw 1.0.0")

(define-obsolete-face-alias 'paw-overlay-face-2
  'paw-highlight-2-face "paw 1.0.0")

(define-obsolete-face-alias 'paw-overlay-face-3
  'paw-highlight-3-face "paw 1.0.0")

(define-obsolete-face-alias 'paw-overlay-face-4
  'paw-highlight-4-face "paw 1.0.0")

(defface paw-mouse-face '((t :inherit mode-line-highlight))
  "Face used for *paw-search* mouse face"
  :group 'paw-faces)


(defface paw-no-notes-exist-face
  '((t
     :foreground "chocolate"
     :weight bold))
  "Face for modeline note count, when 0."
  :group 'paw)

(defface paw-notes-exist-face
  '((((class color) (background light))
     :foreground "ForestGreen"
     :weight bold)
    (((class color) (background dark))
     :foreground "SpringGreen"
     :weight bold)
    (t :inherit default))
  "Face for modeline note count, when not 0."
  :group 'paw)

(defface paw-mark-face '((t :inherit highlight))
  "Face for the mark candidate."
  :group 'paw-faces)

(defface paw-current-page-button-face
  '((((class color) (background light))
     (:weight bold
      :height 1.1))
    (((class color) (background dark))
     (:weight bold
      :height 1.1))
    (t (:inherit default)))
  "Face used for current page button"
  :group 'paw-faces)

(defface paw-inline-date-face
  '((((class color) (background light))
     :foreground "#8b94a5")
    (((class color) (background dark))
     :foreground "#EBCB8B")
    (t :inherit default))
  "Face for the inline date overlay."
  :group 'paw-faces)

(defface paw-inline-word-face
  '((((class color) (background light))
     :foreground "#1b180f" :background "gold2" :height 1.0)
    (((class color) (background dark))
     :foreground "#1b180f" :background "gold2" :height 1.0)
    (t :inherit default))
  "Face used for online word overlay."
  :group 'paw-faces)

(defface paw-inline-exp-face
  '((t :inherit default))
  "Face used for inline exp."
  :group 'paw-faces)

(defface paw-inline-note-face
  '((t :inherit link))
  "Face used for inline note overlay."
  :group 'paw-faces)

(provide 'paw-eink-faces)
