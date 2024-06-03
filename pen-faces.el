;;; pen/pen-faces.el -*- lexical-binding: t; -*-

(defface pen-note-header-title-face
  '((((class color) (background light))
     :foreground "#4F894C"
     :weight bold
     :height 1.1)
    (((class color) (background dark))
     :foreground "#A3BE8C"
     :weight bold
     :height 1.1)
    (t :inherit default))
  "Face used for *pen-note* header tilte face"
  :group 'pen-faces)

(defface pen-note-header-title-path-face
  '((((class color) (background light))
     :foreground "#3B6EA8"
     :height 0.7)
    (((class color) (background dark))
     :foreground "#d9c6d6"
     :height 0.7)
    (t :inherit default))
  "Face used for author."
  :group 'pen-faces)


(defface pen-view-note-title-face '((t :inherit default :height 2.0))
  "Face used for title on compact view."
  :group 'pen-faces)

(defface pen-date-face
  '((((class color) (background light))
     :foreground "#29838D")
    (((class color) (background dark))
     :foreground "#8FBCBB")
    (t :inherit default))
  "Face for the date (last_modified)."
  :group 'pen-faces)

(defface pen-path-face
  '((((class color) (background light))
     :foreground "#3B6EA8")
    (((class color) (background dark))
     :foreground "#d9c6d6")
    (t :inherit default))
  "Face used for author."
  :group 'pen-faces)

(defface pen-type-face
  '((((class color) (background light))
     :foreground "#29838D")
    (((class color) (background dark))
     :foreground "#8FBCBB")
    (t :inherit default))
  "Face for the file path."
  :group 'pen-faces)

(defface pen-wallabag-face
  '((((class color) (background light))
     :foreground "#4F894C")
    (((class color) (background dark))
     :foreground "#A3BE8C")
    (t :inherit default))
  "Face used for format."
  :group 'pen-faces)

(defface pen-nov-face
  '((((class color) (background light))
     :foreground "MediumSlateBlue")
    (((class color) (background dark))
     :foreground "cyan")
    (t :inherit default))
  "Face used for hightlight."
  :group 'pen-faces)

(defface pen-pdf-face
  '((((class color) (background light))
     :foreground "#8b94a5")
    (((class color) (background dark))
     :foreground "#EBCB8B")
    (t :inherit default))
  "Face used for tag."
  :group 'pen-faces)

(defface pen-file-face
  '((((class color) (background light))
     :foreground "#29838D")
    (((class color) (background dark))
     :foreground "#8FBCBB")
    (t :inherit default))
  "Face for the date (last_modified)."
  :group 'pen-faces)

(defface pen-offline-face
  '((((class color) (background light))
     :foreground "#4F894C")
    (((class color) (background dark))
     :foreground "#A3BE8C")
    (t :inherit default))
  "Face used for format."
  :group 'pen-faces)

(defface pen-word-face
  '((((class color) (background light))
     :foreground "#1b180f" :background "#ffe895" :height 1.0)
    (((class color) (background dark))
     :foreground "#e9bb43" :background "#6c572b" :height 1.0)
    (t :inherit default))
  "Face used for overlay."
  :group 'pen-faces)

(defface pen-word-hover-face
  '((((class color) (background light))
     :foreground "#1b180f" :background "#ffe895" :height 1.0 :box (:line-width 2 :color "#424242"))
    (((class color) (background dark))
     :foreground "#e9bb43" :background "#6c572b" :height 1.0 :box (:line-width 2 :color "#E4E4E4"))
    (t :inherit default))
  "Face used for overlay."
  :group 'pen-faces)

(defface pen-image-face
  '((((class color) (background light))
     :foreground "blue" :height 1.0)
    (((class color) (background dark))
     :foreground "LightBlue" :height 1.0)
    (t :inherit default))
  "Face used for overlay."
  :group 'pen-faces)

(defface pen-bookmark-face
  '((((class color) (background light))
     :foreground "blue" :height 1.0)
    (((class color) (background dark))
     :foreground "LightBlue" :height 1.0)
    (t :inherit default))
  "Face used for overlay."
  :group 'pen-faces)

(defface pen-question-face
  '((((class color) (background light))
     :foreground "blue" :height 1.0)
    (((class color) (background dark))
     :foreground "LightBlue" :height 1.0)
    (t :inherit default))
  "Face used for overlay."
  :group 'pen-faces)

(defface pen-todo-face
  '((t :inherit org-todo))
  "Face used for overlay."
  :group 'pen-faces)

(defface pen-done-face
  '((t :inherit org-done))
  "Face used for overlay."
  :group 'pen-faces)

(defface pen-cancel-face
  '((t :inherit org-done :strike-through t))
  "Face used for overlay."
  :group 'pen-faces)

(defface pen-link-face
  '((t :inherit link :foreground "#2a9d8f"))
  "Face used for overlay."
  :group 'pen-faces)

(defface pen-attachment-face
  '((((class color) (background light))
     :foreground "blue" :height 1.0)
    (((class color) (background dark))
     :foreground "LightBlue" :height 1.0)
    (t :inherit default))
  "Face used for overlay."
  :group 'pen-faces)

(defface pen-highlight-1-face
  '((((class color) (background light))
     :background "lightgoldenrodyellow")
    (((class color) (background dark))
     :background "lightgoldenrodyellow" :foreground "#191919")
    (t :inherit default))
  "Face used for overlay."
  :group 'pen-faces)

(defface pen-highlight-2-face
  '((((class color) (background light))
     :background "MediumSpringGreen")
    (((class color) (background dark))
     :background "MediumSpringGreen" :foreground "#191919")
    (t :inherit default))
  "Face used for overlay."
  :group 'pen-faces)

(defface pen-highlight-3-face
  '((((class color) (background light))
     :background "gray")
    (((class color) (background dark))
     :background "gray" :foreground "#191919")
    (t :inherit default))
  "Face used for overlay."
  :group 'pen-faces)

(defface pen-highlight-4-face
  '((((class color) (background light))
     :background "LightSkyBlue")
    (((class color) (background dark))
     :background "LightSkyBlue" :foreground "#191919")
    (t :inherit default))
  "Face used for overlay."
  :group 'pen-faces)

(define-obsolete-face-alias 'pen-overlay-face-1
  'pen-highlight-1-face "pen 1.0.0")

(define-obsolete-face-alias 'pen-overlay-face-2
  'pen-highlight-2-face "pen 1.0.0")

(define-obsolete-face-alias 'pen-overlay-face-3
  'pen-highlight-3-face "pen 1.0.0")

(define-obsolete-face-alias 'pen-overlay-face-4
  'pen-highlight-4-face "pen 1.0.0")

(defface pen-mouse-face '((t :inherit mode-line-highlight))
  "Face used for *pen-search* mouse face"
  :group 'pen-faces)


(defface pen-no-notes-exist-face
  '((t
     :foreground "chocolate"
     :weight bold))
  "Face for modeline note count, when 0."
  :group 'pen)

(defface pen-notes-exist-face
  '((((class color) (background light))
     :foreground "ForestGreen"
     :weight bold)
    (((class color) (background dark))
     :foreground "SpringGreen"
     :weight bold)
    (t :inherit default))
  "Face for modeline note count, when not 0."
  :group 'pen)

(defface pen-mark-face '((t :inherit highlight))
  "Face for the mark candidate."
  :group 'pen-faces)

(provide 'pen-faces)
