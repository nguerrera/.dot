;;; ng-atom-one-dark-theme.el --- Modified Atom One Dark color theme

;; Copyright 2015-2019 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/atom-one-dark-theme
;; Version: 0.4.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs port of the Atom One Dark theme from Atom.io.

;; Modified by Nick Guerrera <nick@technogenic.net>:
;;
;;  * adjust colors for contrast and personal preference
;;  * degrade gracefully to terminals with fewer colors
;;  * use fewer colors to make this degrading easier
;;  * delete blocks for packages I don't use since I probably broke them anyway
;;  * rename package, adding ng prefix
;;  * use ansi color names throughout for easier cross-reference with terminal theme

;;; Code:

(deftheme ng-atom-one-dark
  "NG Atom One Dark - Heavily modified Emacs port of the Atom One Dark theme from Atom.io.")

(defvar ng-atom-one-dark-colors
  '(
    (background    "#282c34")
    (foreground    "#abb2bf")
    (black         "#282c34")
    (blue          "#61afef")
    (cyan          "#56b6c2")
    (green         "#98c379")
    (magenta       "#c678dd")
    (red           "#e06c75")
    (white         "#abb2bf")
    (yellow        "#e5c07b")
    (brightblack   "#8d8d8d")
    (brightblue    "#8ac1ee")
    (brightcyan    "#8eb7bd")
    (brightgreen   "#a9ce8f")
    (brightmagenta "#da9dec")
    (brightred     "#e0606b")
    (brightwhite   "#dcdfe4")
    (brightyellow  "#f3daab")
    (darkblack     "#21252b")))

;; Adjust colors based on available colors. If we have 256 or fewer in 2020+,
;; we're in a terminal where hopefully we've got a matching theme set up so
;; we can reference the colors by ASCII name.
(setq ng--safe-colors
  (let* ((color-count
          (display-color-cells (selected-frame)))
         (basic-colors
          '((black   "black")
            (red     "red")
            (green   "green")
            (yellow  "yellow")
            (blue    "blue")
            (magenta "magenta")
            (cyan    "cyan"))))
       (cond ((> color-count 256)
              ng-atom-one-dark-colors)
             ((= color-count 256)
              `( ,@basic-colors
                (white         "white")
                (brightblack   "brightblack")
                (brightred     "brightred")
                (brightgreen   "brightgreen")
                (brightyellow  "brightyellow")
                (brightblue    "brightblue")
                (brightmagenta "brightmagenta")
                (brightcyan    "brightcyan")
                (brightwhite   "brightwhite")
                (darkblack     "black")))
             ((>= color-count 16)
              `( ,@basic-colors
                 (white         "lightgray")
                 (brightblack   "darkgray")
                 (brightred     "lighttred")
                 (brightgreen   "lightgreen")
                 (brightyellow  "lighttyellow")
                 (brightblue    "lightblue")
                 (brightmagenta "lightmagenta")
                 (brightcyan    "lighttcyan")
                 (brightwhite   "white")
                 (darkblack     "black")))
             (t
              `( ,@basic-colors
                 (white         "white")
                 (brightblack   "black")
                 (brightred     "red")
                 (brightgreen   "green")
                 (brightyellow  "yellow")
                 (brightblue    "blue")
                 (brightmagenta "magenta")
                 (brightcyan    "cyan")
                 (brightwhite   "white"))))))

(defmacro ng-atom-one-dark-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let* (,@ng--safe-colors) ,@body))

(ng-atom-one-dark-with-color-variables
 (custom-theme-set-faces
  'ng-atom-one-dark

  `(default ((t (:foreground ,white :background ,black))))
  `(success ((t (:foreground ,green))))
  `(warning ((t (:foreground ,yellow))))
  `(error ((t (:foreground ,red :weight bold))))
  `(link ((t (:foreground ,blue :underline t :weight bold))))
  `(link-visited ((t (:foreground ,blue :underline t :weight normal))))
  `(cursor ((t (:background ,white))))
  `(fringe ((t (:background ,black))))
  `(highlight ((t (:background ,magenta :foreground ,black))))
  `(region ((t (:background ,white :foreground ,black))))
  `(vertical-border ((t (:foreground ,white))))
  `(secondary-selection ((t (:background ,blue :foreground ,black))))
  `(query-replace ((t (:inherit (isearch)))))
  `(minibuffer-prompt ((t (:foreground ,white))))
  `(font-lock-builtin-face ((t (:foreground ,cyan))))
  `(font-lock-comment-face ((t (:foreground ,brightblack :slant italic))))
  `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
  `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
  `(font-lock-function-name-face ((t (:foreground ,blue))))
  `(font-lock-keyword-face ((t (:foreground ,magenta :weight normal))))
  `(font-lock-preprocessor-face ((t (:foreground ,brightblack))))
  `(font-lock-string-face ((t (:foreground ,green))))
  `(font-lock-type-face ((t (:foreground ,yellow))))
  `(font-lock-constant-face ((t (:foreground ,brightcyan))))
  `(font-lock-variable-name-face ((t (:foreground ,cyan))))
  `(font-lock-warning-face ((t (:foreground ,brightblack :bold t))))
  `(font-lock-negation-char-face ((t (:foreground ,cyan :bold t))))

  ;; cua-rectangle
  `(cua-rectangle ((t (:foreground ,black :background ,white))))

  ;; mode-line
  `(mode-line ((t (:background ,darkblack :foreground ,white :box (:color ,darkblack :line-width 1)))))
  `(mode-line-buffer-id ((t (:weight bold))))
  `(mode-line-emphasis ((t (:weight bold))))
  `(mode-line-inactive ((t (:background ,darkblack :foreground ,white :box (:color ,black :line-width 1)))))

  ;; custom
  `(custom-state ((t (:foreground ,green))))

  ;; ido
  `(ido-first-match ((t (:foreground ,magenta :weight bold))))
  `(ido-only-match ((t (:foreground ,yellow :weight bold))))
  `(ido-subdir ((t (:foreground ,blue))))
  `(ido-virtual ((t (:foreground ,brightblack))))

  ;; ace-jump
  `(ace-jump-face-background ((t (:foreground ,brightblack :background ,black :inverse-video nil))))
  `(ace-jump-face-foreground ((t (:foreground ,red :background ,black :inverse-video nil))))

  ;; isearch
  `(isearch ((t (:foreground ,black :background ,magenta))))
  `(isearch-fail ((t (:foreground ,brightred :background nil))))
  `(lazy-highlight ((t (:foreground ,magenta :background ,black :underline ,magenta))))

  ;; dired-mode
  '(dired-directory ((t (:inherit (font-lock-function-name-face)))))
  '(dired-flagged ((t (:inherit (font-lock-type-delete)))))
  '(dired-symlink ((t (:inherit (font-lock-constant-face)))))

  ;; git-commit
  `(git-commit-comment-action  ((t (:foreground ,green :weight bold))))
  `(git-commit-comment-branch  ((t (:foreground ,blue :weight bold))))
  `(git-commit-comment-heading ((t (:foreground ,yellow :weight bold))))

  ;; magit
  `(magit-section-highlight ((t)))
  `(magit-section-heading ((t (:foreground ,yellow :weight bold))))
  `(magit-section-heading-selection ((t (:weight bold))))
  `(magit-diff-file-heading ((t (:foreground ,white))))
  `(magit-diff-file-heading-highlight ((t)))
  `(magit-diff-file-heading-selection ((t (:foreground ,yellow))))
  `(magit-diff-hunk-heading ((t (:foreground ,white))))
  `(magit-diff-hunk-heading-highlight ((t)))
  `(magit-diff-hunk-heading-selection ((t)))
  `(magit-diff-context ((t (:foreground ,white))))
  `(magit-diff-context-highlight ((t)))
  `(magit-diff-added ((t (:foreground ,green))))
  `(magit-diff-removed ((t (:foreground ,red))))
  `(magit-diff-added-highlight ((t (:foreground ,green))))
  `(magit-diff-removed-highlight ((t (:foreground ,red))))
  `(magit-diffstat-added ((t (:foreground ,green))))
  `(magit-diffstat-removed ((t (:foreground ,red))))
  `(magit-process-ok ((t (:foreground ,green))))
  `(magit-process-ng ((t (:foreground ,red))))
  `(magit-log-author ((t (:foreground ,yellow))))
  `(magit-log-date ((t (:foreground ,brightblack))))
  `(magit-log-graph ((t (:foreground ,white))))
  `(magit-sequence-pick ((t (:foreground ,yellow))))
  `(magit-sequence-stop ((t (:foreground ,green))))
  `(magit-sequence-part ((t (:foreground ,brightyellow))))
  `(magit-sequence-head ((t (:foreground ,blue))))
  `(magit-sequence-drop ((t (:foreground ,red))))
  `(magit-sequence-done ((t (:foreground ,brightblack))))
  `(magit-sequence-onto ((t (:foreground ,brightblack))))
  `(magit-bisect-good ((t (:foreground ,green))))
  `(magit-bisect-skip ((t (:foreground ,brightyellow))))
  `(magit-bisect-bad ((t (:foreground ,red))))
  `(magit-blame-heading ((t (:background ,darkblack :foreground ,brightblack))))
  `(magit-blame-hash ((t (:background ,darkblack :foreground ,magenta))))
  `(magit-blame-name ((t (:background ,darkblack :foreground ,yellow))))
  `(magit-blame-date ((t (:background ,darkblack :foreground ,brightblack))))
  `(magit-blame-summary ((t (:background ,darkblack :foreground ,brightblack))))
  `(magit-dimmed ((t (:foreground ,brightblack))))
  `(magit-hash ((t (:foreground ,magenta))))
  `(magit-tag  ((t (:foreground ,brightyellow :weight bold))))
  `(magit-branch-remote  ((t (:foreground ,green :weight bold))))
  `(magit-branch-local   ((t (:foreground ,blue :weight bold))))
  `(magit-branch-current ((t (:foreground ,blue :weight bold :box t))))
  `(magit-head           ((t (:foreground ,blue :weight bold))))
  `(magit-refname        ((t (:background ,black :foreground ,white :weight bold))))
  `(magit-refname-stash  ((t (:background ,black :foreground ,white :weight bold))))
  `(magit-refname-wip    ((t (:background ,black :foreground ,white :weight bold))))
  `(magit-signature-good      ((t (:foreground ,green))))
  `(magit-signature-bad       ((t (:foreground ,red))))
  `(magit-signature-untrusted ((t (:foreground ,brightyellow))))
  `(magit-cherry-unmatched    ((t (:foreground ,cyan))))
  `(magit-cherry-equivalent   ((t (:foreground ,magenta))))
  `(magit-reflog-commit       ((t (:foreground ,green))))
  `(magit-reflog-amend        ((t (:foreground ,magenta))))
  `(magit-reflog-merge        ((t (:foreground ,green))))
  `(magit-reflog-checkout     ((t (:foreground ,blue))))
  `(magit-reflog-reset        ((t (:foreground ,red))))
  `(magit-reflog-rebase       ((t (:foreground ,magenta))))
  `(magit-reflog-cherry-pick  ((t (:foreground ,green))))
  `(magit-reflog-remote       ((t (:foreground ,cyan))))
  `(magit-reflog-other        ((t (:foreground ,cyan))))

  ;; rainbow-delimiters
  `(rainbow-delimiters-depth-1-face ((t (:foreground ,brightblack))))
  `(rainbow-delimiters-unmatched-face ((t (:foreground ,red :weight bold))))
  `(rainbow-delimiters-mismatched-face ((t (:foreground ,red :weight bold))))

  ;; show-paren
  `(show-paren-match ((t (:foreground ,cyan :inherit bold))))
  `(show-paren-mismatch ((t (:foreground ,red :inherit bold))))

  ;; nxml
  `(nxml-attribute-local-name ((t (:foreground ,brightyellow))))
  `(nxml-element-local-name ((t (:foreground ,red))))
  `(nxml-markup-declaration-delimiter ((t (:inherit (font-lock-comment-face nxml-delimiter)))))
  `(nxml-processing-instruction-delimiter ((t (:inherit nxml-markup-declaration-delimiter))))

  ;; flx-ido
  `(flx-highlight-face ((t (:inherit (link) :weight bold))))

  ;; term
  `(term-color-black ((t (:foreground ,black))))
  `(term-color-blue ((t (:foreground ,blue))))
  `(term-color-cyan ((t (:foreground ,cyan))))
  `(term-color-green ((t (:foreground ,green))))
  `(term-color-magenta ((t (:foreground ,magenta))))
  `(term-color-red ((t (:foreground ,red))))
  `(term-color-white ((t (:foreground ,white))))
  `(term-color-yellow ((t (:foreground ,yellow))))

  ;; org-mode
  `(org-date ((t (:foreground ,cyan))))
  `(org-document-info ((t (:foreground ,brightblack))))
  `(org-document-info-keyword ((t (:inherit org-meta-line :underline t))))
  `(org-document-title ((t (:weight bold))))
  `(org-footnote ((t (:foreground ,cyan))))
  `(org-sexp-date ((t (:foreground ,cyan))))

  ;; undo-tree
  `(undo-tree-visualizer-current-face ((t (:foreground ,red))))
  `(undo-tree-visualizer-register-face ((t (:foreground ,brightyellow))))
  `(undo-tree-visualizer-unmodified-face ((t (:foreground ,cyan))))
  ))

(provide-theme 'ng-atom-one-dark)

