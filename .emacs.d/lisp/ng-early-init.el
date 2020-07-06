;; -*- lexical-binding: t; -*-

;; Prevent Emacs from making #auto-save#, .#lock and backup~ files all
;; over the bloody place. We do this first to avoid making a mess when
;; hacking on a broken init.
;;
;; https://www.emacswiki.org/emacs/BackupDirectory
;;
;; auto-save and backups are nice, but keep them out of sight
(progn
  (defvar ng/auto-save-dir  "~/.emacs.d/saves/")
  (make-directory ng/auto-save-dir t)
  (setq
   auto-save-file-name-transforms `((".*" ,ng/auto-save-dir t))
   backup-directory-alist         `(("." .  ,ng/auto-save-dir))
   backup-by-copying              t
   delete-old-versions            t
   kept-new-versions              5
   kept-old-versions              0
   version-control                t
   ))

;; but lockfiles are terrible and can't be moved, so disable them
(setq create-lockfiles nil)

;; NOTE: package archives must be defined in the early config file
(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ;;below are currently unused, disable to speed things up
        ;;("gnu"        . "https://elpa.gnu.org/packages/")
        ;;("melpa"      . "https://melpa.org/packages/")
        )
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu"        . 5)
        ("melpa"      . 0)))

;; If there's one thing I can't stand, it's hitting C-z reflexively to
;; undo and having it minimize my window. Make sure it's never bound
;; to that even if cua-mode is off. Do this early too to avoid
;; screaming when trying to fix a failed init.
(global-set-key (kbd "C-z") nil)

;; Also do UI changes early so that there isn't a jarring change from
;; the default UI to the customized one during load.

;; hide menu bar and tool bar
(progn
  (menu-bar-mode -1)
  (tool-bar-mode -1))

;; color theme: atom one dark with a few customizations for contrast
(progn
  (setq atom-one-dark-colors-alist
        '(("atom-one-dark-accent"   . "#528BFF")
          ("atom-one-dark-fg"       . "#ABB2BF")
          ("atom-one-dark-bg"       . "#282C34")
          ("atom-one-dark-bg-1"     . "#121417")
          ("atom-one-dark-bg-hl"    . "#2C323C")
          ("atom-one-dark-gutter"   . "#4B5363")
          ("atom-one-dark-mono-1"   . "#ABB2BF")
          ("atom-one-dark-mono-2"   . "#828997")
          ("atom-one-dark-mono-3"   . "#8d8c8C")
          ("atom-one-dark-cyan"     . "#56B6C2")
          ("atom-one-dark-blue"     . "#61AFEF")
          ("atom-one-dark-purple"   . "#C678DD")
          ("atom-one-dark-green"    . "#98C379")
          ("atom-one-dark-red-1"    . "#E06C75")
          ("atom-one-dark-red-2"    . "#BE5046")
          ("atom-one-dark-orange-1" . "#D19A66")
          ("atom-one-dark-orange-2" . "#E5C07B")
          ("atom-one-dark-gray"     . "#3E4451")
          ("atom-one-dark-silver"   . "#9DA5B4")
          ("atom-one-dark-black"    . "#21252B")
          ("atom-one-dark-border"   . "#181A1F")))
  (load-theme 'atom-one-dark t))

;; set font
(when window-system
  (defun ng/try-set-font (font)
  "If the given font is found, sets it as the font for the
current frame and all future frames."
    (if (find-font (font-spec :name font))
        (progn
          (add-to-list 'default-frame-alist `(font . ,font))
          (set-frame-font font)
          t)
      nil))
  (or
   (ng/try-set-font "Cascadia Mono-12")
   (ng/try-set-font "Consolas-12")
   (ng/try-set-font "DejaVu Sans Mono-12")))
