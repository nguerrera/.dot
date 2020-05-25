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
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu"          . 5)
        ("melpa"        . 0)))

;; If there's one thing I can't stand, it's hitting C-z reflexively to
;; undo and having it minimize my window. Make sure it's never bound
;; to that even if cua-mode is off. Do this early too to avoid screaming
;; when trying to fix a failed init.
(global-set-key (kbd "C-z") nil)

;; Also do UI changes early so that there isn't a jarring change from the
;; default UI to the customized one
;; hide menu bar and tool bar
(progn
  (menu-bar-mode -1)
  (tool-bar-mode -1))

;; set font
(defun ng/try-set-font (font)
  "If the given font is found, sets it as the font for the
current frame and all future frames."
  (when (find-font (font-spec :name font))
    (add-to-list 'default-frame-alist `(font . ,font))
    (set-frame-font font)))

(when window-system
  (or (ng/try-set-font "Consolas-11")
      (ng/try-set-font "DejaVu Sans Mono-11.5")))

