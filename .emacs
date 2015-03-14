;; use common lisp extensions below
(eval-when-compile (require 'cl))

;; disable startup message
(setq inhibit-startup-message t)

;; no beeping
(setq visible-bell t)

;; leave the scratch buffer empty by default
(setq initial-scratch-message nil)

;; never use tabs to indent
(setq-default indent-tabs-mode nil)

;; follow symlinks to source-controlled locations without prompting
(setq vc-follow-symlinks t)

;; no menu bar on console
(unless window-system
  (menu-bar-mode -1))

;; no tool bar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; highlight matching parens
(show-paren-mode 1)

;; show line number in mode line
(line-number-mode 1)

;; show column number in mode line
(column-number-mode 1)

;; no cursor blinking
(blink-cursor-mode -1)

;; make links in text clickable
(if (fboundp 'goto-address-mode)
    (add-hook 'find-file-hook 'goto-address-mode))

;; triple productivity Homer Simpson style :)
(defalias 'yes-or-no-p 'y-or-n-p)

;; see usage  below -- saves on parentheses :)
(defmacro ng/dopairs (names pairs body)
  "Process list in pairs."
  (declare (indent 2))
  `(lexical-let ((__p ,pairs))
     (while __p
       (lexical-let ((,(car  names) (car  __p))
                     (,(cadr names) (cadr __p)))
         ,body
         (setq __p (cddr __p))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELPA Packages

;; require-package and maybe-require-package by Steve Purcell
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-elpa.el

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install package `%s': %S" package err)
     nil)))

(require 'package nil t)

;; additional elpa repositories
(setq
 ng/package-archives
 '(
   "marmalade"  "http://marmalade-repo.org/packages/"
   "melpa"      "http://melpa.org/packages/"
   ))


;; packages to install
(setq
 ng/packages
 '(
   ace-jump-mode
   csharp-mode
   expand-region
   flx-ido
   ido-vertical-mode
   markdown-mode
   magit
   multiple-cursors
   projectile
   smex
   solarized-theme
   ))

;; if the package feature is available, install the packages above,
;; otherwise stub-out package-installed-p to always return nil.
(when (featurep 'package)
  (ng/dopairs (name url) ng/package-archives
    (add-to-list 'package-archives (cons name url) t))
  (package-initialize)
  (dolist (package ng/packages)
    (require-package package)))

(unless (fboundp 'package-installed-p)
  (defun package-installed-p (p) nil))

;; ido-mode: better interactive completion
;; http://emacswiki.org/InteractivelyDoThings
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

;; make ido fuzzy match like sublime
;; https://github.com/lewang/flx
(when (fboundp 'flx-ido-mode)
  (flx-ido-mode 1)
  (setq ido-use-faces nil))

;; make ido vertical
;; https://github.com/gempesaw/ido-vertical-mode.el
(when (fboundp 'ido-vertical-mode)
  (ido-vertical-mode 1))

;; project interaction
;; https://github.com/bbatsov/projectile
(when (fboundp 'projectile-global-mode)
  (projectile-global-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme

(when window-system
  (when (package-installed-p 'solarized-theme)
    (setq solarized-contrast 'high)
    (load-theme 'solarized-dark t))
  (when (find-font (font-spec :name "Consolas"))
      (add-to-list 'default-frame-alist '(font . "Consolas-14")))
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto Save (without polluting file system)
;; http://www.emacswiki.org/emacs/BackupDirectory

(setq
 backup-directory-alist   '(("." . "~/.saves"))   
 backup-by-copying        t
 delete-old-versions      t
 kept-new-versions        5
 kept-old-versions        0
 version-control          t
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth scrolling
;; http://www.emacswiki.org/SmoothScrolling

(setq
 mouse-wheel-scroll-amount        '(1 ((shift) . 1)) 
 mouse-wheel-progressive-speed    nil
 mouse-wheel-follow-mouse         t            
 scroll-step                      1
 scroll-preserve-screen-position  1
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++/C#/Java

(add-hook
 'c-mode-common-hook
 (lambda ()
   (c-set-style "bsd")
   (c-set-offset 'case-label '+)
   (setq c-basic-offset 4)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic modes including *.bat, *.cmd, *.ini, and *.reg
;; http://emacswiki.org/emacs/GenericMode

(require 'generic-x)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perl

;; http://www.emacswiki.org/emacs/CPerlMode
(defalias 'perl-mode 'cperl-mode)

(add-hook
 'cperl-mode-hook
 (lambda ()
   (setq cperl-indent-level 4)
   (setq cperl-brace-offset (- cperl-indent-level))
   (setq cperl-invalid-face nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

;; cua-mode: C-x/C-c/C-v/C-z for cut/copy/paste/undo without
;;           interfering too much with standard emacs key
;;           bindings. Sometimes my fingers get confused. :)
;;
;; http://www.emacswiki.org/CuaMode
(cua-mode 1)
(setq cua-auto-tabify-rectangles nil)
(setq cua-keep-region-after-copy t)

;; helper to global-set-key in pairs with fewer parens
(defmacro ng/set-keys (&rest pairs)
  (ng/dopairs (key function) pairs
    (global-set-key (eval `(kbd ,key)) function)))

;; Coming up with non-standard keybindings is tough because they
;; inevitably conflict with other useful things or are too cumbersome
;; to type. I borrow a trick from cua-mode and make standard keys take
;; on new meaning based on whether or not there is an active
;; region. It also helps to choose good mnemonics.

(ng/set-keys
 "C-m"      newline-and-indent
 "C-k"      ng/kill-region-or-kill-line
 "C-w"      ng/kill-region-or-backward-kill-word
 "C-x C-b"  ibuffer
 "M-n"      cua-scroll-up
 "M-p"      cua-scroll-down
 "<M-f4>"   ng/delete-frame-or-exit
 "C-x C-k"  ng/kill-other-buffer-and-window
 "C-x k"    ng/kill-this-buffer-and-window
 )

(if (package-installed-p 'multiple-cursors)
    (ng/set-keys
     "C-a" ng/move-or-mc-edit-beginning-of-line
     "C-e" ng/move-or-mc-edit-end-of-line
     "C-s" ng/isearch-forward-or-mc-mark-all-in-region
     "M-m" ng/move-or-mc-edit-back-to-indentation     
     ))

(if (package-installed-p 'expand-region)
    (ng/set-keys "C-," er/expand-region))

(if (package-installed-p 'ace-jump-mode)
    (ng/set-keys "C-j" ace-jump-mode))

(if (package-installed-p 'smex)
    (ng/set-keys "M-x" smex))

;; For older versions of emacs
(unless (fboundp 'use-region-p)
  (defun use-region-p ()
    (and transient-mark-mode
         mark-active
         (> (region-end) (region-begninng)))))

;; Designed to be bound to C-a
(defun ng/move-or-mc-edit-beginning-of-line (&optional arg)
  "If there's an active region, create cursors at the end of each
line in the region. Otherwise, move to the end of the current line."
  (interactive "p")
  (if (use-region-p) (mc/edit-lines))
  (move-beginning-of-line arg))

;; Designed to be bound to C-e
(defun ng/move-or-mc-edit-end-of-line (&optional arg)
  "If there's an active region, create cursors at the end of each
line in the region. Otherwise, move to the end of the current line."
  (interactive "p")
  (if (use-region-p) (mc/edit-lines))
  (move-end-of-line arg))

;; Designed to be bound to M-m
(defun ng/move-or-mc-edit-back-to-indentation (&optional arg)
  (interactive "p")
  (if (use-region-p) (mc/edit-lines))
  (back-to-indentation))

;; Designed to be bound to C-s
(defun ng/isearch-forward-or-mc-mark-all-in-region ()
  "If there's an active region, create cursors based on the
interactive search term. Otherwise, interactively search
forward."
  (interactive)
  (if (use-region-p)
      (mc/mark-all-in-region (region-beginning) (region-end))
    (isearch-forward)))

;; Designed to be bound to Alt-F4 to match standard Windows behaviour
(defun ng/delete-frame-or-exit ()
  "Delete the current frame. Exit if it is the only frame."
  (interactive)
  (if multiple-frames
      (delete-frame)
    (save-buffers-kill-terminal)))

;; Designed to be bound to C-w -- it keeps its standard behaviour when
;; there's an active region, it erases the previous word like it does
;; in other UNIX programs.
(defun ng/kill-region-or-backward-kill-word (&optional arg)
  "If there is an active region, kill it. Otherwise, kill the preceding word."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

;; Designed to be bound to C-k -- it keeps its standard behaviour when
;; there's no active region, but becomes an alias for kill-region when
;; there is. This strengthens the k == kill, y == yank mnemonic in my
;; mind.
(defun ng/kill-region-or-kill-line (&optional arg)
  "If there is an active region, kill it. Otherwise, kill the following line."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-line arg)))

;; Designed to be bound to C-x k -- similar to standard behacvior, but
;; kills current buffer without prompting and deletes the window if
;; it's not the only one. Note that I use ibuffer to kill anything but
;; the current buffer.
(defun ng/kill-this-buffer-and-window ()
  "Kill the current buffer and delete its window."
  (interactive)
  (lexical-let ((window (selected-window))
                (buffer (current-buffer)))
    (if (window-parent window)
        (delete-window window))
    (kill-buffer buffer)))

;; I bind this to C-x C-k -- it's for those times when another window
;; has popped up and you want it to just go away and keep working in
;; the main window.
(defun ng/kill-other-buffer-and-window ()
  "Kill the buffer in the other window and delete its window."
  (interactive)
  (lexical-let ((old-window (selected-window))
                (old-buffer (current-buffer)))
    (other-window 1)
    (lexical-let ((new-window (selected-window))
                  (new-buffer (current-buffer)))
      (unless (eq new-buffer old-buffer)
        (kill-buffer new-buffer))
      (unless (eq new-window old-window)
        (delete-window new-window)))))

