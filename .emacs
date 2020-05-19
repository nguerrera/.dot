;; -*- lexical-binding: t; -*-

;; start server for emacsclient
(when window-system
  (condition-case nil
      (let ((warning-minimum-level :error))
        (server-start))
    (error nil)))

;; go ahead and kill clients without prompting
(remove-hook 'kill-buffer-query-functions
             'server-kill-buffer-query-function)

;; disable startup message
(setq inhibit-startup-message t)

;; no beeping
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; leave the scratch buffer empty by default
(setq initial-scratch-message nil)

;; never use tabs to indent
(setq-default indent-tabs-mode nil)

;; truncate long lines, don't wrap
(setq-default truncate-lines t)

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

;; triple productivity, Homer Simpson style :)
(defalias 'yes-or-no-p 'y-or-n-p)

;; see usage below -- saves on parentheses :)
(defmacro ng/dopairs (names pairs body)
  "Process list in pairs."
  (declare (indent 2))
  `(let ((__p ,pairs))
     (while __p
       (let ((,(car  names) (car  __p))
             (,(cadr names) (cadr __p)))
         ,body
         (setq __p (cddr __p))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELPA Packages

;; require-package by Steve Purcell
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

(require 'package nil t)

;; additional elpa repositories
(setq
 ng/package-archives
 '(
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
   ido-completing-read+
   magit
   markdown-mode
   multiple-cursors
   projectile
   smex
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
(ido-ubiquitous-mode 1)
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
(when (fboundp 'projectile-mode)
  (projectile-mode 1))

;; use ls-lisp everywhere to portably group directories first
;; and get away from "ls does not support --dired" warnings
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)

;; open magit status in same window
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

;; don't open ediff control in new frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; use side-by-side ediff by default
(setq ediff-split-window-function 'split-window-horizontally)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme

(when window-system
  (when (find-font (font-spec :name "Consolas"))
    (let ((consolas
           (if (eq window-system 'w32)
               "Consolas-11" "Consolas-14")))
      (add-to-list 'default-frame-alist
                   (cons 'font consolas))))
  (when (find-font (font-spec :name "DejaVu Sans Mono"))
    (add-to-list 'default-frame-alist
                 '(font . "DejaVu Sans Mono-11.5")))
  (setq frame-title-format
        '(buffer-file-name "%f" ("%b"))))


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
 hscroll-margin                   0
 hscroll-step                     1
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

(ng/set-keys
 "C-a"       ng/home
 "M-m"       ng/home
 "<home>"    ng/home
 "C-e"       ng/end
 "<end>"     ng/end
 "C-m"       newline-and-indent
 "C-k"       ng/C-k
 "C-w"       ng/C-w
 "C-x C-b"   ibuffer
 "M-n"       cua-scroll-up
 "M-p"       cua-scroll-down
 "C-x C-k"   ng/kill-other-buffer-and-window
 "C-x k"     ng/kill-this-buffer-and-window
 "<C-tab>"   next-buffer
 "<C-S-tab>" previous-buffer
 "C-;"       comment-line
 )

(if (package-installed-p 'expand-region)
    (ng/set-keys
     "<C-S-right>" er/expand-region
     "<C-S-left>"  er/contract-region))
  
(if (package-installed-p 'ace-jump-mode)
    (ng/set-keys "C-j" ace-jump-mode))

(if (package-installed-p 'smex)
    (ng/set-keys "M-x" smex))

(when (package-installed-p 'projectile)  
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (ng/set-keys "C-," projectile-find-file))
  
(defun ng/home (&optional arg)
  "Move back to indentation. If already there, move to beginning
of line. If a region is selected, create multiple cursors in the
selected lines before moving."
  (interactive "p")
  (setq arg (or arg 1))
  (if (/= arg 1)
      (move-beginning-of-line arg)
    (progn
      (if (and (use-region-p) (fboundp 'mc/edit-lines))
          (mc/edit-lines))
      (if (ng/at-indentation-p)
          (move-beginning-of-line 1)
        (back-to-indentation)))))

(defun ng/end (&optional arg)
  "Move back to indentation. If already there, move to beginning
of line. If a region is selected, create multiple cursors in the
selected lines before moving."
  (interactive "p")
  (setq arg (or arg 1))
  (if (/= arg 1)
      (move-end-of-line arg)
    (if (and (use-region-p) (fboundp 'mc/edit-lines))
        (mc/edit-lines))
    (move-end-of-line 1)))

(defun ng/at-indentation-p ()
  "Check if the cursor is already back to indentation."
  (interactive)
  (let ((old-column (current-column)))    
    (save-excursion
      (back-to-indentation)
      (= old-column (current-column)))))

(defun ng/C-w (&optional arg)
  "If there is an active region, kill it. Otherwise, kill the
preceding word. Allows C-w to backward-kill-word like it does on
the terminal, but still kill-region when something is selected."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun ng/C-k (&optional arg)
  "If there is an active region, kill it. Otherwise, kill the
following line. Strengthens the mnemonic k==kill:y==yank."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-line arg)))

(defun ng/kill-this-buffer-and-window ()
  "Kill the current buffer and delete its window."
  (interactive)
  (let ((window (selected-window))
                (buffer (current-buffer)))
    (if (window-parent window)
        (delete-window window))
    (kill-buffer buffer)))

(defun ng/kill-other-buffer-and-window ()
  "Kill the buffer in the other window and delete its window."
  (interactive)
  (let ((old-window (selected-window))
        (old-buffer (current-buffer)))
    (other-window 1)
    (let ((new-window (selected-window))
          (new-buffer (current-buffer)))
      (unless (eq new-buffer old-buffer)
        (kill-buffer new-buffer))
      (unless (eq new-window old-window)
        (delete-window new-window)))))
  
