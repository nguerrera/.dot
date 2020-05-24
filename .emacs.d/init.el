;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use auto-save and backups, but keep them out of my working
;; directories...
;; https://www.emacswiki.org/emacs/BackupDirectory
;;
;; ...and don't bother with lock files, which can't be moved
;; https://www.emacswiki.org/emacs/LockFiles
;;
;; Set this up before anything else to reduce the risk of a failed
;; init causing these files to show up in working directories while
;; I fix things.
(setq
 auto-save-file-name-transforms '((".*" "~/.emacs.d/saves/" t))
 backup-directory-alist         '(("." . "~/.emacs.d/saves/"))
 backup-by-copying              t
 create-lockfiles               nil
 delete-old-versions            t
 kept-new-versions              5
 kept-old-versions              0
 version-control                t
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save automatic customizations in their own file
;; https://www.emacswiki.org/emacs/CustomFile
(setq custom-file "~/.emacs.d/custom.el")

;; Intentionally not loading custom.el yet because it is writing
;; package stuff I shouldn't need
;; (if (file-exists-p custom-file)
;;     (load custom-file))

;; start server for emacsclient
(when window-system
  (condition-case nil
      (let ((warning-minimum-level :error))
        (server-start))
    (error nil)))

;; go ahead and kill clients without prompting
(remove-hook 'kill-buffer-query-functions
             'server-kill-buffer-query-function)
(remove-hook 'kill-emacs-query-functions
             'server-kill-emacs-query-function)

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
   ido-grid-mode
   ido-completing-read+
   magit
   markdown-mode
   move-text
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

;; make ido completions vertical and navigable
;; https://github.com/larkery/ido-grid-mode.el
(when (fboundp 'ido-grid-mode)
  (setq
   ido-grid-mode-max-columns  1
   ido-grid-mode-max-rows     8
   ido-grid-mode-min-rows     8
   ido-grid-mode-scroll-down  #'ido-grid-mode-next-row
   ido-grid-mode-scroll-up    #'ido-grid-mode-previous-row
   ido-grid-mode-scroll-wrap  nil
   ido-grid-mode-order        nil
   )
  (ido-grid-mode 1))

;; project interaction
;; https://github.com/bbatsov/projectile
(when (fboundp 'projectile-mode)
  (projectile-mode 1))

;; use ls-lisp everywhere to portably group directories first
;; and get away from "ls does not support --dired" warnings
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)


(when (package-installed-p 'magit)
  ;; eagerly load magit so that it engages if emacs is started from
  ;; `git commit`
  (require 'magit)

  ;; open magit status in same window
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  
  ;; don't open ediff control in new frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  
  ;; use side-by-side ediff by default
  (setq ediff-split-window-function 'split-window-horizontally)
  )
  

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
 "C-a"         ng/home
 "M-m"         ng/home
 "<home>"      ng/home
 "C-e"         ng/end
 "<end>"       ng/end
 "C-m"         newline-and-indent
 "C-k"         ng/C-k
 "C-w"         ng/C-w
 "C-x C-b"     ibuffer
 "M-n"         cua-scroll-up
 "M-p"         cua-scroll-down
 "C-x C-k"     ng/kill-other-buffer-and-window
 "C-x k"       ng/kill-this-buffer-and-window
 "<C-tab>"     next-buffer
 "<C-S-tab>"   previous-buffer
 "C-;"         comment-line
 "C-c C-c"     ng/done
 "C-x SPC"     cua-rectangle-mark-mode
 "<M-up>"      move-text-up
 "<M-down>"    move-text-down
 "<M-S-up>"    ng/rectangle-mark-up
 "<M-S-down>"  ng/rectangle-mark-down
 "<M-S-left>"  ng/rectangle-mark-left
 "<M-S-right>" ng/rectangle-mark-right
 )

;; remove minor mode conflicts with C-c C-c
(require 'bat-mode)
(define-key bat-mode-map (kbd "C-c C-c") nil)
(require 'conf-mode)
(define-key conf-mode-map (kbd "C-c C-c") nil)

;; disable archaic "secondary selection" on alt clicks, which we'll
;; replace with modern multiple cursor/rectangle functionality
(global-unset-key (kbd "M-<drag-mouse-1>"))
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-unset-key (kbd "M-<mouse-1>"))
(global-unset-key (kbd "M-<mouse-2>"))
(global-unset-key (kbd "M-<mouse-3>"))

(if (package-installed-p 'magit)
    (ng/set-keys "C-x g" magit-status))

(if (package-installed-p 'multiple-cursors)
    (ng/set-keys "M-<mouse-1>" mc/add-cursor-on-click))

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

(defun ng/rectangle-mark-down (&optional arg)
  (interactive "p")          
  (unless cua--rectangle (cua-set-rectangle-mark))
  (cua-resize-rectangle-down arg))
                             
(defun ng/rectangle-mark-up (&optional arg)
  (interactive "p")          
  (unless cua--rectangle (cua-set-rectangle-mark))
  (cua-resize-rectangle-up arg))

(defun ng/rectangle-mark-left (&optional arg)
  (interactive "p")
  (unless cua--rectangle (cua-set-rectangle-mark))
  (cua-resize-rectangle-left arg))

(defun ng/rectangle-mark-right (&optional arg)
  (interactive "p")
  (unless cua--rectangle (cua-set-rectangle-mark))
  (cua-resize-rectangle-right arg))

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
  (interactive "P")
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

(defun ng/done ()
  "Save the current buffer, kill it, and delete its window."
  (interactive)
  (save-buffer)
  (if server-buffer-clients
      (server-done)
    (ng/kill-this-buffer-and-window)))

