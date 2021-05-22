;; -*- lexical-binding: t; -*-

;; disable startup message
(setq inhibit-startup-message t)

;; leave the scratch buffer empty by default
(setq initial-scratch-message nil)

;; follow symlinks to source-controlled locations without prompting
(setq vc-follow-symlinks t)

;; remove sometimes incorrect version control info from mode-line
(setq-default mode-line-format
              (delete '(vc-mode vc-mode) mode-line-format))

;; never use tabs to indent
(setq-default indent-tabs-mode nil)

;; truncate long lines, don't wrap
(setq-default truncate-lines t)

;; triple productivity
(defalias 'yes-or-no-p 'y-or-n-p)

;; prefer UTF-8
(prefer-coding-system 'utf-8)

;; don't beep
(setq
 visible-bell t
 ring-bell-function 'ignore)

;; highlight matching parens
(progn
  (show-paren-mode 1)
  (setq show-paren-priority -1))

;; show line and column numbers
(progn
  (line-number-mode 1)
  (column-number-mode 1))

;; use a vertical bar as cursor
(setq-default cursor-type 'bar)

;; refresh unsaved files
(global-auto-revert-mode 1)

;; don't warn about cl being deprecated as some packages use it
(setq byte-compile-warnings '(cl-functions))

;; http://www.emacswiki.org/SmoothScrolling
(setq
 mouse-wheel-scroll-amount        '(1 ((shift) . 1)) 
 mouse-wheel-progressive-speed    nil
 mouse-wheel-follow-mouse         t            
 scroll-step                      1
 scroll-preserve-screen-position  1
 hscroll-margin                   0
 hscroll-step                     1)

;; fill to 76 cloumns by default
(setq-default fill-column 76)

;; use ls-lisp everywhere to portably group directories first and get
;; away from "ls does not support --dired" warnings
(progn
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t)
  (require 'ls-lisp))

;; start server for emacsclient
;; https://www.emacswiki.org/emacs/EmacsClient
(progn
  (require 'server)
  (when (and window-system (not (server-running-p)))
    (server-start)
    ;; don't nag about live clients when killing buffers or exiting emacs
    (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
    (remove-hook 'kill-emacs-query-functions 'server-kill-emacs-query-function)))

;; Use Git for Windows bash for shell-command. Fixes various issues
;; where things assume bash syntax can be passed to shell-command
(when (and (string= system-type "windows-nt")
           (file-directory-p "C:/Program Files/Git"))
  ;; add or move msys dirs to front of exec-path
  (dolist (dir '("C:/Program Files/Git/usr/bin"
                 "C:/Program Files/Git/mingw64/bin"))
    (setq exec-path (cons dir (remove dir exec-path))))

  ;; sync PATH to exec-path
  (let ((path ""))
    (dolist (p exec-path)
      (setq path (concat (replace-regexp-in-string "/" "\\\\" path) p ";")))
    (setenv "PATH" path))

  ;; use bash for shell-command
  (setq shell-file-name "bash")

  ;; but retain cmdproxy for M-x shell because interactive bash is
  ;; relatively broken in Emacs on Windows. Use eshell instead.
  (setq explicit-shell-file-name "cmdproxy")

  ;; with this in place, we can use projectile's fast "alien" indexing
  ;; on windows, which is the default on other OSes
  (setq projectile-indexing-method 'alien)

  ;; with this in place, emacs finds Windows gpg.exe and tries to use it
  ;; to validate package signatures, but it does not work so revert to
  ;; the default windows behavior of not checking.
  (setq package-check-signature nil))

;; Make sure emacs gets focus when a frame is raised or created on Mac
(when (eq window-system 'ns)
  (advice-add 'raise-frame :after 'ng-mac-activate)
  (add-hook 'after-make-frame-functions 'ng-mac-activate))

(require 'use-package)
(require 'ng-lib)

(use-package diminish
  :config
  (diminish 'eldoc-mode))

;; http://www.emacswiki.org/CuaMode
(progn
  (setq cua-auto-tabify-rectangles nil)
  (setq cua-keep-region-after-copy t)
  (cua-mode 1))

(bind-keys
 ("C-k"             . ng-C-k)
 ("C-w"             . ng-C-w)
 ("C-a"             . ng-home)
 ("<home>"          . ng-home)
 ("C-e"             . ng-end)
 ("<end>"           . ng-end)
 ("C-c c"           . ng-done)
 ("C-x k"           . kill-current-buffer)
 ("<M-S-up>"        . ng-rectangle-mark-up)
 ("<M-S-down>"      . ng-rectangle-mark-down)
 ("<M-S-left>"      . ng-rectangle-mark-left)
 ("<M-S-right>"     . ng-rectangle-mark-right)
 ("C-m"             . newline-and-indent)
 ("C-x C-b"         . ibuffer)
 ("<C-tab>"         . next-buffer)
 ("<C-S-tab>"       . previous-buffer)
 ("<C-iso-lefttab>" . previous-buffer)
 ("C-;"             . comment-line)
 ("<escape>"        . keyboard-quit)
 ("C-M-g"           . keyboard-escape-quit)
 ("<mouse-3>"       . menu-bar-open))

(ng-unbind-keys
 ;; disable archaic "secondary selection" on alt clicks, which we'll
 ;; replace with modern multiple cursor/rectangle functionality
 "M-<drag-mouse-1>"
 "M-<down-mouse-1>"
 "M-<mouse-1>"
 "M-<mouse-2>"
 "M-<mouse-3>")

(use-package rect
  :ensure nil ; built-in
  :demand
  :init
  ;; unbind cua-mode version of rectangle selection
  (define-key cua-global-keymap cua-rectangle-mark-key nil)
  ;; but advise cua to stop standard rectangle-mark-mode-on-cancel
  (advice-add 'cua-cancel
              :before
              (lambda () (rectangle-mark-mode -1)))
  :bind
  ;; make it possible to type over rectangles with multiple cursors
  (:map rectangle-mark-mode-map
        ("<remap> <self-insert-command>" . ng-rectangle-self-insert)))

(use-package multiple-cursors
  :pin melpa
  :commands
  mc/edit-lines
  mc/add-cursor-on-click
  :bind
  ("M-<mouse-1>" . ng-add-cursor-on-click)
  (:map mc/keymap
        ("<remap> <keyboard-quit>" . mc/keyboard-quit)))

(use-package expand-region
  :bind
  ("<C-S-right>" . er/expand-region)
  ("<C-S-left>"  . er/contract-region))

(use-package move-text
  :bind
  ("<M-up>"   . move-text-up)
  ("<M-down>" . move-text-down))

(use-package undo-tree
  :demand
  :diminish
  :config
  (global-undo-tree-mode 1)
  :bind
  (:map undo-tree-map
        ("C-z"   . undo-tree-undo)
        ("C-S-z" . undo-tree-redo)))

(use-package esup
  :commands esup)

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package powershell
  :mode ("\\.psm?1\\'" . powershell-mode))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;; not really a rainbow, just highlight matched/unmatched differently
  (setq rainbow-delimiters-max-face-count 1))

;; https://www.emacswiki.org/emacs/GotoAddress
(use-package goto-addr
  :hook ((find-file . goto-address-mode))
  :config
  (setq goto-address-url-face '((:underline t)))
  (setq goto-address-url-mouse-face '((:underline t :weight bold))))

;; https://github.com/bbatsov/projectile
(use-package projectile
  :diminish
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1))

(use-package which-key
  :defer 3
  :diminish
  :config
  (which-key-mode 1))

;; https://magit.vc/
(use-package magit
  :mode
  ("/COMMIT_EDITMSG\\'"  . git-commit-mode)
  ("/git-rebase-todo\\'" . git-rebase-mode)
  :bind
  ("C-c m" . magit-status)
  :hook
  (git-commit-mode . markdown-mode)
  :init
  (setq
   ;; open magit status in same window
   magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
   ;; don't open ediff control in new frame
   ediff-window-setup-function #'ediff-setup-windows-plain
   ;; use side-by-side ediff by default
   ediff-split-window-function #'split-window-horizontally)
  :config
  ;; disable default global bindings
  (global-magit-file-mode -1))

(use-package avy
  :init
  (setq avy-keys (number-sequence ?a ?z))
  :bind
  ("C-c j" . avy-goto-word-1)
  ("M-g g" . avy-goto-line))

(use-package ivy
  :diminish
  :init
  (setq
   ivy-initial-inputs-alist nil
   ivy-re-builders-alist    '((t . ivy--regex-fuzzy))
   ivy-extra-directories    nil
   ivy-count-format "")

  (defun ng-ivy-C-d ()
    "Before end-of-line delete-char, otherwise ivy-done."
    (interactive)
    (if (eolp)
        (ivy-done)
      (command-execute 'delete-char)))

  :bind
  ("<remap> <switch-to-buffer>" . ivy-switch-buffer)
  ("<remap> <switch-to-buffer-other-window>" . switch-to-buffer-other-window)
  (:map ivy-minibuffer-map
        ("RET"        . ivy-alt-done)
        ("<C-return>" . ivy-alt-done)
        ("C-d"        . ng-ivy-C-d)
        ("C-j"        . ivy-next-line)
        ("C-k"        . ivy-previous-line))

  :config
  (ivy-mode 1))

(use-package flx :defer)
(use-package amx :defer)

(use-package counsel
  :diminish
  :defer 2
  :bind
  ;; common bindings here in case pressed before deferral timer
  ("<remap> <execute-extended-command>" . counsel-M-x)
  ("<remap> <find-file>"                . counsel-find-file)
  ;; custom bindings not mapped by counsel-mode
  ("<remap> <dired>"                    . counsel-dired)
  :config
  (require 'flx)
  (require 'amx)
  (counsel-mode 1)
  ;; counsel-mode pushes more initial ^ defaults, so reset it
  (setq ivy-initial-inputs-alist nil))

(use-package swiper
  :bind
  ("C-c s" . swiper))

(use-package csharp-mode
  :mode "\\.cs\\'")

;; C/C++/C#/Java and other c-mode based language
(progn
  (add-hook
   'c-mode-common-hook
   (lambda ()
     (c-set-style "bsd")
     (c-set-offset 'case-label '+)
     (setq c-basic-offset 4))))

;; Perl
;; http://www.emacswiki.org/emacs/CPerlMode
(progn
  (defalias 'perl-mode 'cperl-mode)
  (add-hook
   'cperl-mode-hook
   (lambda ()
     (setq cperl-indent-level 4)
     (setq cperl-brace-offset (- cperl-indent-level))
     (setq cperl-invalid-face nil))))
