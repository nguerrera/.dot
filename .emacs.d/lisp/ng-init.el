;; -*- lexical-binding: t; -*-

;; disable startup message
(setq inhibit-startup-message t)

;; leave the scratch buffer empty by default
(setq initial-scratch-message nil)

;; follow symlinks to source-controlled locations without prompting
(setq vc-follow-symlinks t)

;; never use tabs to indent
(setq-default indent-tabs-mode nil)

;; truncate long lines, don't wrap
(setq-default truncate-lines t)

;; triple productivity
(defalias 'yes-or-no-p 'y-or-n-p)

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
 ("C-M-g"           . keyboard-escape-quit))

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

;; Experiment: Use evil with minimal emacs binding interference. I
;; call this "mischievous mode". :) I'm not sure yet if I'll stick
;; with it...
;;
;; Before this, my main approach to avoiding RSI has been
;; hardware-based: I use an Ergodox EZ with a layout that lets me
;; reach modifiers and arrows without moving my hands or stretching my
;; fingers.
;;
;; I'm only just learning vim bindings and going all the way was just
;; too confusing, so starting with:
;;
;; - Non text-mode or prog-mode buffers use emacs state.
;;
;; - Insert state has no overridden bindings. It basically differs
;;   from emacs state only in that ESC/C-g goes to normal state. I
;;   also jump through some hoops so that ESC/C-g don't take priority
;;   over other contextual uses such as cancelling an active region
;;   selection or multiple cursor session.
;;
;; - The toggle out of emacs state is moved from C-z (which breaks my
;;   undo) to C-M-S-z where it is basically impossible to do
;;   accidentally. This key is basically never needed in this config
;;   since insert state is already essentially emacs state.
;;
;; - Remove all the CTRL+single letter bindings from normal
;;   state. It's simply too confusing for me at this point to have two
;;   different sets of these to keep track of. Also, some emacs keys
;;   are more comfortable than vim bindings. For example, C-a is
;;   easier to reach than 0 and C-e is easier than $ for me.
;;
;;   As I learn, if there are CTRL sequences that don't have good
;;   emacs equivalents, I'll try to find a home for them, or consider
;;   adopting more evil ways...
(use-package evil
  :demand
  :init
  (setq
   evil-disable-insert-state-bindings t
   evil-default-state 'emacs
   evil-emacs-state-cursor 'bar
   evil-normal-state-cursor 'hbar
   evil-visual-state-cursor 'hbar
   evil-emacs-state-tag ""
   evil-motion-state-modes '()
   evil-insert-state-modes '()
   evil-normal-state-modes '(prog-mode text-mode)
   evil-toggle-key "C-M-S-z"
   evil-shift-width 2
   evil-want-fine-undo t
   evil-cross-lines t)

  ;; minor modes for lower priority keybindings in insert/normal modes
  (define-minor-mode ng-insert-mode nil :keymap (make-sparse-keymap))
  (define-minor-mode ng-normal-mode nil :keymap (make-sparse-keymap))

  (defun ng-enter-insert-mode ()
    (setq ng-enable-multiple-cursor-keys t)
    (ng-insert-mode 1))

  (defun ng-exit-insert-mode ()
    (ng-insert-mode 1))

  (defun ng-enter-normal-mode ()
    (setq ng-enable-multiple-cursor-keys nil)
    (ng-normal-mode 1))

  (defun ng-exit-normal-mode  ()
    (ng-normal-mode 1))

  (defun ng-evil-replace-all ()
    (interactive)
    (evil-ex "%s/"))

  :hook
  (evil-normal-state-entry . ng-enter-normal-mode)
  (evil-normal-state-exit  . ng-exit-normal-mode)
  (evil-insert-state-entry . ng-enter-insert-mode)
  (evil-insert-state-exit  . ng-exit-insert-mode)

  :config
  (evil-mode 1)

  :bind-keymap
  ("C-c w" . evil-window-map)

  :bind
  ("C-%" . ng-evil-replace-all)

  (:map ng-insert-mode-map
        ("<remap> <keyboard-quit>" . evil-normal-state))

  (:map ng-normal-mode-map
        ("<remap> <keyboard-quit>" . evil-force-normal-state))

  (:map evil-insert-state-map
        ("<escape>" . nil)
        ("<delete>" . nil))

  (:map evil-normal-state-map
        ("<escape>" . nil)
        ("C-g"      . nil)
        ("C-n"      . nil)
        ("C-p"      . nil)
        ("C-r"      . nil)
        ("C-t"      . nil))

  (:map evil-motion-state-map
        ("C-x SPC"  . evil-visual-block)
        ("<up>"     . nil)
        ("<down>"   . nil)
        ("<left>"   . nil)
        ("<right>"  . nil)
        ("C-b"      . nil)
        ("C-d"      . nil)
        ("C-e"      . nil)
        ("C-f"      . nil)
        ("C-i"      . nil)
        ("C-o"      . nil)
        ("C-w"      . nil)
        ("C-v"      . nil)
        ("C-y"      . nil))

  (:map evil-visual-state-map
        ("C-g" . evil-exit-visual-state)))

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

(use-package smartparens
  :diminish
  :hook (prog-mode . smartparens-mode)
  :config
  (setq sp-highlight-pair-overlay     nil
        sp-highlight-wrap-overlay     nil
        sp-highlight-wrap-tag-overlay nil
        sp-escape-quotes-after-insert nil)
  (require 'smartparens-config))

;; https://www.emacswiki.org/emacs/GotoAddress
(use-package goto-addr
  :hook ((find-file . goto-address-mode))
  :config
  (setq goto-address-url-face '((:underline t)))
  (setq goto-address-url-mouse-face '((:underline t :weight bold))))

;; http://emacswiki.org/InteractivelyDoThings
(use-package ido
  :defer 2
  :config
  (setq ido-enable-flex-matching t)
  (ido-mode 1)
  (ido-everywhere 1))

;; https://github.com/DarwinAwardWinner/ido-completing-read-plus
(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))

;; https://github.com/lewang/flx
(use-package flx-ido
  :after ido
  :config
  (setq ido-use-faces nil)
  (flx-ido-mode 1))

;; https://github.com/larkery/ido-grid-mode.el
(use-package ido-grid-mode
  :after ido
  :config
  (setq
   ido-grid-mode-max-columns  1
   ido-grid-mode-max-rows     8
   ido-grid-mode-min-rows     8
   ido-grid-mode-scroll-down  #'ido-grid-mode-next-row
   ido-grid-mode-scroll-up    #'ido-grid-mode-previous-row
   ido-grid-mode-scroll-wrap  nil
   ido-grid-mode-order        nil)
  (ido-grid-mode 1))

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
  :config
  (setq
   ;; open magit status in same window
   magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
   ;; don't open ediff control in new frame
   ediff-window-setup-function #'ediff-setup-windows-plain
   ;; use side-by-side ediff by default
   ediff-split-window-function #'split-window-horizontally)
  ;; disable default global bindings
  (global-magit-file-mode -1))

(use-package ace-jump-mode
  :bind
  ("C-c j" . ace-jump-mode))

(use-package smex
  :bind
  ("M-x" . smex)
  :config
  (setq smex-save-file "~/.emacs.d/.smex-items"))

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
