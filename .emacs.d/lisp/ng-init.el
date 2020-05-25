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
(show-paren-mode 1)

;; show line and column numbers
(progn
  (line-number-mode 1)
  (column-number-mode 1))

;; don't blink the cursor
(blink-cursor-mode -1)

;; refresh unsaved files
(global-auto-revert-mode 1)

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

;; use ls-lisp everywhere to portably group directories first and get
;; away from "ls does not support --dired" warnings
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)
(require 'ls-lisp)

;; start server for emacsclient
;; https://www.emacswiki.org/emacs/EmacsClient
(progn
  (require 'server)
  (when (and window-system (not (server-running-p)))
    (server-start)
    ;; don't nag about live clients when killing buffers or exiting emacs
    (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
    (remove-hook 'kill-emacs-query-functions 'server-kill-emacs-query-function)))

(require 'use-package)
(require 'ng-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

;; http://www.emacswiki.org/CuaMode
(cua-mode 1)
(setq cua-auto-tabify-rectangles nil)
(setq cua-keep-region-after-copy t)

(ng/set-keys
 "C-k"         ng/C-k
 "C-w"         ng/C-w
 "C-a"         ng/home
 "M-m"         ng/home
 "<home>"      ng/home
 "C-e"         ng/end
 "<end>"       ng/end
 "C-c C-c"     ng/done
 "C-x C-k"     ng/kill-other-buffer-and-window
 "C-x k"       ng/kill-this-buffer-and-window
 "<M-S-up>"    ng/rectangle-mark-up
 "<M-S-down>"  ng/rectangle-mark-down
 "<M-S-left>"  ng/rectangle-mark-left
 "<M-S-right>" ng/rectangle-mark-right
 "M-n"         cua-scroll-up
 "M-p"         cua-scroll-down
 "C-m"         newline-and-indent
 "C-x C-b"     ibuffer
 "<C-tab>"     next-buffer
 "<C-S-tab>"   previous-buffer
 "C-;"         comment-line
 "C-x SPC"     cua-rectangle-mark-mode
 )

;; remove minor mode conflicts with C-c C-c
(progn
  (add-hook 'bat-mode-hook (lambda () (define-key bat-mode-map (kbd "C-c C-c") nil)))
  (add-hook 'conf-mode-hook (lambda () (define-key conf-mode-map (kbd "C-c C-c") nil))))

;; disable archaic "secondary selection" on alt clicks, which we'll
;; replace with modern multiple cursor/rectangle functionality
(progn
  (global-unset-key (kbd "M-<drag-mouse-1>"))
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-unset-key (kbd "M-<mouse-1>"))
  (global-unset-key (kbd "M-<mouse-2>"))
  (global-unset-key (kbd "M-<mouse-3>")))

(use-package esup
  :commands esup)

(use-package move-text
  :bind
  ("<M-up>"   . move-text-up)
  ("<M-down>" . move-text-down))

;; https://www.emacswiki.org/emacs/GotoAddress
(use-package goto-addr
  :hook ((find-file . goto-address-mode)))

;; http://emacswiki.org/InteractivelyDoThings
(use-package ido
  :init
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-enable-flex-matching t))

;; https://github.com/DarwinAwardWinner/ido-completing-read-plus
(use-package ido-completing-read+
  :requires ido
  :defer 2
  :config
  (ido-ubiquitous-mode 1))

;; https://github.com/lewang/flx
(use-package flx-ido
  :requires ido
  :defer 1
  :init
  (setq ido-use-faces nil)
  :config
  (flx-ido-mode 1))

;; https://github.com/larkery/ido-grid-mode.el
(use-package ido-grid-mode
  :requires ido
  :init
  (setq
   ido-grid-mode-max-columns  1
   ido-grid-mode-max-rows     8
   ido-grid-mode-min-rows     8
   ido-grid-mode-scroll-down  #'ido-grid-mode-next-row
   ido-grid-mode-scroll-up    #'ido-grid-mode-previous-row
   ido-grid-mode-scroll-wrap  nil
   ido-grid-mode-order        nil)
  :config
  (ido-grid-mode 1))

;; https://github.com/bbatsov/projectile
(use-package projectile
  :bind
  ("C-," . projectile-find-file)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1))

;; https://magit.vc/
(use-package magit
  :mode
  ("/COMMIT_EDITMSG\\'"  . git-commit-mode)
  ("/git-rebase-todo\\'" . git-rebase-mode)
  :bind
  ("C-x g" . magit-status)
  :init
  (setq
   ;; open magit status in same window
   magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
   ;; don't open ediff control in new frame
   ediff-window-setup-function 'ediff-setup-windows-plain
   ;; use side-by-side ediff by default
   ediff-split-window-function 'split-window-horizontally))

(use-package multiple-cursors
  :commands
  mc/edit-lines
  :bind
  ("M-<mouse-1>" . mc/add-cursor-on-click))

(use-package expand-region
  :bind
  ("<C-S-right>" . er/expand-region)
  ("<C-S-left>"  . er/contract-region))
  
(use-package ace-jump-mode
  :bind
  ("C-j" . ace-jump-mode))

(use-package smex
  :bind
  ("M-x" . smex))

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
