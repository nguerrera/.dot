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
 hscroll-step                     1
 )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

;; http://www.emacswiki.org/CuaMode
(cua-mode 1)
(setq cua-auto-tabify-rectangles nil)
(setq cua-keep-region-after-copy t)

(bind-keys
 ("C-k"         . ng-C-k)
 ("C-w"         . ng-C-w)
 ("C-a"         . ng-home)
 ("M-m"         . ng-home)
 ("<home>"      . ng-home)
 ("C-e"         . ng-end)
 ("<end>"       . ng-end)
 ("C-c C-c"     . ng-done)
 ("C-x C-k"     . ng-kill-other-buffer-and-window)
 ("C-x k"       . ng-kill-this-buffer-and-window)
 ("<M-S-up>"    . ng-rectangle-mark-up)
 ("<M-S-down>"  . ng-rectangle-mark-down)
 ("<M-S-left>"  . ng-rectangle-mark-left)
 ("<M-S-right>" . ng-rectangle-mark-right)
 ("M-n"         . cua-scroll-up)
 ("M-p"         . cua-scroll-down)
 ("C-m"         . newline-and-indent)
 ("C-x C-b"     . ibuffer)
 ("<C-tab>"     . next-buffer)
 ("<C-S-tab>"   . previous-buffer)
 ("C-;"         . comment-line)
 ("C-x SPC"     . cua-rectangle-mark-mode)
 )

;; remove minor mode conflicts with C-c C-c
(progn
  (add-hook 'bat-mode-hook (lambda () (unbind-key "C-c C-c" bat-mode-map)))
  (add-hook 'conf-mode-hook (lambda () (unbind-key "C-c C-c" conf-mode-map))))

;; disable archaic "secondary selection" on alt clicks, which we'll
;; replace with modern multiple cursor/rectangle functionality
(progn
  (unbind-key "M-<drag-mouse-1>")
  (unbind-key "M-<down-mouse-1>")
  (unbind-key "M-<mouse-1>")
  (unbind-key "M-<mouse-2>")
  (unbind-key "M-<mouse-3>"))

(use-package diminish
  :config
  (diminish 'eldoc-mode))

(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode))

(use-package esup
  :commands esup)

(use-package move-text
  :bind
  ("<M-up>"   . move-text-up)
  ("<M-down>" . move-text-down))

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
  :init
  (setq ido-enable-flex-matching t)
  :config
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
  :init
  (setq ido-use-faces nil)
  :config
  (flx-ido-mode 1))

;; https://github.com/larkery/ido-grid-mode.el
(use-package ido-grid-mode
  :after ido
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
  :diminish
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
  :init
  (setq smex-save-file "~/.emacs.d/.smex-items")
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
