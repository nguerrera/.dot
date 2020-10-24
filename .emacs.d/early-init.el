;; Don't initialize packages at startup. See init.el for how we manage
;; to do this slow step only when the configuration changed.
(setq package-enable-at-startup nil)

;; Disable menu bar, scroll bar, tool bar
;; It's much faster in Emacs 27 to change these in early-init
(if (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
