;; -*- lexical-binding: t; -*-
;; See lisp/ng-*.el for the main configuration
;;
;; init.el's responsibilities:
;;
;; 1. Bootstrap https://github.com/jwiegley/use-package
;;
;; 2. Load day-to-day manual configuration from lisp/ng-*.el
;;
;; 3. Redirect automatically generated custom loading
;;
;; 4. Cache package load path to avoid running slow
;;    (package-initialize) when configuration hasn't changed.
;;
;; Performance optimization techniques inspired by:
;;
;; * https://github.com/nilcons/emacs-use-package-fast
;;
;;   But we don't bother with byte compilation, which is cumbersome
;;   and didn't make a dent for me in practice. Instead, write out a
;;   plain text lock file to remember the load path from a prior
;;   initialization.
;;
;; *  https://blog.d46.us/advanced-emacs-startup
;;
;;   Startup time logging lifted from there. I played with GC settings
;;   and they weren't material so I left it alone. I also removed the
;;   GC count logging as it just led to playing golf with the count
;;   without a perceptible speedup.

(defvar ng/early-init-file "~/.emacs.d/lisp/ng-early-init.el"
  "The configuration file where package-archives are set and any
other configuration that would like to happen early before we
potentially hit the network.")

(defvar ng/init-file  "~/.emacs.d/lisp/ng-init.el"
  "The configuration file where package-archives are defined and
use-package calls are made.")

(defvar ng/package-lock-file "~/.emacs.d/.ng-package-lock.el"
  "The generated file where package load-paths are cached to be
reused when the config files have not changed.")

(defun ng/write-package-lock-file ()
  "Save package-alist to lock file"
  (setq package-selected-packages (mapcar #'car package-alist))
  (let ((package-dir (file-truename package-user-dir)))
    (with-temp-file ng/package-lock-file
      (insert ";; This file is automatically generated to speed up package")
      (insert " loading when the configuration hasn't changed.\n")
      (insert "(setq package-selected-packages '")
      (print package-selected-packages (current-buffer))
      (insert ")\n")
      (insert "(setq package-alist '")
      (print package-alist (current-buffer))
      (insert ")\n")
      (dolist (pkg package-alist)
        (insert (format "(push \"%s\" load-path)\n"
                        (package-desc-dir (cadr pkg)) load-path))))))

(defun ng/load-package-lock-file ()
  "Restore package-alist and load-path from lock file and adds
package directories to load-path."
  (load ng/package-lock-file))

(defun ng/package-lock-file-up-to-date-p ()
  "Determine if lock file is up to date with configuration"
  (and (file-exists-p ng/package-lock-file)
       (file-newer-than-file-p ng/package-lock-file ng/early-init-file)
       (file-newer-than-file-p ng/package-lock-file ng/init-file)))

(defun ng/package-initialize ()
  "Prepare package system from lock file if up to date, or boot
using package.el if it's not."
  (if (ng/package-lock-file-up-to-date-p)
      (ng/load-package-lock-file)
    (progn
      (delete-file ng/package-lock-file)
      (setq use-package-always-ensure t)
      (require 'package)
      (package-initialize)
      (unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package)))))

(defun ng/package-save ()
  "If this init did a full restore, save the lock file to speed
up future inits."
  (if use-package-always-ensure
      (ng/write-package-lock-file)))

;; Set up load paths
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Don't save selected packages to custom file. A selected package in
;; this config is one that has been pulled in by a use-package.
;; Everything else is one-off experimentation that can be cleaned up
;; by package-autoremove. We save package-selected-packages to to the
;; lock file instead of the custom file.
(with-eval-after-load 'package
  (defun package--save-selected-packages (&optional VALUE)))

;; Keep generated config in its own file (loaded at the end of this file)
(setq custom-file "~/.emacs.d/custom.el")

;; Don't initialize packages at startup, see the pains we go through
;; below to avoid this slow step.
(setq package-enable-at-startup nil)

;; Log startup time
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message
    "Startup Time: %.2f seconds"
    (float-time (time-subtract after-init-time before-init-time)))))

;; Do the init dance
(load ng/early-init-file)
(ng/package-initialize) 
(load ng/init-file)
(ng/package-save)
(load custom-file)
