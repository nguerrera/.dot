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
;; *  https://blog.d46.us/advanced-emacs-startup/
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

;; Set up load path for personal *.el files
(add-to-list 'load-path "~/.emacs.d/lisp")

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

;; load early config
(load ng/early-init-file)

;; Check if we have an up-to-date lock file and load it
;; if so. If not, boot using package.el.
(if (and (file-exists-p ng/package-lock-file)
         (file-newer-than-file-p ng/package-lock-file ng/early-init-file)
	 (file-newer-than-file-p ng/package-lock-file ng/init-file))
    (load ng/package-lock-file)
  (progn
    (require 'package)
    (package-initialize)
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (setq use-package-always-ensure t)
    (delete-file ng/package-lock-file)))  

;; load main config
(load ng/init-file)

;; save lock file to speed up future inits
(when use-package-always-ensure
  (let ((package-dir (file-truename package-user-dir)))
    (with-temp-file ng/package-lock-file
      (insert ";; This file is automatically generated to speed up package loading when the configuration hasn't changed.\n")
      (dolist (path load-path)
	(when (string-prefix-p package-dir path)
	  (insert (format "(add-to-list 'load-path \"%s\")\n" path)))))))

;; load generated config
(load custom-file)
