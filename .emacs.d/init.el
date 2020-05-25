;; See lisp/ng-*.el for the main configuration
;; 
;; init.el's only job is to bootstrap use-package:
;; https://github.com/jwiegley/use-package
;;
;; We do so while avoiding calling (package-initialize), which is
;; slow, when the configuration is unchanged. The approach is inspired
;; by https://github.com/nilcons/emacs-use-package-fast, but simpler.
;; We don't bother with byte compilation, and instead write out a lock
;; file to remember the load path from a prior initialization.

(setq package-enable-at-startup nil)
(add-to-list 'load-path "~/.emacs.d/lisp")

(defvar ng/early-config-file "~/.emacs.d/lisp/ng-early-config.el"
  "The configuration file where package-archives are set and any
other configuration that would like to happen early before we
potentially hit the network.")

(defvar ng/config-file  "~/.emacs.d/lisp/ng-config.el"
  "The configuration file where package-archives are defined and
use-package calls are made.")

(defvar ng/package-lock-file "~/.emacs.d/.ng-packages.lock"
  "The generated file where package load-paths are cached to be
reused when the config files have not changed.")

(load ng/early-config-file)

;; Check if we have an up-to-date lock file and load it
;; if so. If not, boot using package.el.
(if (and (file-exists-p ng/package-lock-file)
         (file-newer-than-file-p ng/package-lock-file ng/early-config-file)
	 (file-newer-than-file-p ng/package-lock-file ng/config-file))
    (load ng/package-lock-file)
  (progn
    (require 'package)
    (package-initialize)
    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (setq use-package-always-ensure t)
    (delete-file ng/package-lock-file)))  

;; load configuration
(load ng/config-file)

;; save lock file to speed up future inits
(when use-package-always-ensure
  (let ((package-dir (file-truename package-user-dir)))
    (with-temp-file ng/package-lock-file
      (insert ";; AUTO-GENERATED -- DO NOT EDIT ;;\n")
      (dolist (path load-path)
	(when (string-prefix-p package-dir path)
	  (insert (format "(add-to-list 'load-path \"%s\")\n" path)))))))
