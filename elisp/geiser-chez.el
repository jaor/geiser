;; geiser-chez.el -- Chez Scheme's implementation of the geiser protocols

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(require 'geiser-connection)
(require 'geiser-syntax)
(require 'geiser-custom)
(require 'geiser-base)
(require 'geiser-eval)
(require 'geiser-edit)
(require 'geiser-log)
(require 'geiser)

(require 'compile)
(require 'info-look)

(eval-when-compile (require 'cl))


;;; Customization:

(defgroup geiser-chez nil
  "Customization for Geiser's Chez Scheme flavour."
  :group 'geiser)

(geiser-custom--defcustom geiser-chez-binary
    "chez-scheme"
  "Name to use to call the Chez Scheme executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-chez)


;;; REPL support:

(defun geiser-chez--binary ()
  (if (listp geiser-chez-binary)
      (car geiser-chez-binary)
    geiser-chez-binary))

(defun geiser-chez--parameters ()
  "Return a list with all parameters needed to start Chez Scheme.
This function uses `geiser-chez-init-file' if it exists."
;;  `("--load" ,(expand-file-name "chez/geiser/load.scm" geiser-scheme-dir))
  `(,(expand-file-name "chez/geiser/geiser.ss" geiser-scheme-dir))
  )

(defconst geiser-chez--prompt-regexp "> ")


;;; Evaluation support:

(defun geiser-chez--geiser-procedure (proc &rest args)
  (case proc
    ((eval compile)
     (let ((form (mapconcat 'identity (cdr args) " "))
           (module (cond ((string-equal "'()" (car args))
                          "'()")
                         ((and (car args))
                             (concat "'" (car args)))
                         (t
                          "#f"))))
       (format "(geiser:eval %s '%s)" module form)))
    ((load-file compile-file)
     (format "(geiser:load-file %s)" (car args)))
    ((no-values)
     "(geiser:no-values)")
    (t
     (let ((form (mapconcat 'identity args " ")))
       (format "(geiser:%s %s)" proc form)))))

;; (defconst geiser-chez--module-re
;;   ".*;; package: +\\(([^)]*)\\)")

(defun geiser-chez--get-module (&optional module)
  (cond ((null module)
         :f)
        ((listp module) module)
        ((stringp module)
         (condition-case nil
             (car (geiser-syntax--read-from-string module))
           (error :f)))
        (t :f)))

;; (defun geiser-chez--module-cmd (module fmt &optional def)
;;   (when module
;;     (let* ((module (geiser-chez--get-module module))
;;            (module (cond ((or (null module) (eq module :f)) def)
;;                          (t (format "%s" module)))))
;;       (and module (format fmt module)))))

;; (defun geiser-chez--enter-command (module)
;;   (geiser-chez--module-cmd module "(geiser:ge '%s)" "()"))

(defun geiser-chez--symbol-begin (module)
  (if module
      (max (save-excursion (beginning-of-line) (point))
           (save-excursion (skip-syntax-backward "^(>") (1- (point))))
    (save-excursion (skip-syntax-backward "^'-()>") (point))))

(defun geiser-chez--import-command (module)
  (format "(import %s)" module))

(defun geiser-chez--exit-command () "(exit 0)")
;; 
;; ;;; REPL startup

(defconst geiser-chez-minimum-version "9.4")

(defun geiser-chez--version (binary)
  (shell-command-to-string
   (format "%s --version"
           (shell-quote-argument binary))))

;; (defconst geiser-chez--path-rx "^In \\([^:\n ]+\\):\n")
(defun geiser-chez--startup (remote)
  (let ((geiser-log-verbose-p t))
    (compilation-setup t)
    ;; (when (and (stringp geiser-chez-source-directory)
    ;;            (not (string-empty-p geiser-chez-source-directory)))
    ;;   (geiser-eval--send/wait (format "(geiser:set-chez-scheme-source-directory %S)" geiser-chez-source-directory)))
    (geiser-eval--send/wait "(begin (import (geiser)) (write `((result ) (output . \"\"))) (newline))")))

;;; Implementation definition:

(define-geiser-implementation chez
  (binary geiser-chez--binary)
  (arglist geiser-chez--parameters)
  (version-command geiser-chez--version)
  (minimum-version geiser-chez-minimum-version)
  (repl-startup geiser-chez--startup)
  (prompt-regexp geiser-chez--prompt-regexp)
  (debugger-prompt-regexp nil) ;; geiser-chez--debugger-prompt-regexp
  ;; (enter-debugger geiser-chez--enter-debugger)
  (marshall-procedure geiser-chez--geiser-procedure)
  (find-module geiser-chez--get-module)
  ;; (enter-command geiser-chez--enter-command)
  (exit-command geiser-chez--exit-command)
  (import-command geiser-chez--import-command)
  (find-symbol-begin geiser-chez--symbol-begin)
  ;; (display-error geiser-chez--display-error)
  ;; (external-help geiser-chez--manual-look-up)
  ;; (check-buffer geiser-chez--guess)
  ;; (keywords geiser-chez--keywords)
  ;; (case-sensitive geiser-chez-case-sensitive-p)
  )

;; notes: (available-modules) in (chez modules)
;; (env-exports (module-env (find-module '(scheme char)))), modules: (meta) (chez modules) (chez)

(geiser-impl--add-to-alist 'regexp "\\.ss$" 'chez t)
(geiser-impl--add-to-alist 'regexp "\\.def$" 'chez t)

(provide 'geiser-chez)

