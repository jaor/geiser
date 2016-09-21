;; geiser-mit.el -- MIT/GNU Scheme's implementation of the geiser protocols

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
(require 'subr-x)

(eval-when-compile (require 'cl))


;;; Customization:

(defgroup geiser-mit nil
  "Customization for Geiser's MIT/GNU Scheme flavour."
  :group 'geiser)

(geiser-custom--defcustom geiser-mit-binary
    "mit-scheme"
  "Name to use to call the MIT/GNU Scheme executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-mit)

(geiser-custom--defcustom geiser-mit-source-directory
    ""
  "The path to the MIT/GNU Scheme sources' src/ directory."
  :type 'filename
  :group 'geiser-mit)


;;; REPL support:

(defun geiser-mit--binary ()
  (if (listp geiser-mit-binary)
      (car geiser-mit-binary)
    geiser-mit-binary))

(defun geiser-mit--parameters ()
  "Return a list with all parameters needed to start MIT/GNU Scheme.
This function uses `geiser-mit-init-file' if it exists."
  `("--load" ,(expand-file-name "mit/geiser/load.scm" geiser-scheme-dir))
  )

(defconst geiser-mit--prompt-regexp "[0-9]+ ([^)]+) => ") ;; *not* ]=>, that confuses syntax-ppss
(defconst geiser-mit--debugger-prompt-regexp "[0-9]+ error> ")


;;; Evaluation support:

(defun geiser-mit--geiser-procedure (proc &rest args)
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

(defconst geiser-mit--module-re
  ".*;; package: +\\(([^)]*)\\)")

(defun geiser-mit--get-module (&optional module)
  (cond ((null module)
         (save-excursion
           (geiser-syntax--pop-to-top)
           (if (or (re-search-backward geiser-mit--module-re nil t)
                   (re-search-forward geiser-mit--module-re nil t))
               (geiser-mit--get-module (match-string-no-properties 1))
             :f)))
        ((listp module) module)
        ((stringp module)
         (condition-case nil
             (car (geiser-syntax--read-from-string module))
           (error :f)))
        (t :f)))

(defun geiser-mit--module-cmd (module fmt &optional def)
  (when module
    (let* ((module (geiser-mit--get-module module))
           (module (cond ((or (null module) (eq module :f)) def)
                         (t (format "%s" module)))))
      (and module (format fmt module)))))

(defun geiser-mit--enter-command (module)
  (geiser-mit--module-cmd module "(geiser:ge '%s)" "()"))

(defun geiser-mit--exit-command () "(%exit 0)")

(defun geiser-mit--symbol-begin (module)
  (if module
      (max (save-excursion (beginning-of-line) (point))
           (save-excursion (skip-syntax-backward "^(>") (1- (point))))
    (save-excursion (skip-syntax-backward "^'-()>") (point))))

;; 
;; ;;; REPL startup

(defconst geiser-mit-minimum-version "9.1.1")

(defun geiser-mit--version (binary)
  (car (process-lines binary
                      "--quiet"
                      "--no-init-file"
                      "--eval"
                      "(begin (display (get-subsystem-version-string \"Release\"))
                              (%exit 0))")))

(defconst geiser-mit--path-rx "^In \\([^:\n ]+\\):\n")
(defun geiser-mit--startup (remote)
  (let ((geiser-log-verbose-p t))
    (compilation-setup t)
    (when (and (stringp geiser-mit-source-directory)
               (not (string-empty-p geiser-mit-source-directory)))
      (geiser-eval--send/wait (format "(geiser:set-mit-scheme-source-directory %S)" geiser-mit-source-directory)))))

;;; Implementation definition:

(define-geiser-implementation mit
  (binary geiser-mit--binary)
  (arglist geiser-mit--parameters)
  (version-command geiser-mit--version)
  (minimum-version geiser-mit-minimum-version)
  (repl-startup geiser-mit--startup)
  (prompt-regexp geiser-mit--prompt-regexp)
  (debugger-prompt-regexp geiser-mit--debugger-prompt-regexp)
  ;; (enter-debugger geiser-mit--enter-debugger)
  (marshall-procedure geiser-mit--geiser-procedure)
  (find-module geiser-mit--get-module)
  (enter-command geiser-mit--enter-command)
  (exit-command geiser-mit--exit-command)
  ;; (import-command geiser-mit--import-command)
  (find-symbol-begin geiser-mit--symbol-begin)
  ;; (display-error geiser-mit--display-error)
  ;; (external-help geiser-mit--manual-look-up)
  ;; (check-buffer geiser-mit--guess)
  ;; (keywords geiser-mit--keywords)
  ;; (case-sensitive geiser-mit-case-sensitive-p)
  )

(geiser-impl--add-to-alist 'regexp "\\.scm$" 'mit t)
(geiser-impl--add-to-alist 'regexp "\\.pkg$" 'mit t)

(provide 'geiser-mit)
