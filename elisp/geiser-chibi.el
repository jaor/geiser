;; geiser-chibi.el -- Chibi Scheme's implementation of the geiser protocols

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

(defgroup geiser-chibi nil
  "Customization for Geiser's Chibi Scheme flavour."
  :group 'geiser)

(geiser-custom--defcustom geiser-chibi-binary
    "chibi-scheme"
  "Name to use to call the Chibi Scheme executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-chibi)


;;; REPL support:

(defun geiser-chibi--binary ()
  (if (listp geiser-chibi-binary)
      (car geiser-chibi-binary)
    geiser-chibi-binary))

(defun geiser-chibi--parameters ()
  "Return a list with all parameters needed to start Chibi Scheme.
This function uses `geiser-chibi-init-file' if it exists."
  `("-I" ,(expand-file-name "chibi/geiser/" geiser-scheme-dir)
    "-m" "geiser")
  )

(defconst geiser-chibi--prompt-regexp "> ")


;;; Evaluation support:

(defun geiser-chibi--geiser-procedure (proc &rest args)
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

(defun geiser-chibi--get-module (&optional module)
  (cond ((null module)
         :f)
        ((listp module) module)
        ((stringp module)
         (condition-case nil
             (car (geiser-syntax--read-from-string module))
           (error :f)))
        (t :f)))

(defun geiser-chibi--symbol-begin (module)
  (if module
      (max (save-excursion (beginning-of-line) (point))
           (save-excursion (skip-syntax-backward "^(>") (1- (point))))
    (save-excursion (skip-syntax-backward "^'-()>") (point))))

(defun geiser-chibi--import-command (module)
  (format "(import %s)" module))

(defun geiser-chibi--exit-command () "(exit 0)")
;; 
;; ;;; REPL startup

(defconst geiser-chibi-minimum-version "0.7.3")

(defun geiser-chibi--version (binary)
  (second (split-string
           (car (process-lines binary "-V"))
           " ")))

(defun geiser-chibi--startup (remote)
  (let ((geiser-log-verbose-p t))
    (compilation-setup t)
    ))

;;; Implementation definition:

(define-geiser-implementation chibi
  (binary geiser-chibi--binary)
  (arglist geiser-chibi--parameters)
  (version-command geiser-chibi--version)
  (minimum-version geiser-chibi-minimum-version)
  (repl-startup geiser-chibi--startup)
  (prompt-regexp geiser-chibi--prompt-regexp)
  (debugger-prompt-regexp nil) ;; geiser-chibi--debugger-prompt-regexp
  ;; (enter-debugger geiser-chibi--enter-debugger)
  (marshall-procedure geiser-chibi--geiser-procedure)
  (find-module geiser-chibi--get-module)
  ;; (enter-command geiser-chibi--enter-command)
  (exit-command geiser-chibi--exit-command)
  (import-command geiser-chibi--import-command)
  (find-symbol-begin geiser-chibi--symbol-begin)
  ;; (display-error geiser-chibi--display-error)
  ;; (external-help geiser-chibi--manual-look-up)
  ;; (check-buffer geiser-chibi--guess)
  ;; (keywords geiser-chibi--keywords)
  ;; (case-sensitive geiser-chibi-case-sensitive-p)
  )

(geiser-impl--add-to-alist 'regexp "\\.scm$" 'chibi t)
(geiser-impl--add-to-alist 'regexp "\\.sld$" 'chibi t)

(provide 'geiser-chibi)

