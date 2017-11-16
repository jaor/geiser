;; geiser-tiny.el -- Tinyscheme's implementation of the geiser protocols

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

(defgroup geiser-tiny nil
  "Customization for Geiser's Tinyscheme flavour."
  :group 'geiser)

(geiser-custom--defcustom geiser-tiny-binary
    "tinyscheme"
  "Name to use to call the Tinyscheme executable when starting a REPL."
  :type '(choice string (repeat string))
  :group 'geiser-tiny)


;;; REPL support:

(defun geiser-tiny--binary ()
  (if (listp geiser-tiny-binary)
      (car geiser-tiny-binary)
    geiser-tiny-binary))

(defun geiser-tiny--parameters ()
  "Return a list with all parameters needed to start Tinyscheme.
This function uses `geiser-tiny-init-file' if it exists."
  ;; `("-1" ,(expand-file-name "tiny/geiser/geiser.scm" geiser-scheme-dir))
  nil
  )

(defconst geiser-tiny--prompt-regexp "ts> ")


;;; Evaluation support:

(defun geiser-tiny--geiser-procedure (proc &rest args)
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

(defun geiser-tiny--get-module (&optional module)
  (cond ((null module)
         :f)
        ((listp module) module)
        ((stringp module)
         (condition-case nil
             (car (geiser-syntax--read-from-string module))
           (error :f)))
        (t :f)))

(defun geiser-tiny--symbol-begin (module)
  ;; (if module
  ;;     (max (save-excursion (beginning-of-line) (point))
  ;;          (save-excursion (skip-syntax-backward "^(>") (1- (point))))
    (save-excursion (skip-syntax-backward "^'-()>") (point)))

(defun geiser-tiny--import-command (module)
  ;; (format "(import %s)" module)
  ""
  )

(defun geiser-tiny--exit-command () "(quit 0)")
;; 
;; ;;; REPL startup

(defconst geiser-tiny-minimum-version "1.31")

(defun geiser-tiny--version (binary)
  ;; (car (process-lines binary "--version"))
  "1.31"
  )

(defun geiser-tiny--startup (remote)
  (let ((geiser-log-verbose-p t))
    (compilation-setup t)
    ;; WSG: This is a hack around my not being able to load the file
    ;; and then run the REPL. To be fixed in the future.
    (geiser-eval--send/wait (format "(begin (load %S) (newline))"
				    (expand-file-name
				     "tiny/geiser/geiser.scm"
				     geiser-scheme-dir)))
    ))

;;; Implementation definition:

(define-geiser-implementation tiny
  (binary geiser-tiny--binary)
  (arglist geiser-tiny--parameters)
  (version-command geiser-tiny--version)
  (minimum-version geiser-tiny-minimum-version)
  (repl-startup geiser-tiny--startup)
  (prompt-regexp geiser-tiny--prompt-regexp)
  (debugger-prompt-regexp nil) ;; geiser-chez--debugger-prompt-regexp
  ;; (enter-debugger geiser-chez--enter-debugger)
  (marshall-procedure geiser-tiny--geiser-procedure)
  (find-module geiser-tiny--get-module)
  ;; (enter-command geiser-chez--enter-command)
  (exit-command geiser-tiny--exit-command)
  (import-command geiser-tiny--import-command)
  (find-symbol-begin geiser-tiny--symbol-begin)
  ;; (display-error geiser-chez--display-error)
  ;; (external-help geiser-chez--manual-look-up)
  ;; (check-buffer geiser-chez--guess)
  ;; (keywords geiser-chez--keywords)
  ;; (case-sensitive geiser-chez-case-sensitive-p)
  )

(geiser-impl--add-to-alist 'regexp "\\.scm$" 'tiny t)

(provide 'geiser-tiny)
