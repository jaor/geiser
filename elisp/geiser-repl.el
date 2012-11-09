;;; geiser-repl.el --- Geiser's REPL

;; Copyright (C) 2009, 2010, 2011, 2012 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.



(require 'geiser-company)
(require 'geiser-compile)
(require 'geiser-doc)
(require 'geiser-autodoc)
(require 'geiser-edit)
(require 'geiser-completion)
(require 'geiser-syntax)
(require 'geiser-impl)
(require 'geiser-eval)
(require 'geiser-connection)
(require 'geiser-menu)
(require 'geiser-image)
(require 'geiser-custom)
(require 'geiser-base)

(require 'comint)
(require 'compile)
(require 'scheme)


;;; Customization:

(defgroup geiser-repl nil
  "Interacting with the Geiser REPL."
  :group 'geiser)

(geiser-custom--defcustom geiser-repl-use-other-window t
  "Whether to Use a window other than the current buffer's when
switching to the Geiser REPL buffer."
  :type 'boolean
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-window-allow-split t
  "Whether to allow window splitting when switching to the Geiser
REPL buffer."
  :type 'boolean
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-history-filename
    (expand-file-name "~/.geiser_history")
  "File where REPL input history is saved, so that it persists between sessions.

This is actually the base name: the concrete Scheme
implementation name gets appended to it."
  :type 'filename
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-history-size comint-input-ring-size
  "Maximum size of the saved REPL input history."
  :type 'integer
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-history-no-dups-p t
   "Whether to skip duplicates when recording history."
   :type 'boolean
   :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-save-debugging-history-p nil
  "Whether to skip debugging input in REPL history.

By default, REPL interactions while scheme is in the debugger are
not added to the REPL command history.  Set this variable to t to
change that."
  :type 'boolean
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-autodoc-p t
  "Whether to enable `geiser-autodoc-mode' in the REPL by default."
  :type 'boolean
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-company-p t
  "Whether to use company-mode for completion, if available."
  :group 'geiser-mode
  :type 'boolean)

(geiser-custom--defcustom geiser-repl-read-only-prompt-p t
  "Whether the REPL's prompt should be read-only."
  :type 'boolean
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-auto-indent-p t
  "Whether newlines for incomplete sexps are autoindented."
  :type 'boolean
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-forget-old-errors-p t
  "Whether to forget old errors upon entering a new expression.

When on (the default), every time a new expression is entered in
the REPL old error messages are flushed, and using \\[next-error]
afterwards will jump only to error locations produced by the new
expression, if any."
  :type 'boolean
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-query-on-exit-p nil
  "Whether to prompt for confirmation on \\[geiser-repl-exit]."
  :type 'boolean
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-default-host "localhost"
  "Default host when connecting to remote REPLs."
  :type 'string
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-default-port 37146
  "Default port for connecting to remote REPLs."
  :type 'integer
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-startup-time 10000
  "Time, in milliseconds, to wait for Racket to startup.
If you have a slow system, try to increase this time."
  :type 'integer
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-inline-images-p t
  "Whether to display inline images in the REPL."
  :type 'boolean
  :group 'geiser-repl)

(geiser-custom--defcustom geiser-repl-auto-display-images-p t
  "Whether to automatically invoke the external viewer to display
images popping up in the REPL.

See also `geiser-debug-auto-display-images-p'."
  :type 'boolean
  :group 'geiser-repl)

(geiser-custom--defface repl-input
  'comint-highlight-input geiser-repl "evaluated input highlighting")

(geiser-custom--defface repl-prompt
  'comint-highlight-prompt geiser-repl "REPL prompt")



;;; Implementation-dependent parameters

(geiser-impl--define-caller geiser-repl--binary binary ()
  "A variable or function returning the path to the scheme binary
for this implementation.")

(geiser-impl--define-caller geiser-repl--arglist arglist ()
  "A function taking no arguments and returning a list of
arguments to be used when invoking the scheme binary.")

(geiser-impl--define-caller geiser-repl--prompt-regexp prompt-regexp ()
  "A variable (or thunk returning a value) giving the regular
expression for this implementation's geiser scheme prompt.")

(geiser-impl--define-caller
    geiser-repl--debugger-prompt-regexp debugger-prompt-regexp ()
  "A variable (or thunk returning a value) giving the regular
expression for this implementation's debugging prompt.")

(geiser-impl--define-caller geiser-repl--startup repl-startup (remote)
  "Function taking no parameters that is called after the REPL
has been initialised. All Geiser functionality is available to
you at that point.")

(geiser-impl--define-caller geiser-repl--enter-cmd enter-command (module)
  "Function taking a module designator and returning a REPL enter
module command as a string")

(geiser-impl--define-caller geiser-repl--import-cmd import-command (module)
  "Function taking a module designator and returning a REPL import
module command as a string")

(geiser-impl--define-caller geiser-repl--exit-cmd exit-command ()
  "Function returning the REPL exit command as a string")


;;; Geiser REPL buffers and processes:

(defvar geiser-repl--repls nil)
(defvar geiser-repl--closed-repls nil)

(make-variable-buffer-local
 (defvar geiser-repl--repl nil))

(defsubst geiser-repl--set-this-buffer-repl (r)
  (setq geiser-repl--repl r))

(defun geiser-repl--live-p ()
  (and geiser-repl--repl
       (get-buffer-process geiser-repl--repl)))

(defun geiser-repl--repl/impl (impl &optional repls)
  (catch 'repl
    (dolist (repl (or repls geiser-repl--repls))
      (when (buffer-live-p repl)
        (with-current-buffer repl
          (when (eq geiser-impl--implementation impl)
            (throw 'repl repl)))))))

(defun geiser-repl--set-up-repl (impl)
  (or (and (not impl) geiser-repl--repl)
      (setq geiser-repl--repl
            (let ((impl (or impl
                            geiser-impl--implementation
                            (geiser-impl--guess))))
              (when impl (geiser-repl--repl/impl impl))))))

(defun geiser-repl--active-impls ()
  (let ((act))
    (dolist (repl geiser-repl--repls act)
      (with-current-buffer repl
        (add-to-list 'act geiser-impl--implementation)))))

(defsubst geiser-repl--repl-name (impl)
  (format "%s REPL" (geiser-impl--impl-str impl)))

(defsubst geiser-repl--buffer-name (impl)
  (format "* %s *" (geiser-repl--repl-name impl)))

(defun geiser-repl--switch-to-buffer (buffer)
  (unless (eq buffer (current-buffer))
    (let ((pop-up-windows geiser-repl-window-allow-split))
      (if geiser-repl-use-other-window
          (switch-to-buffer-other-window buffer)
        (switch-to-buffer buffer)))))

(defun geiser-repl--to-repl-buffer (impl)
  (unless (and (eq major-mode 'geiser-repl-mode)
               (eq geiser-impl--implementation impl)
               (not (get-buffer-process (current-buffer))))
    (let* ((old (geiser-repl--repl/impl impl geiser-repl--closed-repls))
           (old (and (buffer-live-p old)
                     (not (get-buffer-process old))
                     old)))
      (geiser-repl--switch-to-buffer
       (or old (generate-new-buffer (geiser-repl--buffer-name impl))))
      (unless old
        (geiser-repl-mode)
        (geiser-impl--set-buffer-implementation impl)))))

(defun geiser-repl--read-impl (prompt &optional active)
  (geiser-impl--read-impl prompt (and active (geiser-repl--active-impls))))

(defsubst geiser-repl--only-impl-p ()
  (and (null (cdr geiser-active-implementations))
       (car geiser-active-implementations)))

(defun geiser-repl--get-impl (prompt)
  (or (geiser-repl--only-impl-p)
      (and (eq major-mode 'geiser-repl-mode) geiser-impl--implementation)
      (geiser-repl--read-impl prompt)))


;;; REPL connections

(make-variable-buffer-local
 (defvar geiser-repl--address nil))

(make-variable-buffer-local
 (defvar geiser-repl--connection nil))

(defun geiser-repl--remote-p () geiser-repl--address)

(defsubst geiser-repl--host () (car geiser-repl--address))
(defsubst geiser-repl--port () (cdr geiser-repl--address))

(defun geiser-repl--read-address (&optional host port)
  (let ((defhost (or (geiser-repl--host) geiser-repl-default-host))
        (defport (or (geiser-repl--port) geiser-repl-default-port)))
    (cons (or host
              (read-string (format "Host (default %s): " defhost)
                           nil nil defhost))
          (or port (read-number "Port: " defport)))))

(defun geiser-repl--autodoc-mode (n)
  (when (or geiser-repl-autodoc-p (< n 0))
    (geiser--save-msg (geiser-autodoc-mode n))))

(defun geiser-repl--save-remote-data (address)
  (setq geiser-repl--address address)
  (setq header-line-format (and address
                                (format "Host: %s   Port: %s"
                                        (geiser-repl--host)
                                        (geiser-repl--port)))))

(defun geiser-repl--output-filter (txt)
  (geiser-con--connection-update-debugging geiser-repl--connection txt)
  (geiser-image--replace-images geiser-repl-inline-images-p
                                geiser-repl-auto-display-images-p)
  (when (string-match-p (geiser-con--connection-prompt geiser-repl--connection)
                        txt)
    (geiser-autodoc--disinhibit-autodoc)))

(defun geiser-repl--start-repl (impl address)
  (message "Starting Geiser REPL for %s ..." impl)
  (geiser-repl--to-repl-buffer impl)
  (sit-for 0)
  (goto-char (point-max))
  (geiser-repl--autodoc-mode -1)
  (let* ((prompt-rx (geiser-repl--prompt-regexp impl))
         (deb-prompt-rx (geiser-repl--debugger-prompt-regexp impl))
         (prompt (geiser-con--combined-prompt prompt-rx deb-prompt-rx)))
    (unless prompt-rx
      (error "Sorry, I don't know how to start a REPL for %s" impl))
    (geiser-repl--save-remote-data address)
    (geiser-repl--start-scheme impl address prompt)
    (geiser-repl--quit-setup)
    (geiser-repl--history-setup)
    (add-to-list 'geiser-repl--repls (current-buffer))
    (geiser-repl--set-this-buffer-repl (current-buffer))
    (setq geiser-repl--connection
          (geiser-con--make-connection (get-buffer-process (current-buffer))
                                       prompt-rx
                                       deb-prompt-rx))
    (geiser-repl--startup impl address)
    (geiser-repl--autodoc-mode 1)
    (geiser-company--setup geiser-repl-company-p)
    (add-hook 'comint-output-filter-functions
              'geiser-repl--output-filter
              nil
              t)
    (message "%s up and running!" (geiser-repl--repl-name impl))))

(defun geiser-repl--start-scheme (impl address prompt)
  (setq comint-prompt-regexp prompt)
  (let* ((name (geiser-repl--repl-name impl))
         (buff (current-buffer))
         (args (if address (list address)
                 `(,(geiser-repl--binary impl)
                   nil
                   ,@(geiser-repl--arglist impl)))))
    (condition-case err
        (apply 'make-comint-in-buffer `(,name ,buff ,@args))
      (error (insert "Unable to start REPL:\n"
                     (error-message-string err)
                     "\n")
             (error "Couldn't start Geiser")))
    (geiser-repl--wait-for-prompt geiser-repl-startup-time)))

(defun geiser-repl--wait-for-prompt (timeout)
  (let ((p (point)) (seen) (buffer (current-buffer)))
    (while (and (not seen)
                (> timeout 0)
                (get-buffer-process buffer))
      (sleep-for 0.1)
      (setq timeout (- timeout 100))
      (goto-char p)
      (setq seen (re-search-forward comint-prompt-regexp nil t)))
    (goto-char (point-max))
    (unless seen (error "%s" "No prompt found!"))))

(defun geiser-repl--is-debugging ()
  (let ((dp (geiser-con--connection-debug-prompt geiser-repl--connection)))
    (and dp
         comint-last-prompt-overlay
         (save-excursion
           (goto-char (overlay-start comint-last-prompt-overlay))
           (re-search-forward dp
                              (overlay-end comint-last-prompt-overlay)
                              t)))))

(defun geiser-repl--connection ()
  (let ((buffer (geiser-repl--set-up-repl geiser-impl--implementation)))
    (or (and (buffer-live-p buffer)
             (get-buffer-process buffer)
             (with-current-buffer buffer geiser-repl--connection))
        (error "No Geiser REPL for this buffer (try M-x run-geiser)"))))

(setq geiser-eval--default-connection-function 'geiser-repl--connection)

(defun geiser-repl--prepare-send ()
  (geiser-autodoc--inhibit-autodoc)
  (geiser-con--connection-deactivate geiser-repl--connection))

(defun geiser-repl--send (cmd)
  (when (and cmd (eq major-mode 'geiser-repl-mode))
    (geiser-repl--prepare-send)
    (goto-char (point-max))
    (comint-kill-input)
    (insert cmd)
    (let ((comint-input-filter (lambda (x) nil)))
      (comint-send-input nil t))))


;;; REPL history

(defconst geiser-repl--history-separator "\n\0\n")

(defsubst geiser-repl--history-file ()
  (format "%s.%s" geiser-repl-history-filename geiser-impl--implementation))

(defun geiser-repl--read-input-ring ()
  (let ((comint-input-ring-file-name (geiser-repl--history-file))
        (comint-input-ring-separator geiser-repl--history-separator))
    (comint-read-input-ring t)))

(defun geiser-repl--write-input-ring ()
  (let ((comint-input-ring-file-name (geiser-repl--history-file))
        (comint-input-ring-separator geiser-repl--history-separator))
    (comint-write-input-ring)))

(defun geiser-repl--history-setup ()
  (set (make-local-variable 'comint-input-ring-size) geiser-repl-history-size)
  (set (make-local-variable 'comint-input-filter) 'geiser-repl--input-filter)
  (geiser-repl--read-input-ring))


;;; Cleaning up

(defun geiser-repl--clear-buffer ()
  "Delete the output generated by the scheme process."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (geiser-repl--last-prompt-start))
    (when (< (point) (geiser-repl--last-prompt-end))
      (goto-char (geiser-repl--last-prompt-end)))
    (recenter t)))


;;; Cleaning up on quit

(defun geiser-repl--on-quit ()
  (geiser-repl--write-input-ring)
  (let ((cb (current-buffer))
        (impl geiser-impl--implementation)
        (comint-prompt-read-only nil))
    (geiser-con--connection-deactivate geiser-repl--connection t)
    (geiser-con--connection-close geiser-repl--connection)
    (setq geiser-repl--repls (remove cb geiser-repl--repls))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (eq geiser-impl--implementation impl)
                     (equal cb geiser-repl--repl))
            (geiser-repl--set-up-repl geiser-impl--implementation)))))))

(defun geiser-repl--sentinel (proc event)
  (let ((pb (process-buffer proc)))
    (when (buffer-live-p pb)
      (with-current-buffer pb
        (let ((comint-prompt-read-only nil)
              (comint-input-ring-file-name (geiser-repl--history-file))
              (comint-input-ring-separator geiser-repl--history-separator))
          (geiser-repl--on-quit)
          (push pb geiser-repl--closed-repls)
          (goto-char (point-max))
          (comint-kill-region comint-last-input-start (point))
          (insert "\nIt's been nice interacting with you!\n")
          (insert "Press C-c C-z to bring me back.\n" ))))))

(defun geiser-repl--on-kill ()
  (geiser-repl--on-quit)
  (setq geiser-repl--closed-repls
        (remove (current-buffer) geiser-repl--closed-repls)))

(defun geiser-repl--input-filter (str)
  (not (or (and (not geiser-repl-save-debugging-history-p)
                (geiser-repl--is-debugging))
           (string-match "^\\s *$" str)
           (string-match "^,quit *$" str))))

(defun geiser-repl--old-input ()
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun geiser-repl--quit-setup ()
  (add-hook 'kill-buffer-hook 'geiser-repl--on-kill nil t)
  (set-process-sentinel (get-buffer-process (current-buffer))
                        'geiser-repl--sentinel))


;;; geiser-repl mode:

(defun geiser-repl--bol ()
  (interactive)
  (when (= (point) (comint-bol)) (beginning-of-line)))

(defun geiser-repl--beginning-of-defun ()
  (save-restriction
    (when comint-last-prompt-overlay
      (narrow-to-region (overlay-end comint-last-prompt-overlay) (point)))
    (let ((beginning-of-defun-function nil))
      (beginning-of-defun))))

(defun geiser-repl--module-function (&optional module)
  (if (and module geiser-eval--get-impl-module)
      (funcall geiser-eval--get-impl-module module)
      :f))

(defun geiser-repl--doc-module ()
  (interactive)
  (let ((geiser-eval--get-module-function
         (geiser-impl--method 'find-module geiser-impl--implementation)))
    (geiser-doc-module)))

(defun geiser-repl--newline-and-indent ()
  (interactive)
  (save-restriction
    (narrow-to-region comint-last-input-start (point-max))
    (insert "\n")
    (lisp-indent-line)))

(defun geiser-repl--last-prompt-end ()
  (if comint-last-prompt-overlay
      (overlay-end comint-last-prompt-overlay)
    (save-excursion (geiser-repl--bol) (point))))

(defun geiser-repl--last-prompt-start ()
  (if comint-last-prompt-overlay
      (overlay-start comint-last-prompt-overlay)
    (save-excursion (geiser-repl--bol) (point))))

(defun geiser-repl--nesting-level ()
  (save-restriction
    (narrow-to-region (geiser-repl--last-prompt-end) (point-max))
    (geiser-syntax--nesting-level)))

(defun geiser-repl--send-input ()
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (and proc (process-mark proc)))
         (intxt (and pmark (buffer-substring pmark (point)))))
    (when intxt
      (and geiser-repl-forget-old-errors-p
           (not (geiser-repl--is-debugging))
           (compilation-forget-errors))
      (geiser-repl--prepare-send)
      (comint-send-input)
      (when (string-match "^\\s-*$" intxt)
        (comint-send-string proc (geiser-eval--scheme-str '(:ge no-values)))
        (comint-send-string proc "\n")))))

(defun geiser-repl--maybe-send ()
  (interactive)
  (let ((p (point)))
    (cond ((< p (geiser-repl--last-prompt-start))
           (ignore-errors (compile-goto-error)))
          ((progn (end-of-line) (<= (geiser-repl--nesting-level) 0))
           (geiser-repl--send-input))
          (t (goto-char p)
             (if geiser-repl-auto-indent-p
                 (geiser-repl--newline-and-indent)
               (insert "\n"))))))

(defun geiser-repl-tab-dwim (n)
  "If we're after the last prompt, complete symbol or indent (if
there's no symbol at point). Otherwise, go to next error in the REPL
buffer."
  (interactive "p")
  (if (>= (point) (geiser-repl--last-prompt-end))
      (or (completion-at-point)
          (lisp-indent-line))
    (compilation-next-error n)))

(defun geiser-repl--previous-error (n)
  "Go to previous error in the REPL buffer."
  (interactive "p")
  (compilation-next-error (- n)))


(define-derived-mode geiser-repl-mode comint-mode "REPL"
  "Major mode for interacting with an inferior scheme repl process.
\\{geiser-repl-mode-map}"
  (scheme-mode-variables)
  (set (make-local-variable 'face-remapping-alist)
       '((comint-highlight-prompt geiser-font-lock-repl-prompt)
         (comint-highlight-input geiser-font-lock-repl-input)))
  (set (make-local-variable 'mode-line-process) nil)
  (set (make-local-variable 'comint-use-prompt-regexp) t)
  (set (make-local-variable 'comint-prompt-read-only)
       geiser-repl-read-only-prompt-p)
  (set (make-local-variable 'beginning-of-defun-function)
       'geiser-repl--beginning-of-defun)
  (set (make-local-variable 'comint-input-ignoredups)
       geiser-repl-history-no-dups-p)
  (setq geiser-eval--get-module-function 'geiser-repl--module-function)
  (geiser-completion--setup t)
  (setq geiser-smart-tab-mode-string "")
  (geiser-smart-tab-mode t)
  ;; enabling compilation-shell-minor-mode without the annoying highlighter
  (compilation-setup t))

(define-key geiser-repl-mode-map "\C-d" 'delete-char)
(define-key geiser-repl-mode-map "\C-m" 'geiser-repl--maybe-send)
(define-key geiser-repl-mode-map [return] 'geiser-repl--maybe-send)
(define-key geiser-repl-mode-map "\C-j" 'geiser-repl--newline-and-indent)
(define-key geiser-repl-mode-map (kbd "TAB") 'geiser-repl-tab-dwim)
(define-key geiser-repl-mode-map [backtab] 'geiser-repl--previous-error)

(define-key geiser-repl-mode-map "\C-a" 'geiser-repl--bol)
(define-key geiser-repl-mode-map (kbd "<home>") 'geiser-repl--bol)

(define-key geiser-repl-mode-map "\C-c\M-o" 'geiser-repl--clear-buffer)

(geiser-menu--defmenu repl geiser-repl-mode-map
  ("Complete symbol" ((kbd "M-TAB"))
   completion-at-point :enable (geiser--symbol-at-point))
  ("Complete module name" ((kbd "C-.") (kbd "M-`"))
   geiser-completion--complete-module :enable (geiser--symbol-at-point))
  ("Edit symbol" "\M-." geiser-edit-symbol-at-point
   :enable (geiser--symbol-at-point))
  --
  ("Switch to module..." "\C-c\C-m" switch-to-geiser-module)
  ("Import module..." "\C-c\C-i" geiser-repl-import-module)
  ("Add to load path..." "\C-c\C-r" geiser-add-to-load-path)
  --
  ("Previous matching input" "\M-p" comint-previous-matching-input-from-input
   "Previous input matching current")
  ("Next matching input" "\M-n" comint-next-matching-input-from-input
   "Next input matching current")
  ("Previous input" "\C-c\M-p" comint-previous-input)
  ("Next input" "\C-c\M-n" comint-next-input)
  --
  (mode "Autodoc mode" ("\C-c\C-da" "\C-c\C-d\C-a") geiser-autodoc-mode)
  ("Symbol documentation" ("\C-c\C-dd" "\C-c\C-d\C-d")
   geiser-doc-symbol-at-point
   "Documentation for symbol at point" :enable (geiser--symbol-at-point))
  ("Lookup symbol in manul" ("\C-c\C-di" "\C-c\C-d\C-i")
   geiser-doc-look-up-manual
   "Documentation for symbol at point" :enable (geiser--symbol-at-point))
  ("Module documentation" ("\C-c\C-dm" "\C-c\C-d\C-m") geiser-repl--doc-module
   "Documentation for module at point" :enable (geiser--symbol-at-point))
  --
  ("Kill Scheme interpreter" "\C-c\C-q" geiser-repl-exit
   :enable (geiser-repl--live-p))
  ("Restart" "\C-c\C-z" switch-to-geiser :enable (not (geiser-repl--live-p)))
  --
  (custom "REPL options" geiser-repl))

(define-key geiser-repl-mode-map [menu-bar completion] 'undefined)


;;; User commands

(defun run-geiser (impl)
  "Start a new Geiser REPL."
  (interactive
   (list (geiser-repl--get-impl "Start Geiser for scheme implementation: ")))
  (let ((buffer (current-buffer)))
    (geiser-repl--start-repl impl nil)
    (geiser-repl--maybe-remember-scm-buffer buffer)))

(defalias 'geiser 'run-geiser)

(defun geiser-connect (impl &optional host port)
  "Start a new Geiser REPL connected to a remote Scheme process."
  (interactive
   (list (geiser-repl--get-impl "Connect to Scheme implementation: ")))
  (let ((buffer (current-buffer)))
    (geiser-repl--start-repl impl
                             (geiser-repl--read-address host port))
    (geiser-repl--maybe-remember-scm-buffer buffer)))

(make-variable-buffer-local
 (defvar geiser-repl--last-scm-buffer nil))

(defun geiser-repl--maybe-remember-scm-buffer (buffer)
  (when (and buffer
             (eq 'scheme-mode (with-current-buffer buffer major-mode))
             (eq major-mode 'geiser-repl-mode))
    (setq geiser-repl--last-scm-buffer buffer)))

(defun switch-to-geiser (&optional ask impl buffer)
  "Switch to running Geiser REPL.
With prefix argument, ask for which one if more than one is running.
If no REPL is running, execute `run-geiser' to start a fresh one."
  (interactive "P")
  (let* ((impl (or impl geiser-impl--implementation))
         (in-repl (eq major-mode 'geiser-repl-mode))
         (in-live-repl (and in-repl (get-buffer-process (current-buffer))))
         (repl (cond ((and (not ask)
                           (not impl)
                           (not in-repl)
                           (or geiser-repl--repl (car geiser-repl--repls))))
                     ((and (not ask)
                           (not in-repl)
                           impl
                           (geiser-repl--repl/impl impl))))))
    (cond ((or in-live-repl
               (and (eq (current-buffer) repl) (not (eq repl buffer))))
           (when (buffer-live-p geiser-repl--last-scm-buffer)
             (geiser-repl--switch-to-buffer geiser-repl--last-scm-buffer)))
          (repl (geiser-repl--switch-to-buffer repl))
          ((geiser-repl--remote-p) (geiser-connect impl))
          (t (run-geiser impl)))
    (geiser-repl--maybe-remember-scm-buffer buffer)))

(defun switch-to-geiser-module (&optional module buffer)
  "Switch to running Geiser REPL and try to enter a given module."
  (interactive)
  (let* ((module (or module
                     (geiser-completion--read-module
                      "Switch to module (default top-level): ")))
         (cmd (and module
                   (geiser-repl--enter-cmd geiser-impl--implementation
                                           module))))
    (unless (eq major-mode 'geiser-repl-mode)
      (switch-to-geiser nil nil (or buffer (current-buffer))))
    (geiser-repl--send cmd)))

(defun geiser-repl-import-module (&optional module)
  "Import a given module in the current namespace of the REPL."
  (interactive)
  (let* ((module (or module
                     (geiser-completion--read-module "Import module: ")))
         (cmd (and module
                   (geiser-repl--import-cmd geiser-impl--implementation
                                            module))))
    (switch-to-geiser nil nil (current-buffer))
    (geiser-repl--send cmd)))

(defun geiser-repl-exit (&optional arg)
  "Exit the current REPL.
With a prefix argument, force exit by killing the scheme process."
  (interactive "P")
  (when (or (not geiser-repl-query-on-exit-p)
            (y-or-n-p "Really quit this REPL? "))
    (geiser-con--connection-deactivate geiser-repl--connection t)
    (let ((cmd (and (not arg)
                    (geiser-repl--exit-cmd geiser-impl--implementation))))
      (if cmd
          (when (stringp cmd) (geiser-repl--send cmd))
        (comint-kill-subjob)))))


;;; Unload:

(defun geiser-repl--repl-list ()
  (let (lst)
    (dolist (repl geiser-repl--repls lst)
      (when (buffer-live-p repl)
        (with-current-buffer repl
          (push (cons geiser-impl--implementation
                      geiser-repl--address)
                lst))))))

(defun geiser-repl--restore (impls)
  (dolist (impl impls)
    (when impl
      (condition-case err
          (geiser-repl--start-repl (car impl) (cdr impl))
        (error (message (error-message-string err)))))))

(defun geiser-repl-unload-function ()
  (dolist (repl geiser-repl--repls)
    (when (buffer-live-p repl)
      (with-current-buffer repl
        (let ((geiser-repl-query-on-exit-p nil)) (geiser-repl-exit))
        (sit-for 0.05)
        (kill-buffer)))))


(provide 'geiser-repl)
