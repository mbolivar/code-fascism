;; code-fascism.el

;; Author: Marti Bolivar, mbolivar@leaflabs.com
;; Version: 0.04
;;
;; A simple minor mode for automating extraneous whitespace removal
;; and untabification.
;;
;; To enable it, just do the usual:
;;
;; (code-fascism-mode 1)
;;
;; You can toggle it with M-x code-fascism-mode.
;;
;; If a buffer is in code-fascism-mode, tab characters and whitespace
;; at the end of lines are highlighted in the face
;; `trailing-whitespace' (the default is an ugly red).
;;
;; Provides the function `code-fascism-whitespace', which, untabifies
;; and removes trailing whitespace from the current buffer.
;;
;; This happens automatically when you save a buffer in code-fascism
;; mode if the variable `code-fascism-run-at-save' is non-nil (this is
;; the default).
;;
;; If `code-fascism-run-at-exit' is non-nil (the default), then upon
;; emacs exit, this process is carried out on all buffers with
;; code-fascism-mode enabled.

;; Version history
;;  0.01: Initial version
;;  0.02: If point is at end-of-line, don't delete trailing whitespace from
;;        there.
;;  0.03: Made a code fascism minor mode so buffer-specific things can be
;;        toggled easily.
;;  0.04: Fixed an embarrassing bug where untabify didn't always get
;;        called.  Removed the behavior where the user would be
;;        queried at Emacs exit about whether or not to run code
;;        fascism if `code-fascism-run-at-exit' was nil, as it was
;;        annoying.  Miscellaneous other cleanups.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup code-fascism nil
  "Because tabs and trailing whitespace are evil."
  :version 0.04
  :group 'programming)

(defcustom code-fascism-run-at-save t
  "Whether or not to run code fascism when buffers are saved.

If set to \"Yes\", code fascism gets run whenever a buffer with
code-fascism-mode enabled is saved."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :group 'code-fascism)

(defcustom code-fascism-run-at-exit t
  "Whether or not to run code fascism at Emacs exit.

If set to \"No\", the user will be queried upon Emacs exit if code
fascism is to be run."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :group 'code-fascism)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun code-fascism-whitespace ()
  "Strips trailing whitespace, and untabifies."
  (interactive)
  (save-excursion
      ;; if point was at the end of the line when we started,
      (if (= (point) (point-at-eol))
          ;; don't remove trailing whitespace on that line, because
          ;; it's being edited and that's annoying.
          (progn
            (code-fascism-delete-trailing-whitespace (point-min)
                                                     (point-at-bol))
            (code-fascism-delete-trailing-whitespace (point-at-eol)
                                                     (point-max)))
        ;; otherwise, delete all the trailing whitespace
        (code-fascism-delete-trailing-whitespace (point-min) (point-max)))
      (message "untabifying...")
      (untabify (point-min) (point-max)))
  ;; Return nil so write-contents-hooks keeps going.
  nil)

;; Essentially stolen wholesale from `delete-trailing-whitespace' in
;; simple.el; the only modification was to allow start and end point
;; positions.
(defun code-fascism-delete-trailing-whitespace (start end)
  "Delete all the trailing whitespace across the current buffer,
beginning at point position `start', and not continuing past
`end'. All whitespace after the last non-whitespace character in
a line is deleted.  This respects narrowing, created by
\\[narrow-to-region] and friends.  A formfeed is not considered
whitespace by this function."
  (save-match-data
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\s-$" end t)
        (skip-syntax-backward "-" (save-excursion (forward-line 0) (point)))
        ;; Don't delete formfeeds, even if they are considered whitespace.
        (save-match-data
          (if (looking-at ".*\f")
              (goto-char (match-end 0))))
        (delete-region (point) (match-end 0))))))

(defun code-fascism-whitespace-on-save-hook ()
  (if (and code-fascism-mode code-fascism-run-at-save)
      (code-fascism-whitespace)))

(defun code-fascism-global-correct-whitespace ()
  ;; Do whitespace fascism and save on all open buffers with
  ;; code-fascism-mode enabled.  Only tries to save buffers that are
  ;; associated with files.
  (dolist (buf (buffer-list) t)
    (with-current-buffer buf
      (if (and code-fascism-mode buffer-file-name)
          (progn (code-fascism-whitespace)
                 (save-buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Minor Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode
  code-fascism-mode
  "Toggle Code Fascism mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode."
  nil
  " Fascism"
  '()
  :group 'programming
  (if code-fascism-mode
      (progn
        ;; highlight trailing whitespace
        (setq show-trailing-whitespace t)
        ;; highlight tabs
        (font-lock-add-keywords nil '(("[\t]+" 0 'trailing-whitespace t)))
        ;; tell the current buffer's write-contents-hooks to do fascism on
        ;; save, if we want to.  The optional 'nil t' arguments mean "don't
        ;; append this hook to the end of the list of hooks, but do make
        ;; this new hook buffer-local".
        (add-hook 'write-contents-functions
                  'code-fascism-whitespace-on-save-hook
                  nil t))
    (progn
      (setq show-trailing-whitespace nil)
      (font-lock-remove-keywords nil
                                 '(("[\t]+" 0 'trailing-whitespace t))))))

;;;;;;;;;;;;;;; `kill-emacs-query-functions' munging ;;;;;;;;;;;;;;;;;

(defun code-fascism-maybe-run-at-exit ()
  "If there are open buffers with in code-fascism-mode and
`code-fascism-run-at-exit' is non-nil, cleans up whitespace in
those buffers with `code-fascism-global-correct-whitespace'."
  (if (and (delq nil
                 (mapcar (lambda (b)
                           (with-current-buffer b code-fascism-mode))
                         (buffer-list)))
           code-fascism-run-at-exit)
      (code-fascism-global-correct-whitespace))
  t)
(add-hook 'kill-emacs-query-functions 'code-fascism-maybe-run-at-exit)

(provide 'code-fascism)
