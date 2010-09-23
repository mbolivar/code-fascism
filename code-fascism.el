;; code-fascism.el

;; Author: Marti Bolivar, mbolivar@mit.edu

;; Version history
;;  0.01: Initial version
;;  0.02: If point is at end-of-line, don't delete trailing whitespace from
;;        there.
;;  0.03: Made a code fascism minor mode so buffer-specific things can be
;;        toggled easily.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup code-fascism nil
  "Because tabs and trailing whitespace are evil."
  :version 0.03
  :group 'programming)

(defcustom code-fascism-run-at-save nil
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
          ;; don't remove trailing whitespace on that line, because it's
          ;; probably being edited.
          (progn
            (code-fascism-delete-trailing-whitespace (point-min)
                                                     (point-at-bol))
            (code-fascism-delete-trailing-whitespace (point-at-eol)
                                                     (point-max)))
        ;; otherwise, delete all the trailing whitespace
        (code-fascism-delete-trailing-whitespace (point-min) (point-max))
        (untabify (point-min) (point-max)))))

;; Essentially stolen wholesale from delete-trailing-whitespace in simple.el.
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

(defun code-fascism-whitespace-on-save ()
  (interactive)
  (if (and code-fascism-mode code-fascism-run-at-save)
      (code-fascism-whitespace)))

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
        ;; tell the current buffer's write-contents-hooks to do fascism on
        ;; save, if we want to.  The optional 'nil t' arguments mean "don't
        ;; append this hook to the end of the list of hooks, but do make
        ;; this new hook buffer-local".
        (add-hook 'write-contents-hooks
                  'code-fascism-whitespace-on-save
                  nil t))
    (progn
      (setq show-trailing-whitespace nil))))

;;;;;;;;;;;;;;; `kill-emacs-query-functions' munging ;;;;;;;;;;;;;;;;;

(defun code-fascism-global-correct-whitespace ()
  "Do whitespace fascism and save on all open buffers with
 code-fascism-mode enabled.  Only tries to save buffers that are
 associated with files."
  (dolist (buf (buffer-list) t)
    (with-current-buffer buf
      (if code-fascism-mode
          (progn (code-fascism-whitespace)
                 (if buffer-file-name (save-buffer)))))))

;; if we're told to, run whitespace removal without being asked; otherwise,
;; ask the user if we should run whitespace fascism before exit.
(add-hook 'kill-emacs-query-functions
          '(lambda ()
             (if (or code-fascism-run-at-exit
                     (yes-or-no-p "Run whitespace code fascism? "))
                 (code-fascism-global-correct-whitespace))
             t))

(provide 'code-fascism)
