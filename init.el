;;;;
;;;; ts4z emacs.el -- GNU Emacs flavor
;;;;

;;; global/general settings

(defconst at-linkedin (not (null (string-match "\.linkedin\." (system-name)))))
(defconst running-on-unix-p t)
(defconst running-as-root-p (and running-on-unix-p (eq 0 (user-uid)))
  "Am I running as root?")

(blink-cursor-mode 1)
;; this is default since Emacs 21.  Yay progress!
;; (condition-case nil (resize-minibuffer-mode 1) (error nil))
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map "\e\e" 'eval-expression) ; 18.54 forever
(define-key global-map [(control meta g)] 'keyboard-escape-quit)
(delete-selection-mode 1)
(global-font-lock-mode 1)
(global-unset-key [(control x) ?f])    ; I never do this, but I mistype C-x C-f
(global-unset-key [(control z)])       ; use C-x C-z if I must
(global-set-key "\C-c6d" 'base64-decode-region)
(global-set-key "\C-c6e" 'base64-encode-region)
(global-set-key [(control c) ?3]      'slice-window-horizontally)
(global-set-key [(control c) ?\;]     'comment-region)
(global-set-key [(control c) ?b]      'bury-buffer)
(global-set-key [(control c) ?c]      'compile)
(global-set-key [(control c) (shift d)] 'toggle-debug-on-error)
(global-set-key [(control c) ?g]      'goto-line)
(global-set-key [(control c) ?i]      'imenu)
(global-set-key [(control c) ?l]      'tjs-insert-local-variable-template)
(global-set-key [(control c) ?n]      'linum-mode)
(global-set-key [(control c) ?w]      'toggle-word-wrap)
(global-set-key [(control z)] 'undo) ;the universe has decided C-z is undo
(global-set-key [(meta control backspace)] 'backward-kill-sexp)
(global-subword-mode t)
;(ido-mode t)                           ;good idea, annoying impl quirks
(menu-bar-mode 0)                       ; I never do this, but try C-mouse 3
(mouse-wheel-mode 1)
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)    ; Does this still need to be enabled?
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(savehist-mode)
(set-variable 'enable-local-eval 'query)
(set-variable 'enable-recursive-minibuffers t)
(set-variable 'inhibit-startup-message t)
(set-variable 'version-control t)
(setq ido-enable-flex-matching t)
(setq line-move-visual nil)             ; old skool
(setq save-interprogram-paste-before-kill t)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq-default fill-column 79)
(show-paren-mode 1)
(tool-bar-mode 0)
(which-function-mode t)

;; GNU folks: You bastards. I tried to use set-variable.  It didn't work.
;; I couldn't figure out why.  Go fuck yourselves.
;; Everybody else: Look at $EMACS_SOURCE/lisp/startup.el.
(setq inhibit-startup-echo-area-message "tjs")
(setq inhibit-startup-echo-area-message "tshowalt")

;; bump up gc threshold.  as of 2013, it is 800k.  We can afford a little more.
(let ((big-number 4000000))
  (if (< gc-cons-threshold big-number)
      (setq gc-cons-threshold big-number)
    (run-with-idle-timer
     30 nil #'message
     "time to update the gc big-number in %s" user-init-file)))

;;; Environment

;; Work around broken terminals--or broken Emacs, whichever.
;; It's 2012, maybe this isn't needed anymore.
;; (when (and (boundp 'tty-erase-char) (eq tty-erase-char ?\C-h))
;;   (keyboard-translate ?\C-h ?\C-?))

(mapc (lambda (short-name)
	(let ((dir (expand-file-name short-name)))
	  (if (file-exists-p dir)
	      (setq load-path (cons dir load-path)))))
	(list "/usr/local/share/emacs/site-lisp/"
              "~/share/emacs/groovy"
	      "~/share/emacs/slime"
	      (concat (or (getenv "TJS_CVS")
			  (concat (getenv "HOME") "/cvs-tjs")) "/elisp/")))

(setq load-path (nconc (list "/usr/local/share/emacs/site-lisp/") load-path))

;; if I let custom do this stuff, it screws up on ttys.  It has never
;; worked quite right.  I state this in 2012.  Apparently I need to set backround to "light".

(condition-case nil
    (set-face-font 'default "DejaVu Sans Mono 9")
  (error nil))

;; on terminals, these colors are OK but Emacs does stupid things with
;; backgrounds.  try not to define them -- the defaults are nice
;;
;; enable rainbow-mode before messing with this -- it's neat.
(let ((white-background-color "white"))
  (set-face-foreground 'font-lock-builtin-face "#ff00ff")
  ;; (set-face-background 'default white-background-color)
  (set-face-attribute 'font-lock-comment-face nil :foreground "#005500"
                      ;; :background white-background-color
                      :slant 'italic)
  (set-face-attribute 'font-lock-doc-face nil :inherit 'font-lock-common-face
                      ;; :background white-background-color
                      :slant 'italic :foreground "brown")
  (set-face-attribute 'font-lock-function-name-face nil :foreground "red" :weight 'bold)
  (set-face-attribute 'font-lock-string-face nil
                      ;; :background white-background-color
                      :foreground "darkgreen"))


(defun other-window-previous (n &optional which-frames which-devices)
  "Select the COUNT'th different (previous) window on this frame.
Behaves exactly like `other-window', but acts in the opposite
direction."
  (interactive "p")
  (other-window (- n)))
(global-set-key "\C-cp" 'other-window-previous)

(defun find-tag-next ()
  "Find the next tag, ala `find-tag'.  C-u M-. is too hard to type."
  (interactive)
  (find-tag nil t))
;; I like this, but ESC ESC is a historic holy key binding and this conflicts.
;;(global-set-key [escape (meta ?.)] 'find-tag-next)
(global-set-key [(control ?.)] 'find-tag-next)

(defun fill-paragraph-unless-word-wrap-enabled (&optional justify region)
  "Fill, using `fill-paragraph', unless `word-wrap' is enabled."
  (interactive)
  (if word-wrap
      (message "Not filling while word-wrapping enabled.  OCD much?")
    (fill-paragraph justify region)))
(global-set-key [(meta ?q)] 'fill-paragraph-unless-word-wrap-enabled)

(defun slice-window-horizontally (w)
  "Split window into horizontal slices of roughly 80 (or argument)
columns.  More useful than `split-window-horizontally' on wide-screen
displays, where dividing by half is not that useful."
  (interactive "p")
  (let* ((ow (selected-window))
	 (nw ow)
	 ;; allow for scrollbars, etc; split-window-horizontally's
	 ;; input seems to be off by some number
	 ;;(fudge 2) ;; correct for XEmacs
	 ;; (fudge 7) ;; apparently correct for Emacs 21.4
	 (fudge 6) ;; apparently correct for Emacs 23.4 in my environment
	 (m  2))
    
    ;; if arg negative, we want the remainder as a seperate window
    ;; if arg positive, combine remainder with last windwo
    (and (< w 0)
	 (setq m 1)
	 (setq w (- w)))
    (if (or (< w 10) (null w))
	(setq w 80))
    
    ;; 7 is to make sure we don't try and split a painfully small window;
    ;; XEmacs gets upset.
    (while (and (> (window-width) (* m (+ fudge w)))
		(> (window-width) 7))
      (select-window (split-window-horizontally (+ fudge w))))
    (select-window ow)))

(defun kill-stupid-whitespace ()
  "Make lines of empty space into blank lines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]+$" nil t)
      (replace-match "" nil nil))))

;; from jwz: http://www.jwz.org/doc/tabs-vs-spaces.html
(defun untabify-buffer ()
  "Replace all indentation tabs with spaces in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
	(untabify (1- (point)) (point-max))))
  nil)

;; XXX this should work on the current region
(defun mangle-line-endings-to-lf ()
  "Mangle all CRLF-ended lines to LF."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      ;; Just convert everything to LF.  (I can't figure out how
      ;; to do LF->CRLF conversion in a regex, so I won't use
      ;; regexes to do this.  Yes, it ought to be easy, but
      ;; replacing blank lines (which match across multiple
      ;; matches) is hard!)
      (while (search-forward "\r\n" nil t)
        (replace-match "\n" nil t))
      t)))

(defun mangle-line-endings-to-crlf ()
  "Mangle all CRLF-ended lines to LF."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (while (re-search-forward "\\([^\r]\\)\n" nil t)
        (replace-match (concat (match-string 1) "\r\n") nil t))
      t)))

;; This is a hack, but not quite as huge a hack as when I first wrote it.
(defun tjs-insert-local-variable-template ()
  "tjs: insert the thing where local variables go at the bottom of the file."
  (interactive)
  (let (begin
        end
        edit-here)
    (push-mark)
    (goto-char (point-max))
    (setq begin (point))
    (insert "Local Variables:\n")

    ;; if the mode is not the default, it would be nice to note that;
    ;; C++ is the only major culprit, so this hack will do for now
    (when (eq major-mode 'c++-mode)
      (insert "mode: " (symbol-name major-mode) "\n"))

    ;; Insert local variables that may be interesting.  If it's set,
    ;; it's pretty interesting.
    (mapc (lambda (sym)
            (when (boundp sym)
              (insert (format "%s: %d" sym (symbol-value sym)))
              (setq edit-here (point))
              (insert "\n")))
          '(c-basic-offset js-indent-level))

    (insert "End:\n")
    (setq end (point))
    (if edit-here (goto-char edit-here))
    (comment-region begin end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; hippie-expand
;;

(global-set-key [(meta ??)] 'hippie-expand)

;; derived from emacswiki.org/emacs/HippieExpand with my initials added
;; and some minor cleanup.
;;
(defun tjs-tags-complete-tag (string predicate what)
  (require 'etags)                      ; tags-completion-table
  (if tags-completion-table
      (save-excursion
        ;; If we need to ask for the tag table, allow that.
        ;; ... no, that's prevented by what's above.  if the tags table
        ;; isn't loaded, reflex doesn't provide for loading it while
        ;; thumping on M-/.
        (if (eq what t)
            (all-completions string (tags-completion-table) predicate)
          (try-completion string (tags-completion-table) predicate)))))

(defun tjs-try-expand-tag (old)
  (unless old
    (he-init-string (save-excursion (backward-word 1) (point)) (point))
    (setq he-expand-list
          (sort (all-completions he-search-string 'tjs-tags-complete-tag)
                #'string-lessp)))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
              (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))

(add-hook 'hippie-expand-try-functions-list 'tjs-try-expand-tag nil)

;; I don't like the default order of hippie-expand.  This reorders whatever
;; is there to put the filename expansions at the end. 
(require 'cl)                           ; gensym, copy-list
(setq hippie-expand-try-functions-list
      (let ((ordering (append
                       `((try-expand-abbrevs . ,(gensym "a"))
                         (try-expand-dabbrev . ,(gensym "a"))
                         (tjs-try-expand-tag . ,(gensym "z"))
                         (try-complete-file-name . ,(gensym "z"))
                         (try-complete-file-name-partially . ,(gensym "z")))
                       (mapcar (lambda (x) (cons x (gensym x)))
                               hippie-expand-try-functions-list))))
        ;; Emacs' sort is destructive.  But there is no reason to
        ;; copy-list here except it makes debugging easier.
        (sort (copy-list hippie-expand-try-functions-list)
              (lambda (a b) (string< (cdr (assoc a ordering))
                                     (cdr (assoc b ordering)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'saveplace)
(setq-default save-place t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'winner)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Programming Language Configuration

;; C

(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; Make c-mode's indentation style righteous.
	    (c-set-style "k&r")
	    ;; this could be global, right?
	    (c-set-offset 'inline-open 0)
	    ;; Canonical K&R offset is 4, not 5.
	    ;; Actually, maybe 8 is, but...
	    (set-variable 'c-basic-offset 4)
	    (set-variable 'fill-column 79)
	    (set-variable 'indent-tabs-mode nil)))

(add-hook 'c-initialization-hook
	  (lambda ()
	    (global-cwarn-mode 1)

	    ;; see ccmode sample emacs for the "right" way to do this?
	    (define-key c-mode-base-map "\C-m" 'c-context-line-break)
	    
	    ;; M-; is cool, but it shouldn't indent comments to col 40
	    ;; without code on the current line.
	    (set-variable 'c-indent-comments-syntactically-p t)
	    ;; leave /* hanging out in front of code in general
	    (set-variable 'c-hanging-comment-starter-p nil)
	    ;; leave */ hanging out after code in general
	    (set-variable 'c-hanging-comment-ender-p nil)
	    ;; insert a row of stars in multiline comments
	    (set-variable 'c-block-comment-prefix "* ")))

;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode) ; interactive elisp mode

;;; Groovy

;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; make Groovy mode electric by default.
;; no, wait, don't, that sucks
;; (add-hook 'groovy-mode-hook
;;           '(lambda ()
;;              (require 'groovy-electric)
;;              (groovy-electric-mode)))

(add-hook 'groovy-mode-hook (lambda ()
                              (c-set-style "k&r")
                              ;; this could be global, right?
                              (c-set-offset 'inline-open 0)
                              ;; Canonical K&R offset is 4, not 5.
                              ;; Actually, maybe 8 is, but...
                              ;; LinkedIn: 2.
                              (set-variable 'c-basic-offset 2)
                              (set-variable 'fill-column 79)
                              (set-variable 'indent-tabs-mode nil)
                              ;; (groovy-electric-mode nil)
                              ))

;; Java

(add-hook 'java-mode-hook (lambda () 
	    (c-set-style "k&r")
	    ;; this could be global, right?
	    (c-set-offset 'inline-open 0)
	    (set-variable 'c-basic-offset (cond (at-linkedin 2) (t 4)))
	    (set-variable 'fill-column 79)
	    (set-variable 'indent-tabs-mode nil)))

;; Javascript
(when at-linkedin
  (add-hook 'js-mode-hook (lambda ()
		    (set-variable 'js-indent-level 2))))

(add-to-list 'auto-mode-alist '("\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\.avsc$" . js-mode))
(add-to-list 'auto-mode-alist '("\.pdsc$" . js-mode))

;; jsp
(add-to-list 'auto-mode-alist '("\.jsp$" . sgml-mode))

;; Lisp

; (setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq inferior-lisp-program "/home/tjs/local/bin/sbcl")

;; Perl

(defalias 'perl-mode 'cperl-mode)	; cripple old perl-mode
(defun my-cperl-mode-hook ()
  (filladapt-mode 0)	; it sucks at perl
  (cperl-set-style "K&R")
  (set-variable 'cperl-indent-level 4))

(add-hook 'cperl-mode-hook (lambda ()
			     (cperl-set-style "K&R")
			     (set-variable 'cperl-indent-level 4)))

;; this is fine, but C-c BS does the job, too.
;;(c-toggle-hungry-state 1)

;; (defun first-file-that-exists (&rest args)
;;   "tjs: Return the first argument that `file-exists-p', or nil."
;;   (message (format "called with: %s" args))
;;   (let ((f (car args))
;; 	(rest (cdr args)))
;;     (or (and f (file-exists-p f) f)
;; 	(and rest
;; 	     (apply #'first-file-that-exists rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Karl Fogel's transposition corrector, as posted to the Arcana list
;; (probably redundant with flyspell, but flyspell makes me nervous)
;; 24 jun 2012

;; (defvar kf-fix-typo-current-failed-candidates ()
;;   "Failed candidates so far in a consecutive series of automated typo fixes,
;; e.g., across successive invocations of `kf-fix-previous-transposition'.

;; The format is simply a list of the failures.  The first element in the
;; list is always the original word -- that is, the one the user originally
;; invoked the typo corrector on -- since by definition that word is a failure.")

;; (defun kf-fix-typo-consider-candidate (candidate)
;;   "Return non-nil iff current typo-fix CANDIDATE could succeed.
;; This means it is a word, and has not been rejected previously by the user."
;;   (and (gethash candidate kf-words)
;;        (not (member candidate kf-fix-typo-current-failed-candidates))))

;; (defun kf-previous-command-was-typo-fix-attempt ()
;;   "Return non-nil iff the last command was one of the typo-fixing commands."
;;   ;; As we have more, we'll add them.
;;   (equal last-command 'kf-fix-previous-transposition))

;; (defun kf-fix-previous-transposition ()
;;   "Fix a single transposition in the previous word.
;; Or if unable to find a single transposition to fix, then leave point
;; in the middle of the previous word so the user can fix it by hand.
;; The return value is currently undefined; do not depend on it.

;; Repeated invocation with no intervening commands runs
;; successively through the various potential fixes of the original
;; word that are reachable via transposition; each successive
;; attempt signals rejection of all previous candidates.  For example, if
;; point is after \"baen\", the first invocation will produce \"bane\",
;; then the next one will produce \"bean\", which might be the user's
;; real target.

;; TODO: This function could handle much more than in-word transposition:

;;   Run the transposition across the previous *two* words.
;;     (Often the typo is of the form, e.g., \"fis hfood\" when one
;;     meant to type \"fish food\".  Expanding the window to two
;;     words can fix that kind of typo too.  But note there's no
;;     point expanding to three words: by the time it's happened
;;     with two words the user has noticed it and is ready to run
;;     the corrector.)

;;   Else if transposing doesn't work, try eliminating one letter.
;;     (Because a frequent typo is the insertion of a spurious letter.)

;;   Else try adding each letter in each position.
;;     (Because a frequent typo is to accidentally drop one letter.)

;;   Else try adding a single space.
;;     (Because a frequent typo is to fail to separate two words.  This
;;     can re-use the check-two-words logic.)

;;   Also, if something was done in an invocation, remember what it was
;;   so that the next immediately successive invocation can undo it and
;;   try the next technique on the list.  E.g., if it transposed two
;;   chars but that turned out to be the wrong fix, then immediately
;;   invoking the function again should undo the transposition and try
;;   adding a letter instead; if that still produces the wrong word, then
;;   undo it and try adding a single space."
;;   (interactive)
;;   (let* ((orig-pos    (point))
;;          (word-first  (progn (forward-word -1) (point)))
;;          (word-last   (progn (forward-word 1) (forward-char -1) (point)))
;;          (word-past   (1+ word-last))
;;          (word-now    (buffer-substring-no-properties
;;                        word-first (1+ word-last)))
;;          (orig-word   word-now)
;;          (current-pos word-last)
;;          (fixed-something nil))
;;     (if (kf-previous-command-was-typo-fix-attempt)
;;         (progn
;;           ;; Restore the original word, since the point is to start
;;           ;; the algorithm over from the beginning state (not some
;;           ;; random intermediate state) but this time with a longer
;;           ;; list of immediately rejectable candidates.
;;           (delete-region word-first word-past)
;;           (save-excursion
;;             (goto-char word-first)
;;             (insert (car kf-fix-typo-current-failed-candidates)))
;;           (setq kf-fix-typo-current-failed-candidates
;;                 (append kf-fix-typo-current-failed-candidates
;;                         (list word-now))))
;;       ;; Else initialize the rejectables list with the current word.
;;       (setq kf-fix-typo-current-failed-candidates (list word-now)))
;;     (setq fixed-something
;;           (catch 'fixed
;;             (while (> current-pos word-first)
;;               (goto-char current-pos)
;;               (transpose-chars 1)
;;               (setq word-now (buffer-substring-no-properties
;;                               word-first word-past))
;;               (if (kf-fix-typo-consider-candidate word-now)
;;                   (throw 'fixed t)
;;                 ;; else undo the transpose chars
;;                 (forward-char -1)
;;                 (transpose-chars 1)
;;                 (setq current-pos (1- current-pos))))))
;;     (if fixed-something
;;         (goto-char orig-pos)
;;       ;; If didn't manage to fix it, at least put point in the middle
;;       ;; of the word, closer to where the user might manually fix it.
;;       (goto-char (/ (+ word-first word-last) 2)))))


;; (defconst kf-words
;;   (let ((dict (make-hash-table :test 'equal :size 100000))
;;         (word-source "/usr/share/dict/words"))
;;     (when (file-exists-p word-source)
;;       (save-excursion
;;         (set-buffer (find-file-noselect word-source))
;;         (goto-char (point-min))
;;         (while (< (point) (point-max))
;;           (let ((this-line-word (buffer-substring-no-properties
;;                                  (point) (progn (end-of-line) (point)))))
;;             (puthash this-line-word 0 dict)
;;             (let ((capitalized (capitalize this-line-word))
;;                   (upcased (upcase this-line-word)))
;;               (when (not (string-equal capitalized this-line-word))
;;                 (puthash capitalized 0 dict))
;;               (when (not (string-equal upcased this-line-word))
;;                 (puthash upcased 0 dict)))
;;             (forward-line 1)))
;;         (kill-buffer)))
;;     dict)
;;   "Hash table whose keys are English words and whose values are ignored.")

;; (global-set-key [(control c) ?t] 'kf-fix-previous-transposition)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Conclusion

(random t)
(run-with-idle-timer
 (random 3600) nil
 (lambda ()
   (let ((msgs
	  ["maze of twisty little"
	   "twisty maze of little"
	   "little twisty maze of"
	   "maze of little twisty"
	   "little maze of twisting"
	   "little maze of twisty"
	   "twisting maze of little"
	   "twisty little maze of"
	   "twisting little maze of"
	   "maze of little twisting"
	   "maze of twisting little"
	   "little twisting maze of"]))
     (message (concat "You are in another "
		      (aref msgs (random (length msgs)))
		      " editors, all different.")))))
(server-start)

;; Yow!  Legally-imposed CULTURE-reduction is CABBAGE-BRAINED!
(let ((yow "~/.emacs.d/yow.lines"))
  (when (file-exists-p (expand-file-name yow))
    (setq yow-file yow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Appendix A. Custom
;;;
;;; Automated dreck.  Touch carefully, if at all.

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cperl-invalid-face (quote default))
 '(indent-tabs-mode nil)
 '(mail-host-address "psaux.com")
 '(mail-user-agent (quote gnus-user-agent))
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-mail-address "tjs@psaux.com")
 '(visual-line-fringe-indicators (quote (nil nil))))
