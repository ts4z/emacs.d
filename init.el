;;;; emacs.el --- ts4z, GNU Emacs flavor

;;; Commentary:
;;;
;;; flycheck bitches at you if you don't write one of these.
;;;
;;; This is my .emacs.
;;; There are many like it.
;;; This one is mine.

;;; Flycheck thinks you need to know when the code starts, so:

;;; Code:

;;; global/general settings

(if (boundp 'package-archives)
    (add-to-list 'package-archives
		 '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t))

(require 'saveplace)
(require 'winner)

(defconst running-on-mac-p
  "Are we running on a Mac?"
  (when (equal system-type "darwin")))

(blink-cursor-mode 1)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map "\e\e" 'eval-expression) ; 18.54 forever
(define-key global-map [(control meta g)] 'keyboard-escape-quit)
(delete-selection-mode 1)
(global-font-lock-mode 1)
(global-unset-key [(control x) ?f])    ; I never do this, but I mistype C-x C-f
(global-unset-key [(control z)])       ; use C-x C-z if I must
(global-set-key [(control c) ?v ?s] 'magit-status)
(global-set-key [(control c) ?v ?=] 'magit-diff)
(global-set-key [(control c) ?v ?l] 'magit-log)
(global-set-key [(control c) ?v ?a] 'magit-stage-file)
(global-set-key [(control c) ?v ?b] 'magit-blame)
(global-set-key [(control c) ?v ?c] 'magit-commit)
(global-set-key [(control c) ?v ?r ?i] 'magit-rebase-interactive)
(global-set-key [(control c) ?v ?r ?c] 'magit-rebase-continue)
(global-set-key [(control c) ?v ?r ?S] 'magit-rebase-skip)
(global-set-key "\C-c6d" 'base64-decode-region)
(global-set-key "\C-c6e" 'base64-encode-region)
(global-set-key [(control ?\\)] 'align-regexp)
(global-set-key [(control c) ?3]        'slice-window-horizontally)
(global-set-key [(control c) ?\;]       'comment-region)
(global-set-key [(control c) ?b]        'bury-buffer)
(global-set-key [(control c) ?c]        'compile)
(global-set-key [(control c) (shift d)] 'toggle-debug-on-error)
(global-set-key [(control c) ?g]        'goto-line)
(global-set-key [(control c) ?i]        'imenu)
(global-set-key [(control c) ?l]        'tjs-insert-local-variable-template)
(global-set-key [(control c) ?n]        'linum-mode)
(global-set-key [(control c) ?w]        'toggle-word-wrap)
(global-set-key [(control z)] 'undo) ;the universe has decided C-z is undo
(global-set-key [(meta control backspace)] 'backward-kill-sexp)
(global-set-key [(meta z)] 'zap-up-to-char)
(global-set-key (kbd "M-*") 'pop-tag-mark)
(global-set-key (kbd "M-_") 'text-scale-decrease)
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-subword-mode t)
;;(mouse-wheel-mode 1)
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)    ; Does this still need to be enabled?
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(savehist-mode)
(set-variable 'enable-local-eval 'query)
(set-variable 'enable-recursive-minibuffers t)
(set-variable 'inhibit-startup-message t)
(set-variable 'version-control t)
(setq garbage-collection-messages t)    ; old skool
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq initial-scratch-message nil)
(setq line-move-visual nil)             ; old skool
(setq save-interprogram-paste-before-kill t)
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq-default save-place t)
(show-paren-mode 1)
(tool-bar-mode 0)
(which-function-mode t)
(which-key-mode t)
(winner-mode t)

;; Disable startup-time message in echo area.  Do this the ugly way, because
;; GNU thinks it's so important that they made it difficult to disable.
;;
;; https://github.com/emacs-mirror/emacs/blob/450b0d1c0dabc2a9f4a5e63db87590e9681b9319/lisp/startup.el#L84
(defun display-startup-echo-area-message () "Don't display anything at startup.")

;; Menu bar or no menu bar?  On Mac, definitely menu bar.
(unless (cl-search "darwin" (emacs-version))
  (menu-bar-mode 0))                       ; I never do this, but try C-mouse 3
;; On tty, definitely disable menu bar (I don't even know how to use it there).
(add-hook 'tty-setup-hook (lambda () (menu-bar-mode -1)))

;; bump up gc threshold.  as of 2018, it is still 800k.  We can afford a little
;; more.
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
      (nreverse
       (list "/usr/local/share/emacs/site-lisp/"
	     "~/share/emacs/slime"
             ;; use my yaml mode from this path; it has a bug fix (4/2018)
             "~/git/yaml-mode"
	     (concat (or (getenv "TJS_CVS")
			 (concat (getenv "HOME") "/cvs-tjs")) "/elisp/"))))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; Can custom be trusted not to do this on ttys?  We shall see.
;; Untested (but maybe OK on Macs and might work on X11)
(condition-case nil
    (if running-on-mac-p
        (set-face-font 'fixed-pitch (font-spec :family "Courier"))
      (set-face-font 'default "DejaVu Sans Mono 10"))
  (error nil))

;; on terminals, these colors are OK but Emacs does stupid things with
;; backgrounds.  try not to define them -- the defaults are nice
;;
;; enable rainbow-mode before messing with this -- it's neat.
(let ((white-background-color "white"))
  (set-face-foreground 'font-lock-builtin-face "#990066")
  ;; (set-face-background 'default white-background-color)
  (set-face-attribute 'font-lock-comment-face nil :foreground "#668866"
                      ;; :background white-background-color
                      :slant 'italic)
  (set-face-attribute 'font-lock-doc-face nil :inherit 'font-lock-common-face
                      ;; :background white-background-color
                      :slant 'italic :foreground "brown")
  (set-face-attribute 'font-lock-function-name-face nil :foreground "Brown")
  (set-face-attribute 'font-lock-string-face nil
                      ;; :background white-background-color
                      :foreground "darkgreen"))

(defun other-window-previous (n &optional which-frames which-devices)
  "Select the N th different (previous) window on this frame.
Behaves exactly like `other-window', but acts in the opposite
direction.  WHICH-FRAMES and WHICH-DEVICES are like `other-window`."
  (interactive "p")
  (other-window (- n)))
(global-set-key "\C-cp" 'other-window-previous)

;; Rebound to make it easier to type.
(defun find-tag-next ()
  "Find the next tag, ala `find-tag' with a universal argument."
  (interactive)
  (find-tag nil t))

;; I like this, but ESC ESC is a historic holy key binding and this conflicts.
;;(global-set-key [escape (meta ?.)] 'find-tag-next)
(global-set-key [(control ?.)] 'find-tag-next)

(defun fill-paragraph-unless-word-wrap-enabled (&optional justify region)
  "Fill, using `fill-paragraph', unless `word-wrap' is enabled.

JUSTIFY and REGION as in `fill-paragraph'."
  (interactive)
  (if word-wrap
      (message "Not filling while word-wrapping enabled.  OCD much?")
    (fill-paragraph justify region)))
(global-set-key [(meta ?q)] 'fill-paragraph-unless-word-wrap-enabled)

(defun slice-window-horizontally (w)
  "Split window into horizontal slices of roughly 80 (or W)
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
    ;; if arg positive, combine remainder with last window
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

(defun thats-not-meta ()
  "Attach this function to keys that do things that might confuse.
For instance, attach it to s-q, in case I forget which key is
meta and which is super on which keyboard.  Because on the Mac,
Command-Q might quit, but Meta-Q will `fill-paragraph' -- and
getting the two confused is very frustrating."
  (interactive)
  (beep)
  (message "That's not the meta key!!"))

;; better rebind this on Mac, or fill reflexes cause editor exits.
(global-set-key [(super q)] #'thats-not-meta)

;; This is a hack, but not quite as huge a hack as when I first wrote it.
(defun tjs-insert-local-variable-template ()
  "Insert the thing where local variables go at the bottom of the file."
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

;;(global-set-key [(meta ??)] 'hippie-expand)
(global-set-key [(meta ?/)] 'hippie-expand)

;; derived from emacswiki.org/emacs/HippieExpand with my initials added
;; and some minor cleanup.
(defun tjs-tags-complete-tag (string predicate what)
  "Callback for `all-completions' in `tjs-try-expand-tag'.
Arguments STRING, PREDICATE, and WHAT are as `all-completions'
requires."
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
  "Hook for `hippie-expand' that, IIRC, expands tags in the tags table.

OLD is an argument to this function."
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

;; flycheck

(global-flycheck-mode t)
(flycheck-gometalinter-setup)

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
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

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
	    (set-variable 'c-basic-offset 4)
	    (set-variable 'fill-column 79)
	    (set-variable 'indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

;; json
(add-to-list 'auto-mode-alist '("\\.avsc$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.pdsc$" . json-mode))

;; Lisp

; (setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq inferior-lisp-program "/home/tjs/local/bin/sbcl")

;; Perl

(defalias 'perl-mode 'cperl-mode)	; cripple old perl-mode

(add-hook 'cperl-mode-hook (lambda ()
			     (cperl-set-style "K&R")
			     (set-variable 'cperl-indent-level 4)))

;; this is fine, but C-c BS does the job, too.
;;(c-toggle-hungry-state 1)

(defun first-file-that-exists (&rest args)
  "Return the first of ARGS that `file-exists-p', or nil."
  ;; (message (format "called with: %s" args))
  (let ((f (car args))
	(rest (cdr args)))
    (or (and f (file-exists-p f) f)
	(and rest
	     (apply #'first-file-that-exists rest)))))

;; Go

;; need 'go' on path?  can't depend on zshrc to do this; we might be
;; running from display manager or Mac or something else that can't
;; set $PATH properly.
(let ((gobin (first-file-that-exists "/usr/lib/golang-1.10/bin"
                                     "/usr/lib/golang-1.9/bin")))
  (when gobin
    (add-to-list 'exec-path gobin)))

;; Go in /usr/local/bin on Mac (via Homebrew)
(add-to-list 'exec-path "/usr/local/bin")

;; need godef, etc., on path.
;; can't trust shell setup to do this, as it may not get run
;; if we're spawned from display manager.  (alas)
;; 
;; (this may not be an ideal setting, but we need to add at least
;; one of them.)
(add-to-list 'exec-path (expand-file-name "~/go/bin"))

;; https://github.com/dominikh/go-mode.el
;; says look at http://emacsredux.com/blog/2014/05/16/melpa-stable/

(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'godef-jump)))

(require 'go-eldoc)
(add-hook 'go-mode-hook #'go-eldoc-setup)
(require 'go-guru)
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

;; autocomplete
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; URL crap
;; http://www.blogbyben.com/2010/08/handy-emacs-function-url-decode-region.html

(defun url-decode-region (start end)
  "Replace a region from START to END with the same contents, only URL decoded.

lifted from
http://www.blogbyben.com/2010/08/handy-emacs-function-url-decode-region.html"
  (interactive "r")
  (let ((text (url-unhex-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

(defun url-encode-region (start end)
  "Replace a region (from START to END) with the same contents, only URL encoded."
  ;; yes, it's just the code above with a different encoder.
  (interactive "r")
  (let ((text (url-hexify-string (buffer-substring start end))))
    (delete-region start end)
    (insert text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fill column -- I prefer 79 in general, sort of.

(defun maybe-set-fill-column-to-72 ()
  "Set the fill column to 72 some of the time.

This does so according to the whims of this method.  Mostly this
is so git commits look nice when wrapped."
  (when (member (buffer-name) '("COMMIT_EDITMSG" "*vc-log*"))
    (setq fill-column 72)))

(setq-default fill-column 79)
(add-hook 'text-mode-hook #'maybe-set-fill-column-to-72)

(add-to-list 'auto-mode-alist '("_EDITMSG$" . text-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Conclusion

;; put Go back on the path, because apparently we didn't do that
;; by just modifying exec-path, no no no.
(setenv "PATH" (mapconcat 'identity exec-path ":"))

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
;; This doesn't work; yow has been expunged.
;; (let ((yow "~/.emacs.d/yow.lines"))
;;   (when (file-exists-p (expand-file-name yow))
;;     (setq yow-file yow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Appendix A. Custom
;;; Automated dreck.  Touch carefully, if at all.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-invalid-face 'default)
 '(indent-tabs-mode nil)
 '(mail-host-address "psaux.com")
 '(mail-user-agent 'gnus-user-agent)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(package-selected-packages
   '(ac-emoji dockerfile-mode flycheck flycheck-gometalinter go-autocomplete go-eldoc go-errcheck go-guru go-mode go-playground go-rename hound json-mode magit markdown-mode minimal-session-saver minimap rainbow-mode rust-mode rust-playground sokoban terraform-mode which-key))
 '(show-paren-mode t)
 '(show-paren-style 'expression)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(user-mail-address "tjs@psaux.com")
 '(visual-line-fringe-indicators '(nil nil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; flycheck is the only thing surprised by this
(provide 'init)
;;; init.el ends here
