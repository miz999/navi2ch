;;; navi2ch-message.el --- write message module for navi2ch

;; Copyright (C) 2000-2004 by Navi2ch Project

;; Author: Taiki SUGAWARA <taiki@users.sourceforge.net>
;; Keywords: network, 2ch

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:
(provide 'navi2ch-message)
(defconst navi2ch-message-ident
  "$Id$")

(eval-when-compile (require 'cl))

(require 'navi2ch)

(defvar navi2ch-message-aa-map nil)
(unless navi2ch-message-aa-map
  (let ((map (make-sparse-keymap "Type ? for further options")))
    (navi2ch-set-keymap-default-binding map 'navi2ch-message-self-insert-aa)
    (define-key map "?" 'navi2ch-message-insert-aa)
    (setq navi2ch-message-aa-map map)))

(defvar navi2ch-message-mode-map nil)
(unless navi2ch-message-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map navi2ch-global-map)
    (define-key map "\C-c\C-c" 'navi2ch-message-send-message)
    (define-key map "\C-c\C-k" 'navi2ch-message-exit)
    (define-key map "\C-c\C-y" 'navi2ch-message-cite-original)
    (define-key map "\C-c\C-j" 'navi2ch-message-cite-original-from-number)
    (define-key map "\C-c\C-i" 'navi2ch-message-insert-backup)
    (define-key map "\C-c\C-b" 'navi2ch-base64-insert-file)
    (define-key map "\et" 'navi2ch-toggle-offline)
    (define-key map navi2ch-message-aa-prefix-key navi2ch-message-aa-map)
    (setq navi2ch-message-mode-map map)))

(defvar navi2ch-message-mode-menu-spec
  '("Message"
    ["Toggle offline" navi2ch-toggle-offline]
    ["Send message" navi2ch-message-send-message]
    ["Cancel" navi2ch-message-exit]
    ["Cite message" navi2ch-message-cite-original]))

(defvar navi2ch-message-buffer-name "*navi2ch message*")
(defvar navi2ch-message-backup-buffer-name "*navi2ch message backup*")
(defvar navi2ch-message-current-article-buffer nil)
(defvar navi2ch-message-current-article nil)
(defvar navi2ch-message-current-board nil)
(defvar navi2ch-message-new-message-p nil)
(defvar navi2ch-message-window-configuration nil)
(defvar navi2ch-message-header-separator "----------------\n")

(defvar navi2ch-message-paragraph-separate
  (concat (regexp-quote navi2ch-message-header-separator) "\\|"
	  ">\\|"			; $B0zMQ(B
	  "[ \t]*$")			; $B6u9T(B
  "*`navi2ch-message-mode' $B$G;HMQ$5$l$k(B `paragraph-separate'$B!#(B")

(defvar navi2ch-message-paragraph-start
  navi2ch-message-paragraph-separate
  "*`navi2ch-message-mode' $B$G;HMQ$5$l$k(B `paragraph-start'$B!#(B")

(defvar navi2ch-message-sendlog-board
  `((name . "$BAw?.95$((B")
    (type . board)
    (id . "sendlog")
    (bbstype . localfile)
    (uri . ,(concat "x-localbbs://" (navi2ch-expand-file-name "sendlog/")))))

(defun navi2ch-message-write-message (board article &optional new sage)
  (when (or (not navi2ch-message-ask-before-write)
	    (if (functionp navi2ch-message-ask-before-write)
		(funcall navi2ch-message-ask-before-write "Write new message? ")
	      (y-or-n-p "Write new message? ")))
    (if (and (get-buffer navi2ch-message-buffer-name)
	     (or navi2ch-message-always-pop-message
		 (not (navi2ch-message-kill-message))))
	(navi2ch-message-pop-message-buffer)
      (setq navi2ch-message-window-configuration
	    (current-window-configuration))
      (delete-other-windows)
      (split-window-vertically)
      (other-window 1)
      (setq navi2ch-message-current-article article)
      (setq navi2ch-message-current-board board)
      (setq navi2ch-message-new-message-p new)
      (setq navi2ch-message-current-article-buffer
	    (if new nil (current-buffer)))
      (switch-to-buffer (get-buffer-create navi2ch-message-buffer-name))
      (navi2ch-message-mode)
      (erase-buffer)
      (navi2ch-message-insert-header new sage)
      (setq navi2ch-mode-line-identification
	    (if new
		(format "*new message* [%s]"
			(cdr (assq 'name navi2ch-message-current-board)))
	      (format "Re: %s [%s]"
		      (cdr (assq 'subject navi2ch-message-current-article))
		      (cdr (assq 'name navi2ch-message-current-board)))))
      (navi2ch-set-mode-line-identification)
      (run-hooks 'navi2ch-message-setup-message-hook)
      (when sage
	(run-hooks 'navi2ch-message-setup-sage-message-hook)))))

(defun navi2ch-message-pop-message-buffer ()
  (interactive)
  (let ((buf (get-buffer navi2ch-message-buffer-name)))
    (when buf
      (cond ((get-buffer-window buf)
             (select-window (get-buffer-window buf)))
            (buf
             (setq navi2ch-message-window-configuration
                   (current-window-configuration))
             (delete-other-windows)
             (split-window-vertically)
             (other-window 1)
             (switch-to-buffer navi2ch-message-buffer-name))))))

(defun navi2ch-message-insert-backup ()
  (interactive)
  (when (get-buffer navi2ch-message-backup-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (insert-buffer navi2ch-message-backup-buffer-name)))

(defun navi2ch-message-insert-header (new sage)
  (and sage (setq sage "sage"))
  (when new
    (insert "Subject: \n"))
  (insert "From: "
	  (or (and navi2ch-message-remember-user-name
		   (cdr (assq 'name navi2ch-message-current-article)))
	      (cdr (assoc (cdr (assq 'id navi2ch-message-current-board))
			  navi2ch-message-user-name-alist))
	      navi2ch-message-user-name "")
	  "\n"
	  "Mail: "
	  (or sage
	      (and navi2ch-message-remember-user-name
		   (cdr (assq 'mail navi2ch-message-current-article)))
	      (cdr (assoc (cdr (assq 'id navi2ch-message-current-board))
			  navi2ch-message-mail-address-alist))
	      navi2ch-message-mail-address "")
	  "\n"
	  (navi2ch-propertize navi2ch-message-header-separator
			      'read-only t
			      'front-sticky t 'rear-nonsticky t))
  (setq buffer-undo-list nil)
  (set-buffer-modified-p nil))

(defsubst navi2ch-message-header-end ()
  (save-restriction
    (widen)
    (if (get-text-property (point-min) 'read-only)
	(point-min)
      (next-single-property-change (point-min) 'read-only))))

(defun navi2ch-message-cleanup-message ()
  (save-excursion
    (let ((start (progn (goto-char (navi2ch-message-header-end))
			(forward-line)
			(point))))
      (when navi2ch-message-cleanup-trailing-whitespace
	(goto-char start)
	(while (re-search-forward "[ \t]+$" nil t)
	  (replace-match "")))
      (when navi2ch-message-cleanup-trailing-newline
	(goto-char start)
	(if (re-search-forward "[ \t\n]+\\'" nil t)
	    (replace-match ""))))))

(defun navi2ch-message-send-message ()
  (interactive)
  (if navi2ch-offline
      (message "Now offline")
    (when (or (not navi2ch-message-ask-before-send)
	      (if (functionp navi2ch-message-ask-before-send)
		  (funcall navi2ch-message-ask-before-send "Send message? ")
		(y-or-n-p "Send message? ")))
      (widen)
      (run-hooks 'navi2ch-message-before-send-hook)
      (navi2ch-message-cleanup-message)
      (save-excursion
        (let ((end (navi2ch-message-header-end))
	      (from "")
	      (mail "")
	      subject message)
	  (goto-char (point-min))
	  (when navi2ch-message-new-message-p
	    (if (re-search-forward "^Subject: ?\\(.*\\)" end t)
		(setq subject (match-string 1))
	      (setq subject "")))
	  (goto-char (point-min))
	  (when (re-search-forward "^From: ?\\(.*\\)" end t)
	    (setq from (match-string 1))
	    (when (and (not navi2ch-message-new-message-p)
		       navi2ch-message-remember-user-name)
	      (navi2ch-message-set-name from)))
	  (goto-char (point-min))
	  (when (re-search-forward "^Mail: ?\\(.*\\)" end t)
	    (setq mail (match-string 1))
	    (when (and (not navi2ch-message-new-message-p)
		       navi2ch-message-remember-user-name)
	      (navi2ch-message-set-mail mail)))
	  (goto-char end)
	  (forward-line)
	  (setq message (buffer-substring-no-properties (point) (point-max)))
	  (let ((buffer (current-buffer))
		(inhibit-read-only t))
	    (save-excursion
	      (set-buffer (get-buffer-create
			   navi2ch-message-backup-buffer-name))
	      (erase-buffer)
	      (insert-buffer buffer)))
	  (when navi2ch-message-trip
	    (setq from (concat from "#" navi2ch-message-trip)))
	  (let ((board navi2ch-message-current-board)
		(article navi2ch-message-current-article)
		result)
	    (navi2ch-net-cleanup)
	    ;; $B"-(Bresult$B$r8E$$;EMM$KLa$7$?!#(Bspid$B$O!"(Bnavi2ch-multibbs.el$B$N(B
	    ;; $B"-(B   navi2ch-2ch-send-message $B$G=hM}$9$k!#(B
	    (setq result (navi2ch-multibbs-send-message
			  from mail message subject board article))
	    (navi2ch-net-cleanup)
	    (when result
	      (when navi2ch-message-save-sendlog
		(navi2ch-message-add-sendlog from mail message subject
					     board article))
	      (message "Waiting new message...")
	      (sleep-for navi2ch-message-wait-time)
	      (message "%s%s" (current-message) "done")
	      (save-excursion
		(if navi2ch-message-new-message-p
		    (progn
		      (set-buffer navi2ch-board-buffer-name)
		      (navi2ch-board-sync))
		  (when (buffer-live-p navi2ch-message-current-article-buffer)
		    (set-buffer navi2ch-message-current-article-buffer)
		    (navi2ch-article-sync navi2ch-message-force-sync)))))
	    (when (get-buffer navi2ch-message-backup-buffer-name)
	      (bury-buffer navi2ch-message-backup-buffer-name)))))
      (run-hooks 'navi2ch-message-after-send-hook)
      (navi2ch-message-exit 'after-send))))

(defun navi2ch-message-set-name (name)
  (save-excursion
    (if (buffer-live-p navi2ch-message-current-article-buffer)
	(set-buffer navi2ch-message-current-article-buffer)
      (navi2ch-article-view-article navi2ch-message-current-board
				    navi2ch-message-current-article
				    nil))
    (setq navi2ch-article-current-article
	  (navi2ch-put-alist 'name name
			     navi2ch-article-current-article))))

(defun navi2ch-message-set-mail (mail)
  (let ((case-fold-search t))
    (unless (string-match "sage" mail)
      (save-excursion
	(if (buffer-live-p navi2ch-message-current-article-buffer)
	    (set-buffer navi2ch-message-current-article-buffer)
	  (navi2ch-article-view-article navi2ch-message-current-board
					navi2ch-message-current-article
					nil))
	(setq navi2ch-article-current-article
	      (navi2ch-put-alist 'mail mail
				 navi2ch-article-current-article))))))

(defun navi2ch-message-cite-original (&optional arg)
  "$B0zMQ$9$k!#(B"
  (interactive "P")
  (navi2ch-message-cite-original-from-number
   (save-excursion
     (set-buffer (navi2ch-article-current-buffer))
     (navi2ch-article-get-current-number))
   arg))

(defun navi2ch-message-cite-original-from-number (num &optional arg)
  "$BHV9f$rA*$s$G!"0zMQ$9$k!#(B"
  (interactive "nInput number: \nP")
  (when (< (point) (navi2ch-message-header-end))
    (error "Cannot cite in header"))
  (let (same msg board article)
    (save-excursion
      (set-buffer (navi2ch-article-current-buffer))
      (setq msg (navi2ch-article-get-message-string num))
      (setq article navi2ch-article-current-article)
      (setq board navi2ch-article-current-board)
      (setq same (and (string-equal (cdr (assq 'id board))
				    (cdr (assq 'id navi2ch-message-current-board)))
		      (string-equal (cdr (assq 'artid article))
				    (cdr (assq 'artid navi2ch-message-current-article))))))
    (if same
	(insert ">>" (number-to-string num) "\n")
      (insert (navi2ch-article-to-url board article num num nil) "\n"))
    (unless arg
      (push-mark)
      (let ((point (point)))
	(insert msg "\n")
	(string-rectangle point (point) navi2ch-message-cite-prefix)))))

(defun navi2ch-message-exit (&optional after-send)
  (interactive)
  (run-hooks 'navi2ch-message-exit-hook)
  (when (navi2ch-message-kill-message after-send)
    ;; $B$`$%!"(Bset-window-configuration $B$r;H$&$H%+!<%=%k0LCV$,JQ$K$J$k$s$+$$!)(B
    (set-window-configuration navi2ch-message-window-configuration)
    (when (and (not navi2ch-message-new-message-p)
               after-send)
      (if (buffer-live-p navi2ch-message-current-article-buffer)
	  (set-buffer navi2ch-message-current-article-buffer)
	(navi2ch-article-view-article navi2ch-message-current-board
				      navi2ch-message-current-article
				      navi2ch-message-force-sync)))
    (navi2ch-article-load-number)))

(defun navi2ch-message-kill-message (&optional no-ask)
  (when (or no-ask
	    (not navi2ch-message-ask-before-kill)
	    (if (functionp navi2ch-message-ask-before-kill)
		(funcall navi2ch-message-ask-before-kill "Kill current message? ")
	      (y-or-n-p "Kill current message? ")))
    (kill-buffer navi2ch-message-buffer-name)
    t))

(easy-menu-define navi2ch-message-mode-menu
  navi2ch-message-mode-map
  "Menu used in navi2ch-message"
  navi2ch-message-mode-menu-spec)

(defun navi2ch-message-setup-menu ()
  (easy-menu-add navi2ch-message-mode-menu))

(defun navi2ch-message-fill-paragraph (arg)
  (interactive)
  (let ((before (point)))
    (save-excursion
      (forward-paragraph)
      (or (bolp) (newline 1))
      (let ((end (point))
	    (beg (progn (backward-paragraph) (point))))
	(when (eq beg (point-min))
	  (forward-line 3)
	  (setq beg (point)))
	(goto-char before)
	(fill-region-as-paragraph beg end arg)
	t))))

(defun navi2ch-message-mode ()
  "\\{navi2ch-message-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'navi2ch-message-mode)
  (setq mode-name "Navi2ch Message")
  (set (make-local-variable 'fill-paragraph-function)
       'navi2ch-message-fill-paragraph)
  (set (make-local-variable 'paragraph-separate)
       navi2ch-message-paragraph-separate)
  (set (make-local-variable 'paragraph-start)
       navi2ch-message-paragraph-start)
  (set (make-local-variable 'auto-fill-inhibit-regexp)
       "^[A-Z][^: \n\t]+:")		; $B%X%C%@(B
  (use-local-map navi2ch-message-mode-map)
  (navi2ch-message-setup-menu)
  (run-hooks 'navi2ch-message-mode-hook)
  (force-mode-line-update))

(defun navi2ch-message-self-insert-aa ()
  "$B:G8eF~NO$7$?%-!<$K$7$?$,$C$F(B AA $B$rF~NO$9$k!#(B"
  (interactive)
  (let ((char last-command-char) aa)
    (if (and (navi2ch-char-valid-p char)
	     (setq aa (cdr (assoc (string last-command-char)
				  (append navi2ch-message-aa-alist
					  navi2ch-message-aa-default-alist)))))
	(insert aa)
      (ding))))

(defun navi2ch-message-insert-aa-list ()
  (let ((aa-width navi2ch-message-popup-aa-width)
	(nl nil)
	alist keys)
    (dolist (elt (append navi2ch-message-aa-alist
			 navi2ch-message-aa-default-alist))
      (when (and (not (member (car elt) keys))
		 (stringp (car elt))
		 (stringp (cdr elt)))
	(setq alist (cons elt alist))
	(setq keys (cons (car elt) keys))))
    (dolist (key (sort keys 'string<))
      (let ((val (cdr (assoc key alist)))
	    string width)
	(setq string (format "%s: %s" (key-description key) val)
	      width (string-width string))
	(if (> width aa-width)
	    (setq string (concat (navi2ch-truncate-string-to-width
				  string (- aa-width 3))
				 "...")))
	(insert (navi2ch-truncate-string-to-width string aa-width nil ?\ )
		(if nl "\n" " "))
	(setq nl (not nl))))))

(defun navi2ch-message-popup-aa-list ()
  "aa $B$N%j%9%H$rI=<($9$k!#(B"
  (interactive)
  (let ((buffer (get-buffer-create "*AA List*"))
	(continue t)
	c)
    (unwind-protect
	(save-window-excursion
	  (with-current-buffer buffer
	    (erase-buffer)
	    (navi2ch-message-insert-aa-list)
	    (goto-char (point-min))
	    (pop-to-buffer (current-buffer))
	    (while continue
	      (setq c (navi2ch-read-char
		       "Type key for AA (or SPC forward, DEL back): "))
	      (cond
	       ((memq c '(?\  ?\C-v))
		(ignore-errors (scroll-up)))
	       ((memq c '(?\C-h ?\177))
		(ignore-errors (scroll-down)))
	       ((eq c ?\C-l)
		(recenter))
	       (t (setq continue nil)))))
	  c)
      (if (bufferp buffer)
	  (kill-buffer buffer)))))

(defun navi2ch-message-insert-aa ()
  "aa $B$rF~NO$9$k!#(B"
  (interactive)
  (let* ((char (navi2ch-message-popup-aa-list))
	 (aa (cdr (assoc (char-to-string char)
			 (append navi2ch-message-aa-alist
				 navi2ch-message-aa-default-alist)))))
    (if (stringp aa)
	(insert aa)
      (ding))))

(defun navi2ch-message-jump-to-message-buffer ()
  "message buffer $B$,$"$k$H$-!"=q$-9~$_@h$N%9%l(B/$BHD$rI=<($7(B message buffer $B$K@Z$jBX$(!#(B"
  (interactive)
  (if (not (get-buffer navi2ch-message-buffer-name))
      (message "No message buffer")
    (delete-other-windows)
    (if navi2ch-message-current-article-buffer
	;; $B4{B8%9%l$K=q$-9~$_(B $B"*(B $B=q$-9~$_@h$N%9%l$rI=<((B
	(if (buffer-live-p navi2ch-message-current-article-buffer)
	    (switch-to-buffer navi2ch-message-current-article-buffer)
	  (navi2ch-article-view-article navi2ch-message-current-board
					navi2ch-message-current-article)
	  (setq navi2ch-message-current-article-buffer (current-buffer)))
      ;; $B?75,%9%lN)$F(B $B"*(B $B=q$-9~$_@h$NHD$rI=<((B
      (or (and (get-buffer navi2ch-board-buffer-name)
	       (progn (switch-to-buffer (get-buffer
					 navi2ch-board-buffer-name))
		      t)
	       (eq major-mode 'navi2ch-board-mode)
	       (eq navi2ch-board-current-board
		   navi2ch-message-current-board))
	  (navi2ch-bm-select-board navi2ch-message-current-board)))
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer (get-buffer navi2ch-message-buffer-name))))

;; sendlog$B5!G=(B
(defun navi2ch-message-sendlog-subject (board article)
  ;; $BAw?.$7$?%l%9$rJ]B8$9$k%9%l$N%?%$%H%k$rJV$9!#(B
  ;; $B%?%$%H%k$r:Y$+$/;XDj$7$?$$$H$-$O$3$N4X?t$r>e=q$-$7$F$M!#(B
  ;; nil $B$rJV$9$H!"MzNr$OJ]B8$5$l$^$;$s!#(B
  navi2ch-message-sendlog-subject)

(defun navi2ch-message-sendlog-subject-with-volume (base format limit
							 subject-list)
  (let ((subject base)
	(regexp (concat "\\`"
			(format (regexp-quote format)
				(regexp-quote base) "\\([0-9]+\\)")
			"\\'"))
	(max 0)
	article)
    (when limit
      (dolist (x subject-list)
	(let ((sbj (cdr (assq 'subject x)))
	      num)
	  (when (and (or (and (string= base sbj)
			      (setq num 1))
			 (and (string-match regexp sbj)
			      (setq num
				    (string-to-number (match-string 1 sbj)))))
		     (> num max))
	    (setq max num
		  article x))))
      (when article
	(if (>= (string-to-number (cdr (assq 'response article))) limit)
	    (setq subject (format format base (number-to-string (1+ max))))
	  (setq subject (cdr (assq 'subject article))))))
    subject))

(defun navi2ch-message-add-sendlog (from mail message subject board article)
  (let ((navi2ch-localfile-default-file-modes ?\700)
	;; $BAw?.95$((B $B$N%9%l%?%$$K(B &hearts; $B$H$+$r;H$($k$h$&$K!#(B
	(navi2ch-decode-character-references nil)
	(url (navi2ch-article-to-url board article))
	(sbj (or subject (cdr (assq 'subject article))))
	(lsubject (navi2ch-message-sendlog-subject board article))
	(lboard navi2ch-message-sendlog-board)
	(fmt navi2ch-message-sendlog-volume-format)
	(limit navi2ch-message-sendlog-response-limit)
	larticle lsbj-list)
    (when (and lsubject lboard)
      (setq message (format "Subject: %s\nURL: %s\n\n%s" sbj url message)
	    lsbj-list (navi2ch-board-get-updated-subject-list lboard)
	    lsubject (navi2ch-message-sendlog-subject-with-volume
		      lsubject fmt limit lsbj-list))
      (catch 'loop
	(dolist (s lsbj-list)
	  (when (string= (cdr (assq 'subject s)) lsubject)
	    (throw 'loop (setq larticle s)))))
      (when larticle (setq lsubject nil))
      (navi2ch-multibbs-send-message from mail message
				     lsubject lboard larticle))))

(run-hooks 'navi2ch-message-load-hook)
;;; navi2ch-message.el ends here
