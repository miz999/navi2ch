;;; navi2ch-util.el --- useful utilities for navi2ch

;; Copyright (C) 2000 by 2$B$A$c$s$M$k(B

;; Author: (not 1)
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

;;; Code:

(require 'cl)
(require 'regexp-opt)
(require 'navi2ch-vars)
(require 'timezone)

(defvar navi2ch-mode-line-identification nil)
(make-variable-buffer-local 'navi2ch-mode-line-identification)

(defvar navi2ch-replace-html-tag-alist
  '(("&gt;" . ">")
    ("&lt;" . "<")
    ("&quot;" . "\"")
	("&nbsp;" . " ")
    ("$B!w!.(B" . ","))
  "$BCV49$9$k(B html $B$N%?%0$NO"A[%j%9%H(B($B@55,I=8=$O;H$($J$$(B)")

(defvar navi2ch-replace-html-tag-regexp
  (regexp-opt (mapcar
               'car
               navi2ch-replace-html-tag-alist))
  "$BCV49$9$k(B html $B$N%?%0$N@55,I=8=(B
`navi2ch-replace-html-tag-alist' $B$+$i@8@.$5$l$k(B")

(defsubst navi2ch-replace-string (rep new str &optional all)
  (if all
      (let (start)
        (while (setq start (string-match rep str start))
          (setq str (replace-match new nil nil str))))
    (when (string-match rep str)
      (setq str (replace-match new nil nil str))))
  str)
  
(if (featurep 'xemacs)
    (defmacro navi2ch-define-mouse-key (map num command)
      `(define-key ,map ',(intern (format "button%d" num)) ,command))
  (defmacro navi2ch-define-mouse-key (map num command)
    `(define-key ,map ,(vector (intern (format "mouse-%d" num))) ,command)))

(defun navi2ch-bigint-int-to-list (i)
  (if (listp i)
      i
    (mapcar (lambda (x)
              (- x 48))
            (string-to-list (int-to-string i)))))

(defun navi2ch-bigint-multiply (a b)
  (setq a (reverse (navi2ch-bigint-int-to-list a))
        b (reverse (navi2ch-bigint-int-to-list b)))
  (let (list c)
    (dolist (y b)
      (let ((z 0))
        (setq list (cons
                    (append c (mapcar
                               (lambda (x)
                                 (let (w)
                                   (setq w (+ (* x y) z))
                                   (setq z (/ w 10))
                                   (mod w 10))) a)
                            (if (> z 0) (list z)))
                    list)))
      (setq c (cons 0 c)))
    (let (list2)
      (dolist (x list)
        (setq list2 (navi2ch-bigint-add list2 (reverse x))))
      list2)))

(defun navi2ch-bigint-add (a b)
  (setq a (reverse (navi2ch-bigint-int-to-list a))
        b (reverse (navi2ch-bigint-int-to-list b)))
  (let ((x 0) list)
    (while (or a b)
      (let (y)
        (setq y (+ (or (car a) 0) (or (car b) 0) x))
        (setq x (/ y 10))
        (setq list (cons (mod y 10) list))
        (setq a (cdr a)
              b (cdr b))))
    list))

(defun navi2ch-insert-file-contents (file &optional begin end)
  (let ((coding-system-for-read navi2ch-net-coding-system)
        (coding-system-for-write navi2ch-net-coding-system))
    (insert-file-contents file nil begin end)))

(defun navi2ch-expand-file-name (file)
  (expand-file-name file navi2ch-directory))
  
(defun navi2ch-uudecode-region (start end)
  (interactive "r")
  (let (dir)
    (save-window-excursion
      (delete-other-windows)
      (setq dir (read-file-name "directory name: ")))
    (unless (file-directory-p dir)
      (error "%s is not directory" dir))
    
    (let ((default-directory dir)
          (coding-system-for-read 'binary)
          (coding-system-for-write 'binary)
          rc)
      (setq rc (apply
                'call-process-region
                start end
                navi2ch-uudecode-program
                nil nil nil
                navi2ch-uudecode-args))
      (when (not (= rc 0))
        (error "uudecode error")))))

;; (defun navi2ch-read-number (prompt)
;;   "$B?t;z$r(B minibuffer $B$+$iFI$_9~$`(B"
;;   (catch 'loop
;;     (while t
;;       (let (elt)
;;         (setq elt (read-string prompt init history default))
;;         (cond ((string= elt "")
;;                (throw 'loop nil))
;;               ((string-match "^[ \t]*0+[ \t]*$" elt)
;;                (throw 'loop 0))
;;               ((not (eq (string-to-number elt) 0))
;;                (throw 'loop (string-to-int elt)))))
;;       (message "Please enter a number.")
;;       (sit-for 1))))

(defsubst navi2ch-replace-html-tag (str)
  (unless (string= str "")
    (setq str (navi2ch-replace-string " *<br> " "\n" str t))
    (setq str (navi2ch-replace-string "<[^<]+>" "" str t))
    (setq str (navi2ch-replace-string "&amp" "&" str t))
    (let (start)
      (while (setq start (string-match
                          navi2ch-replace-html-tag-regexp
                          str start))
        (let ((new (cdr (assoc (match-string 0 str)
                               navi2ch-replace-html-tag-alist))))
          (setq str (replace-match new nil nil str)))))
    (setq str (navi2ch-replace-string "&#[0-9]+;" "$B".(B" str t)))
  str)

(defsubst navi2ch-replace-html-tag-with-temp-buffer (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward " *<br> " nil t)
      (replace-match "\n"))
    (goto-char (point-min))
    (while (re-search-forward "<[^<]+>" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "&amp" nil t)
      (replace-match "&"))
    (goto-char (point-min))
    (while (re-search-forward navi2ch-replace-html-tag-regexp nil t)
      (replace-match (cdr (assoc (match-string 0)
                                 navi2ch-replace-html-tag-alist))))
    (goto-char (point-min))
    (while (re-search-forward "&#[0-9]+;" nil t)
      (replace-match "$B".(B"))
    (buffer-string)))
      
      
(defun navi2ch-url-to-board (url)
  (let (uri id board kako)
    (cond ((string-match
            "http://\\(.+\\)/test/read\\.cgi.*bbs=\\([^&]+\\)" url)
           (setq id (match-string 2 url)
                 uri (format "http://%s/%s/" (match-string 1 url) id)))
	  ((string-match
            "http://\\(.+\\)/test/read\\.cgi/\\([^/]+\\)/" url)
           (setq id (match-string 2 url)
                 uri (format "http://%s/%s/" (match-string 1 url) id)))
          ((string-match
            "http://\\(.+\\)/\\([^/]+\\)/\\(kako/[0-9]+/\\)" url)
           (setq id (match-string 2 url)
                 uri (format "http://%s/%s/" (match-string 1 url) id)
                 kako (match-string 3 url)))
          ((string-match "http://\\(.+\\)/\\([^/]+\\)" url)
           (setq id (match-string 2 url)
                 uri (format "http://%s/%s/" (match-string 1 url) id))))
    (when id
      (let (name)
        (dolist (x (navi2ch-list-get-board-name-list
                    navi2ch-list-category-list))
          (when (string= (cdr (assq 'uri x)) uri)
            (setq board x)))
        (unless board
          (setq board (list (cons 'uri uri)
                            (cons 'id id)
                            (cons 'name "No Name"))))
        (cons (cons 'kako kako)
              board)))))


(defun navi2ch-url-to-article (url)
  (let (list)
    (cond ((string-match "http://.+/test/read\\.cgi.*&key=\\([0-9]+\\)" url)
           (setq list (list (cons 'artid (match-string 1 url))))
           (when (string-match "&st=\\([0-9]+\\)" url)
             (setq list (cons (cons 'number
                                    (string-to-number (match-string 1 url)))
                              list))))
	  ((string-match "http://.+/test/read\\.cgi/[^/]+/\\([^/]+\\)" url)
           (setq list (list (cons 'artid (match-string 1 url))))
           (when (string-match "http://.+/test/read\\.cgi/.+/[ni.]?\\([0-9]+\\)[^/]*$" url)
             (setq list (cons (cons 'number
                                    (string-to-number (match-string 1 url)))
                              list))))
          ((string-match "http://.+/\\([0-9]+\\)\\.\\(dat\\|html\\)" url)
           (setq list (list (cons 'artid (match-string 1 url))))))
    list))

(defun navi2ch-article-to-url (board article &optional start end nofirst)
  "BOARD, ARTICLE $B$+$i(B url $B$KJQ49!#(B
START, END, NOFIRST $B$GHO0O$r;XDj$9$k(B"
  (let ((url (navi2ch-board-get-readcgi-url board)))
    (setq url (concat
	       url
	       (cdr (assq 'artid article)) "/"
	       (and (or start end nofirst) "?")
	       (and start (format "&st=%d" start))
	       (and end (format "&to=%d" end))
	       (and nofirst "&nofirst=true")))))

(defun navi2ch-board-to-url (board)
  "BOARD $B$+$i(B url $B$KJQ49(B"
  (navi2ch-board-get-uri board))
							   
(defun navi2ch-2ch-url-p (url)
  (let (list)
    (setq list
          (mapcar
           (lambda (x)
             (let ((str (cdr (assq 'uri x))))
               (and str
                    (string-match "http://\\([^/]+\\)" str)
                    (match-string 1 str))))
           (navi2ch-list-get-board-name-list
            navi2ch-list-category-list)))
    (when (string-match "http://\\([^/]+\\)" url)
      (setq url (match-string 1 url))
      (member url list))))
                      
  
(defun navi2ch-goto-url (url &optional force)
  "URL $B$+$i%9%l$^$?$OHD$rA*$V(B"
  (interactive "sURL: ")
  (let ((list-win (get-buffer-window navi2ch-list-buffer-name))
        (board-win (get-buffer-window navi2ch-board-buffer-name))
        (art-win (and (navi2ch-article-current-buffer)
                      (get-buffer-window (navi2ch-article-current-buffer))))
	(article (navi2ch-url-to-article url))
	(board (navi2ch-url-to-board url)))
    (when board
      (cond (art-win
	     (select-window art-win)
	     (unless article
	       (navi2ch-article-exit)))
	    (board-win
	     (select-window board-win)
	     (when article
	       (split-window-vertically navi2ch-board-window-height)
	       (other-window 1)))
	    (list-win
	     (select-window list-win)
	     (when navi2ch-list-stay-list-window
	       (split-window-horizontally navi2ch-list-window-width)
	       (other-window 1))))
      (if article
	  (progn
	    (navi2ch-history-add board article)
	    (navi2ch-article-view-article board
					  article
					  force
					  (cdr (assq 'number article))))
	(navi2ch-board-select-board board force)))))

(defun navi2ch-y-or-n-p (prompt &optional quit-symbol)
  (let ((prompt (concat prompt "(y, n, or q) "))
	(again nil))
    (catch 'exit
      (while t
	(message prompt)
	(let ((c (read-char)))
	  (cond ((memq c '(?q ?Q))
		 (message (concat prompt "q"))
		 (throw 'exit (or quit-symbol nil)))
		((memq c '(?y ?Y ?\ ))
		 (message (concat prompt "y"))
		 (throw 'exit t))
		((memq c '(?n ?N ?\177 ))
		 (message (concat prompt "n"))
		 (throw 'exit nil))
		((eq c 12)
		 (recenter))
		(t
		 (ding)
		 (or again
		     (setq prompt (concat "Please answer y, n, or q.  " prompt)
			   again t)))))))))
  
(defun navi2ch-browse-url (url)
  (cond ((and navi2ch-browse-url-image-program	; images
	      (file-name-extension url)
	      (member (downcase (file-name-extension url))
		      navi2ch-browse-url-image-extentions))
	 (navi2ch-browse-url-image url))
	(t (browse-url				; others
	    url
	    (cond ((boundp 'browse-url-new-window-p) browse-url-new-window-p)
		  ((boundp 'browse-url-new-window-flag) browse-url-new-window-flag))))))

(defun navi2ch-browse-url-image (url &optional new-window)
  ;; new-window ignored
  "Ask the WWW browser defined by `browse-url-image-program' to load URL.
Default to the URL around or before point.  A fresh copy of the
browser is started up in a new process with possible additional arguments
`navi2ch-browse-url-image-args'.  This is appropriate for browsers which
don't offer a form of remote control."
  (interactive (browse-url-interactive-arg "URL: "))
  (if (not navi2ch-browse-url-image-program)
    (error "No browser defined (`navi2ch-browse-url-image-program')"))
  (apply 'start-process (concat navi2ch-browse-url-image-program url) nil
         navi2ch-browse-url-image-program
         (append navi2ch-browse-url-image-args (list url))))

;; from apel
(defun navi2ch-put-alist (item value alist)
  "Modify ALIST to set VALUE to ITEM.
If there is a pair whose car is ITEM, replace its cdr by VALUE.
If there is not such pair, create new pair (ITEM . VALUE) and
return new alist whose car is the new pair and cdr is ALIST.
\[tomo's ELIS like function]"
  (let ((pair (assoc item alist)))
    (if pair
        (progn
          (setcdr pair value)
          alist)
      (cons (cons item value) alist))))

(defun navi2ch-next-property (point prop)
  (when (get-text-property point prop)
    (setq point (next-single-property-change point prop)))
  (when point
    (setq point (next-single-property-change point prop)))
  point)

(defun navi2ch-previous-property (point prop)
  (when (get-text-property point prop)
    (setq point (previous-single-property-change point prop)))
  (when point
    (unless (get-text-property (1- point) prop)
      (setq point (previous-single-property-change point prop)))
    (when point
      (1- point))))

(defun navi2ch-set-minor-mode (mode name map)
  (make-variable-buffer-local mode)
  (unless (assq mode minor-mode-alist)
    (setq minor-mode-alist
          (cons (list mode name) minor-mode-alist)))
  (unless (assq mode minor-mode-map-alist)
    (setq minor-mode-map-alist 
          (cons (cons mode map) minor-mode-map-alist))))

(defun navi2ch-call-process-buffer (program &rest args)
  "$B:#$N(B buffer $B$G(B PROGRAM $B$r8F$s$GJQ99$9$k(B"
  (apply 'call-process-region (point-min) (point-max) program t t nil args))

(defsubst navi2ch-alist-list-to-alist (list key1 key2)
  (mapcar
   (lambda (x)
     (cons (cdr (assq key1 x))
	   (cdr (assq key2 x))))
   list))

(defun navi2ch-write-region (begin end filename)
  (write-region begin end filename nil 'no-msg))

(defun navi2ch-enable-readcgi-p (board)
  "read.cgi $B$r;H$&HD$+$I$&$+$rJV$9!#(B"
  (if navi2ch-enable-readcgi
      (not (member (navi2ch-board-get-host board)
		   navi2ch-disable-readcgi-host-list))
    (member (navi2ch-board-get-host board)
	    navi2ch-enable-readcgi-host-list)))

;; from apel
(if (featurep 'xemacs)
    (defun navi2ch-set-buffer-multibyte (flag)
      flag)
  (defun navi2ch-set-buffer-multibyte (flag)
    (set-buffer-multibyte flag)))

;; from apel
(if (featurep 'xemacs)
    (defun navi2ch-string-as-unibyte (string)
      string)
  (defun navi2ch-string-as-unibyte (string)
    (string-as-unibyte string)))

(defun navi2ch-get-major-mode (buffer)
  (when (get-buffer buffer)
    (save-excursion
      (set-buffer buffer)
      major-mode)))

(defun navi2ch-set-mode-line-identification ()
  (let ((offline '(navi2ch-offline navi2ch-offline-off navi2ch-offline-on)))
    (unless navi2ch-mode-line-identification
      (setq navi2ch-mode-line-identification "%12b"))
    (setq mode-line-buffer-identification
          (list ""
                offline
                navi2ch-mode-line-identification)))
  (force-mode-line-update t))

(defun navi2ch-make-datevec (time)
  (timezone-fix-time
   (let ((dtime (decode-time time)))
     (apply 'timezone-make-arpa-date
            (mapcar (lambda (x) (nth x dtime)) '(5 4 3 2))))
   nil nil))

;;; from Wanderlust (elmo-date.el)
(defun navi2ch-get-offset-datevec (datevec offset &optional time)
  (let ((year  (aref datevec 0))
        (month (aref datevec 1))
        (day   (aref datevec 2))
        (hour     (aref datevec 3))
        (minute   (aref datevec 4))
        (second   (aref datevec 5))
        (timezone (aref datevec 6))
        day-number p
        day-of-month)
    (setq p 1)
    (setq day-number (- (timezone-day-number month day year)
                        offset))
    (while (<= day-number 0)
      (setq year (1- year)
            day-number (+ (timezone-day-number 12 31 year)
                          day-number)))
    (while (> day-number (setq day-of-month
                               (timezone-last-day-of-month p year)))
      (setq day-number (- day-number day-of-month))
      (setq p (1+ p)))
    (setq month p)
    (setq day day-number)
    (timezone-fix-time
     (format "%d %s %d %s %s"
             day
             (car (rassq month timezone-months-assoc))
             year
             (if time
                 (format "%d:%d:%d" hour minute second)
               "0:00")
             (cadr timezone)) nil nil)))

;;; from Wanderlust (elmo-date.el)
(defmacro navi2ch-make-sortable-date (datevec)
  "Make a sortable string from DATEVEC."
  (` (timezone-make-sortable-date
      (aref (, datevec) 0)
      (aref (, datevec) 1)
      (aref (, datevec) 2)
      (aref (, datevec) 3))))

(defun navi2ch-clear-seen ()
  (interactive)
  (dolist (board (navi2ch-list-get-board-name-list
		  navi2ch-list-category-list))
    (let ((navi2ch-board-current-board board))
      (unless (assq 'type navi2ch-board-current-board)
	(navi2ch-board-load-info)
	(setq navi2ch-board-current-board
	      (delete (assq 'seen navi2ch-board-current-board)
		      navi2ch-board-current-board))
	(setq navi2ch-board-current-board
	      (delete (assq nil navi2ch-board-current-board)
		      navi2ch-board-current-board))
	(setq navi2ch-board-current-board
	      (navi2ch-put-alist 'seen nil navi2ch-board-current-board))
	(navi2ch-board-save-info)))))

(defun navi2ch-end-of-buffer ()
  (interactive)
  (goto-char (point-max))
  (forward-line -1))

(provide 'navi2ch-util)
