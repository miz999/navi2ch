;;; navi2ch-thumbnail-internal.el --- thumbnail view for navi2ch -*- coding: utf-8-unix; -*-
;; Copyright (C) 2020 by Navi2ch Project

;; Authors: MIZUNUMA Yuto <mizmiz@users.sourceforge.net>
;; Keywords: network 2ch

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

;; サムネイルを表示する機能です
;; 画像表示に対応したemacsで動きます

;; 画像リンクURL上で','を押すとサムネイル画像を挿入表示します。自動取得、
;; 自動表示はしません。基本的にキーで駆動です。キャッシュを持っている画
;; 像は自動表示されます。キャッシュの自動削除機能はありません。
;;

;;; Code

(require 'json)

(defcustom navi2ch-thumbnail-p t
  "* サムネイル表示する"
  :type 'boolean
  :group 'navi2ch)

(defcustom navi2ch-thumbnail-thumbnail-directory
  (expand-file-name "navi2ch-thumbnails/" navi2ch-directory)
  "* 画像キャッシュディレクトリ"
  :type 'string
  :group 'navi2ch)

(defcustom navi2ch-thumbnail-thumbsize-width 300
  "* サムネイル表示サイズ横(等倍縮小でアスペクト比保持)"
  :type 'integer
  :group 'navi2ch)

(defcustom navi2ch-thumbnail-thumbsize-height 150
  "* サムネイル表示サイズ縦(等倍縮小でアスペクト比保持)"
  :type 'integer
  :group 'navi2ch)

(define-key navi2ch-article-mode-map "." 'navi2ch-thumbnail-show-image-external-full);;普通のサイズの画像表示
(define-key navi2ch-popup-article-mode-map "." 'navi2ch-thumbnail-show-image-external-full);;オリジナルサイズの画像表示

;(defvar navi2ch-thumbnail-reduction-width 1200 "画像ビューアーでいい感じの大きさを表示するときの横")
;(defvar navi2ch-thumbnail-reduction-height 800 "画像ビューアーでいい感じの大きさを表示するときの縦")

(defvar navi2ch-browse-local-image-program nil "画像ビューアー")
(defvar navi2ch-browse-local-image-args nil "画像ビューアーを呼ぶときの引数")

(defvar navi2ch-thumbnail-script-dir "~/navi2ch/navi2ch-dev/thumbnail-script/" "画像取得用スクリプトのあるディレクトリ")
(defvar navi2ch-thumbnail-curl_imgur_thumb.script "curl_imgur_thumb.sh" "imgurの画像を取得するスクリプト")
(defvar navi2ch-thumbnail-curl_external.script "curl_external.sh" "twitter等の画像を取得するスクリプト" )
(defvar navi2ch-thumbnail-appspot-script "appspot.sh" "画像縮小サーバ経由で画像を取得するスクリプト" )

(defvar navi2ch-thumbnail-count-string nil "サムネイル関連でモードラインに表示してほしい文字列" )

(cond
 ((equal system-type 'gnu/linux)
  (setq navi2ch-browse-local-image-program "eog")
  (setq navi2ch-thumbnail-curl_imgur_thumb.script "curl_imgur_thumb.sh")
  (setq navi2ch-thumbnail-curl_external.script "curl_external.sh")
  (setq navi2ch-thumbnail-appspot-script "appspot.sh"))
 ((or (equal system-type 'windows-nt) (equal system-type 'cygwin))
  (setq navi2ch-thumbnail-curl_imgur_thumb.script "curl_imgur_thumb.bat")
  (setq navi2ch-thumbnail-curl_external.script "curl_external.bat")
  (setq navi2ch-thumbnail-appspot-script "appspot.bat")))

;;cygwinはパス名が面倒
(when (equal system-type 'cygwin)
  (require 'cygwin-mount))

(defun navi2ch-thumbnail-cygwin-to-win (path)
  (if (eq system-type 'cygwin) (cygewin-convert-file-name-to-windows path) path))
    
(defmacro navi2ch-thumbnail-cygwin-to-win-setq-macro (path)
  (list 'setq path (list 'if '(eq system-type cygwin) '(cygwin-convert-file-name-to-windows path) path)))

(defvar navi2ch-thumbnail-image-url-regex
  "\\(h?t?tps?://[^ 　\t\n\r]+\\.\\(gif\\|jpe?g\\|png\\)\\)" "articleから画像らしきリンクを探すregexを1行にまとめる")

(defun navi2ch-thumbnail-insert-image-reload ()
  "スレが再描画される時にサムネも再描画"
;  (interactive)
  (message "image-reload")
  (when navi2ch-thumbnail-p
    (let (url)
      (when (display-images-p)
	(save-excursion
;	  (let ((buffer-read-only nil))
	    (goto-char (point-min))
	    (while (re-search-forward navi2ch-thumbnail-image-url-regex nil t)
	      (setq url (match-string 1))
	      (navi2ch-thumbnail-image-pre url nil)))))))
;  )

(defvar navi2ch-thumbnail-point-list nil)
(defvar navi2ch-thumbnail-bat-process nil)

(defun navi2ch-thumbnail-bat-process-push (l)
;  (message "pl=%s" (length navi2ch-thumbnail-point-list))
  (if (and (> (length navi2ch-thumbnail-point-list) 0)
;    (message "l=%s pl=%s" (cdr (assq 'file (navi2ch-net-split-url (nth 3 l)))) (mapcar (lambda (url) (cdr (assq 'file (navi2ch-net-split-url (nth 3 url))))) navi2ch-thumbnail-point-list))
    (member (cdr (assq 'file (navi2ch-net-split-url (nth 3 l)))) (mapcar (lambda (url) (cdr (assq 'file (navi2ch-net-split-url (nth 3 url))))) navi2ch-thumbnail-point-list)))
      (message "push already in %s" l)
  (navi2ch-thumbnail-process-count-up)
  (add-to-list 'navi2ch-thumbnail-point-list l t)
  (when (or (not navi2ch-thumbnail-bat-process)
	    (and navi2ch-thumbnail-bat-process (not (eq (process-status navi2ch-thumbnail-bat-process) 'run))))
      (navi2ch-thumbnail-bat-process-pop))))

(defun navi2ch-thumbnail-bat-process-pop ()
  (let ((poped (car navi2ch-thumbnail-point-list)))
    (when poped
      (setq navi2ch-thumbnail-point-list (cdr navi2ch-thumbnail-point-list))
      (cond ((string= (nth 0 poped) navi2ch-thumbnail-curl_imgur_thumb.script)
	     (setq navi2ch-thumbnail-bat-process
		   (start-process (nth 2 poped)
				  "curl-get-image" (concat navi2ch-thumbnail-script-dir (nth 0 poped)) (nth 3 poped)
				  (nth 4 poped) (nth 5 poped) (nth 6 poped)))
	     (set-process-filter navi2ch-thumbnail-bat-process (nth 1 poped)))
	    ((string= (nth 0 poped) navi2ch-thumbnail-curl_external.script)
	     (setq navi2ch-thumbnail-bat-process
		   (start-process (nth 2 poped)
				  "curl-get-image" (concat navi2ch-thumbnail-script-dir (nth 0 poped)) (nth 3 poped)
				  (nth 4 poped)))
	     (set-process-filter navi2ch-thumbnail-bat-process (nth 1 poped)))
	    (t
	     (message "謎process pop %s" poped))))))

(defvar navi2ch-thumbnail-process-count 0)
;(defvar navi2ch-thumbnail-process-list nil)

(defun navi2ch-thumbnail-process-count-up ()
    (navi2ch-thumbnail-process-count-updown 1))

(defun navi2ch-thumbnail-process-count-down ()
    (navi2ch-thumbnail-process-count-updown -1))

(defun navi2ch-thumbnail-process-count-updown (updown)
  (setq navi2ch-thumbnail-process-count (+ navi2ch-thumbnail-process-count updown))
  (setq navi2ch-thumbnail-count-string (if (= navi2ch-thumbnail-process-count 0)
                                                nil (format "画待:%s " navi2ch-thumbnail-process-count))))
    
(defun navi2ch-thumbnail-process-count-reset ()
    (interactive)
    (setq navi2ch-thumbnail-process-count 0))

(setq navi2ch-thumbnail-use-image-server t)
(setq navi2ch-thumbnail-imgserver-name "http://thumbmake.appspot.com/main")

(defun navi2ch-thumbnail-browse-local-image (filename)
  (if (not navi2ch-browse-local-image-program)
      (error "No browse-local-image-proguram defined (`navi2ch-browse-local-image-program')"))
  (apply 'start-process (concat navi2ch-browse-local-image-program filename) nil
         navi2ch-browse-local-image-program
         (append navi2ch-browse-local-image-args (list filename))))

(defun navi2ch-thumbnail-save-content (&optional target-dir)
  "キャッシュから画像を保存(サムネイルではなく元画像)"
  (interactive
   (let* ((overwrite t)
          (prop-filename (get-text-property (point) 'navi2ch-link))
	  (default-filename (and prop-filename (file-name-nondirectory prop-filename))))
     (or (file-exists-p prop-filename)
         (error "Not yet get file to save."))     
     (let ((filename
		  (read-file-name
		   (if default-filename
		       (format "Save file (default `%s'): "
			       default-filename)
		     "Save file: ")
		   navi2ch-thumbnail-save-content-dir
		   (expand-file-name default-filename
				     navi2ch-thumbnail-save-content-dir))))
	     (if (file-directory-p filename)
		 (if default-filename
		     (expand-file-name default-filename filename)
		   (error "%s is a directory" filename))
	       filename)
             (copy-file prop-filename filename overwrite)))))

(defun navi2ch-thumbnail-twitter (noexturl ext)
  (let ((url (concat noexturl "." ext))
	(thumb-name (expand-file-name (concat (navi2ch-thumbnail-url-to-file noexturl) ".thumb." ext))))
    (if (not (file-exists-p thumb-name))
	(navi2ch-thumbnail-twitter-process-push url)
      (navi2ch-thumbnail-insert-image nil nil nil url thumb-name))))

;; (defun navi2ch-thumbnail-twitter-insert (url)
;;   "twitterのAPI(？)風のURLを使ってサムネイルを取得する。url末尾に:thumbをつけるとサムネ"
;;   (navi2ch-thumbnail-process-count-up)
;;   (message "twitter-insert:%s => %s" url (navi2ch-thumbnail-url-to-file url))
;;   (string-match "\\(https?://pbs.twimg.com/media/.+\\)\\..+$" url)
;;   (when (match-string 1 url)
;;     (let* ((local-file (navi2ch-thumbnail-url-to-file (concat (match-string 1 url) ".thumb.jpg")))
;; 	   (url-thumb (concat url ":thumb"))
;; 	   (proc 
;; 	    (start-process (concat "curl-get-image_" url-thumb "_" (buffer-name) "_" (number-to-string (point)))
;; 			   "curl-get-image" navi2ch-thumbnail-curl_external.script
;; 			   url-thumb (navi2ch-thumbnail-cygwin-to-win local-file))))
;;       (set-process-filter proc 'navi2ch-thumbnail-twitter-process-callback))))

(defun navi2ch-thumbnail-twitter-process-push (url)
  "twitterのAPI(？)風のURLを使ってサムネイルを取得する。url末尾に:thumbをつけるとサムネ。スタックに積む"
;  (navi2ch-thumbnail-process-count-up)
  (message "twitter-process-push:%s" url)
;;  (string-match "\\(https?://pbs.twimg.com/.+\\)\\..+$" url)
  (string-match (concat "\\(https?://pbs\.twimg\.com/\\(?:media\\|ext_tw_video_thumb\\)/[^:#]+\\)\."
			(regexp-opt navi2ch-browse-url-image-extentions t) ":?\\(.*\\)$") url)
    (let* ((thumb (navi2ch-thumbnail-url-to-file (concat (match-string 1 url) ".thumb." (match-string 2 url))))
	   (url-thumb (concat url ":thumb")))
      (navi2ch-thumbnail-bat-process-push
       (list navi2ch-thumbnail-curl_external.script 'navi2ch-thumbnail-twitter-process-callback
	   (concat "curl-get-image_" url-thumb "_" (buffer-name) "_" (number-to-string (point)))
	   url-thumb thumb))))

(defun navi2ch-thumbnail-twitter-process-callback (proc result)
  (navi2ch-thumbnail-process-count-down)
  (message "twitter callback:%s" (replace-regexp-in-string  "\n+$" "" result))
  (let ((pn (process-name proc)))
    (string-match "curl-get-image_\\(https?://pbs.twimg.com/\\(?:media\\|ext_tw_video_thumb\\)/\\)\\([^.]+\\)\.\\([^:]+\\):thumb_\\(.+\\)_\\(.+\\)$" pn)
;;    (string-match "curl-get-image_\\(https?://pbs.twimg.com/media/\\([^.]+\\)\.\\([^:]+\\)\\):thumb_\\(.+\\)_\\(.+\\)$" pn)
    (let* ((link (match-string 1 pn))
	   (id (match-string 2 pn))
	   (ext (match-string 3 pn))
	   (bufname (match-string 4 pn))
	   (pointnum (match-string 5 pn))
	   (local-file (navi2ch-thumbnail-url-to-file (concat link id ".thumb." ext)))
	   (url (concat link id "." ext)))
      (when (file-exists-p local-file)
	(with-current-buffer (set-buffer bufname)
	  (save-excursion
	    (let ((buffer-read-only nil))
	      (goto-char (string-to-number pointnum))
	      ;; ;;最初のサーチでエラーが出るが謎(point忘れてる？)
	      (re-search-forward id nil t)
	      ;; (unless (re-search-forward id nil t)
	      ;; 	(message  "twitter-callback search error %s pointnum->%s point->%s" id pointnum (point)))
	      (navi2ch-thumbnail-insert-image nil nil nil url local-file))))))
    (when (listp navi2ch-thumbnail-point-list)
      (navi2ch-thumbnail-bat-process-pop))))

;Authorization: ebe3ee4157ab24a 8c5a1f6cd438be7b21d6cdf8cdba2f917bb97513
;https://api.imgur.com/3/image/{id}
;t 	Small Thumbnail 	160x160 	Yes
;m 	Medium Thumbnail 	320x320 	Yes
;l 	Large Thumbnail 	640x640 	Yes
;h 	Huge Thumbnail 	1024x1024 	Yes
(setq navi2ch-thumbnail-imgur-use-api t) ;to navi2ch-var.el
(defun navi2ch-thumbnail-imgur-insert-thumbnail (id ext)
  (let* ((size-flag "t")
	 (url-t (concat "https://i.imgur.com/" id size-flag ".jpg"))
	 (url-full (concat "https://i.imgur.com/" id "." ext))
	 (filename-t (navi2ch-thumbnail-url-to-file url-t))
	 (filename-full (navi2ch-thumbnail-url-to-file url-full))
	 (target-file (concat navi2ch-thumbnail-thumbnail-directory "i.imgur.com/" id ".json"))
	 w h s gifv link header fname prop-list)

    (if (setq prop-list (navi2ch-thumbnail-image-prop-list-get url-full))
        (progn
          (setq link (nth 0 prop-list))
          (setq w (nth 1 prop-list))
          (setq h (nth 2 prop-list)) 
          (setq s (nth 3 prop-list)))
      (when (and (file-exists-p target-file) (navi2ch-thumbnail-valid-file-p target-file))
	(let ((imgur-json (json-read-file target-file)))
	  (setq w (cdr (assoc 'width (cdr (assoc 'data imgur-json)))))
	  (setq h (cdr (assoc 'height (cdr (assoc 'data imgur-json)))))
	  (setq s (cdr (assoc 'size (cdr (assoc 'data imgur-json)))))
	  (setq gifv (cdr (assoc 'gifv (cdr (assoc 'data imgur-json)))))
	  (setq link (cdr (assoc 'link (cdr (assoc 'data imgur-json)))))
	  (navi2ch-thumbnail-image-prop-list-set link w h s))))
    
    (if (file-exists-p filename-t)
	(navi2ch-thumbnail-insert-image w h s url-full filename-t)
      (if navi2ch-thumbnail-imgur-use-api
	  (navi2ch-thumbnail-imgur-insert-thumbnail-curl id ext)
	(navi2ch-thumbnail-get-from-imgserver url-full)))))

(defun navi2ch-thumbnail-insert-image (w h s url thumb-fname)
  (save-excursion
    (let ((buffer-read-only nil))
      (move-beginning-of-line nil)
      (insert-image (create-image thumb-fname))
      (add-text-properties (1- (point)) (point)
                           (list 'link t 'link-head t 'url url
                                 'navi2ch-link-type 'image 'navi2ch-link (navi2ch-thumbnail-url-to-file url)
				 'original-filesize s 'file-name thumb-fname))
      (when (and w h s)
        (insert (format " (thumb %sx%s:%sk)" w h (round (/ (if (number-or-marker-p s)s (string-to-number s)) 1024)))))
      (navi2ch-thumbnail-set-url-shown))))

(defun navi2ch-thumbnail-valid-file-p (file)
  (cond
   ((not (file-exists-p file))
    (message "file not exist %s" file) nil)
   ((= (nth 7 (file-attributes file)) 0)
    (message "file size zero -> delete file %s" file)
    (delete-file file) nil)
   ((with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (re-search-forward
       "<title>imgur: the simple 404 page</title>"
       nil t))
    (message "%s 404 error file delete" file)
    (delete-file file) nil)
   (t
    t)))

(defun navi2ch-thumbnail-imgur-insert-thumbnail-curl (id ext)
  (let ((url (concat "https://i.imgur.com/" id "." ext))
	(url-thumb (concat "https://i.imgur.com/" id "t.jpg"))
	(target-file (concat navi2ch-thumbnail-thumbnail-directory "i.imgur.com/" id "t.jpg"))
	(url-json (concat "https://api.imgur.com/3/image/" id ".json"))
	(file-json (concat navi2ch-thumbnail-thumbnail-directory "i.imgur.com/" id ".json")))    
    (navi2ch-thumbnail-bat-process-push
     (list navi2ch-thumbnail-curl_imgur_thumb.script 'navi2ch-thumbnail-imgur-process-callback-thumb-curl
	   (concat "curl-get-image_" id "_" (buffer-name) "_" (number-to-string (point)))
	   url-thumb target-file url-json file-json))))

(defun navi2ch-thumbnail-imgur-process-callback-thumb-curl (proc result)
  (message "imgur callback:%s" (replace-regexp-in-string  "\n+$" "" result))
  (navi2ch-thumbnail-process-count-down)
  (let ((pn (process-name proc)))
    (string-match "curl-get-image_\\(.+\\)_\\(.+\\)_\\(.+\\)$" pn)
    (let* ((id (match-string 1 pn))
	   (bufname (match-string 2 pn))
	   (pointnum (match-string 3 pn))
	   (json-file (concat navi2ch-thumbnail-thumbnail-directory "i.imgur.com/" id ".json"))
	   (thumb (concat navi2ch-thumbnail-thumbnail-directory "i.imgur.com/" id "t.jpg")))
      (when (and (file-exists-p json-file) (file-exists-p thumb))
	(let* ((imgur-json (cdr (assoc 'data (json-read-file json-file))))
	       (w (cdr (assoc 'width imgur-json)))
	       (h (cdr (assoc 'height imgur-json)))
	       (s (cdr (assoc 'size imgur-json)))
	       (gifv (cdr (assoc 'gifv imgur-json)))
	       (link (cdr (assoc 'link imgur-json))))
	(if (not (get-buffer bufname))
	    (message "imgur buffer is dead:%s" bufname)
	    (with-current-buffer (set-buffer bufname)
	  (save-excursion
		(when (and w h s)
		  (navi2ch-thumbnail-image-prop-list-set link w h s)
		  (goto-char (- (string-to-number pointnum) 1))
		  ;;   ;;最初のサーチでエラーが出るが謎(point忘れてる？)
		  ;; (unless (re-search-forward id nil t)
		  (re-search-forward id nil t)
		  ;;     (message  "imgur-callback search error %s pointnum->%s point->%s" id pointnum (point)))
		  (navi2ch-thumbnail-insert-image w h s link thumb)
;;		  (move-beginning-of-line nil)
;; 		  (insert-image (navi2ch-create-image thumb))
;; 		  (add-text-properties
;; 		   (1- (point)) (point)
;; 		   (list 'link t 'link-head t
;; 			 'url link
;; ;			 'help-echo thumb
;; 			 'original-filesize s
;; 			 'navi2ch-link-type 'image
;; 			 'navi2ch-link (navi2ch-thumbnail-url-to-file link)
;; 			 'file-name thumb))
;; 		  (insert (format " (thumb %sx%s:%sk%s)" w h (round (/ s 1024)) (if gifv " AnimeGIF" "")))
		  )))))))
	(navi2ch-thumbnail-bat-process-pop)))

(defun navi2ch-thumbnail-imgur-external-open (id ext url mode)
  (let* ((original-fname (navi2ch-thumbnail-url-to-file (concat "https://i.imgur.com/" id ext)))
         (h-fname (navi2ch-thumbnail-url-to-file (concat "https://i.imgur.com/" id "h.jpg" )))
         (json-file (navi2ch-thumbnail-url-to-file (concat "https://i.imgur.com/" id ".json" )))
         (fname original-fname)
	 (navi2ch-net-http-proxy nil)
	 (original-filesize (get-text-property (point) 'original-filesize))
	 h-filesize)
    (cond 
     ((file-exists-p original-fname)
      (navi2ch-thumbnail-browse-local-image original-fname))
     ((and (eq mode 'reduction) (file-exists-p h-fname))
      (navi2ch-thumbnail-browse-local-image h-fname))
     (t
      (cond ((eq mode 'reduction)
	     (setq h-filesize (cdr (assq 'content-length (navi2ch-net-get-header (navi2ch-net-send-request (concat "https://i.imgur.com/" id "h.jpg") "HEAD")))))
	     (cond
	      ;;縮小画像の圧縮率が悪くメリットが薄い場合はオリジナルのファイルを取得
	      ((and original-filesize (< original-filesize 80000))
	       (setq fname original-fname))
	      ((or (not  original-filesize)
		   (and 
		    (> original-filesize 70000)
		    (< (/ (float (string-to-number h-filesize)) (float original-filesize)) 0.8)))
	       (setq fname h-fname)
	       (setq url (concat "https://i.imgur.com/" id "h.jpg")))
	      (t (message "圧縮率が低いのでオリジナルサイズのファイル取得")))))
;      (navi2ch-thumbnail-external-open-thread url fname)
      (set-process-filter (start-process (concat "curl-get-image_" fname)
					 "curl-get-image" (concat navi2ch-thumbnail-script-dir navi2ch-thumbnail-curl_external.script) url fname)
			  'navi2ch-thumbnail-external-open-callback)))))

(defun navi2ch-thumbnail-external-open-callback (proc result)
  (let ((pn (process-name proc))
	(result (replace-regexp-in-string  "\n+$" "" result)))
    (if (string-match "^zero " result)
        (message "ファイル取得できません:%s" result)
;      (message "外部ビューアーで開きます:%s" result)
      (string-match "curl-get-image_\\([^<]+\\)" pn)
      (navi2ch-thumbnail-browse-local-image (match-string 1 pn)))))

(defun navi2ch-thumbnail-external-open-thread (url fname &optional referer force)
  (let (
	(thread (make-thread `(lambda (url fname)
		    (message "thread call %s" fname)
		    (setq proc (navi2ch-net-update-file url fname nil nil nil nil nil))
		    (message "%s %s" fname proc)
		    (when proc
		      (message "header %s" (navi2ch-net-get-header proc)))) url fname))
	proc header thread)
    (message "thread name %s" thread)))
;; (all-threads)
;; (thread-join (current-thread))
;; (thread-last-error)
;; (thread-yield)

(defun navi2ch-thumbnail-get-from-imgserver (url &optional thumb referer force)
  (when (< navi2ch-thumbnail-process-count 15)
    (setq navi2ch-thumbnail-process-count (+ navi2ch-thumbnail-process-count 1))
      (setq proc
            (start-process (concat "bat-get-image_" url "_" (buffer-name) "_" (number-to-string (point)))
                           "bat-get-image"
                           (expand-file-name "java_lock.bat" navi2ch-top-directory )
                           navi2ch-thumbnail-java-imagefetch-classpass
                           url
                           navi2ch-thumbnail-thumbnail-directory
                       (format "%s" navi2ch-thumbnail-thumbsize-width)
                           (format "%s" navi2ch-thumbnail-thumbsize-height) (if thumb thumb "jpg")))
  (set-process-filter proc 'navi2ch-thumbnail-process-callback-thumb)))

(setq navi2ch-thumbnail-process-callback-thumb-line "")
(defun navi2ch-thumbnail-process-callback-thumb (proc file)
  "FILE で渡された画像をスレに挿入する．PROC が終了すると呼ばれる．"  
  (if (not (string-match ".+}" file))
      (progn 
        (setq navi2ch-thumbnail-process-callback-thumb-line (concat navi2ch-thumbnail-process-callback-thumb-line file)))
    (let ((buf (buffer-name (process-buffer proc)))
          header w h s fname id)
      (setq file (concat navi2ch-thumbnail-process-callback-thumb-line file))
      (setq navi2ch-thumbnail-process-callback-thumb-line "")
      (setq navi2ch-thumbnail-process-count (- navi2ch-thumbnail-process-count 1))
   
      (setq navi2ch-message-samba24-mode-string (if (> navi2ch-thumbnail-process-count 0)
                                                    (format "imgp:%s " navi2ch-thumbnail-process-count)
                                                  nil))
     
   (setq pn (process-name proc))
   (string-match "bat-get-image_https?://\\(.+\\)_\\(.+\\)_\\(.+\\)$" pn)
   (setq id (match-string 1 pn))
   (setq bufname (match-string 2 pn))
   (setq pointnum (match-string 3 pn))
   (if (not (string-match "{.+}" file))
       (message "json format error:%s " file)
     (setq header (json-read-from-string file))
     (if (or (cdr (assq 'X-IMAGE-IMGUR-INFO-ERROR header)) (cdr (assq 'X-IMAGE-WGET-INFO-ERROR header)) (not (assq 'FileName header)))
         (message "imageFetch error:%s " file)
       (setq w (cdr (assq 'X-IMAGE-WIDTH header)))
       (setq h (cdr (assq 'X-IMAGE-HEIGHT header)))
       (setq s (cdr (assq 'X-IMAGE-SIZE header)))
       (setq gifv (cdr (assq 'X-IMAGE-GIFV header)))
;       (setq fname (cdr (assq 'FileName header)))
       (if (cdr (assq 'FileName header))
           (setq fname
                 (if (eq system-type 'cygwin)
                     (cygwin-convert-file-name-from-windows (cdr (assq 'FileName header)))
                   (cdr (assq 'FileName header))))
                (message "fname error %s" header))
       (when (not (and w h s fname))
         (when (cdr (assq 'X-IMAGE-SAME header))
           (if (setq prop-list(navi2ch-thumbnail-image-prop-list-get (cdr (assq 'URL header))))
               (progn 
                 (setq w (nth 1 prop-list)) 
                 (setq h (nth 2 prop-list)) 
                 (setq s (nth 3 prop-list))))))

       (if (not (image-type-from-file-header fname))
           (progn 
             (message "delete ascii file: %s" fname)
             (delete-file fname))
       (if (and (not (and w h s fname)) (not (cdr (assq 'X-IMAGE-SAME header))) (not (or (cdr (assq 'x-twimg header))
                                                                                         (cdr (assq 'x-imepic header)))))
           (message "not sufficient:%s" file)
         (if (not (navi2ch-thumbnail-image-prop-list-get (cdr (assq 'URL header))))
             (navi2ch-thumbnail-image-prop-list-set (cdr (assq 'URL header)) w h s))
         
         (when (cdr (assq 'X-IMAGE-TOO-SMALL header))
           (message "x-image-too-small: %s" (cdr (assq 'X-IMAGE-TOO-SMALL header))))
         (if (= (nth 7 (file-attributes fname)) 0)
             (message "file emtpy: %s" fname))
         (when (and (process-mark proc))
           (with-current-buffer (set-buffer bufname)
             (save-excursion
               (let ((buffer-read-only nil))
                 (goto-char 1)
                 (search-forward id nil t)
                 (move-beginning-of-line nil)
                 (cond ((cdr (assq 'X-IMAGE-DENY header))
                        (message "x-image-deny: %s" (cdr (assq 'X-IMAGE-DENY header)))
                        (insert " (deny image)"))                       
                       (t
                         (insert-image (create-image fname))
                        (add-text-properties
                         (1- (point)) (point)
                         (list 'link t 'link-head t
                               'url (cdr (assq 'URL header))
                               'help-echo fname
                               'original-filesize s
                               'navi2ch-link-type 'image
                               'navi2ch-link (if (eq system-type 'cygwin) 
                                                 (cygwin-convert-file-name-from-windows (cdr (assq 'OriginalFilename header)))
                                               (cdr (assq 'OriginalFilename header)))
                               'file-name fname))

                        (when (and w h s fname)
                          (insert (format " (thumb %sx%s:%sk%s)" w h (round (/ (string-to-number s) 1024)) (if gifv " AnimeGIF" "")))))))))))))))))

(defun navi2ch-thumbnail-process-callback (proc file)
  "FILE で渡された画像をスレに挿入する．PROC が終了すると呼ばれる．"
  (if (not (string-match ".+}" file))
      (progn 
        (setq navi2ch-thumbnail-process-callback-thumb-line (concat navi2ch-thumbnail-process-callback-thumb-line file)))
    (let ((buf (buffer-name (process-buffer proc)))
          (stat (and proc (process-status proc)))
          header w h s fname)
      (setq navi2ch-thumbnail-process-callback-thumb-line (concat navi2ch-thumbnail-process-callback-thumb-line file))
      (setq file (concat navi2ch-thumbnail-process-callback-thumb-line file))
      (setq navi2ch-thumbnail-process-callback-thumb-line "")
    (if (not (string-match "^{.+}" file))
        (message "json format error:%s " file)
      (setq header (json-read-from-string file))
      (setq w (cdr (assq 'X-IMAGE-WIDTH header)))
      (setq h (cdr (assq 'X-IMAGE-HEIGHT header)))
      (setq s (cdr (assq 'X-IMAGE-SIZE header)))
      (setq fname (cdr (assq 'FileName header)))
      (when (cdr (assq 'X-IMAGE-TOO-SMALL header))
        (message "x-image-too-small: %s" (cdr (assq 'X-IMAGE-TOO-SMALL header))))
      (when (cdr (assq 'X-IMAGE-SAME header))
        (setq fname (navi2ch-thumbnail-url-to-file (cdr (assq 'X-IMAGE-SAME header))))
        (if (file-exists-p fname)
            (navi2ch-browse-url-image
             (if (eq system-type 'windows-nt)
                 (navi2ch-replace-string "/" "\\\\" fname t)
               fname))))
      (when (cdr (assq 'X-IMAGE-DENY header))
        (message "x-image-deny: %s" (cdr (assq 'X-IMAGE-DENY header)))
        (insert " (deny image)"))))))

(defun navi2ch-thumbnail-show-image-external-full ()
  (interactive)
  (navi2ch-thumbnail-show-image-external 'full))

(defun navi2ch-thumbnail-select-current-link (&optional browse-p)
  (interactive "P")
  (let ((type (get-text-property (point) 'navi2ch-link-type))
	(prop (get-text-property (point) 'navi2ch-link))
	url)
    (cond
     ((eq type 'url)
      (cond
       ((and (not (navi2ch-thumbnail-image-shown-p))
             (string-match navi2ch-thumbnail-image-url-regex prop))
        (navi2ch-thumbnail-image-pre prop t))

       ((and (file-name-extension prop)
	     (member (downcase (file-name-extension prop))
		     navi2ch-browse-url-image-extentions)))))
     ((eq type 'image)
      (navi2ch-thumbnail-show-image-external)))))

(defun navi2ch-thumbnail-show-image-external (&optional mode)
  (let* ((type (car (get-text-property (point) 'display)))
	(prop (get-text-property (point) 'navi2ch-link))
	(url (get-text-property (point) 'url))
        (fname (navi2ch-thumbnail-url-to-file url mode))
	proc)
    (if (not mode)
        (setq mode 'reduction))
    (cond
     ;;ANIME GIFやmp4動画はダウンロードしないでブラウザで直接見る
     ((or (string-match ".+\.gif$" prop) (string-match ".+\.mp4$" prop))
      (navi2ch-thumbnail-open-gif prop))
     ((file-exists-p prop)
      (navi2ch-thumbnail-browse-local-image (if (eq system-type 'windows-nt)
						(navi2ch-replace-string "/" "\\\\" prop t)
					      (navi2ch-thumbnail-cygwin-to-win prop))))
     ((string-match (concat "https?://[mi]?\.?imgur\.com/\\(.+\\)\\(\." (regexp-opt navi2ch-browse-url-image-extentions t) "\\)") url)
	(navi2ch-thumbnail-imgur-external-open (match-string 1 url) (match-string 2 url) url mode))
     ;; ((string-match (concat "\\(https?://pbs\.twimg\.com/media/[^:#]+\\)\."
     ;; 			    (regexp-opt navi2ch-browse-url-image-extentions t) ":?\\(.*\\)$") url)
     ;;  (let* ((noexturl (match-string 1 url))
     ;; 	     (ext (match-string 2 url)))
     ;; 	(setq fname (concat (navi2ch-thumbnail-url-to-file noexturl) (if (eq mode 'reduction) ".large." ".") ext))
     ;; 	(if (file-exists-p fname)
     ;; 	    (navi2ch-thumbnail-browse-local-image fname)
     ;; 	  (set-process-filter
     ;; 	   (start-process (concat "curl-get-image_" fname)
     ;; 			  "curl-get-image" (concat navi2ch-thumbnail-script-dir navi2ch-thumbnail-curl_external.script)
     ;; 			  (concat noexturl "." ext (if (eq mode 'redcution) ":large" "")) fname)
     ;; 	   'navi2ch-thumbnail-external-open-callback))))
     ((file-exists-p (navi2ch-thumbnail-url-to-file url mode))
      (navi2ch-thumbnail-browse-local-image fname))
     (t
      (message "外部ビューアーで開きます %s size:%s" url (cdr (assq 'content-length (navi2ch-net-get-header (navi2ch-net-send-request url "HEAD")))))
      (set-process-filter
       (start-process (concat "curl-get-image_" fname)
		      "curl-get-image" (concat navi2ch-thumbnail-script-dir navi2ch-thumbnail-curl_external.script) url fname)
       'navi2ch-thumbnail-external-open-callback)))))

(defun navi2ch-thumbnail-open-gif (prop)
  (cond
   ((eq window-system 'ns)
    (let ((new-window-flag (cond ((boundp 'browse-url-new-window-flag)
                                          browse-url-new-window-flag)
                                         ((boundp 'browse-url-new-window-p)
                                          browse-url-new-window-p)))
	  (fname (concat "file://" prop))
	  (args (list "-a" "firefox" prop)))
      (start-process "animation gif" nil "open" "-a" "firefox" prop)))
   ((and (string-match ".+imgur\.com/.+\.gif$" url))
    (navi2ch-browse-url (concat url "v")))
   (t
    (navi2ch-browse-url url))))
      
(defun navi2ch-thumbnail-image-pre (urlorg &optional force)
  "forceはスレ再描画ではnil"
  (when navi2ch-thumbnail-p
    (if (get-text-property (point) 'navi2ch-image-shown)
      (message "image already shown" urlorg)
      (let ((url (substring-no-properties urlorg)))
	(when (string-match "h?t?\\(tps?://.+\\)$" url)
	(setq url (concat "ht" (match-string 1 url))))
      (cond
       ;;imgurは公式サーバからサムネイルが取得できる
       ((string-match (concat "https?:/+[mi]?\.?imgur\.com/\\([^./]+\\)\." (regexp-opt navi2ch-browse-url-image-extentions t)) url)
;	 (string-match "https?:/+imgur\.com/a/\\([^./]+\\)" url))
	(navi2ch-thumbnail-imgur-insert-thumbnail (match-string 1 url) (match-string 2 url)))

       ;;twitterは公式サーバからサムネイルが取得できる       
;;       ((string-match (concat "\\(https?://pbs\.twimg\.com/media/[^:#]+\\)\."
       ((string-match (concat "\\(https?://pbs\.twimg\.com/\\(?:media\\|ext_tw_video_thumb\\)/[^:#]+\\)\."
			      (regexp-opt navi2ch-browse-url-image-extentions t) ":?\\(.*\\)$") url)
	(navi2ch-thumbnail-twitter (match-string 1 url) (match-string 2 url)))

       ;; twitterのurlがphoto形式の場合は2段
       ((string-match "h?t?tps?://twitter.com/.+/status/[0-9]+/photo/1" url)
	(let (cont (navi2ch-net-get-content (proc (navi2ch-net-send-request url "GET"))))
	  (string-match "src=\"\\(https?://pbs\.twimg\.com/media/.+\.\\(jpg\\|png\\)\\)\"" cont)
	  (navi2ch-thumbnail-twitter (match-string 1 cont) (match-string 2 cont))))

       ;;imepic(誰も使ってない？)
       ((string-match "h?ttp://imepic\\.jp/[0-9/]+" url)
	(if (not (file-exists-p (concat (navi2ch-thumbnail-url-to-file url) ".thumb.jpg")))
	    (progn 
	      (navi2ch-thumbnail-get-from-imgserver url))
	  (navi2ch-thumbnail-insert-image nil nil nil (concat (navi2ch-thumbnail-url-to-file url) ".thumb.jpg") nil)))

       ;;お絵描き(サービス停止？)
       ((string-match "sssp://o.5ch.net/" url)
	(message "5chお絵描き機能:%s" url)
	(let ((fname (navi2ch-thumbnail-url-to-file url))
	      (link-type (get-text-property (point) 'face)))
	  (unless (file-exists-p fname)
	    (navi2ch-net-update-file url fname nil nil nil nil nil))
	  (when fname
	    (save-excursion
	      (let ((buffer-read-only nil))
		(move-beginning-of-line nil)
		(insert-image (create-image fname))
		(add-text-properties
		 (1- (point)) (point)
		 (list 'link t 'link-head t
		       'url url 'help-echo fname
		       'navi2ch-link-type 'image 'navi2ch-link url 'file-name fname)))))))
     
       ((and (not (string-match "https?://.+/.+\.gif$" url))
	     (string-match "https?://.+/.+\..+$" url))
	(navi2ch-thumbnail-insert-image-cache url)))))))

(defun navi2ch-thumbnail-insert-image-cache (url)
  (let* ((file  (navi2ch-thumbnail-url-to-file url))
	 (thumb (concat file ".jpg")))
    (when (and (not (file-exists-p thumb)) (file-exists-p file))
      (setq thumb file))
    (when (and navi2ch-thumbnail-use-image-server (not (file-exists-p thumb)))
      (message "appspotサーバから画像を取得します:%s" url)
      (unless (navi2ch-thumbnail-appspot-insert-thumbnail url)
        (error "unable to get remote image server: %s" url))
      (when (and (not (file-exists-p thumb)) (file-exists-p file))
        (setq thumb file)))
      (save-excursion
    (let (
;	  (buffer-read-only nil)
	  w h s	proc header)
      (when (file-exists-p thumb)
	(move-beginning-of-line nil)
	(insert-image
;	 (if (fboundp 'imagemagick-types)
;	     (navi2ch-create-scaled-image thumb
;					  'imagemagick nil
;					  :width navi2ch-thumbnail-thumbsize-width
;					  :height navi2ch-thumbnail-thumbsize-height)
	   (create-image thumb))
	(add-text-properties
	 (1- (point)) (point)
	 (list 'link t 'link-head t
	       'url url 'help-echo file
	       'navi2ch-link-type 'image 'navi2ch-link file 'file-name file))
	;; get image attribute 
	    (cond
	     ((setq prop-list (or (cdr (navi2ch-thumbnail-image-prop-list-get url))
				  (navi2ch-thumbnail-header-file-read (concat file ".header"))))
	      (setq w (nth 0 prop-list))
	      (setq h (nth 1 prop-list)) 
	      (setq s (nth 2 prop-list)))
	     ((file-exists-p file)
	      (let (image-attr (navi2ch-thumbnail-image-identify file))
		(setq w (nth 0 image-attr))
		(setq h (nth 1 image-attr))
		(setq s (round (/ (nth 7 (file-attributes file)) 1024)))))
	     ((and navi2ch-thumbnail-use-image-server (image-type-from-file-header thumb)
		   (setq proc (navi2ch-net-send-request (concat navi2ch-thumbnail-imgserver-name "?info&url=" url) "GET" )))
	      (message "image property list not hit retrieve imgserver:%s" url)
	      (setq header (navi2ch-net-get-header proc))
	      (setq w (cdr (assq 'x-image-width header)))
	      (setq h (cdr (assq 'x-image-height header)))
	      (setq s (cdr (assq 'x-image-size header)))
	      (if (and w h s)
		  (navi2ch-thumbnail-image-prop-list-set url w h s)))
	     (t (message "unable to get image property: %s" url)))
;	    		(insert (format " (%sx%s:%sk%s)" w h s
;			      (nth 0 image-attr)
;			      (nth 1 image-attr)
;			      (round (/ (nth 7 (file-attributes file)) 1024))
;				(if (nth 2 image-attr) " GIF ANIME" "")))))

	    (if (and w h s)
		(insert (format " (thumb %sx%s:%sk)" w h (round (/ (string-to-number s) 1024)))))))
          
;    (save-excursion
      (when (re-search-forward
	   (concat "h?ttps?://\\([^ \t\n\r]+\\."
		   (regexp-opt navi2ch-browse-url-image-extentions t)
		   "\\)") nil t)
	(add-text-properties (match-beginning 0)(match-end 0) '(navi2ch-image-shown "shown")))
      (move-end-of-line nil))))

(defun navi2ch-thumbnail-set-url-shown ()
  (when (re-search-forward
	 (concat "h?ttps?://\\([^ \t\n\r]+\\."
		 (regexp-opt navi2ch-browse-url-image-extentions t)
		 "\\)") nil t)
    (add-text-properties (match-beginning 0)(match-end 0) '(navi2ch-image-shown t)))
  (move-end-of-line nil))

(defun navi2ch-thumbnail-appspot-insert-thumbnail (url)
  (navi2ch-thumbnail-process-count-up)
  (let* ((file (navi2ch-thumbnail-url-to-file url))
	 (w (number-to-string navi2ch-thumbnail-thumbsize-width))
	 (h (number-to-string navi2ch-thumbnail-thumbsize-height)))
    (message "appspot call: %s" url)
    (set-process-filter
     (start-process (concat "curl-get-image|" url "|" (buffer-name) "|" (number-to-string (point)))
		    "curl-get-image" (concat navi2ch-thumbnail-script-dir navi2ch-thumbnail-appspot-script) "http://thumbmake.appspot.com/main"
		    url w h (navi2ch-thumbnail-cygwin-to-win file))
     'navi2ch-thumbnail-appspot-process-callback)))

(defun navi2ch-thumbnail-appspot-process-callback (proc result)
  (navi2ch-thumbnail-process-count-down)
  (let* ((pn (process-name proc))
	 (start-index (string-match "curl-get-image|\\(http.+\\)|\\(.+\\)|\\(.+\\)$" pn))
	 (url (match-string 1 pn))
	 (bufname (match-string 2 pn))
	 (pointnum (string-to-number (match-string 3 pn)))
	 (replaced-id (replace-regexp-in-string "-" "\-" (nth 1 (split-string url ":"))))
	 (result (replace-regexp-in-string  "\n+$" "" result))
	 (thumb (concat (navi2ch-thumbnail-url-to-file url) ".jpg"))
	 w h s)
    (cond ((string-match "^zero.+" result)
	   (message "appspot callback abort:%s" result))
	  ((and (file-exists-p thumb) (= (nth 7 (file-attributes thumb)) 0))
	   (message "file empty: %s" thumb)
	   (delete-file thumb))
	  (t 
	   (message "appspot callback:%s" (replace-regexp-in-string  "\n+$" "" result))
	   (when (setq prop-list (navi2ch-thumbnail-header-file-read (concat (navi2ch-thumbnail-url-to-file url) ".header")))
	     (setq w (nth 0 prop-list))
	     (setq h (nth 1 prop-list)) 
	     (setq s (nth 2 prop-list)))
	   ;; (with-temp-buffer
	   ;;   (insert-file-contents (concat thumb ".header"))
	   ;;   (goto-char (point-min))
	   ;;   (while (re-search-forward "X-IMAGE-WIDTH: \\([0-9]+\\)" nil t)
	   ;;     (setq w (match-string 1)))
	   ;;   (goto-char (point-min))
	   ;;   (while (re-search-forward "X-IMAGE-HEIGHT: \\([0-9]+\\)" nil t)
	   ;;     (setq h (match-string 1)))
	   ;;   (goto-char (point-min))
	   ;;   (while (re-search-forward "X-IMAGE-SIZE: \\([0-9]+\\)" nil t)
	   ;;     (setq s (match-string 1))))
     
	   (when (file-exists-p thumb)
	       (with-current-buffer (set-buffer bufname)
	     (save-excursion
		 (when (and w h s)
		   (navi2ch-thumbnail-image-prop-list-set url w h s))
;		 (let ((buffer-read-only nil))
;		   (goto-char pointnum)
		   (goto-char 1)
		   (re-search-forward replaced-id)
		   (navi2ch-thumbnail-insert-image w h s url thumb)
		   )))))))
					;)

(defun navi2ch-thumbnail-header-file-read (header-file)
  (with-temp-buffer
    (let (w h s)
      (insert-file-contents header-file)
      (goto-char (point-min))
      (while (re-search-forward "X-IMAGE-WIDTH: \\([0-9]+\\)" nil t)
	(setq w (match-string 1)))
      (goto-char (point-min))
      (while (re-search-forward "X-IMAGE-HEIGHT: \\([0-9]+\\)" nil t)
	(setq h (match-string 1)))
      (goto-char (point-min))
      (while (re-search-forward "X-IMAGE-SIZE: \\([0-9]+\\)" nil t)
	(setq s (match-string 1)))
      (list w h s))))

(defvar navi2ch-thmbnail-image-prop-list nil "画像のプロパティを保存しておくリスト")
;; (defvar navi2ch-thumbnail-image-prop-list-file-name (concat navi2ch-thumbnail-thumbnail-directory "image-prop.el")
;;   "画像のプロパティを保存しておくファイル")

(defun navi2ch-thumbnail-image-prop-list-set (url w h size)
  (if (navi2ch-thumbnail-image-prop-list-get url)
      nil
  (setq navi2ch-thmbnail-image-prop-list
        (cons (list url w h size) navi2ch-thmbnail-image-prop-list))))

;; (add-hook 'navi2ch-hook 'navi2ch-thumbnail-load-image-prop)
;; (defun navi2ch-thumbnail-load-image-prop ()
;;   (setq navi2ch-thmbnail-image-prop-list (navi2ch-load-info navi2ch-thumbnail-image-prop-list-file-name)))

;; (add-hook 'navi2ch-exit-hook 'navi2ch-thumbnail-save-image-prop)
;; (defun navi2ch-thumbnail-save-image-prop ()
;;   (navi2ch-save-info navi2ch-thumbnail-image-prop-list-file-name navi2ch-thmbnail-image-prop-list))

(defun navi2ch-thumbnail-image-prop-list-get (url)
  (assoc url navi2ch-thmbnail-image-prop-list))

(defun navi2ch-thumbnail-windows-escape-filepath (filepath)
  (if (eq system-type 'windows-nt)
      (navi2ch-replace-string "/" "\\\\" filepath t)
    filepath))

(defun navi2ch-thumbnail-url-to-file (url &optional thumb)
  (unless (and (stringp url)
               (string-match "tps?://\\(.+\\)$" url))
    (error "URL not match"))
  (let ((file (navi2ch-thumbnail-windows-escape-filepath
               (expand-file-name (match-string 1 url) navi2ch-thumbnail-thumbnail-directory))))
    (cond ((eq thumb 'reduction)
           (cond ((string-match "\.jpe?g$" url)
                  (concat file ".reduction.jpg"))
                 ((string-match "\.png$" url)
                  (concat file ".reduction.png"))
                 (t
                  file)))
          ((eq thumb 'full)
           file)
          (thumb
           (concat file ".jpg"))
          (t
           file))))

(provide 'navi2ch-thumbnail-new)

