;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(define-key navi2ch-article-mode-map "D" 'navi2ch-thumbnail-insert-into-blacklist)

(defun navi2ch-thumbnail-insert-into-blacklist ()
  (interactive)
  (let ((url))
    (when (display-images-p)
      (save-excursion
        (if (not navi2ch-thumbnail-image-url-regex)
            (navi2ch-thumbnail-image-url-regex-build))
	(let ((buffer-read-only nil))
	  (goto-char (point-min))
	  (while (re-search-forward navi2ch-thumbnail-image-url-regex nil t)
	    (setq url (match-string 1))
            (navi2ch-thumbnail-insert-into-blacklist-subr url)))))))

(defun navi2ch-thumbnail-insert-into-blacklist-subr (url)
  (let ((to-server-url (concat "http://thumbmake.appspot.com/main?url=" (navi2ch-url-encode-string url) "&deny"))
        (thumbname (navi2ch-thumbnail-url-to-file url t))
        (filename (navi2ch-thumbnail-url-to-file url)))

    (setq proc (navi2ch-net-send-request to-server-url "GET" ))
    (if (file-exists-p thumbname)
        (delete-file thumbname))
    (if (file-exists-p filename)
        (delete-file filename))))

(defun navi2ch-thumbnail-expire ()
  (interactive)
  (let ((to-server-url (concat navi2ch-thumbnail-imgserver-name "?expire"))
        header status proc)
    (setq proc (navi2ch-net-send-request to-server-url "GET" ))
    (setq header (navi2ch-net-get-header proc))
    (setq status (navi2ch-net-get-status proc))
    (if (string= status "200")
        (message "delete image count: %s" (cdr (assq 'x-expire header)))
      (message "expire error: %s" status))))

(defun navi2ch-thumbnail-image-delete-cache ()
  "取得した画像を削除。キャッシュが無くなるの表示されなくなる"
  (interactive)
  (let* ((type (car (get-text-property (point) 'display)))
	 (file (get-text-property (point) 'navi2ch-link))
	 (url (get-text-property (point) 'url))
	 (thumb (concat file ".jpg")))
    (navi2ch-thumbnail-insert-into-blacklist-subr url)
    (when (and file (file-exists-p file))
      (delete-file file)
      (message "deleting file:%s " file))
    (when (and thumb (file-exists-p thumb))
      (delete-file thumb)
      (message "deleting thumbnail:%s " thumb))))

(defun navi2ch-thumbnail-hiraku-url-region (&optional beg end)
  (interactive "r")
  (save-excursion
    (message "s:%s e:%s" beg end)
    (save-restriction
      (narrow-to-region beg end)
      ;(goto-char 0)
      (end-of-buffer)
      (while (re-search-backward "ttps?://\\(.+\\)" nil t)
        (save-excursion
        (move-beginning-of-line nil)
        (navi2ch-thumbnail-show-image-external 'reduction)
        )))))
         
(defun navi2ch-thumbnail-get-file-region (&optional beg end)
  (interactive "r")
(cond
 ((equal system-type 'windows-nt)
  (defvar navi2ch-thumbnail-perl (list "wsl" "perl")))
 ((equal system-type 'cygwin)
  (defvar navi2ch-thumbnail-perl "perl")))

  (save-excursion
;    (message "s:%s e:%s" beg end)
    (let ((target-dir (read-directory-name
                 (format "Save to dir: ")
                 navi2ch-thumbnail-save-content-dir)))
    (save-restriction
    (when target-dir
        (narrow-to-region beg end)
      (goto-char 0)
      (while (re-search-forward "ttps?://\\(.+\\)" nil t)
        (setq url-name (match-string 1))
        (setq url (concat "http://" url-name))
        (setq filename (expand-file-name (file-name-nondirectory url-name) target-dir))
        (message "url:%s -> filename:%s" url filename)
        (navi2ch-net-update-file url filename)
        (setq proc
              (start-process (concat "java-get-image_" url)
                             "java-get-image" navi2ch-thumbnail-perl "-e" 
                             (format "open(LOCK,\"> /tmp/sema_perl\") or die \"cant lock\";flock(LOCK,2);system(\"\\\"%s\\\" -classpath %s imageFetch %s %s\");"
                                     navi2ch-java-bin
                                     navi2ch-thumbnail-java-imagefetch-classpass
                                     url
                                     target-dir
                                     )))))))))
  
;; (defun navi2ch-thumbnail-get-from-imgserver-org (url &optional thumb referer force)
;;   (when (< navi2ch-thumbnail-process-count 25)
;;     (setq navi2ch-thumbnail-process-count (+ navi2ch-thumbnail-process-count 1))
;; ; 
;;       (setq proc
;;             (start-process (concat "java-get-image_" url)
;; ;                           "java-get-image"  "wsl" "perl" "-e" 
;;                            "java-get-image" "perl" "-e" 
;;                            (format "open(LOCK,\"> /tmp/sema_perl\") or die \"cant lock\";flock(LOCK,2);system(\"\\\"%s\\\" -classpath %s imageFetch %s %s \\\"\\\" %s %s %s\");"
;; ;                           (expand-file-name "java_lock.bat" navi2ch-top-directory ) %s %s %s %s %s %s %s"
;; ;                           navi2ch-java-bin
;; ;                                   temporary-file-directory
;; ;                                   "java"
;;                                    navi2ch-java-bin
;; ;"c:/Users/H61/Dropbox/emacs/navi2ch/navi2ch-dat"
;; navi2ch-thumbnail-java-imagefetch-classpass
;; ;"~/navi2ch/navi2ch-dat"
;;                            url
;;                            navi2ch-thumbnail-imagefetch-thumbnail-directory
;;                        (format "%s" navi2ch-thumbnail-thumbsize-width)
;;                        (format "%s" navi2ch-thumbnail-thumbsize-height) (if thumb thumb "jpg"))))
;;   (set-process-filter proc 'navi2ch-thumbnail-process-callback-thumb)
;;   (set-marker (process-mark proc) (point))))

;; (defun navi2ch-thumbnail-show-image-external (&optional mode)
;;   "外部ビューアーで表示"
;;   (interactive)
;;   (let ((type (car (get-text-property (point) 'display)))
;; 	(prop (get-text-property (point) 'navi2ch-link))
;; 	(original-filesize (get-text-property (point) 'original-filesize))
;; 	(url (get-text-property (point) 'url))
;;         fname)

;;     (if (not mode)
;;         (setq mode 'reduction))

;;     (cond 
;;            ((and (file-exists-p prop) (eq window-system 'ns) (string-match ".+\.gif$" prop))
;;             (let ((new-window-flag (cond ((boundp 'browse-url-new-window-flag)
;;                                           browse-url-new-window-flag)
;;                                          ((boundp 'browse-url-new-window-p)
;;                                           browse-url-new-window-p))))
;;               (setq fname (concat "file://" prop))
;;               (setq args (list "-a" "firefox" prop))
;;               (start-process "animation gif" nil "open" "-a" "firefox" prop)))
;;            ((and (string-match ".+imgur\.com/.+\.gif$" url))
;;                 (navi2ch-browse-url (concat url "v")))
;;            ((file-exists-p prop)
;;             (navi2ch-browse-url-image
;;              (if (eq system-type 'windows-nt)
;;                  (navi2ch-replace-string "/" "\\\\" prop t)
;;                (if (eq system-type 'cygwin) (cygwin-convert-file-name-to-windows prop) prop))))
           
;;            ((and (string-match (concat "https?://[mi]?\.?imgur\.com/\\(.+\\)\.\\(" (regexp-opt navi2ch-browse-url-image-extentions t) "\\)") url)
;;             (let* ((id (match-string 1 url))
;;                   (original-fname (navi2ch-thumbnail-url-to-file (concat "http://i.imgur.com/" id ".jpg")))
;;                   (h-fname (navi2ch-thumbnail-url-to-file (concat "http://i.imgur.com/" id "h.jpg"))))
;;               (cond 
;;                ((file-exists-p original-fname)
;;                 (navi2ch-browse-url-image (if (eq system-type 'cygwin) (cygwin-convert-file-name-to-windows original-fname) original-fname)))
;;                ((eq mode 'reduction)
;;                 (if (file-exists-p h-fname)
;;                     (navi2ch-browse-url-image (if (eq system-type 'cygwin) (cygwin-convert-file-name-to-windows h-fname) h-fname))
;;                   (progn
;;                     (setq h-filesize (cdr (assq 'content-length (navi2ch-net-get-header (navi2ch-net-send-request (concat "http://i.imgur.com/" id "h.jpg") "HEAD")))))

;;                     (cond
;;                      ((and original-filesize (< (string-to-number original-filesize) 80000))
;;                       (setq fname original-fname)
;;                       (setq mode nil))
;;                      ((or (not  original-filesize)
;;                           (and 
;;                            (> (string-to-number original-filesize) 70000)
;;                           (< (/ (float (string-to-number h-filesize)) (float (string-to-number original-filesize))) 0.8)))
;;                       (setq fname h-fname))
;;                      (t
;;                       (setq fname original-fname)
;;                       (setq mode nil)
;;                       (message "imgur h-size original:%s %s" h-filesize original-filesize )))
;;                     nil)))
;;                (t
;;                 (setq fname original-fname)
;;                 nil)))))
           
;;            ((and (eq mode 'reduction) (file-exists-p (navi2ch-thumbnail-url-to-file url mode)))
;;             (setq fname (navi2ch-thumbnail-url-to-file url mode))
;;             (navi2ch-browse-url-image (if (eq system-type 'cygwin) (cygwin-convert-file-name-to-windows fname) fname)))
;;            (t
;;             (setq proc
;;                   (if (eq mode 'reduction)
;;                       (start-process "java-get-image" "java-get-image" navi2ch-java-bin "-classpath" 
;;                                navi2ch-thumbnail-java-imagefetch-classpass "imageFetch"
;;                                url
;;                                navi2ch-thumbnail-imagefetch-thumbnail-directory
;;                                navi2ch-thumbnail-browse-program
;;                                (format "%s" navi2ch-thumbnail-reduction-width) (format "%s" navi2ch-thumbnail-reduction-height) "reduction")
;;               (start-process "java-get-image" "java-get-image" navi2ch-java-bin "-classpath" 
;;                              navi2ch-thumbnail-java-imagefetch-classpass "imageFetch"
;;                              url
;;                              navi2ch-thumbnail-imagefetch-thumbnail-directory
;;                              navi2ch-thumbnail-browse-program)))
;;             (set-process-filter proc 'navi2ch-thumbnail-process-callback)
;; ;            (set-marker (process-mark proc) (point))
;;             ))))

;; (defun navi2ch-thumbnail-insert-image-new (file &opetional proc link w h s gifv)
;;     (save-excursion
;;       (with-current-buffer (marker-buffer (process-mark proc))
;;         (let ((buffer-read-only nil))
;;         (when (and w h s)
;;           (navi2ch-thmbnail-image-prop-list-set link w h s)
;;           (message "buffer read only %s" buffer-read-only)
;; ;          (message "seve-excursion:%s" (point))
;;           (goto-char (process-mark proc))
;;           (insert (format " (thumb %sx%s:%sk%s)" w h (round (/ s 1024)) (if gifv " AnimeGIF" "")))
;; ;          (message "seve-excursion2:%s" (point))
;;           (move-beginning-of-line nil)          
;;           (insert-image (navi2ch-create-image local-file))
;;           (add-text-properties
;;            (1- (point)) (point)
;;            (list 'link t 'link-head t
;;                  'url link
;;                  'help-echo local-file
;;                  'original-filesize s
;;                  'navi2ch-link-type 'image
;;                  'navi2ch-link link
;;                  'file-name local-file))
;;           )))))

;; (defun navi2ch-thumbnail-show-image-external-org (&optional mode)
;;   (let ((type (car (get-text-property (point) 'display)))
;; 	(prop (get-text-property (point) 'navi2ch-link))
;; 	(original-filesize (get-text-property (point) 'original-filesize))
;; 	(url (get-text-property (point) 'url))
;;         fname)
;;     (if (not mode)
;;         (setq mode 'reduction))

;;     (cond
;;      ((and (string-match (concat "https?://[mi]?\.?imgur\.com/\\(.+\\)\.\\(" (regexp-opt navi2ch-browse-url-image-extentions t) "\\)") url)
;;            (let* ((id (match-string 1 url))
;;                   (ext (match-string 2 url))
;;                   (original-fname (navi2ch-thumbnail-url-to-file (concat "http://i.imgur.com/" id "." ext)))
;;                   (h-fname (navi2ch-thumbnail-url-to-file (concat "http://i.imgur.com/" id "h." ext))))
;;              (cond 
;;               ((file-exists-p original-fname)
;;                (navi2ch-browse-url-image (if (eq system-type 'cygwin) (cygwin-convert-file-name-to-windows original-fname) original-fname)))
;;               ((eq mode 'reduction)
;;                 (if (file-exists-p h-fname)
;;                     (navi2ch-browse-url-image (if (eq system-type 'cygwin) (cygwin-convert-file-name-to-windows h-fname) h-fname))
;;                   (progn
;;                     (setq h-filesize (string-to-number  (cdr (assq 'content-length (navi2ch-net-get-header (navi2ch-net-send-request (concat "http://i.imgur.com/" id "h.jpg") "HEAD"))))))
;;                     (message "h-filesize %s %s" h-filesize original-filesize)
;;                     (cond
;;                      ((and original-filesize (< original-filesize 80000))
;;                       (setq fname original-fname)
;;                       (setq mode nil))
;;                      ((or (not  original-filesize)
;;                           (and 
;;                            (> original-filesize 70000)
;;                            (< (/ (float h-filesize) (float original-filesize)) 0.8)
;;                           ))
;;                       (setq fname h-fname))
;;                      (t
;;                       (setq fname original-fname)
;;                       (setq mode nil)
;;                       (message "imgur h-size original:%s %s" h-filesize original-filesize )))
;;                     )))
;;               (t
;;                (setq fname original-fname)
;;                 nil

;;                 ))))))
;;     (message "fname -> %s" fname)
;;     ))


;; (defun navi2ch-thumbnail-process-callback-thumb-curl-json (proc result)
;;   (let ((pn (process-name proc)))
;;     (string-match "curl-get-image_\\([^<]+\\)" pn)
;;     (setq id (match-string 1 pn))
;;     (setq target-file (concat navi2ch-thumbnail-java-imagefetch-thumbnail-directory "i.imgur.com/" id ".json"))
;;     (write-region result nil target-file)
;;     (setq imgur-json (json-read-file target-file))
;;     (set-process-buffer proc nil)
;; ;    (message "thumb-curl after nil:%s" (process-mark proc))
;;     (setq local-file (concat navi2ch-thumbnail-java-imagefetch-thumbnail-directory "i.imgur.com/" id "t.jpg"))
;;     (setq w (cdr (assoc 'width (cdr (assoc 'data imgur-json)))))
;;     (setq h (cdr (assoc 'height (cdr (assoc 'data imgur-json)))))
;;     (setq s (cdr (assoc 'size (cdr (assoc 'data imgur-json)))))
;;     (setq gifv (cdr (assoc 'gifv (cdr (assoc 'data imgur-json)))))
;;     (setq link (cdr (assoc 'link (cdr (assoc 'data imgur-json)))))
;;     (save-excursion
;;       (with-current-buffer (marker-buffer (process-mark proc))
;;         (let ((buffer-read-only nil))
;;         (when (and w h s)
;;           (navi2ch-thmbnail-image-prop-list-set link w h s)
;;           (message "buffer read only %s" buffer-read-only)
;;           (goto-char (process-mark proc))
;;           (insert (format " (thumb %sx%s:%sk%s)" w h (round (/ s 1024)) (if gifv " AnimeGIF" "")))
;;           (move-beginning-of-line nil)          
;;           (insert-image (navi2ch-create-image local-file))
;;           (add-text-properties
;;            (1- (point)) (point)
;;            (list 'link t 'link-head t
;;                  'url link
;;                  'help-echo local-file
;;                  'original-filesize s
;;                  'navi2ch-link-type 'image
;;                  'navi2ch-link link
;;                  'file-name local-file))))))
;;   (setq navi2ch-thumbnail-global-lock nil)
;;   (message "unlock")))

;(navi2ch-thumbnail-imgur-insert-thumbnail-curl "XJf6lnG")
