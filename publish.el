
(setq make-backup-files nil)

(setq custom-site-lisp-directory (expand-file-name "~/.emacs.d/site-lisp"))
(mapcar (lambda (addition)
          (add-to-list 'load-path addition)
          (let ((default-directory addition))
            (normal-top-level-add-subdirs-to-load-path)))
        `(,custom-site-lisp-directory))

(let ((themes-directory (expand-file-name (concat custom-site-lisp-directory "/themes"))))
  (mapcar (lambda (file)
            (let ((expanded-file (expand-file-name file themes-directory)))
              (when (file-directory-p expanded-file)
                (add-to-list 'custom-theme-load-path expanded-file))))
          (directory-files themes-directory)))

(require 'package)
(setq package-user-dir (expand-file-name (concat custom-site-lisp-directory "/elpa")))
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(require 'use-package)

(use-package exec-path-from-shell
  :ensure exec-path-from-shell
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)
          (exec-path-from-shell-copy-env "LANG")))

(require 'uuid)
(require 'htmlize)
(require 'ox-publish)
(setq enable-local-variables :all)

(defun insert-iso-timestamp ()
  (interactive)
  (let ((saved-tz (getenv "TZ")))
    (setenv "TZ" "Etc/UTC")
    (let ((date (with-temp-buffer
		  (call-process "gdate" nil (current-buffer) nil "--iso-8601=seconds")
		  (delete-backward-char 1)
		  (buffer-substring (point-min) (point-max)))))
      (setenv "TZ" saved-tz)
      date)))

(setq org-publish-project-alist
      '(("beltmogul-notes"
         :base-directory "~/code/beltmogul.me/orgs/"
         :base-extension "org"
         :publishing-directory "~/code/beltmogul.me/public_html/"
         :recursive t
         :publishing-function org-html-publish-to-html)
        ("beltmogul-static"
         :base-directory "~/code/beltmogul.me/orgs/"
         :base-extension "js\\|css\\|otf\\|eot\\|svg\\|ttf\\|woff\\|woff2\\|png\\|jpg\\|gif\\|ico\\|xml\\|txt"
         :publishing-directory "~/code/beltmogul.me/public_html/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("beltmogul"
         :components ("beltmogul-notes" "beltmogul-static")
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         :sitemap-title "Sitemap")))

(defun publish-beltmogul ()
  (interactive)
  (let ((default-directory "~/code/beltmogul.me/"))
    (org-publish-project "beltmogul" t)))

;; "<div class=\"footer\"><a href=\"/\">belt mogul</a></div>"

(defun publish-beltmogul-locally ()
  (interactive)
  (let ((default-directory "~/code/beltmogul.me/")
	(org-src-fontify-natively nil)
	(org-src-preserve-indentation t)
        (org-html-head "
<meta name=\"viewport\" content=\"width=device-width,user-scalable=no\">
<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/normalize.css\" />
<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/styles.css\" />
<link rel='stylesheet' type='text/css' href='https://fonts.googleapis.com/css?family=Roboto:400,100'>
<link rel=\"shortcut icon\" href=\"/favicon.ico\" type=\"image/x-icon\">
<link rel=\"icon\" href=\"/favicon.ico\" type=\"image/x-icon\">
<link href=\"atom.xml\" type=\"application/atom+xml\" rel=\"alternate\" title=\"belt mogul feed\">
<link href=\"/css/font-awesome.min.css\" rel=\"stylesheet\">
<script src=\"//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js\"></script>
<script src=\"//cdnjs.cloudflare.com/ajax/libs/lodash.js/2.4.1/lodash.min.js\"></script>
")
        (org-html-postamble-format '(("en" "
<!-- <div id=\"footer\">Made with <span class=\"fa fa-heart\"><span> in Piha.</div> -->
")))
        (org-html-preamble-format
         '(("en" "
<div id=\"top-bar\"><div id=\"title-bar\"><a href=\"/\">belt mogul</a></div><div id=\"nav-bar\">
  <span id=\"2015\">
    <a href=\"/2015\">2015</a>
  </span>
  <span id=\"2014\">
    <a href=\"/2014\">2014</a>
  </span>
  <span id=\"topics\">
    <a href=\"/topics.html\">topics</a>
  </span>
  <span id=\"feed\">
    <a href=\"/atom.xml\"><span class=\"fa fa-rss\"></span></a>
  </span>
  <span id=\"me\">
    <a href=\"http://felix.geller.io\"><span class=\"fa fa-child\"></span></a>
  </span>
</div></div>
"))))
    (org-publish-project "beltmogul" t)))

;;   (compile "rsync -azv ~/code/beltmogul.me/public_html hipp0:~/beltmogul.me/")

(setq org-src-fontify-natively nil)
(global-font-lock-mode 0)
(load-theme 'basic t)

(publish-beltmogul-locally)

;; (serve-beltmogul-locally)
(defun serve-beltmogul-locally ()
  (let ((default-directory "~/code/beltmogul.me/public_html/"))
    (compile "python -m SimpleHTTPServer 3033")
    (with-current-buffer "*compilation*"
      (rename-buffer "*serve-beltmogul*"))))

;; ~/Applications/Emacs.app/Contents/MacOS/Emacs -q -l publish.el
;; (compile-beltmogul-locally)
(defun compile-beltmogul-locally ()
  (compile "~/Applications/Emacs.app/Contents/MacOS/Emacs -q -l ~/code/beltmogul/publish.el")
  (with-current-buffer "*compilation*"
    (rename-buffer "*compile-beltmogul*")))
