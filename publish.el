(use-package uuid :ensure uuid)

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
         :base-directory "~/blog/orgs/"
         :base-extension "org"
         :publishing-directory "~/blog/public_html/"
         :recursive t
         :publishing-function org-html-publish-to-html)
        ("beltmogul-static"
         :base-directory "~/blog/orgs/"
         :base-extension "css\\|png\\|jpg\\|gif\\|ico\\|xml"
         :publishing-directory "~/blog/public_html/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("beltmogul"
         :components ("beltmogul-notes" "beltmogul-static")
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         :sitemap-title "Sitemap")))


(defun publish-beltmogul ()
  (interactive)
  (let ((default-directory "~/blog/"))
    (org-publish-project "beltmogul")
    (compile "rsync -azv public_html hipp0:~/beltmogul.me/")))

(defun publish-beltmogul-locally ()
  (interactive)
  (let ((default-directory "~/blog/"))
    (org-publish-project "beltmogul")))

(let ((org-html-postamble-format
       '(("en" "<div class=\"footer\"><a href=\"/\">belt mogul</a></div>"))))
  (publish-beltmogul-locally))

(publish-beltmogul-locally)
