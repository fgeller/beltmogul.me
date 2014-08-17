(require 'org-publish)
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

(publish-beltmogul)
