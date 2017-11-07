;;; ---------------------------------------------------------
;;; Based on: https://ogbe.net/blog/blogging_with_org.html
;;;
(setq my-blog-extra-head "<link rel=\"stylesheet\" href=\"../css/stylesheet.css\" />")

;;; ---------------------------------------------------------
;;;
(defun my-blog-get-preview (file)
  "The comments in FILE have to be on their own lines, prefereably before and after paragraphs."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((beg (+ 1 (re-search-forward "^#\\+BEGIN_PREVIEW$")))
          (end (progn (re-search-forward "^#\\+END_PREVIEW$")
                      (match-beginning 0))))
      (buffer-substring beg end))))

;;; ---------------------------------------------------------
;;; 
(defun my-blog-sitemap (project &optional sitemap-filename)
  "Generate the sitemap for my blog."
  (let* ((project-plist (cdr project))
         (dir (file-name-as-directory
               (plist-get project-plist :base-directory)))
         (localdir (file-name-directory dir))
         (exclude-regexp (plist-get project-plist :exclude))
         (files (nreverse
                 (org-publish-get-base-files project exclude-regexp)))
         (sitemap-filename (concat dir (or sitemap-filename "sitemap.org")))
         (sitemap-sans-extension
          (plist-get project-plist :sitemap-sans-extension))
         (visiting (find-buffer-visiting sitemap-filename))
         file sitemap-buffer)
    (with-current-buffer
        (let ((org-inhibit-startup t))
          (setq sitemap-buffer
                (or visiting (find-file sitemap-filename))))
      (erase-buffer)
      ;; loop through all of the files in the project
      (while (setq file (pop files))
        (let ((fn (file-name-nondirectory file))
              (link ;; changed this to fix links. see postprocessor.
               (file-relative-name file (file-name-as-directory
                                         (expand-file-name (concat (file-name-as-directory dir) "..")))))
              (oldlocal localdir))
          (when sitemap-sans-extension
            (setq link (file-name-sans-extension link)))
          ;; sitemap shouldn't list itself
          (unless (equal (file-truename sitemap-filename)
                         (file-truename file))
            (let (;; get the title and date of the current file
                  (title (org-publish-format-file-entry "%t" file project-plist))
                  (date (org-publish-format-file-entry "%d" file project-plist))
                  ;; get the preview section from the current file
                  (preview (my-blog-get-preview file))
                  (regexp "\\(.*\\)\\[\\([^][]+\\)\\]\\(.*\\)"))
              ;; insert a horizontal line before every post, kill the first one
              ;; before saving
              (insert "-----\n")
              (cond ((string-match-p regexp title)
                     (string-match regexp title)
                     ;; insert every post as headline
                     (insert (concat"* " (match-string 1 title)
                                    "[[file:" link "]["
                                    (match-string 2 title)
                                    "]]" (match-string 3 title) "\n")))
                    (t (insert (concat "* [[file:" link "][" title "]]\n"))))
              ;; add properties for `ox-rss.el' here
              (let ((rss-permalink (concat (file-name-sans-extension link) ".html"))
                    (rss-pubdate (format-time-string
                                  (car org-time-stamp-formats)
                                  (org-publish-find-date file))))
                (org-set-property "RSS_PERMALINK" rss-permalink)
                (org-set-property "PUBDATE" rss-pubdate))
              ;; insert the date, preview, & read more link
              (insert (concat date "\n\n"))
              (insert preview)
              (insert (concat "[[file:" link "][Weiterlesen...]]\n"))))))
      ;; kill the first hrule to make this look OK
      (goto-char (point-min))
      (let ((kill-whole-line t)) (kill-line))
      (goto-char (point-min))
      (setq sitemap-title (plist-get project-plist :sitemap-title))
      (insert (format "#+TITLE: %s\n\n" sitemap-title))
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))

;;; ---------------------------------------------------------
;;;
(defun my-blog-articles-postprocessor ()
  "Massage the sitemap file and move it up one directory.

for this to work, we have already fixed the creation of the
relative link in the sitemap-publish function"
  (let* ((sitemap-fn (concat (file-name-sans-extension (plist-get project-plist :sitemap-filename)) ".html"))
         (sitemap-olddir (plist-get project-plist :publishing-directory))
         (sitemap-newdir (expand-file-name (concat (file-name-as-directory sitemap-olddir) "..")))
         (sitemap-oldfile (expand-file-name sitemap-fn sitemap-olddir))
         (sitemap-newfile (expand-file-name (concat (file-name-as-directory sitemap-newdir) sitemap-fn))))
    (with-temp-buffer
      (goto-char (point-min))
      (insert-file-contents sitemap-oldfile)
      ;; massage the sitemap if wanted

      ;; delete the old file and write the correct one
      (delete-file sitemap-oldfile)
      (write-file sitemap-newfile))))

;;; ---------------------------------------------------------
;;; 
(setq org-publish-project-bookhacker
      '(("bookhacker" :components ("bookhacker-articles" "bookhacker-pages" "bookhacker-static"))
	("bookhacker-articles"
 	 :base-directory "~/org/websites/bookhacker.org/org-files/blog/"
	 :base-extension "org"
	 :publishing-directory "~/org/websites/bookhacker.org/public_html/blog/"
	 :completion-function my-blog-articles-postprocessor
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :with-author t
	 :with-creator nil
	 :with-date t
	 :headline-level 4
	 :section-numbers nil
	 :with-toc nil
	 :with-drawers t
	 :html-link-home "/"
	 :html-preamble
	 "<header id=\"banner\">
<div class=\"wrapper\">
  <h1><a href=\"https://bookhacker.org\">bookhacker.org</a></h1>
  <nav><ul>
    <li><a href=\"https://bookhacker.org/datenschutz.html\">Datenschutz</a></li>
    <li><a href=\"https://bookhacker.org/impressum.html\">Impressum</a></li>
  </ul></nav>
</div>
</header>"	 
	 :html-postamble
	 "<footer>
<div class=\"wrapper\">
	 <h2 class=\"footer-heading\">bookhacker.org</h2>
<div class=\"footer-left\">
<ul class=\"contact-list\">
<li>bookhacker.org</li>
<li><a href=\"mailto:kontakt@bookhacker.org\">kontakt@bookhacker.org</a></li>
</ul>
</div>
<div class=\"footer-right\">
<p>Kompjuta Tekknolodschie</p>
</div>
</div>
</footer>"
	 :html-head nil ;; cleans up anything that would have been in there.
	 :html-head-extra "<link rel=\"stylesheet\" href=\"css/stylesheet.css\" />"
	 :html-head-include-default-style nil
	 :html-head-include-scripts nil
	 :auto-preamble t
	 ;; sitemap - list of blog articles
         :auto-sitemap t
	 :sitemap-title "Kompjuta Tekknolodschie"
	 :sitemap-filename "index.org"
         ;; custom sitemap generator function
         :sitemap-function my-blog-sitemap
         :sitemap-sort-files anti-chronologically
         :sitemap-date-format "%d.%m.%Y")
	("bookhacker-pages"
 	 :base-directory "~/org/websites/bookhacker.org/org-files/"
	 :base-extension "org"
	 :publishing-directory "~/org/websites/bookhacker.org/public_html/"
	 :recursive nil
	 :publishing-function org-html-publish-to-html
	 :with-author t
	 :with-creator nil
	 :with-date t
	 :headline-level 4
	 :section-numbers nil
	 :with-toc nil
	 :with-drawers t
	 :html-link-home "/"
	 :html-preamble
	 "<header id=\"banner\">
<div class=\"wrapper\">
  <h1><a href=\"https://bookhacker.org\">bookhacker.org</a></h1>
  <nav><ul>
    <li><a href=\"https://bookhacker.org/datenschutz.html\">Datenschutz</a></li>
    <li><a href=\"https://bookhacker.org/impressum.html\">Impressum</a></li>
  </ul></nav>
</div>
</header>"	 
	 :html-postamble
	 "<footer>
<div class=\"wrapper\">
	 <h2 class=\"footer-heading\">bookhacker.org</h2>
<div class=\"footer-left\">
<ul class=\"contact-list\">
<li>bookhacker.org</li>
<li><a href=\"mailto:kontakt@bookhacker.org\">kontakt@bookhacker.org</a></li>
</ul>
</div>
<div class=\"footer-right\">
<p>Kompjuta Tekknolodschie</p>
</div>
</div>
</footer>")	
	("bookhacker-static"
	 :base-directory "~/org/websites/bookhacker.org/org-files/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "~/org/websites/bookhacker.org/public_html/"
	 :recursive t
	 :publishing-function org-publish-attachment)))
