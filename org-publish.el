;; org-publish
;; always rebuild all files
(setq org-publish-use-timestamps-flag nil)

(setq org-export-default-language "de")

; No author / date at the bottom
(setf org-html-home/up-format "")
(setf org-html-footnotes-section "<div id='footnotes'><!--%s-->%s</div>")
(setf org-html-link-up "")
(setf org-html-link-home "")
(setf org-html-preamble nil)
(setf org-html-postamble nil)
(setf org-html-scripts "")

(require 'ox-publish)
(setq org-publish-project-alist
      '(("org-notes"
	 :base-directory "~/org-website-template/org-files/"
	 :base-extension "org"
	 :publishing-directory "~/org-website-template/public_html/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :with-author t
	 :with-creator nil
	 :headline-level 4
	 :section-numbers nil
	 :with-toc nil
	 :with-drawers t
	 :html-link-home "/"
	 :html-preamble t
	 :html-postamble t
	 :html-head-include-default-style nil
	 :html-head-include-scripts nil
	 :auto-preamble t)
	("org-static"
	 :base-directory "~/org-website-template/org-files/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "~/org-website-template/public_html/"
	 :recursive t
	 :publishing-function org-publish-attachment)	
	("org-website-template" :components ("org-notes" "org-static"))
	("achaia" :components ("achaia-pages" "achaia-static"))
	("achaia-pages"
 	 :base-directory "~/org/index-achaia/org-files/"
	 :base-extension "org"
	 :publishing-directory "~/org/index-achaia/public_html/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :with-author t
	 :with-creator nil
	 :headline-level 4
	 :section-numbers nil
	 :with-toc nil
	 :with-drawers t
	 :html-link-home "/"
	 :html-preamble
	 "<header id=\"banner\">
<div class=\"wrapper\">
  <h1><a href=\"http://achaia.org\">INDEX ACHAIA</a></h1>
  <nav><ul>
    <li><a href=\"./datenschutz.html\">Datenschutz</a></li>
    <li><a href=\"./impressum.html\">Impressum</a></li>
    <li><a href=\"./about.html\">About</a></li>
  </ul></nav>
</div>
</header>"	 
	 :html-postamble
	 "<footer>
<div class=\"wrapper\">
	 <h2 class=\"footer-heading\">INDEX ACHAIA</h2>
<div class=\"footer-left\">
<ul class=\"contact-list\">
<li>INDEX ACHAIA</li>
<li><a href=\"mailto:kontakt@achaia.org\">kontakt@achaia.org</a></li>
</ul>
</div>
<div class=\"footer-right\">
<p>Hintergrundinfos zur griechisch-mythologischen Alternativ-Welt Achaia.</p>
</div>
</div>
</footer>"		 
	 :html-head-include-default-style nil
	 :html-head-include-scripts nil
	 :auto-preamble t)
	("achaia-static"
	 :base-directory "~/org/index-achaia/org-files/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "~/org/index-achaia/public_html/"
	 :recursive t
	 :publishing-function org-publish-attachment)
	("bookhacker" :components ("bookhacker-pages" "bookhacker-static"))
	("bookhacker-pages"
 	 :base-directory "~/org/bookhacker/org-files/"
	 :base-extension "org"
	 :publishing-directory "~/org/bookhacker/public_html/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :with-author t
	 :with-creator nil
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
    <li><a href=\"./datenschutz.html\">Datenschutz</a></li>
    <li><a href=\"./impressum.html\">Impressum</a></li>
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
	 :html-head-include-default-style nil
	 :html-head-include-scripts nil
	 :auto-preamble t)
	("bookhacker-static"
	 :base-directory "~/org/bookhacker/org-files/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "~/org/bookhacker/public_html/"
	 :recursive t
	 :publishing-function org-publish-attachment)))
