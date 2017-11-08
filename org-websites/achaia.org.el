(setq org-publish-project-achaia
      '(
("achaia.org" :components ("achaia-pages" "achaia-static"))
	("achaia-pages"
 	 :base-directory "~/org/websites/achaia.org/org-files/"
	 :base-extension "org"
	 :publishing-directory "~/org/websites/achaia.org/public_html/"
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
  <h1><a href=\"https://achaia.org\">INDEX ACHAIA</a></h1>
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
	 :base-directory "~/org/websites/achaia.org/org-files/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "~/org/websites/achaia.org/public_html/"
	 :recursive t
	 :publishing-function org-publish-attachment)))
