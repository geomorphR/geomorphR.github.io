# Barcelona 2027 temporary homepage

## Edit and render

Edit `Barcelona.Rmd`. It uses the regular workshop's `html_document` format,
standard R Markdown template, shared `site_libs/`, and navigation from `_site.yml`.
There is no custom CSS file, HTML template, or custom Knit function.
The two-column instructor layout and sponsor row use the site's existing
Bootstrap classes, with Markdown content and image links.

Knit in RStudio, or run this from R with GMcourse as the working directory:

```r
rmarkdown::render("Barcelona.Rmd", encoding = "UTF-8")
```

This creates `Barcelona.html`. To make it the homepage, copy `Barcelona.html`
over `index.html`, or run in PowerShell:

```powershell
./Set-WorkshopHomepage.ps1 -Mode Advertisement
```

Publish the resulting `index.html`, `Barcelona.html`, `materials.html`, and the
`barcelona-2027/` image folder, together with the normal `site_libs/` directory.
Keep `Barcelona.Rmd` in the repository as the editable source.
The earlier `barcelona-2027.html`, custom `style.css`, and `template.html` are
superseded and have been removed from this local version.

## Restore the regular homepage

The original homepage is preserved as `materials.html`. Copy it over `index.html`,
or run:

```powershell
./Set-WorkshopHomepage.ps1 -Mode Materials
```

Publish the updated index.html. No material or image folders need to move.
The original `index.Rmd` is unchanged. Rendering it also regenerates the regular
homepage; rendering the full R Markdown site will therefore replace the advert.
After a full site render, first preserve the freshly rendered regular homepage
as materials.html, then copy Barcelona.html over index.html if advertising.

## Content and QR code

The organizer confirmed June 14–18, 2027 at Museu de Ciències Naturals de Barcelona,
NO registration fee, and "Apply by January 15, 2027 for full consideration".
Instructor names and photos link to dean-adams.org and michaelcollyer.com.
The original homepage is linked for more course details and current participants.

The QR PNG in `barcelona-2027/application-qr.png` encodes the exact Google Forms
URL supplied by the organizer. It needs no external QR service. If the URL changes,
update the Markdown links and regenerate the QR code as well.

All changes are local until published through the normal GitHub process.
