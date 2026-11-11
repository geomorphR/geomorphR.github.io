# Barcelona 2027 temporary homepage

The existing workshop homepage is preserved, byte for byte, as `materials.html`.
The temporary advertisement is `barcelona-2027.html`, with local images, QR code,
and CSS in `barcelona-2027/`. `index.html` is a copy of the selected homepage.
No build tool or JavaScript is needed to view the advertisement, including locally.

## Close applications / restore the regular homepage

From PowerShell in this directory:

```powershell
./Set-WorkshopHomepage.ps1 -Mode Materials
```

Or simply copy `materials.html` over `index.html`. Publish that change to GitHub
Pages using the normal process. The regular workshop materials URL continues to work.
The advertisement stays available at `barcelona-2027.html` for reference.

To show the advertisement again:

```powershell
./Set-WorkshopHomepage.ps1 -Mode Advertisement
```

## Editing and R Markdown

Edit `Barcelona.Rmd` for content and `barcelona-2027/style.css` for styling.
In RStudio, the Knit button generates `barcelona-2027.html`. From the R console,
with this directory as the working directory, use:

```r
rmarkdown::render("Barcelona.Rmd", output_file = "barcelona-2027.html")
```

The custom `barcelona-2027/template.html` preserves the advertisement's layout
without adding the regular site's navigation or Bootstrap styling. Keep this
template alongside the CSS and image assets. Rendering requires `rmarkdown`,
`knitr`, and Pandoc (included with RStudio). Published HTML needs none of these.

After rendering, run the Advertisement command above to copy the generated
`barcelona-2027.html` to `index.html`. Or copy it manually. Rendering alone does
not change the active homepage. Do not edit the generated HTML files directly.
The original `index.Rmd` is unchanged. Rendering the original R Markdown site
regenerates the regular `index.html`. After an R Markdown render, copy that fresh
regular homepage to `materials.html`, then run the Advertisement command if the
advertisement should still be the homepage. Avoid copying the advertisement to
`materials.html`.

## Content and application QR

The date and city come from the workshop organizer. Instructor affiliations and
course overview follow the existing workshop site. Sponsors and portraits come
from the supplied `2027-Barcelona` folder. The organizer confirmed that there is NO registration fee and the venue is
Museu de Ciències Naturals de Barcelona. The application deadline reads: "Apply by January 15, 2027 for full consideration". Capacity
is intentionally absent until confirmed for Barcelona.

The application button and QR code use the exact Google Forms URL supplied by
the organizer, including its query parameters. The QR is a static local PNG with
a four-module white margin and medium error correction; it needs no external QR
service. If the form URL changes, update both form links and regenerate the QR.

All changes are local until committed and published to the existing GitHub site.
