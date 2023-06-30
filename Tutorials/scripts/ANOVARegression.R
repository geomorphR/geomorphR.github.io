## ----prep----

lmks <- gpagen(plethodon$land, print.progress = F)$coords
spec <- plethodon$species
site <- plethodon$site

df <- geomorph.data.frame(lmks, spec = spec, site = site)

## ----fit----

fit <- procD.lm(lmks ~ spec * site, 
                data = df, iter = 999, turbo = TRUE,
                RRPP = TRUE, print.progress = FALSE)

## ----sum----

summary(fit)

