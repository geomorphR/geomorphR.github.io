## ----prep----

Y.gpa <- gpagen(plethspecies$land, print.progress = F)

gdf <- geomorph.data.frame(Y.gpa, phy = plethspecies$phy)

## ----fit----

fit <- procD.pgls(coords ~ Csize, phy = phy, data = gdf, print.progress = F)

## ----sum----

summary(fit)