#Code and analyses for plotallometry

## ----prelims----

library(geomorph)

data(plethodon) 
gpa <- gpagen(plethodon$land, print.progress = FALSE)    #GPA-alignment  

gdf <- geomorph.data.frame(gpa, site = plethodon$site, 
                           species = plethodon$species) 
fit <- procD.lm(coords ~ log(Csize), data=gdf, iter=0, 
                print.progress = FALSE)

## ----predline----

plotAllometry(fit, size = gdf$Csize, logsz = TRUE, 
              method = "PredLine", pch = 19)

## ----regscore----

plotAllometry(fit, size = gdf$Csize, logsz = TRUE, 
              method = "RegScore", pch = 19)

## ----sizeshape----

plotAllometry(fit, size = gdf$Csize, logsz = TRUE, 
              method = "size.shape", pch = 19)

## ----cac----

plotAllometry(fit, size = gdf$Csize, logsz = TRUE, 
              method = "CAC", pch = 19)