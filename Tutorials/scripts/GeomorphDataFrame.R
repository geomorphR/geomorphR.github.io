#Code and analyses for GeomorphDataFram

## ----prelims----

library(geomorph)
data("plethodon")
lmks <- plethodon$land
spec <- plethodon$species
site <- plethodon$site

## ----dataframe----

df <- geomorph.data.frame(lmks = plethodon$land, species = plethodon$species)

## ----print----

df$lmks[,,1] #This will print the landmark coordinates of the first specimen

df[[2]] #This will print the second object in the data frame, the factor with species information