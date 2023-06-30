#Code and analyses for Flip-RotateLmks

## ----prelims----

library(geomorph)
data("mosquito")

## ----origdata----

wings <- gpagen(mosquito$wingshape[,,1:5], print.progress = F)

plotAllSpecimens(wings$coords)

## ----flipdata----

wingsflip <- rotate.coords(wings$coords, type = "flipX")
plotAllSpecimens(wingsflip)

## ----flipone----

flip <- c(0,0,0,1,0)

wings <- rotate.coords(wings$coords, type = "flipX", index = flip)

## ----plot----

plotAllSpecimens(wings)

## ----flipback----

flip <- c(0,0,0,1,0)

wingsflip <- rotate.coords(wings, type = "flipX", index = flip)

plotAllSpecimens(wingsflip)