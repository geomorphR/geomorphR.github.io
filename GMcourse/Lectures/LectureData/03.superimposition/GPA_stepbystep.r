library(geomorph)

liz <- readland.nts("lizards_LAT.nts")
links <- read.table("links.txt")

y.gpa <- gpagen(liz, print.progress = FALSE)
ref <- y.gpa$consensus * mean(y.gpa$Csize)
newdat <- hummingbirds$land[,,1:2]; newdat[,,1] <- ref

#Center-Scale: 
dat.cs <- lapply(1:2, function(j) geomorph:::center.scale(newdat[,,j])$coords)
dat.csplot <- simplify2array(dat.cs)

#rotate
dat.rot <- geomorph:::apply.pPsup(dat.cs[[1]],dat.cs)
dat.gpa <- simplify2array(dat.rot)








  