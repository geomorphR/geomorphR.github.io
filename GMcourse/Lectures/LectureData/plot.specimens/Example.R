library(geomorph)
source('plot.Specimens.r')

liz <- readland.nts("lizards_LAT.nts")
links <- read.table("links.txt")

sex <- factor(sample(c("F", "M"), dim(liz)[3], replace = T))
x <- plot.specimens(liz)
x <- plot.specimens(liz, links = links)
x <- plot.specimens(liz, pch = ifelse(sex=="F", 21, 22), col = ifelse(sex=="F", "red", "blue"))
x <- plot.specimens(liz, links = links, pch = ifelse(sex=="F", 21, 22), col = ifelse(sex=="F", "red", "blue"))

SP <- plot.specimens(liz, links = links, pch = ifelse(sex=="F", 21, 22), col = ifelse(sex=="F", "red", "blue"))
add.mshape(SP)
add.mshape(SP, pch = 24, col = "grey", lwd = 3)

