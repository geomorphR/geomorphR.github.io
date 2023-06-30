#Code and analyses for 1.4-StandardizeArticulations

## ----prelims----

library(geomorph)
library(plotrix)
data(plethodon)

## ----gpadata----

Y.gpa <- gpagen(plethodon$land, print.progress = FALSE)

rand.jaw <-Y.gpa$coords
for (i in 1:dim(Y.gpa$coords)[[3]]){
  tmp <- fixed.angle(Y.gpa$coords,art.pt=1, angle.pts.1 = 5, angle.pts.2 = 6, 
                     rot.pts = c(2,3,4,5),angle = -1*runif(1,min=0,max=40),degrees=TRUE)
  rand.jaw[,,i] <- tmp[,,i]
}
gpa.rand <- gpagen(rand.jaw, print.progress = FALSE)$coords

## ----plotallspecimens----

plotAllSpecimens(gpa.rand, links = plethodon$links)
draw.circle(-0.3,0.05, 0.08, border = "red", lwd = 2)
draw.circle(-0.25,-0.15, 0.08, border = "red", lwd = 2)
mtext("Jaws Random Angles")

## ----fixarticul----

jaw.fixed <- fixed.angle(gpa.rand,
                         art.pt=1, angle.pts.1 = 5, 
                         angle.pts.2 = 6, rot.pts = c(2,3,4,5))

gpa.fixed <- gpagen(jaw.fixed, print.progress = FALSE)$coords

## ----endpoints----

GP <- gridPar(txt.pos = 2)
links.art <- matrix(c(1,5,1,6), ncol=2, byrow=TRUE)
colnames(links.art) <- c("start", "end")

## ----plotcompare----

par(mfrow=c(1,2))
plotAllSpecimens(gpa.rand, links = plethodon$links)
mtext("Jaws Random Angles")
plotAllSpecimens(gpa.fixed, links = plethodon$links)
mtext("Jaws Standardized to Common Angle")
plotRefToTarget(gpa.rand[,,1], gpa.rand[,,1], gridPars = GP, links = links.art, label = TRUE, method = "points")