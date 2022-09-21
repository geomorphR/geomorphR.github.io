# Recreate figures for Lecture 4: shape spaces, TPS, PCA

library(geomorph)
library(scatterplot3d)
lizards <- readland.nts('lizards_LAT.nts')
links <- read.csv('links.txt', header=FALSE, sep = " ")
liz.lab <- read.csv('liz_groups.csv',header=TRUE, sep="\t")

col.gp <- rep("red",nrow(liz.lab))
col.gp[which(liz.lab$SEX=='M')] <- 'blue'

Y.gpa <- gpagen(lizards, print.progress = FALSE)
Y.gpa2 <- gpagen(lizards, Proj = FALSE, print.progress = FALSE)

plotAllSpecimens(lizards, links = links)
plotAllSpecimens(Y.gpa$coords, links=links)

#shape variable comparison
Kendall.d <- dist(two.d.array(Y.gpa$coords))
GPA.d <- dist(two.d.array(Y.gpa2$coords))

cor(Kendall.d,GPA.d)
plot(GPA.d,Kendall.d)
#Need PCA, and male/female TPS
plotTangentSpace(Y.gpa$coords,warpgrids = FALSE, groups=col.gp)

###male/female means
male <- mshape(Y.gpa$coords[,,which(liz.lab$SEX=='M')])
female <- mshape(Y.gpa$coords[,,which(liz.lab$SEX=='F')])

plotRefToTarget(male,female,links = links, mag = 5)
plotRefToTarget(female,male,links = links, mag = 5)

####  RWA with alpha -1 & 1
ref <- mshape(Y.gpa$coords)
p<-nrow(ref)
L.inv <- geomorph:::Ltemplate(ref)
BE <- svd(L.inv)
PrW.1 <- kronecker(diag(2),BE$u[,1:(p-3)]%*%diag(BE$d[1:(p-3)]^-(1/2)))
PrW.m1 <- kronecker(diag(2),BE$u[,1:(p-3)]%*%diag(BE$d[1:(p-3)]^-(-1/2)))

PW.1 <- two.d.array(simplify2array(lapply(1:dim(Y.gpa$coords)[3], 
                                          function(j) as.vector(Y.gpa$coords[,,j])%*%PrW.1)))

PW.m1 <- two.d.array(simplify2array(lapply(1:dim(Y.gpa$coords)[3], 
                                          function(j) as.vector(Y.gpa$coords[,,j])%*%PrW.m1)))

plot(prcomp(PW.1)$x,asp=1, pch=21, bg=col.gp)
plot(prcomp(PW.m1)$x,asp=1, pch=21, bg=col.gp)

#############
# shape spaces: 
n=2000
p=3
k=2
seed = 2
tri<- arrayspecs(matrix(runif(n*p*k),nrow=n),p=p,k=k)
tri.gpa <- gpagen(tri,Proj = FALSE, print.progress = FALSE)

pc.tri <- prcomp(two.d.array(tri.gpa$coords))$x
mult.pc <- ifelse(which.min(abs(range(pc.tri[,3])))==1,-1,1) #make'up-facing'

plot<-scatterplot3d(pc.tri[,1],pc.tri[,2],mult.pc*pc.tri[,3], asp=1, pch=21,bg="red",
                    tick.marks = FALSE, box=FALSE)
plot(pc.tri[,1:2], asp=1, pch=21, bg="red")

tri.gpa2 <- gpagen(tri, print.progress = FALSE)
plotAllSpecimens(tri.gpa2$coords)
dist.shape <- dist(two.d.array(tri.gpa$coords))
dist.Kendall <- dist(two.d.array(tri.gpa2$coords))

cor(dist.shape,dist.Kendall)


