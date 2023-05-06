# This R script contains the concatenated code from the day 4 tutorials.
# This is what you should work through during lab, using your own data.

library(geomorph)

## 1: Asymmetry

## matching symmetry
data(mosquito)
Y.gpa <- gpagen(mosquito$wingshape, print.progress = FALSE)
plot(Y.gpa)
mosquito.sym <- bilat.symmetry(A = Y.gpa, ind = mosquito$ind, side=mosquito$side, 
                               object.sym = FALSE, print.progress = FALSE)
summary(mosquito.sym)

## object symmetry
data('lizards')
Y.gpa <- gpagen(lizards$coords, print.progress = FALSE)
plot(Y.gpa)

lizard.sym <- bilat.symmetry(A = Y.gpa, ind = lizards$ind, replicate = lizards$rep,
                             object.sym = TRUE, land.pairs = lizards$lm.pairs, print.progress = FALSE)
summary(lizard.sym)
plot(lizard.sym, warpgrids = TRUE)

#### Comparison of overall vs. symmetrized aligned shapes
plotAllSpecimens(Y.gpa$coords)
plotAllSpecimens(lizard.sym$symm.shape)

## 2: Integration and Modularity
## Overall Integration
data("plethodon")
Y.gpa <- gpagen(plethodon$land, print.progress = FALSE)
#Separate data by species
coords.gp <- coords.subset(Y.gpa$coords, plethodon$species)

#Z_Vrel by species
Vrel.gp <- Map(function(x) integration.Vrel(x), coords.gp) 
compare.ZVrel(Vrel.gp$Jord, Vrel.gp$Teyah)

## Integration Across Spatial Scales
globalIntegration(Y.gpa$coords) #data are not spatially integrated

## Integration Among Subsets
data(pupfish) # GPA previously performed
group <- factor(paste(pupfish$Pop, pupfish$Sex, sep = "."))

# Subset 3D array by group, returning a list of 3D arrays
tail.LM <- c(1:3, 5:9, 18:38)
head.LM <- (1:56)[-tail.LM]
tail.coords <- pupfish$coords[tail.LM,,]
head.coords <- pupfish$coords[head.LM,,]

IT <- integration.test(tail.coords, head.coords, print.progress = F)
summary(IT)
plot(IT)

land.gp <- rep(1,56); land.gp[tail.LM] <- 2
integration.test(pupfish$coords, partition.gp=land.gp, print.progress = FALSE)

two.b.pls(tail.coords, head.coords, print.progress = FALSE)

## Comparing the Strength of Integration
tail.coords.gp <- coords.subset(tail.coords, group)
head.coords.gp <- coords.subset(head.coords, group)

# Obtain Integration for groups
integ.tests <- Map(function(x,y) integration.test(x, y, iter=499, 
                                                  print.progress = FALSE), head.coords.gp, tail.coords.gp)
compare.pls(integ.tests)

## Tests of Modularity
MT <- modularity.test(pupfish$coords,land.gp,CI=FALSE,print.progress = FALSE)
summary(MT)
plot(MT)

## Comparing the Strength of Modularity
coords.gp <- coords.subset(pupfish$coords, group)
modul.tests <- Map(function(x) modularity.test(x, land.gp,print.progress = FALSE), coords.gp) 
compare.CR(modul.tests, CR.null = FALSE)

## Comparing Alternative Modular Partitions
land.gps3 <- rep('a',56); land.gps3[39:48]<-'b'; land.gps3[c(6:9,28:38)] <- 'c' 
#3 module hypothesis (tail now a module)
land.gps4 <- rep('a',56); land.gps4[39:48]<-'b'; land.gps4[c(6:9,28:38)] <- 'c'; 
land.gps4[c(10,49:56)] <- 'd'  #4 module hypothesis (eye now a module)

m3.test <- modularity.test(coords.gp$Marsh.F,land.gps3, iter = 499, print.progress = FALSE)
m4.test <- modularity.test(coords.gp$Marsh.F,land.gps4, iter = 499, print.progress = FALSE)

model.Z <- compare.CR(m3.test,m4.test, CR.null = TRUE)
model.Z 
#########################################################

## 3: Morphological Disparity
data(plethodon)
Y.gpa <- gpagen(plethodon$land, print.progress = FALSE)    
gdf <- geomorph.data.frame(Y.gpa, species = plethodon$species, site = plethodon$site)

# Typical group comparisons
MD <- morphol.disparity(coords ~ species*site, groups = ~species*site, 
                        data = gdf, iter = 999, print.progress = FALSE)
summary(MD)
gp <- interaction(plethodon$species, plethodon$site)
plotTangentSpace(Y.gpa$coords, groups=gp)

# Comparing species, despite species * site means estimation
MD2 <- morphol.disparity(coords ~ species*site, groups = ~species, 
                         data = gdf, iter = 999, print.progress = FALSE)
summary(MD2)

# Comparing group disparities from overall mean
MD3 <- morphol.disparity(coords ~ 1, groups = ~species, 
                         data = gdf, iter = 999, print.progress = FALSE)
summary(MD3)

# Comparing Foote's (1993) partial disparities
MD4 <- morphol.disparity(coords ~ 1, groups = ~species, partial = TRUE,
                         data = gdf, iter = 999, print.progress = FALSE)
summary(MD4)

### ----------------------------------------------------------------------------------
# Evaluating disparity in an allometric model

data(pupfish) # data already aligned
gdf <- geomorph.data.frame(coords = pupfish$coords, 
                           CS = pupfish$CS,
                           Pop = pupfish$Pop,
                           Sex = pupfish$Sex)
MD <- morphol.disparity(coords ~ Pop*Sex, groups = ~ Pop*Sex, 
                        data = gdf, iter = 999, print.progress = FALSE)
summary(MD)
gp <- interaction(pupfish$Pop, pupfish$Sex)
plotTangentSpace(pupfish$coords, groups=gp)

pupfish.allometry <- procD.lm(coords ~ log(CS) + Pop * Sex, data = gdf, 
                              iter = 999, print.progress = FALSE)
summary(pupfish.allometry)
MD2 <- morphol.disparity(pupfish.allometry, groups = gp, print.progress = FALSE)
summary(MD2)


