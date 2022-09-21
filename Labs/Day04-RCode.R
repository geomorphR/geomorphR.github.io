# This R script contains the concatenated code from the day 4 tutorials.
# This is what you should work through during lab, using your own data.

library(geomorph)

##### 1: Asymmetry ==================================================================

## matching symmetry

data(mosquito)
str(mosquito)
mosq.sym <- bilat.symmetry(mosquito$wingshape, 
                           ind = mosquito$ind, side = mosquito$side,
                           replicate = mosquito$replicate, 
                           object.sym = FALSE, print.progress = FALSE)
summary(mosq.sym)
plot(mosq.sym)

## object symmetry
data(scallops)
str(scallops)
plot(mshape(scallops$coorddata))
scallops$land.pairs

scallop.sym <- bilat.symmetry(scallops$coorddata, 
                              ind = scallops$ind, object.sym = TRUE,
                              land.pairs = scallops$land.pairs, 
                              print.progress = FALSE)
summary(scallop.sym)
plot(scallop.sym)

##### 2: Integration and Modularity ==================================================

# Integration
data(plethodon)
Y.gpa <- gpagen(plethodon$land, print.progress = FALSE)
integration.test(Y.gpa$coords[1:5,,], Y.gpa$coords[6:12,,],
                 iter=999, print.progress = FALSE)

integration.test(Y.gpa$coords, partition.gp=c(rep("A", 5), rep("B", 7)), 
                 iter=999, print.progress = FALSE)

two.b.pls(Y.gpa$coords[1:5,,], Y.gpa$coords[6:12,,], iter=999, 
          print.progress = FALSE)

pleth.integr <- integration.test(Y.gpa$coords[1:5,,], 
                                 Y.gpa$coords[6:12,,], iter=999, 
                                 print.progress = FALSE)
summary(pleth.integr)
pls.plot <- plot(pleth.integr)

picknplot.shape(pls.plot)

### ----------------------------------------------------------------------------------

# Compare Strength of Integration
gps <- factor(paste(plethodon$species, plethodon$site))
ljaw.gp <- coords.subset(Y.gpa$coords[1:5,,], gps)
ujaw.gp <- coords.subset(Y.gpa$coords[6:12,,], gps)
integ.tests <- Map(function(x,y) integration.test(x, y, iter=999, 
                   print.progress = FALSE),
                   ljaw.gp, ujaw.gp)
group.z <- compare.pls(integ.tests)
summary(group.z)

### ----------------------------------------------------------------------------------

# Global Integration
globalIntegration(Y.gpa$coords)

### ----------------------------------------------------------------------------------
# Modularity
land.gps <- c("A","A","A","A","A","B","B","B","B","B","B","B") #specify partitions
modularity.test(Y.gpa$coords, land.gps, iter=999, print.progress = FALSE)

### ----------------------------------------------------------------------------------
# Compare modularity patterns

data(pupfish) 
Y.gpa<-gpagen(pupfish$coords, print.progress = FALSE)    #GPA-alignment    

## landmarks on the body and operculum
land.gps<-rep('a',56); land.gps[39:48]<-'b'

group <- factor(paste(pupfish$Pop, pupfish$Sex, sep = "."))
levels(group)

coords.gp <- coords.subset(Y.gpa$coords, group)

modularity.tests <- lapply(1:nlevels(group), function(j) modularity.test(coords.gp[[j]],
                                                                         land.gps, iter = 499, print.progress = FALSE))
## the lapply function performs the modularity test on each 3D array in the lists provided

modularity.tests[[1]]
modularity.tests[[2]]
modularity.tests[[3]]
modularity.tests[[4]]

group.Z <- compare.CR(modularity.tests, CR.null = FALSE)
group.Z 


##### 3: Morphological Disparity ====================================================
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


