###  FOR ASYMMETRY LAB
library(geomorph)
data('lizards')

Y.gpa <- gpagen(lizards$coords)
liz.sym2 <- bilat.symmetry(A = Y.gpa, ind = lizards$ind, replicate = lizards$rep,
                           object.sym = TRUE, land.pairs = lizards$lm.pairs)
summary(liz.sym2)


# DONE ANOTHER WAY (see March 8 email for options)
gdf <- geomorph.data.frame(shape = lizards$coords, 
ind = lizards$ind, replicate = lizards$rep)
liz.sym <- bilat.symmetry(A = shape, ind = ind, rep = rep, 
object.sym = TRUE, land.pairs = lizards$lm.pairs, data = gdf)
summary(liz.sym)


# object symmetry
data(mosquito)
Y.gpa <- gpagen(mosquito$wingshape)
wing.sym <- bilat.symmetry(A = Y.gpa, ind = mosquito$ind, side=mosquito$side, object.sym = FALSE)
summary(wing.sym)


gdf <- geomorph.data.frame(shape = Y.gpa$coords,
                           ind = mosquito$ind,
                           side = mosquito$side,
                           replicate = mosquito$rep)
## NOTE what one puts in 'A'

wing.anova<-procD.lm(shape ~ ind + side + ind/side, data= gdf)

wing.sym$shape.anova

## NOTE specification of error terms for main effects
anova(wing.anova, error = c("ind:side", "ind:side","Residuals"))

anova(wing.anova, error = c("ind:side", "ind:side","Residuals"))
