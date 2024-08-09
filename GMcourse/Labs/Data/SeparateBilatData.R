#split wings from image to data (and reflect to match)
library(geomorph)

data("lizards")
plotAllSpecimens(lizards$coords)
n <- dim(lizards$coords)[3]

# NOTE: you must create this!!!
pairs <- lizards$lm.pairs

left <- lizards$coords[pairs[,1],,]
right <- rotate.coords(lizards$coords[pairs[,2],,], type = "flipY")

#check
par(mfcol = c(1, 2))
plotAllSpecimens(left[,,1:2], label = TRUE)
plotAllSpecimens(right[,,1:2], label = TRUE)
par(mfcol = c(1, 1))

#combine
all.lm <- combine.subsets(left = left,
        right = right, gpa = FALSE, CS.sets = NULL)$coords
plotAllSpecimens((all.lm))
 ind <- c(seq(1:n),seq(1:n))
 side <- c(rep(1,n), rep(2, n))
 
             