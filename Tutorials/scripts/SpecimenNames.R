#Code and analyses for 2.2-SpecimenNames

## ----prelims----

library(geomorph)
data("plethodon")
Array <- plethodon$land[1:5,,1:2]

## ----arraydims----

dimnames(Array)

## ----specnames1----

dimnames(Array)[[3]] <- c("Specimen_1", "Specimen_2")
dimnames(Array)

## ----specnames2----

Names <- c("Specimen_1", "Specimen_3")
dimnames(Array)[[3]] <- Names
dimnames(Array)

## ----changename----

dimnames(Array)[[3]][[2]] <- c("Specimen_2")
dimnames(Array)

## ----alldims----

dimnames(Array) <- list(c("Lmk1", "Lmk2", "Lmk3", "Lmk4", "Lmk5"),
                        c("X", "Y"),
                        c("Specimen_1", "Specimen_2"))
dimnames(Array)    
