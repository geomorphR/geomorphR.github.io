#Code and analyses for 2.3-ExtractingClassifiers

## ----prelims----

library(geomorph)
data("plethodon")
data <- plethodon$land[,,c(1:3, 12:14)]
names <- c("P_jord_1234", "P_jord_4312", "P_jord_2314", "P_teyah_3214", "P_teyah_1324", "P_teyah_1423")
dimnames(data)[[3]] <- names

## ----strsplit----

categories <- strsplit(dimnames(data)[[3]], "_")  
# separates the specimen names by underscore

## ----classifiers----

classifiers <- matrix(unlist(categories), ncol=3, byrow=T) 

classifiers <- cbind(dimnames(data)[[3]], classifiers)
# add the specimen ID to the first column of the table 

## ----colnames----

colnames(classifiers) <- c("fileID", "Genus", "Species", "ID")

classifiers <- as.data.frame(classifiers)

classifiers

