#' This script won't be part of the build package.
#' Its purpose is to transfer the source data to a R data.frame.
#'

#' Load all paths to the images in vector
imgList <- list.files("data-raw/IMG/CS CZ",full.names = T, ignore.case = F, recursive = T)
imgListH <- list.files("data-raw/IMG/CS CZ halved",full.names = T, ignore.case = F, recursive = T)
imgListQ <- list.files("data-raw/IMG/CS CZ quarter",full.names = T, ignore.case = F, recursive = T)
# see order of folders
unique(substr(imgList,20,23))

library(png)
img = readPNG("data-raw/IMG/CS CZ/Vit/hl 2017-06-14 18-05-42-48.png")
dim(img)
str(img)

plot(1:2, type='n')
rasterImage(img, 1, 1, 2, 2, interpolate=FALSE)
# raster just part of the image
rasterImage(img[185:235,325:425,], 1, 1, 2, 2, interpolate=FALSE)

# but img is now 3-dimensional matrix
# we need to make it an vector
imgV <- as.vector(img)

clasColin <- read.csv("data-raw/classes_full_size/ClassificationList-Colin.csv", sep = ";")
clasMaren <- read.csv("data-raw/classes_full_size/ClassificationList-Maren.csv", sep = ";")
clasNils <- read.csv("data-raw/classes_full_size/ClassificationList-Nils.CSV", sep = ";")
clasNils2 <- read.csv("data-raw/classes_full_size/ClassificationList-Nils2.CSV", sep = ";")
clasSascha <- read.csv("data-raw/classes_full_size/ClassificationList-Sascha.csv", sep = ";")
clasTac <- read.csv("data-raw/classes_full_size/ClassificationList-Tac.csv", sep = ";")
clasVit <- read.csv("data-raw/classes_full_size/ClassificationList-Vit.csv", sep = ";")

# merge all classifications into one matrix
clasAll <- rbind(clasColin, rbind(clasMaren, rbind(clasNils, rbind(clasNils2,
                  rbind(clasSascha,rbind(clasTac, clasVit))))))
# sort matrix by name column
clasAll <- clasAll[order(clasAll[,1]),]
# set rownames to name column
rownames(clasAll) <- clasAll[,1]
# discard name column
clasAll <- clasAll[, 2:3]
# add new column for "person seen"
clasAll <- cbind(clasAll, P = rowSums(clasAll))
# overwrite elements where both classes are seen by one
clasAll$P[clasAll$P == 2] <- 1
# check if it worked
clasAll[20:40,]

# write classes to file
devtools::use_data(clasAll, overwrite = T)


