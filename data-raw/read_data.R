#' This script won't be part of the build package.
#' Its purpose is to transfer the source data to a R data.frame.
#'

#' Load all paths to the images in vector
imgList <- list.files("data-raw/IMG/CS CZ",full.names = T, ignore.case = F, recursive = T)
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

clasVit <- read.csv("data-raw/ClassificationList-Vit.csv",sep = ";")


devtools::use_data(imgV, overwrite = T)
devtools::use_data(clasVit, overwrite = T)
