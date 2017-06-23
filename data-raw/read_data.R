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

clasVit <- read.csv("data-raw/ClassificationList-Vit.csv",sep = ";")


devtools::use_data(imgV, overwrite = T)
devtools::use_data(clasVit, overwrite = T)

set.seed(77)

imgIndexRand <- sample(1:length(imgList),length(imgList))
blocks <- new.env()

blockCreator <- function(x, y, bucketNum, randList){
  lastIndexLeftTrainBlock <- round(length(randList) / bucketNum * (x-1))
  firstIndexRightTrainBlock <- round(length(randList) / bucketNum * x + 1)
  trainName <- paste("train", toString(bucketNum+1-x), sep="")
  assign(trainName,
         c(ifelse(lastIndexLeftTrainBlock != 0,
                  randList[1:lastIndexLeftTrainBlock],
                    numeric(0)),
           ifelse(firstIndexRightTrainBlock < length(randList),
                  randList[firstIndexRightTrainBlock:length(randList)],
                  numeric(0))),
         envir=blocks)

  firstIndexTestBlock <- round(length(randList) / bucketNum * (x-1) + 1)
  lastIndexTestBlock <- round(length(randList) / bucketNum * x)
  testName <- paste("test", toString(bucketNum+1-x), sep="")
  assign(testName,
         randList[firstIndexTestBlock:lastIndexTestBlock],
         envir=blocks)
}
blockNum = 10

bin <- sapply(blockNum:1, closure, bucketNum=blockNum, randList=imgIndexRand)
