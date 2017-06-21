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

imgSample <- sample(1:length(imgList),length(imgList))
blocks <- new.env()
blocks$train1 <- imgSample[1:(round(length(imgSample)/10*9))]
blocks$test1 <- imgSample[(round(length(imgSample)/10*9)+1):length(imgSample)]

blocks$train2 <- c(imgSample[1:(round(length(imgSample)/10*8))],
                   imgSample[(round(length(imgSample)/10*9)+1):length(imgSample)])
blocks$test2 <- imgSample[(round(length(imgSample)/10*8)+1):(round(length(imgSample)/10*9))]

blocks$train3 <- c(imgSample[1:(round(length(imgSample)/10*7))],
                   imgSample[(round(length(imgSample)/10*8)+1):length(imgSample)])
blocks$test3 <- imgSample[(round(length(imgSample)/10*7)+1):(round(length(imgSample)/10*8))]

blocks$train4 <- c(imgSample[1:(round(length(imgSample)/10*6))],
                   imgSample[(round(length(imgSample)/10*7)+1):length(imgSample)])
blocks$test4 <- imgSample[(round(length(imgSample)/10*6)+1):(round(length(imgSample)/10*7))]

blocks$train5 <- c(imgSample[1:(round(length(imgSample)/10*5))],
                   imgSample[(round(length(imgSample)/10*6)+1):length(imgSample)])
blocks$test5 <- imgSample[(round(length(imgSample)/10*5)+1):(round(length(imgSample)/10*6))]

blocks$train6 <- c(imgSample[1:(round(length(imgSample)/10*4))],
                   imgSample[(round(length(imgSample)/10*5)+1):length(imgSample)])
blocks$test6 <- imgSample[(round(length(imgSample)/10*4)+1):(round(length(imgSample)/10*5))]

blocks$train7 <- c(imgSample[1:(round(length(imgSample)/10*3))],
                   imgSample[(round(length(imgSample)/10*4)+1):length(imgSample)])
blocks$test7 <- imgSample[(round(length(imgSample)/10*3)+1):(round(length(imgSample)/10*4))]

blocks$train8 <- c(imgSample[1:(round(length(imgSample)/10*2))],
                   imgSample[(round(length(imgSample)/10*3)+1):length(imgSample)])
blocks$test8 <- imgSample[(round(length(imgSample)/10*2)+1):(round(length(imgSample)/10*3))]

blocks$train9 <- c(imgSample[1:(round(length(imgSample)/10*1))],
                   imgSample[(round(length(imgSample)/10*2)+1):length(imgSample)])
blocks$test9 <- imgSample[(round(length(imgSample)/10*1)+1):(round(length(imgSample)/10*2))]

blocks$train10 <- imgSample[(round(length(imgSample)/10*1)+1):length(imgSample)]
blocks$test10 <- imgSample[1:(round(length(imgSample)/10*1))]
