
#' @title Feature Extraction -  Wrapper function
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' This is a wrapper function for ...
#'
#' It executes these functions:
#' \enumerate{
#'   \item \code{\link{b.b.createTrainTestDataIndexes}}
#'   \item \code{\link{b.c.loadClassData}}
#' }
#'
#' @author Vitali Friesen
b.a.preprocessing.start <- function(){
  source("https://bioconductor.org/biocLite.R")
  biocLite("EBImage")
  
  # Explanation
  b.b.createTrainTestDataIndexes()

  # Explanation
  b.c.loadClassData()
  
  #Create preprocessed images
  
  folder <- "data-raw\IMG\CS CZ"
  
  #The following works only for folders with images in them not with ones with subfolders.
  dirList <- list.dirs("data-raw/IMG/CS CZ",full.names = T, recursive = F)
  
  sapply(1:length(dirList), function(y){
    name <- dirList[y]
    subfolders <- list.dirs(name, full.names = T, recursive = F)
    
    #Check if the folder contains no subfolders
    if(length(subfolders) == 0){
      imgList <- list.files(name, full.names = T, ignore.case = F, recursive = F)
      
      sapply(1:length(imgList), function(x){
        filename <- imgList[x]
        image <- readPNG(filename)
        namePos <- gregexpr(name, filename)
        fileending <- substr(filename, namePos[[1]]+nchar(name)+1, nchar(filename))
        filename <- paste(paste(substr(filename, 1, namePos[[1]]-1), paste(paste("HistEqual/", name, sep = ""), "/", sep = ""), sep = ""), fileending, sep = "")
        b.f.transformHistEqualRgb(image, filename)
        b.d.rgbNorm(imgList[x])
      })
    }
  })
}

#' @title Preprocessing - Create Train Test DataSet Indexes
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Vitali Friesen & Tassilo Tobollik
b.b.createTrainTestDataIndexes <- function(){

  blockCreator <- function(x, bucketNum, randList){
    lastIndexLeftTrainBlock <- round(length(randList) / bucketNum * (x-1))
    firstIndexRightTrainBlock <- round(length(randList) / bucketNum * x + 1)
    trainName <- paste0("train", toString(bucketNum+1-x))
    if(lastIndexLeftTrainBlock > 0)
      trainLeftBlock <- randList[1:lastIndexLeftTrainBlock]
    else
      trainLeftBlock <- numeric(0)
    if(firstIndexRightTrainBlock < length(randList))
      trainRightBlock <- randList[firstIndexRightTrainBlock:length(randList)]
    else
      trainRightBlock <- numeric(0)
    assign(trainName, c(trainLeftBlock, trainRightBlock), envir=blocks)

    firstIndexTestBlock <- round(length(randList) / bucketNum * (x-1) + 1)
    lastIndexTestBlock <- round(length(randList) / bucketNum * x)
    testName <- paste0("test", toString(bucketNum+1-x))
    assign(testName,
           randList[firstIndexTestBlock:lastIndexTestBlock],
           envir=blocks)
  }

  imgList <- list.files("data-raw/IMG/CS CZ",full.names = T, ignore.case = F, recursive = T)

  set.seed(77)
  imgIndexRand <- sample(1:length(imgList),length(imgList))
  blocks <<- new.env()

  blockNum <<- 10

  bin <- sapply(blockNum:1, blockCreator, bucketNum=blockNum, randList=imgIndexRand)
}

#' @title Feature Extraction - Step 2
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author
b.c.loadClassData <- function(){
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
  # replace NA with zeros because some CSV were empty for a 0
  clasAll[is.na(clasAll)] <- 0
  # add new column for "person seen"
  clasAll <- cbind(clasAll, P = rowSums(clasAll))
  # overwrite elements where both classes are seen by one
  clasAll$P[clasAll$P == 2] <- 1
  # check if it worked
  clasAll[20:40,]

  # write classes to file
  devtools::use_data(clasAll, overwrite = T)
}

#' @title Preprocessing - Normalize rgb images to remove shadow and light effects
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Tassilo Tobollik
#Takes approx. 1.1294 secs per screenshot
b.d.rgbNorm <- function(filename, x = 480, y = 640, z = 3){
  library(png)
  pos <- gregexpr(".png", filename)
  filename2 <- substr(filename, 1, (pos[[1]]-1))
  
  img = readPNG(filename)
  
  # Compute the sums of the pixel's values over all channels
  S <- apply(img,c(1,2),sum)
  
  normImg <- array(seq(0,0), dim = c(x,y,z))
  # Compute the normalized RGB values by dividing each pixel's value in each dimension by S
  normV <- sapply(1:z,function(x) img[,,x]/S)
  normImg <- array(normV, dim = c(x,y,z))
  # Set NaN values, received from dividing 0 by 0, to 0
  normImg[is.nan(normImg)] = 0
  
  writePNG(normImg,paste(filename2,"Norm.png", sep = ""))
  return(normImg)
}

#' @title Preprocessing - Transform rgb images to grayscale
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Tassilo Tobollik
b.e.transformGreyscale <- function(image) {
  library(imager)
  return(grayscale(image))
}

#' @title Preprocessing - Transform rgb images using histogram equalization
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Tassilo Tobollik
#' Takes aprox. 0.38 sec/image
b.f.transformHistEqualRgb <- function(image = NULL, filename) {
  #Package installation
  #source("https://bioconductor.org/biocLite.R")
  #biocLite("EBImage")
  library(EBImage)
  
  if(is.null(image)){
    image <- readPNG(filename)
  }
  
  pos <- gregexpr(".png", filename)
  filename2 <- substr(filename, 1, (pos[[1]]-1))
  
  #pos2 <- gregexpr("CZ/", filename)
  #person <- substr(filename, pos2[[1]]+1, (pos[[1]]-1))
  
  equalImage <- equalize(image)
  
  writePNG(equalImage,paste(filename2,"Equal.png", sep = ""))
}

#' @title Preprocessing - Transform grayscale images using histogram equalization
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Tassilo Tobollik
b.g.transformHistEqualGray <- function(image = NULL, filename) {
  library(imager)
  library(png)
  
  if(is.null(image)){
    image <- readImage(filename)
  }
  
  grayscale(image)
}

#' @title Preprocessing - Display the histogram of an image
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Tassilo Tobollik
b.h.displayImageHist <- function(image){
  #TODO: Add colors to histogram
  hist(equalImage)
  grid()
}