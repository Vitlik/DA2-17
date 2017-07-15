
#' @title Feature Extraction -  Wrapper function
#' @description The needed package /code{EBImage} can be installed by using the source /code{\linkg{"https://bioconductor.org/biocLite.R"}} then it can be loaded
#' with the command /code{biocLite("EBImage")}. To get (back) to the overview of all steps and functions use this link:
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
#' @author Vitali Friesen, Tassilo Tobollik
b.a.preprocessing.start <- function(){
  library(EBImage)
  library(png)
  
  # Explanation
  b.b.createTrainTestDataIndexes()

  # Explanation
  b.c.loadClassData()
  
  #Create preprocessed images
  b.d.normalization.start(T)
  b.d.normalization.start(F)
}

#' @title Preprocessing - Create Train Test DataSet Indexes
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Vitali Friesen, Tassilo Tobollik
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

  imgList <- list.files("data-raw/IMG/CS CZ original/normal/",full.names = T, ignore.case = F, recursive = T)

  set.seed(77)
  imgIndexRand <- sample(1:length(imgList),length(imgList))
  blocks <- new.env()

  blockNum <- 10
  
  # only take subset of the images to test if less train data worsen the accuracy
  # imgIndexRand <- imgIndexRand[1:1785]
  # imgIndexRand <- imgIndexRand[1:892]

  bin <- sapply(blockNum:1, blockCreator, bucketNum=blockNum, randList=imgIndexRand)
  
  save(blocks, blockNum, file = "data/blocks2677IMG.rda")
  # save(blocks, blockNum, file = "data/blocks1785IMG.rda")
  # save(blocks, blockNum, file = "data/blocks892IMG.rda")
}

#' @title Feature Extraction - Step 2
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Vitali Friesen
b.c.loadClassData <- function(){
  fold <- (
    "classes_full_size"
    #"classes_eighth_size"
  )
  clasColin <- read.csv(paste0("data-raw/", fold, "/ClassificationList-Colin.csv"), sep = ";")
  clasMaren <- read.csv(paste0("data-raw/", fold, "/ClassificationList-Maren.csv"), sep = ";")
  clasNils <- read.csv(paste0("data-raw/", fold, "/ClassificationList-Nils.CSV"), sep = ";")
  clasNils2 <- read.csv(paste0("data-raw/", fold, "/ClassificationList-Nils2.CSV"), sep = ";")
  clasSascha <- read.csv(paste0("data-raw/", fold, "/ClassificationList-Sascha.csv"), sep = ";")
  clasTac <- read.csv(paste0("data-raw/", fold, "/ClassificationList-Tac.csv"), sep = ";")
  clasVit <- read.csv(paste0("data-raw/", fold, "/ClassificationList-Vit.csv"), sep = ";")

  # merge all classifications into one matrix
  classes <- rbind(clasColin, rbind(clasMaren, rbind(clasNils, rbind(clasNils2,
                   rbind(clasSascha,rbind(clasTac, clasVit))))))
  # sort matrix by name column
  classes <- classes[order(classes[,1]),]
  # set rownames to name column
  rownames(classes) <- classes[,1]
  
  # from here only for classifications with CT and T classes
  # discard name column
  classes <- classes[, 2:3]
  # replace NA with zeros because some CSV were empty for a 0
  classes[is.na(classes)] <- 0
  # add new column for "person seen"
  classes <- cbind(classes, P = rowSums(classes))
  # overwrite elements where both classes are seen by one
  classes$P[classes$P == 2] <- 1
  # check if it worked
  #classes[20:40,]
  
  # execute this part only if 3 classes are available but only the third ("P") is filles
  # discard name column
  classes <- classes[, "P", drop=FALSE]
  # replace NA with zeros because some CSV were empty for a 0
  classes[is.na(classes)] <- 0

  # write classes to file
  classesOrig <- classes
  save(classesOrig, file = "data/classesOrig.rda")
  #classesEights <- classes
  #save(classesEights, file = "data/classesEights.rda")
}

#' @title Preprocessing - Method that starts either the rgb-normalization or the histogram-equalization of an image in this project
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#' @param rgbNorm A boolean that describes whether RGB-normalization (/code{TRUE}) or histogram-equalization (/code{FALSE}) should be used
#' @author Tassilo Tobollik
b.d.normalization.start <- function(rgbNorm = T){
  folder <- "data-raw/IMG/CS CZ/rgbNorm"
  
  #The following works only for folders with images in them not with ones with subfolders.
  dirList <- list.dirs(folder,full.names = T, recursive = F)
  
  sapply(1:length(dirList), function(y){
    path <- dirList[y]
    namePos <- gregexpr(folder,path)
    name <- substr(path, attr(namePos[[1]], "match.length") + 2, nchar(path))
    subfolders <- list.dirs(path, full.names = T, recursive = F)
    
    #Check if the folder contains no subfolders
    if(length(subfolders) == 0){
      imgList <- list.files(path, full.names = T, ignore.case = F, recursive = F)
      
      sapply(1:length(imgList), function(x){
        filename <- imgList[x]
        image <- readPNG(filename)
        namePos <- gregexpr(name, filename)
        fileending <- substr(filename, namePos[[1]]+nchar(name)+1, nchar(filename))
        filename <- paste(paste(substr(filename, 1, namePos[[1]]-1), 
                                paste(paste("histEqual/", name, sep = ""), "/", sep = ""), sep = ""), 
                          fileending, sep = "")
        if(rgbNorm){
          b.d.rgbNorm(filename)
        }else{
          b.f.transformHistEqualRgb(image, filename) 
        }
      })
    }
  })
}

#' @title Preprocessing - Normalize rgb images to remove shadow and light effects
#' @description The value of each pixel, of the inserted image, in each colour-dimension will be divided by the sum of the pixels values in all dimensions.
#' By this shadows and light highlights in the image are removed to some degree.
#' To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#' @param filename a string that describes the path to an image from the workingdirectory and it's filename
#' @param x A number that describes the images horizontal number of pixels
#' @param y A number that describes the images vertical number of pixeks
#' @param z A number that describes the images number of color dimensions (normally 3 for RGB or 1 for grayscale)
#' @return \code{normImg} an array of size \code{x * y} and with \code{z} dimensions that holds the normalized RGB-values of the image described by \code{filename}
#' @examples \code{b.d.rgbNorm(filename = "data/image.png", x = 480, y = 640, z = 3)}
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
#' @param image An array with three dimensions that holds the values of an RGB-image
#' @return An array with one dimension that holds the values of a grayscale version of the inserted image
#' @author Tassilo Tobollik
b.e.transformGrayscale <- function(image) {
  library(imager)
  return(grayscale(image))
}

#' @title Preprocessing - Transform rgb images using histogram equalization
#' @description The needed package /code{EBImage} can be installed by using the source /code{\linkg{"https://bioconductor.org/biocLite.R"}} then it can be loaded
#' with the command /code{biocLite("EBImage")}. To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#' @param image An array that can have multiple dimensions and holds the values of image (for example an RGB image). This parameter is optional.
#' @param filename A string that holds the path to the image from the workingdirectory and it's filename. If the parameter \code{image} isn't given the image will be loaded 
#' by using this path.
#' @return An image with histogram equalized RGB-values
#' @example \code{histEqualImage <- b.f.transformHistEqualRgb(imageArray, "data/image.png")}
#' @author Tassilo Tobollik
#' Takes aprox. 0.38 sec/image
b.f.transformHistEqualRgb <- function(image = NULL, filename) {
  library(EBImage)
  
  if(is.null(image)){
    image <- readPNG(filename)
  }
  
  pos <- gregexpr(".png", filename)
  filename2 <- substr(filename, 1, (pos[[1]]-1))
  
  equalImage <- equalize(image)
  
  writePNG(equalImage,paste(filename2,"Equal.png", sep = ""))
  return(equalImage)
}

#' @title Preprocessing - Transform grayscale images using histogram equalization
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#' @param image An array with one dimensions that holds the values of a grayscale image. This parameter is optional.
#' @param filename A string that holds the path to the image from the workingdirectory and it's filename. If the parameter \code{image} isn't given the image will be loaded 
#' by using this path.
#' @return An image with histogram equalized grayscale values
#' @example \code{histEqualImage <- b.f.transformHistEqualGray(imageArray, "data/image.png")}
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
#' @param image An array with three dimensions that holds the values of an RGB-image
#' @return A chart that displays the histogram of the inserted image
#' @author Tassilo Tobollik
b.h.displayImageHist <- function(image){
  hist(equalImage)
  grid()
}