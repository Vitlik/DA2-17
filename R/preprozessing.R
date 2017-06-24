
#' @title Feature Extraction -  Wrapper function
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' This is a wrapper function for ...
#'
#' It executes these functions:
#' \enumerate{
#'   \item \code{\link{b.b.createTrainTestDataIndexes}}
#'   \item \code{\link{b.c.step2}}
#' }
#'
#' @author Vitali Friesen
b.a.preprocessing.start <- function(){
  # Explanation
  b.b.createTrainTestDataIndexes()

  # Explanation
  b.c.step2()
}

#' @title Preprocessing - Create Train Test DataSet Indexes
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Vitali Friesen
b.b.createTrainTestDataIndexes <- function(){
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

  bin <- sapply(blockNum:1, blockCreator, bucketNum=blockNum, randList=imgIndexRand)
  # retrieve the variable again
  #mget("train1",envir=blocks)
}

#' @title Feature Extraction - Step 2
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author
b.c.step2 <- function(){

}
