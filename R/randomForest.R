
#' @title Classifier 1 -  Wrapper function
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' This is a wrapper function for ...
#'
#' It executes these functions:
#' \enumerate{
#'   \item \code{\link{d.b.step1}}
#'   \item \code{\link{d.c.step2}}
#' }
#'
#' @author Vitali Friesen, Colin Juers, Tassilo Tobollik
d.a.randomForest.start <- function(){
  library(raster)
  library(snow)
  library(caret)

  load("data/clasAll.rda")
  load("data/colorHistOriginal.rda")
  data <- cbind(colorHistOriginal, clasAll[,3])

  #Loop through the train/test-data-sets
  sapply(1:blockNum, function(curBlock){
    # retrieve the indexes of the corresponding train block
    trainBlockIndexes <- mget(paste0("train", curBlock), envir=blocks)

    # for calculating the processing time: save start time
    start.time <- Sys.time()
    # Explanation
    d.b.step1(data[trainBlockIndexes,])
    # print processing time
    print(paste0("Processing time for training the random forest block ", curBlock, ": ",
                 (Sys.time() - start.time)))


    # retrieve the indexes of the corresponding test block
    testBlockIndexes <- mget(paste0("test", curBlock), envir=blocks)

    # for calculating the processing time: save start time
    start.time <- Sys.time()
    # Evaluate the result for the train-test-set
    d.c.step2(data[testBlockIndexes,])
    # print processing time
    print(paste0("Processing time for testing the random forest block ", curBlock, ": ",
                 (Sys.time() - start.time)))
  })

}

#' @title Classifier 1 - Step 1
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Vitali Friesen, Colin Juers, Tassilo Tobollik
d.b.step1 <- function(trainData){
  set.seed(1337)


  #Train the randomForest model on the train data
  parallelRfModel <- train(as.factor(characterVisible) ~ bucketData,
               data=trainData,
               method = "rf",
               importance=TRUE,
               #Parameter-Tuning
               ntree=2000)

  #Plot the variable importance of the trained model
  variableImportance <- varImp(parallelRfModel)
  plot.varImp.train(variableImportance)
}

#' @title Classifier 1 - Step 2
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Vitali Friesen, Colin Juers, Tassilo Tobollik
d.c.step2 <- function(testData){
  #Compare prediction of the Test-Data with the real characterVisible column


  #Predict the test data on the trained model parallalized
  beginCluster()
  preds_rf <- clusterR(testData, raster::predict, args = list(model = parallelRfModel))
  endCluster()
}
