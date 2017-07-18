#' @title Classifier Support Vector Machine -  Wrapper function
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' This is a wrapper function for classifying data with support vector machines
#'
#' It executes these functions:
#' \enumerate{
#'   \item \code{\link{g.b.step1}}
#'   \item \code{\link{g.c.step2}}
#' }
#' @param block A string that holds a path to a cross-validation block file
#' @param feature1 A string that holds a path to a feature data file (hog or colorHist)
#' @param feature2 A string that holds a path to a second feature data file (the type that was not used in \code{feature1}).This parameter is optional
#' @param saveFile A string that holds a path to which the result should be stored as .rda file.
#' @param classLabel A string that holds a path to a file with classification labels for the feature data
#' @return A table that holds the pairs of correct and not correct predicted images, the number of correct predicted images and the accuracy percentage
#' @example \code{result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_original_8_9_complete.rda",
#' "data/colorHistRGBNorm_original_255buckets_hog_original_8_9_nodes7_rf100_result.rda", "data/classesOrig.rda",
#' 100, feature2 = "data/colorHistOriginalRGBNorm255Buckets.rda",nodesize = 7)}
#' @author  Colin Juers

g.a.svm.start <- function(block, feature1, saveFile, classLabel, feature2 = NULL){
  
  library(e1071)
  set.seed(1337)
  
  options(warn=-1)
  
  remove(colorHist)
  remove(hogData)
  remove(pixelFeatureMatrix28Squared)
  remove(pixelFeatureMatrixEighths)
  remove(classesEights)
  remove(classesOrig)
  
  options(warn=-0)
  
  # load essential files 
  load(block)
  load(classLabel)
  
  load(feature1)
  if(!is.null(feature2)){
    load(feature2)
    colorHist <- cbind(hogData,colorHist)
  }
  
  if(exists("colorHist"))
    data <- colorHist
  else
    if (exists("hogData"))
      data <- hogData
    else
      if (exists("pixelFeatureMatrix28Squared"))
        data <- pixelFeatureMatrix28Squared
      else
        data <- pixelFeatureMatrixEighths
  
  if(exists("classesEights"))
    classes <- classesEights
  else
    classes <- classesOrig
  

  data <- cbind(data, P = classes[,"P"])
  
  #Loop through the train/test-data-sets
  resultData <- sapply(1:blockNum, function(curBlock){
    
    # retrieve the indexes of the corresponding train block
    trainBlockIndexes <- get(paste0("train", curBlock), envir=blocks)
    
    # for calculating the processing time: save start time
    start.time <- Sys.time()
  
    # svm vector creation with train data bucket
    trainData <- data[testBlockIndexes,]
    model_svm <- g.b.step1(trainData)
    
    # print processing time
    print(paste0("Train proctime svm block ", curBlock, ": ",
                 round(difftime(Sys.time(), start.time, tz, units = "secs")), " sec"))
    
    # retrieve the indexes of the corresponding test block
    testBlockIndexes <- get(paste0("test", curBlock), envir=blocks)
    testData <- data[testBlockIndexes,]
    
    # predict test set with previous trained svm
    pred_svm <- g.c.step2(model_svm, testData)
    
    result <- matrix(nrow = length(pred_svm), ncol = 2)
    result[,1] <- as.vector(pred_svm)
    result[,2] <- testData[,ncol(testData)]
    return(result)
  })
  
  overallResult <- do.call(rbind, resultData)
  
  # save(blocks, file = saveFile)

  # evaluate the result of the prediction
  result <- d.d.evaluation(pred=overallResult[,1], testData=overallResult[,2])
  
  return(result)
  
}


#' @title Classifier Support Vector Machine - Step 1
#' @description The train matrix is used to compute hyperplanes of the feature vector. The parameters of the function have
#' been tuned with the \code{tune()} function.
#' To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#' @param trainData A sample matrix of all files to train the svm classifier
#' @return Support Vector Machine model trained with sampled data
#' @author Colin Juers
g.b.step1 <- function(trainData){
  
  # Support vector machine function
  model_svm <- svm(as.factor(P)~.-P, trainData, kernel="radial",tolerance=0.1,cost=100, epsilon=0)
  # Default
  # model_svm <- svm(as.factor(P)~.-P, trainData)
  
  # tune svm to get the best cost for the svm (once used)
  # tune_svm <- tune(svm, as.factor(P)~.-P, data=data.frame(trainData), epsilon=0, tolerance=0.1, ranges=list(cost=c(0.001, 0.01, 0.1, 1, 10, 100)))
  # summary(tune_svm)

  return(model_svm)
  
}



#' @title Classifier Support Vector Machine - Step 2
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#' @param model_svm The trained svm model with all hyperplanes
#' @param testData A sample matrix of all files to apply the trained svm model on and predict "P"
#' @return Vector of predicted lables for the test set
#' @author Colin Juers
g.c.step2 <- function(model_svm, testData){
  
  # predicting the testdata
  pred_svm <- predict(model_svm, testData, type="class")
  
  return(pred_svm)
  
}
