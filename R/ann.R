#' @title Artifical Neural Network - Wrapper function
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' This is a wrapper function for an artifical neural network
#'
#' It executes these functions:
#' \enumerate{
#'   \item \code{\link{e.b.ann.step1}}
#' }
#'
#' @param blocks A string that holds a path to a cross-validation block file
#' @param feature1 A string that holds a path to a feature data file (hog, colorHist or pixelFeature28)
#' @param feature2 A string that holds a path to a second feature data file (the type that was not used in \code{feature1}).This parameter is optional
#' @param classification A string that holds a path to which the result should be stored as .rda file.
#' @param dataType er that defines the number of trees randomForest should use (see package randomForest)
#' @param imageType A number that defines the number of variables randomly sampled as candidates at each split of randomForest (see package randomForest). This parameter is optional
#' @param rounds A number that defines the minimum size of terminal nodes for randomForest (see package randomForest). This parameter is optional
#' @param lr A vector of length equal to number of classes that defines the ratio of proportion of votes to cutoff (see package randomForest). This parameter is optional
#' @param nodes A vector containing node amounts per layer, e.g. rep(1000, 2) for two layers with 1000 nodes.
#' @param nodeString String representation of nodes that are used
#' @param batch The amount of objects which the model is trained on every round
#' @return A table that holds the pairs of correct and not correct predicted images, the number of correct predicted images and the accuracy percentage
#' @examples 
#' \code{result <- e.a.ann.start(blocks = "data/blocks2677IMG.rda", feature1 = "data/hog_original_15_6_complete.rda", classification = "data/classesOrig.rda", 
#'   dataType = "hog_15_6", imageType = "orig", rounds = "50", lr = "0.00001",
#'   nodes = rep(10,100), nodeString = "10x100", batch = "100")}
#' @author Nils Meckmann, Maren Reuter, Sascha di Bernardo
e.a.ann.start <- function(blocks, feature1, feature2 = NULL, classification, dataType, imageType, rounds, lr, nodes, nodeString, batch){
  
  options(warn=-1)
  
  remove(colorHist)
  remove(hogData)
  remove(pixelFeatureMatrix28Squared)
  remove(pixelFeatureMatrixEighths)
  remove(classesEights)
  remove(classesOrig)
  
  options(warn=0)
  
  load(blocks)
  load(feature1)
  # load("data/classesEights.rda")
  load(classification)
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
  
  load("data/blocks2677IMG.rda")
  ##CNNModels <- new.env()
  resultData1 <- sapply(1:blockNum, function(curBlock){
    library(mxnet)
    
    # retrieve the indexes of the corresponding train block
    trainBlockIndexes <- get(paste0("train", curBlock), envir=blocks)
    
    # for calculating the processing time: save start time
    start.time <- Sys.time()
    
    # Extract training data
    trainData <- data[trainBlockIndexes,]
    train_x <- data.matrix(trainData[,-ncol(trainData)])
    train_y <- trainData[,ncol(trainData)]
    
    ANNModel <- e.b.ann.step1(train_x, train_y, as.numeric(rounds), as.numeric(lr), nodes, as.numeric(batch))
    
    assign(paste0("annModel", curBlock), ANNModel, envir = blocks)
    
    # print processing time
    print(paste0("Train proctime ANN block ", curBlock, ": ",
                 round(difftime(Sys.time(), start.time, tz, units = "secs")), " sec"))
    
    # retrieve the indexes of the corresponding test block
    testBlockIndexes <- get(paste0("test", curBlock), envir=blocks)
    
    # for calculating the processing time: save start time
    start.time <- Sys.time()
    
    # Extract test data
    testData <- data[testBlockIndexes,]
    test_x <- data.matrix(testData[,-ncol(testData)])
    test_y <- testData[,ncol(testData)]
    
    # Predictions for the test set
    preds <- predict(ANNModel, test_x)
    
    colnames(preds) <- rownames(test_x)
    rownames(preds) <- c(0,1)
    preds <- t(preds)

    assign(paste0("predsWithProbs", curBlock), preds, envir = blocks)
    
    predsValues <- as.numeric(colnames(preds)[max.col(preds, ties.method = 'first')])
    
    # print processing time
    print(paste0("Test  proctime rf block ", curBlock, ": ",
                 round(difftime(Sys.time(), start.time, tz, units = "secs")), " sec"))
    
    result <- matrix(nrow = length(predsValues), ncol = 2)
    result[,1] <- as.vector(predsValues)
    result[,2] <- as.vector(test_y)
    return(result)
  })
  
  overallResult <- do.call(rbind, resultData1)
  
  assign(paste0("overallResult"), overallResult, envir = blocks)
  
  fileName = paste(dataType, imageType, rounds, lr, nodeString, batch, sep="_")
  filePath = paste("data/", fileName, ".rda")
  
  save(blocks, file = filePath)
  #save(blocks, file = "data/hog_5_6_eighth_rounds30_lr0_0007_nodes1500_batch50.rda")
  
  evaluation <- d.d.evaluation(overallResult[,1], overallResult[,2])
  return(evaluation)
}

#' @title Artificial Neural Network - Model generation
#' @description Creates the ANN Model from the library "mxnet".
#' To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#' @param train_x A data matrix with features to train on, rows being objects and columns being features
#' @param train_y A vector containing all classifications for the given objects
#' @param rounds A numeric value that is the amount of rounds the model is trained
#' @param lr The learning rate
#' @param nodes A vector containing node amounts per layer, e.g. rep(1000, 2) for two layers with 1000 nodes.
#' @param batch The amount of objects which the model is trained on every round
#' @return ANN model of the package mxnet
#' @author Nils Meckmann, Maren Reuter, Sascha di Bernardo
e.b.ann.step1 <- function(train_x, train_y, rounds, lr, nodes, batch){
  mx.set.seed(1)
  
  # Model
  ANNModel <- mx.mlp(train_x, train_y, hidden_node=nodes, out_node=2, out_activation="softmax",
                 num.round=rounds, array.batch.size=batch, learning.rate=lr, momentum=0.9,
                 eval.metric=mx.metric.accuracy)
  return(ANNModel)
}
