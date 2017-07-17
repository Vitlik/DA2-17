setwd("C:/dev/DA2/DA2-17/")

load("data/classesOrig.rda")
load("data/hog_original_15_6_complete.rda")

inputData <- cbind(hogData, P = classesOrig[, "P"])
hiddenNodes <- rep(10,100)
e.a.ann.start(inputData, dataType = "hog_15_6", imageType = "orig", rounds = "50", lr = "0.00001", nodes = hiddenNodes, nodeString = "10x100", batch = "100")

#' @title Artificial Neural Network -  Wrapper function
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' This is a wrapper function for ...
#'
#' It executes these functions:
#' \enumerate{
#'   \item \code{\link{e.a.ann.step1}}
#' }
#'
#' @author Nils Meckmann, Maren Reuter,
e.a.ann.start <- function(inputData, dataType, imageType, rounds, lr, nodes, nodeString, batch){
  load("data/blocks2677IMG.rda")
  ##CNNModels <- new.env()
  resultData1 <- sapply(1:blockNum, function(curBlock){
    library(mxnet)
    
    # retrieve the indexes of the corresponding train block
    trainBlockIndexes <- get(paste0("train", curBlock), envir=blocks)
    
    # for calculating the processing time: save start time
    start.time <- Sys.time()
    
    # Explanation
    trainData <- inputData[trainBlockIndexes,]
    # trainData <- data1
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
    testData <- inputData[testBlockIndexes,]
    
    # Extract test data
    test_x <- data.matrix(testData[,-ncol(testData)])
    test_y <- testData[,ncol(testData)]
    
    # Evaluate the result for the train-test-set
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
#' @author Nils Meckmann, Maren Reuter
e.b.ann.step1 <- function(train_x, train_y, rounds, lr, nodes, batch){
  mx.set.seed(1)
  
  # Model
  ANNModel <- mx.mlp(train_x, train_y, hidden_node=nodes, out_node=2, out_activation="softmax",
                 num.round=rounds, array.batch.size=batch, learning.rate=lr, momentum=0.9,
                 eval.metric=mx.metric.accuracy)
  return(ANNModel)
}
