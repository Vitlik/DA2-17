f.a.cnn.start(rounds = 20, lr = 0.00000001, batch = 50)
f.a.cnn.start(rounds = 20, lr = 0.00000001, batch = 20)
f.a.cnn.start(rounds = 20, lr = 0.00000001, batch = 100)
f.a.cnn.start(rounds = 30, lr = 0.00000001, batch = 50)
f.a.cnn.start(rounds = 40, lr = 0.00000001, batch = 50)
f.a.cnn.start(rounds = 50, lr = 0.00000001, batch = 50)
f.a.cnn.start(rounds = 50, lr = 0.00000001, batch = 50)
f.a.cnn.start(rounds = 30, lr = 0.000000001, batch = 50)
f.a.cnn.start(rounds = 80, lr = 0.00000001, batch = 50)


#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' This is a wrapper function for ...
#'
#' It executes these functions:
#' \enumerate{
#'   \item \code{\link{f.b.cnn.step1}}
#'   \item \code{\link{f.b.cnn.step2}}
#' }
#'
#' @author Maren Reuter, Nils Meckmann

f.a.cnn.start <- function(rounds, lr, batch){
  library(mxnet)
  
  load("data/pixelFeatureMatrix28Squared.rda")
  load("data/classesEights.rda")
  load("data/blocks2677IMG.rda")
  
  data1 <- cbind(pixelFeatureMatrix28Squared, classesEights)
  
  # Initialize Model
  data <- mx.symbol.Variable('data')
  # 1st convolutional layer 5x5 kernel and 20 filters.
  conv_1 <- mx.symbol.Convolution(data= data, kernel = c(5,5), num_filter = 20)
  tanh_1 <- mx.symbol.Activation(data= conv_1, act_type = "tanh")
  pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2,2), stride = c(2,2))
  # 2nd convolutional layer 5x5 kernel and 50 filters.
  conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5,5), num_filter = 50)
  tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
  pool_2 <- mx.symbol.Pooling(data = tanh_2, pool_type = "max", kernel = c(2,2), stride = c(2,2))
  # 1st fully connected layer
  flat <- mx.symbol.Flatten(data = pool_2)
  fcl_1 <- mx.symbol.FullyConnected(data = flat, num_hidden = 500)
  tanh_3 <- mx.symbol.Activation(data = fcl_1, act_type = "tanh")
  # 2nd fully connected layer
  fcl_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 2)
  # Output of initial CNN Model
  CNN_model <- mx.symbol.SoftmaxOutput(data = fcl_2)
  
  resultData1 <- sapply(1:blockNum, function(curBlock){
  
    # retrieve the indexes of the corresponding train block
    trainBlockIndexes <- get(paste0("train", curBlock), envir=blocks)
    
    # for calculating the processing time: save start time
    start.time <- Sys.time()
    
    # Extract train data
    trainData <- data1[trainBlockIndexes,]
    train_x <- trainData[,-ncol(trainData)]
    train_y <- trainData[,ncol(trainData)]
    train.x <- t(train_x)
    train_array <- train.x
    dim(train_array) <- c(28, 28, 1, ncol(train.x))
  
    # Start training the CNN_Model with the train data set
    CNN_model <- cnn.b.step1(CNN_model, train_array, train_y, rounds, lr, batch)
    
    # print processing time of the training the CNN_Model
    print(paste0("Train processing time CNN_Model of block ", curBlock, ": ",
                 round(difftime(Sys.time(), start.time, tz, units = "secs")), " sec"))
    
    # retrieve the indexes of the corresponding test block
    testBlockIndexes <- get(paste0("test", curBlock), envir=blocks)
    
    # for calculating the processing time: save start time
    start.time <- Sys.time()
    
    # Extract test data
    testData <- data1[testBlockIndexes,]
    test_x <- data.matrix(testData[,-ncol(testData)])
    test_y <- testData[,ncol(testData)]
    test.x <- t(test_x)
    test_array <- test.x
    dim(test_array) <- c(28, 28, 1, ncol(test.x))
    
    # Test trained CNN_Model on data set test_array
    preds <- cnn.c.step2(CNN_model, test_array)
    
    # assign colnames and rownames to the predictions
    colnames(preds) <- rownames(test_x)
    rownames(preds) <- c(0,1)
    
    preds <- t(preds)
    
    assign(paste0("predsWithProbs", curBlock), preds, envir = blocks)
    
    predsValues <- as.numeric(colnames(preds)[max.col(preds, ties.method = 'first')])

    # print processing time
    print(paste0("Test  processing time CNN_Model of block ", curBlock, ": ",
                 round(difftime(Sys.time(), start.time, tz, units = "secs")), " sec"))
    
    #As.vector is needed here because factors change their values in a matrix or data.frame (0 to 1, 1 to 2)
    #And a matrix is needed instead of a data.frame so that sapply does not change pred's type back to factor
    result <- matrix(nrow = length(predsValues), ncol = 2)
    result[,1] <- as.vector(predsValues)
    result[,2] <- as.vector(test_y)
    
    return(result)
  })

  overallResult <- do.call(rbind, resultData1)
  View(overallResult)

  assign(paste0("overallResult"), overallResult, envir = blocks)

  # save the overall test results of the CNN_Model into a rda file
  fileName = paste("pixelFeatureMatrix28Squared_classesEighths", rounds, lr, batch, sep="_")
  filePath = paste("data/",fileName,".rda")
  save(blocks, file = filePath)

  d.d.evaluation(overallResult[,1], overallResult[,2])
}


#' @title Classifier 2 - Step 1
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#' @param CNN_model The initial model that gets trained in this function
#' @param train_array An data array with features to train on, rows being objects and columns being features
#' @param train_y A vector containing all classifications for the given objects
#' @param rounds A numeric value that is the amount of rounds the model is trained
#' @param lr The learning rate
#' @param batch The amount of objects which the model is trained on every round
#' @return CNN model the trained model gets returned
#' @author Maren Reuter, Nils Meckmann
f.b.cnn.step1 <- function(CNN_model, train_array, train_y, rounds, lr, batch){

  mx.set.seed(100)
  
  device <- mx.cpu()
  CNN_model <- mx.model.FeedForward.create(CNN_model, X = train_array, y = train_y,
                                       ctx = device,
                                       num.round = rounds,
                                       array.batch.size = batch,
                                       learning.rate = lr,
                                       momentum = 0.9,
                                       wd = 0.00001,
                                       eval.metric = mx.metric.accuracy,
                                       epoch.end.callback = mx.callback.log.train.metric(100)
  )
  return(CNN_model)
}


#' @title Classifier 2 - Step 2
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#' @param CNN_Model the trained CNN_Model
#' @param test_array An data array with features to test the CNN_Model, rows being objects and columns being features
#' @return preds predictions of the CNN_Modelregarding the test data set
#' @author Maren Reuter, Nils Meckmann
f.c.cnn.step2 <- function(CNN_model, test_array){
  
  # predict the test data on the trained model
  preds <- predict(CNN_model, test_array)
  
  return(preds)
}



  