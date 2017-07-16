#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' This is a wrapper function for ...
#'
#' It executes these functions:
#' \enumerate{
#'   \item \code{\link{}}
#'   \item \code{\link{}}
#' }
#'
#' @author Maren Reuter, Nils Meckmann, Sascha

load("data/pixelFeatureMatrix28Squared.rda")
load("data/classesEights.rda")
load("data/blocks2677IMG.rda")

data1 <- cbind(pixelFeatureMatrix28Squared, classesEights)

cnn.execute(rounds = 20, lr = 0.00000001, batch = 50)

cnn.execute <- function(rounds, lr, batch){
  library(mxnet)
  

  
  resultData1 <- sapply(1:blockNum, function(curBlock){
  
    # retrieve the indexes of the corresponding train block
    trainBlockIndexes <- get(paste0("train", curBlock), envir=blocks)
    
    # for calculating the processing time: save start time
    start.time <- Sys.time()
    
    # Explanation
    trainData <- data1[trainBlockIndexes,]
    train_x <- trainData[,-ncol(trainData)]
    train_y <- trainData[,ncol(trainData)]
    train.x <- t(train_x)
    train_array <- train.x
    dim(train_array) <- c(28, 28, 1, ncol(train.x))
  
    CNNmodel <- cnn.b.step1(train_array, train_y, rounds, lr, batch)
    ##assign(paste0("CNNModel", curBlock), CNNModel, envir = CNNModels)
    # print processing time
    print(paste0("Processing time for training the CNN block ", curBlock, ": ",
                 (Sys.time() - start.time)))
    
    # retrieve the indexes of the corresponding test block
    testBlockIndexes <- get(paste0("test", curBlock), envir=blocks)
    
    # for calculating the processing time: save start time
    start.time <- Sys.time()
    testData <- data1[testBlockIndexes,]
    test_x <- data.matrix(testData[,-ncol(testData)])
    test_y <- testData[,ncol(testData)]
  
    test.x <- t(test_x)
    test_array <- test.x
    dim(test_array) <- c(28, 28, 1, ncol(test.x))
    
    # Evaluate the result for the train-test-set
    preds <- cnn.c.step2(CNNmodel, test_array)
    
    colnames(preds) <- rownames(test_x)
    rownames(preds) <- c(0,1)
    
    preds <- t(preds)
    
    assign(paste0("predsWithProbs", curBlock), preds, envir = blocks)
    
    predsValues <- as.numeric(colnames(preds)[max.col(preds, ties.method = 'first')])
    # print processing time
    print(paste0("Processing time for testing the CNN block ", curBlock, ": ",
                 (Sys.time() - start.time)))
  
    # print(pred.label <- max.col(t(preds)) - 1)
    # print(table(pred.label))
    # 
    # print(table(test_y, pred.label))
    # print(sum(diag(table(test_y, pred.label)))/530)
    
    result <- matrix(nrow = length(predsValues), ncol = 2)
    result[,1] <- as.vector(predsValues)
    result[,2] <- as.vector(test_y)
    return(result)
  })

  overallResult <- do.call(rbind, resultData1)

  assign(paste0("overallResult"), overallResult, envir = blocks)

  fileName = paste("pixelFeatureMatrix28Squared_classesEighths", rounds, lr, batch, sep="_")
  #fileName = paste("pixelFeatureMatrix28Squared_classesEights_rounds20_lr0_00000001_batch50")
  filePath = paste("data/", fileName, ".rda")

  save(blocks, file = filePath)
  # save(blocks, file = "data/pixelFeatureMatrix28Squared, classesEights_rounds20_lr0_00000001_batch50.rda")

  d.d.evaluation(overallResult[,1], overallResult[,2])
}


cnn.b.step1 <- function(train_array, train_y, rounds, lr, batch){

  mx.set.seed(100)
  
  device <- mx.cpu()
  CNNmodel <- mx.model.FeedForward.create(NN_model, X = train_array, y = train_y,
                                       ctx = device,
                                       num.round = rounds,
                                       array.batch.size = batch,
                                       learning.rate = lr,
                                       momentum = 0.9,
                                       wd = 0.00001,
                                       eval.metric = mx.metric.accuracy,
                                       epoch.end.callback = mx.callback.log.train.metric(100)
  )
  return(CNNmodel)
}

cnn.c.step2 <- function(CNNmodel, test_array){
  
  preds <- predict(CNNmodel, test_array)
  
  return(preds)
}

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
# Outout of initial CNN Model
NN_model <- mx.symbol.SoftmaxOutput(data = fcl_2)

  