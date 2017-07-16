setwd("C:/dev/DA2/DA2-17/")


#' @title Classifier -  Wrapper function
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

load("data/blocks2677IMG.rda")
load("data/classesOrig.rda")
load("data/colorHistEighthRGBNorm16Buckets.rda")
load("data/hog_original_15_6_complete.rda")
load("data/colorHistEighth255Buckets.rda")
load("data/hog_eighth__complete.rda")
load("data/classesEights.rda")
load("data/hog_eighth_8_9_complete.rda")

data1 <- cbind(hogData, P = classesOrig[, "P"])

ann.execute <- function(dataType, imageType, rounds, lr, nodes, batch){
  ##CNNModels <- new.env()
  resultData1 <- sapply(1:blockNum, function(curBlock){
    
    library(mxnet)
    
    # retrieve the indexes of the corresponding train block
    trainBlockIndexes <- get(paste0("train", curBlock), envir=blocks)
    
    # for calculating the processing time: save start time
    start.time <- Sys.time()
    
    # Explanation
    trainData <- data1[trainBlockIndexes,]
    # trainData <- data1
    train_x <- data.matrix(trainData[,-ncol(trainData)])
    train_y <- trainData[,ncol(trainData)]
    
    ANNModel <- ann.step1(train_x, train_y, rounds, lr, nodes, batch)
    
    assign(paste0("annModel", curBlock), ANNModel, envir = blocks)
    
    ##assign(paste0("CNNModel", curBlock), CNNModel, envir = CNNModels)
    # print processing time
    print(paste0("Processing time for training the ANN block ", curBlock, ": ",
                 (Sys.time() - start.time)))
    
    # retrieve the indexes of the corresponding test block
    testBlockIndexes <- get(paste0("test", curBlock), envir=blocks)
    
    # for calculating the processing time: save start time
    start.time <- Sys.time()
    testData <- data1[testBlockIndexes,]
    
    test_x <- data.matrix(testData[,-ncol(testData)])
    test_y <- testData[,ncol(testData)]
    
    # Evaluate the result for the train-test-set
    preds <- predict(ANNModel, test_x)
    
    colnames(preds) <- rownames(test_x)
    rownames(preds) <- c(0,1)
    #View(tPreds)
    
    preds <- t(preds)
    #View(tPreds2)
    
    assign(paste0("predsWithProbs", curBlock), preds, envir = blocks)
    
    predsValues <- as.numeric(colnames(preds)[max.col(preds, ties.method = 'first')])
    
    #preds <- ann.step2(test.x, ANNModel)
    # print processing time
    print(paste0("Processing time for testing the ANN block ", curBlock, ": ",
                 (Sys.time() - start.time)))
    
    # print(pred.label <- max.col(t(preds)) - 1)
    # print(table(pred.label))
    # 
    # print(table(test_y, pred.label))
    # print(sum(diag(table(test_y, pred.label)))/530)
    
    #result = data.frame(cbind(predsValues, test_y))
    
    result <- matrix(nrow = length(predsValues), ncol = 2)
    result[,1] <- as.vector(predsValues)
    result[,2] <- as.vector(test_y)
    return(result)
  })
  
  overallResult <- do.call(rbind, resultData1)
  
  assign(paste0("overallResult"), overallResult, envir = blocks)
  
  fileName = paste(dataType, imageType, rounds, lr, nodes, batch, sep="_")
  filePath = paste("data/", fileName, ".rda")
  
  save(blocks, file = filePath)
  #save(blocks, file = "data/hog_5_6_eighth_rounds30_lr0_0007_nodes1500_batch50.rda")
  
  d.d.evaluation(overallResult[,1], overallResult[,2])
}

ann.step1 <- function(train_array, train_y, rounds = 50, lr = 0.00001, nodes = 1500, batch = 20){
  library(mxnet)
  mx.set.seed(1)
  
  # Model
  ANNModel <- mx.mlp(train_array, train_y, hidden_node=nodes, out_node=2, out_activation="softmax",
                 num.round=rounds, array.batch.size=batch, learning.rate=lr, momentum=0.9,
                 eval.metric=mx.metric.accuracy)
  return(ANNModel)
}

ann.execute("hog_8_9", "orig", rounds = "30", "0.00001", "1500", "20")


data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=576)
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=288)
act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
fc3 <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=144)
act3 <- mx.symbol.Activation(fc3, name="relu3", act_type="relu")
fc4 <- mx.symbol.FullyConnected(act3, name="fc4", num_hidden=144)
act4 <- mx.symbol.Activation(fc4, name="relu3", act_type="relu")
fc5 <- mx.symbol.FullyConnected(act4, name="fc5", num_hidden=2)
softmax <- mx.symbol.SoftmaxOutput(fc4, name="sm")

devices <- mx.cpu()

mx.set.seed(1234)
model <- mx.model.FeedForward.create(softmax, X = train.x, y = train.y, 
                                     ctx = devices, num.round = 100, 
                                     array.batch.size = 15, learning.rate = 0.007, 
                                     momentum = 0.9, eval.metric = mx.metric.accuracy
)