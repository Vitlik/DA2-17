load("data/pixelFeatureMatrixEighthsSquared.rda")
load("data/classesEights.rda")
load("data/blocks.rda")
load("data/blockNum.rda")


data1 <- cbind(pixelFeatureMatrixEighthsSquared, classesEights)
data1 <- t(data1)

##CNNModels <- new.env()
resultData <- sapply(1:blockNum, function(curBlock){
  # retrieve the indexes of the corresponding train block
  trainBlockIndexes <- get(paste0("train", curBlock), envir=blocks)
  
  # for calculating the processing time: save start time
  start.time <- Sys.time()
  # Explanation
  trainData <- data1[trainBlockIndexes,]
  CNNModel <- cnn.b.step1(trainData)
  ##assign(paste0("CNNModel", curBlock), CNNModel, envir = CNNModels)
  # print processing time
  print(paste0("Processing time for training the random forest block ", curBlock, ": ",
               (Sys.time() - start.time)))
  
  # retrieve the indexes of the corresponding test block
  testBlockIndexes <- get(paste0("test", curBlock), envir=blocks)
  
  # for calculating the processing time: save start time
  start.time <- Sys.time()
  testData <- data1[testBlockIndexes,]
  # Evaluate the result for the train-test-set
  preds <- cnn.c.step2(testData, CNNModel)
  # print processing time
  print(paste0("Processing time for testing the random forest block ", curBlock, ": ",
               (Sys.time() - start.time)))

  # pred.label <- max.col(t(preds)) - 1
  # table(pred.label)
  # 
  # table(test_classes, pred.label)
  # sum(diag(table(test_classes, pred.label)))/530
  result <- matrix(nrow = length(preds), ncol = 2)
  result[,1] <- as.vector(preds)
  result[,2] <- testData[,ncol(testData)]
  return(result)
})



cnn.b.step1 <- function(trainData){
  library(mxnet)
  # Model
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
  # Output
  NN_model <- mx.symbol.SoftmaxOutput(data = fcl_2)
  
  mx.set.seed(100)
  
  device <- mx.cpu()
  
  train_x <- data1[,-nrow(data1)]
  train_y <- data1[,nrow(data1)]
  
  CNNmodel <- mx.model.FeedForward.create(NN_model, X = train_x, y = train_y,
                                       ctx = device,
                                       num.round = 30,
                                       array.batch.size = 20,
                                       learning.rate = 0.00000001,
                                       momentum = 0.9,
                                       wd = 0.00001,
                                       eval.metric = mx.metric.accuracy,
                                       epoch.end.callback = mx.callback.log.train.metric(100)
  )
  return(CNNmodel)
}

d.c.step2 <- function(testData, CNNModel){
  
  test_x <- testData[,-nrow(testData)]
  test_y <- testData[,nrow(testData)]
  
  preds <- predict(CNNmodel, test_y)
  
  return(preds)
}
  