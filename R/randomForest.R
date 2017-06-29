
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
  library(snow)
  library(caret)

  load("data/clasAll.rda")
  load("data/colorHistOriginal.rda")
  
  data <- cbind(colorHistOriginal, clasAll[,3])
  
  colnames(data)[dim(data)[2]] <- "P"

  #Loop through the train/test-data-sets
  sapply(1:blockNum, function(curBlock){
    # retrieve the indexes of the corresponding train block
    trainBlockIndexes <- get(paste0("train", curBlock), envir=blocks)

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

  # TODO exchange bucketData with all other column of trainData
  #Train the randomForest model on the train data
  #-P?
  parallelRfModel <- train(as.factor(P) ~ . - P, # maybe trainData[,-(ncol(trainData))]
               data=trainData,
               method = "rf",
               importance=TRUE,
               #Parameter-Tuning
               ntree=2000)
  
  #TODO (all): Run with diff. parameter permutations
  rfModel <- randomForest(as.factor(P) ~ . - P,
                          data=trainData,
                          importance=TRUE,
                          #Parameter-Tuning
                          ntree=2000)

  #Plot the variable importance of the trained model
  variableImportance <- varImp(parallelRfModel)
  #TODO (Vit): VarImpPlot schöner/farbig machen (ggplot?)
  variableImportance2 <- varImp(rfModel)
  plot(variableImportance)
  plot(variableImportance2)
  varImpPlot(rfModel)
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
  #beginCluster()
  #preds_rf <- clusterR(parallelRfModel, predict,#testData[,-(ncol(testData))]
                       #args = list(newdata = testData))
  #endCluster()
  #TODO (Tac): Research possible parallelization possibilities
  #TODO:  Einmal ganze Klasse über alle Bilder laufen lassen und auswerten
  pred <- predict(rfModel, testData)
  
  #TODO (Colin): Evaluation in andere Methode und schön graf. Darstellen mit wichtigen Kennzahlen (Fehler 1., 2. Art und Accuracy)
  #TODO (Vit): Research how to evaluate the overall accuracy of all test-trees together
  table(pred, testData[,ncol(testData)])
}

d.d.evaluation <- function(){}
