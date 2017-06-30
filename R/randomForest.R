
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

  load("data/blocks.rda")
  load("data/blockNum.rda")
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

  #Train the randomForest model on the train data
  # #-P?
  # parallelRfModel <- train(as.factor(P) ~ . - P, # maybe trainData[,-(ncol(trainData))]
  #              data=trainData,
  #              method = "rf",
  #              importance=TRUE,
  #              #Parameter-Tuning
  #              ntree=2000)
  
  #TODO (all): Run with diff. parameter permutations
  trainData
  rfModel <- randomForest(as.factor(P) ~ . - P,
                          data=trainData,
                          importance=TRUE,
                          #Parameter-Tuning
                          ntree=2000)

  #Plot the variable importance of the trained model
  # variableImportance <- varImp(parallelRfModel)
  #TODO (Vit): VarImpPlot sch?ner/farbig machen (ggplot?)
  # plot(variableImportance)
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
  #TODO:  Einmal ganze Klasse ?ber alle Bilder laufen lassen und auswerten
  pred <- predict(rfModel, testData)
  
  #TODO (Vit): Research how to evaluate the overall accuracy of all test-trees together
}




d.d.evaluation <- function(pred){
  #TODO (Colin): Evaluation in andere Methode und sch?n graf. Darstellen mit wichtigen Kennzahlen (Fehler 1., 2. Art und Accuracy)
  
  # Compute results
  
  # dummy <- seq(1,1,length.out=268)
  # result <- table(dummy, testData[,ncol(testData)])
  result <- table(pred, testData[,ncol(testData)])
  colnames(result)=c("No person","Person")
  rownames(result)=c("No person predicted","Person predicted")
  
  
  acc <- (result["0","0"]+result["1","1"])/sum(result)
  
  Error1 <- result["Person predicted","No person"]
  Error2 <- result["No person predicted","Person"]
  
}
