
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
  
  #Loop through the train/test-data-sets
  
  
  sapply(1:numSets, function(x){
    # Explanation
    d.b.step1(x)
    
    # Evaluate the result for the train-test-set
    d.c.step2(x)
  }

}

#' @title Classifier 1 - Step 1
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Vitali Friesen, Colin Juers, Tassilo Tobollik
d.b.step1 <- function(){
  set.seed(1337)
  
  #Get or set Train-data and Test-data
  
  start.time.train <- Sys.time()

  #Train the randomForest model on the train data
  parallelRfModel <- train(as.factor(characterVisible) ~ bucketData, 
               data=train,
               method = "rf",
               importance=TRUE,
               #Parameter-Tuning
               ntree=2000)
  
  #Vllt. auslagern
  end.time.train <- Sys.time()
  time.taken.train <- end.time.train - start.time.train
  time.taken.train
  
  #Plot the variable importance of the trained model
  variableImportance <- varImp(parallelRfModel)
  plot.varImp.train(variableImportance)
  
  start.time.pred <- Sys.time()
  
  #Predict the test data on the trained model parallalized
  beginCluster()
    preds_rf <- clusterR(testData, raster::predict, args = list(model = parallelRfModel))
  endCluster()
  
  #Vllt. auslagern
  end.time.pred <- Sys.time()
  time.taken.pred <- end.time.pred - start.time.pred
  time.taken.pred
}

#' @title Classifier 1 - Step 2
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author
d.c.step2 <- function(){
  #Compare prediction of the Test-Data with the real characterVisible column
}
