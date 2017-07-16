e.a.svm.start("data/blocks2677IMG.rda", "data/colorHistOriginal255Buckets.rda",
                       "data/hist_255_orig_rf250_result.rda", "data/classesOrig.rda")


#' @title Classifier Support Vector Machine -  Wrapper function
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' This is a wrapper function for ...
#'
#' It executes these functions:
#' \enumerate{
#'   \item \code{\link{e.b.step1}}
#'   \item \code{\link{e.c.step2}}
#' }
#'
#' @author  Colin Juers

e.a.svm.start <- function(a, b1, c, d, b2 = NULL){
  
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
  load(a)
  load(d)
  
  load(b1)
  if(!is.null(b2)){
    load(b2)
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
    model_svm <- e.b.step1(trainData)
    
    # print processing time
    print(paste0("Train proctime svm block ", curBlock, ": ",
                 round(difftime(Sys.time(), start.time, tz, units = "secs")), " sec"))
    
    # retrieve the indexes of the corresponding test block
    testBlockIndexes <- get(paste0("test", curBlock), envir=blocks)
    testData <- data[testBlockIndexes,]
    
    # predict test set with previous trained svm
    pred_svm <- e.c.step2(model_svm, testData)
    
    result <- matrix(nrow = length(pred_svm), ncol = 2)
    result[,1] <- as.vector(pred_svm)
    result[,2] <- testData[,ncol(testData)]
    return(result)
  })
  
  overallResult <- do.call(rbind, resultData)
  
  save(blocks, file = c)

  # evaluate the result of the prediction
  result <- d.d.evaluation(pred=overallResult[,1], testData=overallResult[,2])

  # Save results for the future
  # save(overallResult, file = "data/SVM_results")
  
  return(result)
  
}


#' @title Classifier 1 - Step 1
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Colin Juers
e.b.step1 <- function(trainData){
  
  
  # svm_train <- as.list(as.data.frame(trainData))
  
  # Support vector machine function
  model_svm <- svm(as.factor(P)~.-P, trainData, kernel="polynomial",tolerance=0.1,cost=100, epsilon=0)
  
  # tune svm to get the best cost for the svm (once used)
  # tune_svm <- tune(svm, as.factor(P)~.-P, data=data.frame(trainData), ranges=list(cost=c(0.001,0.01,0.1,1,10,100,1000)))
  # 
  # summary(tune_svm)
  # plot(model_svm, trainData)
  
  return(model_svm)
  
}



#' @title Classifier 1 - Step 1
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Colin Juers
e.c.step2 <- function(model_svm, testData){
  
  # predicting the testdata
  pred_svm <- predict(model_svm, testData, type="class")
  
  return(pred_svm)
  
}


#' #' @title Classifier 1 - Step 1
#' #' @description To get (back) to the overview of all steps and functions use this link:
#' #' \code{\link{a.a.main}}
#' #'
#' #' ...
#' #'
#' #' @author Colin Juers
#' e.d.evaluation <- function(pred_svm, testData){
#'   
#'   library(gridExtra)
#'   library(plotrix)
#'   
#'   result_svm <- table(pred_svm, testData)
#'   
#'   # Give columns and rows names
#'   colnames(result_svm)=c("No person","Person")
#'   rownames(result_svm)=c("No person predicted","Person predicted")
#'   
#'   correct_svm <- result_svm["No person predicted","No person"]+result_svm["Person predicted","Person"]
#'   acc_svm <- (correct_svm)/sum(result_svm)
#'   
#'   # Set theme for grid.plot
#'   t1 <- ttheme_minimal(
#'     core=list(
#'       fg_params=list(col="black", fontface="bold.italic"),
#'       bg_params = list(fill=c(c("green3","red"),c("red","green3")))),
#'     colhead=list(
#'       fg_params=list(col="darkgreen", fontface=4L)),
#'     rowhead=list(
#'       fg_params=list(col="black",fontface=4L))
#'   )
#'   
#'   resultTable <- tableGrob(result_svm, theme=t1)
#'   grid.arrange(resultTable)
#'   
#'   # Pie chart for results with parameters
#'   pieResults <- pie3D(c(acc,Error1Perc,Error2Perc),
#'                       main="Accuracy vs. Errorpercentages",
#'                       col = c("green","red","red"),
#'                       radius = 1.5,
#'                       labels = c("Accuracy","Error 1. degree","Error 2. degree"),
#'                       shade = 0.7,
#'                       explode=0.1)
#'   
#'   acc_svm
#'   
#'   return(acc_svm)
#'   
#' }
#' 
