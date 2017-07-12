
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

e.a.svm.start <- function(){
  
  load("data/blocks.rda")
  load("data/blockNum.rda")
  load("data/classesOrig.rda")
  load("data/classesEights.rda")
  load("data/colorHistOriginal.rda")
  
  library(e1071)
  set.seed(1337)
  
  e.b.step1(trainData)
  
  e.c.step2(model_svm, testData)
  
  e.d.evaluation(pred_svm, testData)

  
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
  model_svm <- svm(as.factor(P)~., trainData, cost=100)
  
  # tune svm to get the best cost for the svm (once used)
  # tune_svm <- tune(svm, as.factor(P)~., data=data.frame(trainData), ranges=list(cost=c(0.001,0.01,.1,1,10,100)))
  
  summary(tune_svm)
  # plot(model_svm, trainData)
  
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
  
}


#' @title Classifier 1 - Step 1
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Colin Juers
e.d.evaluation <- function(pred_svm, testData){
  
  library(gridExtra)
  library(plotrix)
  
  result_svm <- table(pred_svm, testData[,ncol(testData)])
  
  # Give columns and rows names
  colnames(result_svm)=c("No person","Person")
  rownames(result_svm)=c("No person predicted","Person predicted")
  
  correct_svm <- result_svm["No person predicted","No person"]+result_svm["Person predicted","Person"]
  acc_svm <- (correct_svm)/sum(result_svm)
  
  # Set theme for grid.plot
  t1 <- ttheme_minimal(
    core=list(
      fg_params=list(col="black", fontface="bold.italic"),
      bg_params = list(fill=c(c("green3","red"),c("red","green3")))),
    colhead=list(
      fg_params=list(col="darkgreen", fontface=4L)),
    rowhead=list(
      fg_params=list(col="black",fontface=4L))
  )
  
  resultTable <- tableGrob(result_svm, theme=t1)
  grid.arrange(resultTable)
  
  # Pie chart for results with parameters
  pieResults <- pie3D(c(acc,Error1Perc,Error2Perc),
                      main="Accuracy vs. Errorpercentages",
                      col = c("green","red","red"),
                      radius = 1.5,
                      labels = c("Accuracy","Error 1. degree","Error 2. degree"),
                      shade = 0.7,
                      explode=0.1)
  
  acc_svm
  
}
