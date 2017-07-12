
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
  #library(snow)
  #library(caret)

  load("data/blocks2677IMG.rda")
  load("data/classesOrig.rda")
  load("data/classesEights.rda")
  load("data/colorHistOriginal16Buckets.rda")
  
  data <- cbind(colorHist, P = classesOrig[,"P"])

  # env var for all different models
  rfModels <- new.env()
  
  #Loop through the train/test-data-sets
  resultData <- sapply(1:blockNum, function(curBlock){
    # retrieve the indexes of the corresponding train block
    trainBlockIndexes <- get(paste0("train", curBlock), envir=blocks)

    # for calculating the processing time: save start time
    start.time <- Sys.time()
    # Explanation
    rfModel <- d.b.step1(data[trainBlockIndexes,])
    assign(paste0("rfModel", curBlock), rfModel, envir = rfModels)
    # print processing time
    print(paste0("Processing time for training the random forest block ", curBlock, ": ",
                 (Sys.time() - start.time)))


    # retrieve the indexes of the corresponding test block
    testBlockIndexes <- get(paste0("test", curBlock), envir=blocks)

    # for calculating the processing time: save start time
    start.time <- Sys.time()
    testData <- data[testBlockIndexes,]
    # Evaluate the result for the train-test-set
    pred <- d.c.step2(testData, rfModel)
    # print processing time
    print(paste0("Processing time for testing the random forest block ", curBlock, ": ",
                 (Sys.time() - start.time)))
    #As.vector is needed here because factors change their values in a matrix or data.frame (0 to 1, 1 to 2)
    #And a matrix is needed instead of a data.frame so that sapply dosn't change pred's type back to factor
    result <- matrix(nrow = length(pred), ncol = 2)
    result[,1] <- as.vector(pred)
    result[,2] <- testData[,ncol(testData)]
    return(result)
  })
  
  overallResult <- do.call(rbind, resultData)
  save(rfModels, file = "data/rfModels.rda")

  d.d.evaluation(overallResult[,1], overallResult[,2])
  
  d.e.plotImportanceColorHist(rfModels)
}

#' @title Classifier 1 - Step 1
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Vitali Friesen, Colin Juers, Tassilo Tobollik
d.b.step1 <- function(trainData){
  library(randomForest)
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
  rfModel <- randomForest(as.factor(P) ~ . - P,
                          data=trainData,
                          importance=TRUE,
                          #Parameter-Tuning
                          ntree=2000)

  #Plot the variable importance of the trained model
  # variableImportance <- varImp(parallelRfModel)
  #TODO (Vit): VarImpPlot sch?ner/farbig machen (ggplot?)
  # plot(variableImportance)
  # varImpPlot(rfModel)
  return(rfModel)
}




#' @title Classifier 1 - Step 2
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Vitali Friesen, Colin Juers, Tassilo Tobollik
d.c.step2 <- function(testData, rfModel){
  #Compare prediction of the Test-Data with the real characterVisible column


  #Predict the test data on the trained model parallalized
  #beginCluster()
  #preds_rf <- clusterR(parallelRfModel, predict,#testData[,-(ncol(testData))]
                       #args = list(newdata = testData))
  #endCluster()
  #TODO (Tac): Research possible parallelization possibilities
  #TODO:  Einmal ganze Klasse ?ber alle Bilder laufen lassen und auswerten
  pred <- predict(rfModel, testData)
  
  return(pred)
  #TODO (Vit): Research how to evaluate the overall accuracy of all test-trees together
}

#' @title Classifier 1 - Evaluation
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Colin Juers, Tassilo Tobollik
d.d.evaluation <- function(pred, testData){
  #TODO (Colin): Evaluation in andere Methode und sch?n graf. Darstellen mit wichtigen Kennzahlen (Fehler 1., 2. Art und Accuracy)

  library(gridExtra)
  library(plotrix)
  
  # Compute result table
  result <- table(pred, testData)
  # Give columns and rows names
  colnames(result)=c("No person","Person")
  rownames(result)=c("No person predicted","Person predicted")
  
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
  
  resultTable <- tableGrob(result, theme=t1)
  
  # Draw grid for errors and true predictions
  grid.arrange(resultTable)

  # Calculate accuracy
  correct <- result["No person predicted","No person"]+result["Person predicted","Person"]
  acc <- (correct)/sum(result)
  
  # Calculate error 1. and 2. degree (percentage)
  Error1 <- result["Person predicted","No person"]
  Error1Perc <- Error1/sum(result)
  Error2 <- result["No person predicted","Person"]
  Error2Perc <- Error2/sum(result)
  
  # barplot for accuracy vs. error percentage
  # maybe as stacked barplot possible?
  barResults <- barplot(c(acc, Error1Perc, Error2Perc), 
                        main="Accuracy vs. Errorpercentages",
                        col=c("green","red","red"), 
                        horiz=TRUE,
                        xlim = c(0,1),
                        beside=TRUE,
                        names.arg = c("Accuracy","Error 1. degree", "Error 2n degree"))
  
  
  # Put values as text into plot
  text(c(acc-0.1,Error1Perc-0.1,Error2Perc-0.1),c(0.7,1.9,3.1), cex=2,col="black",labels = c(
    paste(round(acc*100,2),"%"), 
    paste(round(Error1Perc*100,2),"%"), 
    paste(round(Error2Perc*100,2),"%")))
  
  # Pie chart for results with parameters
  pieResults <- pie3D(c(acc,Error1Perc,Error2Perc),
    main="Accuracy vs. Errorpercentages",
    col = c("green","red","red"),
    radius = 1.5,
    labels = c("Accuracy","Error 1. degree","Error 2. degree"),
    shade = 0.7,
    explode=0.1)

  
  
  #TODO: Create Plot
  return(rbind(result,c(correct,acc)))
}

#' @title Classifier 1 - Step 1
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Vitali Friesen, Colin Juers, Tassilo Tobollik
d.e.plotImportanceColorHist <- function(rfModels){
  
  load("data/blockNum.rda")
  
  # MeanDecreaseAccuracy
  MeanDecreaseAccuracy <- colMeans(t(sapply(rfModels, function(rfModel){
    rfModel$importance[,"MeanDecreaseAccuracy"]
  })))
  
  ordered <- MeanDecreaseAccuracy[order(-MeanDecreaseAccuracy)]
  barCol <- sapply(1:length(ordered), function(pos){
    tmp <- substr(attributes(ordered)$names[pos], 1, 1)
    ifelse(tmp == "b", "blue", 
           ifelse(tmp == "g", "green", "red"))
  })
  Names <- attributes(ordered)$names
  dfImp <- data.frame(Names, ordered)
  
  ggplot(dfImp, aes(reorder(Names, ordered), ordered)) + 
    geom_bar(stat = "identity", show.legend = F, fill = rev(barCol)) + coord_flip() + 
    xlab("Importance")
  
  ggsave("plots/RandomForestAverageMeanDecreaseAccuracyColorHistogram.png")
  
  # MeanDecreaseGini
  MeanDecreaseGini <- colMeans(t(sapply(rfModels, function(rfModel){
    rfModel$importance[,"MeanDecreaseGini"]
  })))
  
  ordered <- MeanDecreaseGini[order(-MeanDecreaseGini)]
  barCol <- sapply(1:length(ordered), function(pos){
    tmp <- substr(attributes(ordered)$names[pos], 1, 1)
    ifelse(tmp == "b", "blue", 
           ifelse(tmp == "g", "green", "red"))
  })
  Names <- attributes(ordered)$names
  dfImp <- data.frame(Names, ordered)
  
  ggplot(dfImp, aes(reorder(Names, ordered), ordered)) + 
    geom_bar(stat = "identity", show.legend = F, fill = rev(barCol)) + coord_flip() + 
    xlab("Importance")
  
  ggsave("plots/RandomForestAverageMeanDecreaseGiniColorHistogram.png")
}
