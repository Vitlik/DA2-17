d.a.randomForest.start("data/blocks2677IMG.rda", "data/colorHistOriginal255Buckets.rda",
                       "data/hist_255_orig_rf100_result.rda", "data/classesOrig.rda",
                       100)
d.a.randomForest.start("data/blocks2677IMG.rda", "data/colorHistOriginalEqual16Buckets.rda",
                       "data/colorHistOriginalEqual16BucketsRFModelResult.rda", "data/classesOrig.rda",
                       2000)
d.a.randomForest.start("data/blocks2677IMG.rda", "data/colorHistOriginalRGBNorm16Buckets.rda",
                       "data/hist_16_orig_rgbNorm_rf100_result.rda", "data/classesOrig.rda",
                       100)
d.a.randomForest.start("data/blocks2677IMG.rda", "data/colorHistOriginalEqualRGBNorm16Buckets.rda",
                       "data/colorHistOriginalEqualRGBNorm16BucketsRFModelResult.rda", "data/classesOrig.rda",
                       2000)

d.a.randomForest.start("data/blocks2677IMG.rda", "data/colorHistEighth16Buckets.rda",
                       "data/colorHistEighth16BucketsRFModelResult.rda", "data/classesEights.rda",
                       2000)

d.a.randomForest.start("data/blocks2677IMG.rda", "data/colorHistEighth255Buckets.rda",
                       "data/colorHistEighth255BucketsRFModelResult.rda", "data/classesEights.rda",
                       2000)

d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_original_4_9_complete.Rda",
                       "data/hog_4_9_orig_rf100_result.rda", "data/classesOrig.rda",
                       100)

d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_eighth_4_9_complete.Rda",
                       "data/hog_4_9_eighth_rf_result.rda", "data/classesEights.rda",
                       2000)

result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/colorHistEighth255Buckets.rda",
                                 "data/colorHistEighth255Buckets_hog_10_6_eighth_rf250_result.rda", "data/classesEights.rda",
                                 250, "data/hog_eighth_10_6_complete.Rda")

result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_original_8_9_complete.Rda",
                                 "data/hog_8_9_original_rf2000_result.rda", "data/classesOrig.rda",
                                 2000)

result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_original_10_6_complete.Rda",
                                 "data/hog_10_6_original_rf2000_result.rda", "data/classesOrig.rda",
                                 2000)

result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/colorHistOriginalRGBNorm255Buckets.rda",
                                 "data/colorHistOriginalRGBNorm255Buckets_rf2000_result.rda", "data/classesOrig.rda",
                                 2000)

result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/colorHistEighthRGBNorm16Buckets.rda",
                                 "data/colorHistEighthRGBNorm16Buckets_rf2000_result.rda", "data/classesEights.rda",
                                 2000)

result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/colorHistEighthRGBNorm255Buckets.rda",
                                 "data/colorHistEighthRGBNorm255Buckets_rf2000_result.rda", "data/classesEights.rda",
                                 2000)

#####

gc()
result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_eighth_8_9_complete.rda",
                                 "data/hog_eighth_8_9_rf2000_result.rda", "data/classesEights.rda",
                                 2000)
save(result,file="data/result1")
gc()

result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_eighth_10_6_complete.rda",
                                 "data/hog_eighth_10_6_rf2000_result.rda", "data/classesEights.rda",
                                 2000)
save(result,file="data/result2")
gc()

result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_eighth_8_9_complete.rda",
                                 "data/hog_eighth_8_9_rf1000_result.rda", "data/classesEights.rda",
                                 1000)
save(result,file="data/result3")
gc()

result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_eighth_10_6_complete.rda",
                                 "data/hog_eighth_10_6_rf1000_result.rda", "data/classesEights.rda",
                                 1000)
save(result,file="data/result4")
gc()

result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_eighth_5_6_complete.rda",
                                 "data/hog_eighth_5_6_rf1000_result.rda", "data/classesEights.rda",
                                 1000)
save(result,file="data/result5")
gc()

result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_original_10_6_complete.rda",
                                 "data/hog_original_10_6_rf1000_result.rda", "data/classesOrig.rda",
                                 1000)
save(result,file="data/result6")

gc()
result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_original_8_9_complete.rda",
                                 "data/hog_original_8_9_rf1000_result.rda", "data/classesOrig.rda",
                                 1000)
save(result,file="data/result7")

gc()
result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_original_5_6_complete.rda",
                                 "data/hog_original_5_6_rf1000_result.rda", "data/classesOrig.rda",
                                 1000)
save(result,file="data/result8")

gc()
result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/colorHistOriginal255Buckets.rda",
                                 "data/colorHist_original_255buckets_hog_original_10_6_rf500_result.rda", "data/classesOrig.rda",
                                 500, "data/hog_original_10_6_complete.rda")
save(result,file="data/result9")

gc()
result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/colorHistOriginalRGBNorm255Buckets.rda",
                                 "data/colorHistRGBNorm_original_255buckets_hog_original_10_6_rf500_result.rda", "data/classesOrig.rda",
                                 500, "data/hog_original_10_6_complete.rda")
save(result,file="data/result10")

gc()
result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_original_12_9_complete.rda",
                                 "data/hog_original_12_9_rf500_result.rda", "data/classesOrig.rda",
                                 500)
save(result,file="data/result10")

#####

gc()
result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_original_8_9_complete.rda",
                                 "data/hog_original_8_9_rf2000_result.rda", "data/classesOrig.rda",
                                 2000, mtry = 29)
save(result,file="data/result10")

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
d.a.randomForest.start <- function(a, b1, c, d, numTrees, b2 = NULL){
  #library(snow)
  #library(caret)
  
  options(warn=-1)
  
  remove(colorHist)
  remove(hogData)
  remove(pixelFeatureMatrix28Squared)
  remove(pixelFeatureMatrixEighths)
  remove(classesEights)
  remove(classesOrig)
  
  options(warn=0)

  load(a)
  load(d)
  # load("data/classesEights.rda")
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
    # Explanation
    rfModel <- d.b.step1(data[trainBlockIndexes,], numTrees)
    # store model for later evaluation
    assign(paste0("rfModel", curBlock), rfModel, envir = blocks)
    # print processing time
    print(paste0("Train proctime rf block ", curBlock, ": ",
                 round(difftime(Sys.time(), start.time, tz, units = "secs")), " sec"))


    # retrieve the indexes of the corresponding test block
    testBlockIndexes <- get(paste0("test", curBlock), envir=blocks)

    # for calculating the processing time: save start time
    start.time <- Sys.time()
    testData <- data[testBlockIndexes,]
    # Evaluate the result for the train-test-set
    pred <- d.c.step2(testData, rfModel)
    # store prediction for later evaluation
    assign(paste0("pred", curBlock), pred, envir = blocks)
    # print processing time
    print(paste0("Test  proctime rf block ", curBlock, ": ",
                 round(difftime(Sys.time(), start.time, tz, units = "secs")), " sec"))
    #As.vector is needed here because factors change their values in a matrix or data.frame (0 to 1, 1 to 2)
    #And a matrix is needed instead of a data.frame so that sapply dosn't change pred's type back to factor
    result <- matrix(nrow = length(pred), ncol = 2)
    result[,1] <- as.vector(pred)
    result[,2] <- testData[,ncol(testData)]
    # store prediction & real classes matrix for later evaluation
    assign(paste0("result", curBlock), result, envir = blocks)
    return(result)
  })
  
  overallResult <- do.call(rbind, resultData)
  save(blocks, file = c)

  result <- d.d.evaluation(overallResult[,1], overallResult[,2])
  
  # d.e.plotImportanceColorHist(blocks)
  
  return(result)
}

#' @title Classifier 1 - Step 1
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Vitali Friesen, Colin Juers, Tassilo Tobollik
d.b.step1 <- function(trainData, numTrees){
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
                          ntree=numTrees)

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
  if(nrow(result) == 1)
    result = rbind(result, c(0,0))
  if (ncol(result) == 1)
    result = cbind(result, c(0,0))
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
  
  if(Error1Perc != 0 && Error2Perc != 0){
    pieData <- c(acc,Error1Perc,Error2Perc)
    pieCol <- c("green","red","red")
    pieLabels <- c("Accuracy","Error 1. degree","Error 2. degree")
  }else if(Error1Perc == 0 && Error2Perc == 0){
    pieData <- c(acc)
    pieCol <- c("green")
    pieLabels <- c("Accuracy")
  }else if(Error1Perc == 0){
    pieData <- c(acc,Error2Perc)
    pieCol <- c("green","red")
    pieLabels <- c("Accuracy","Error 2. degree")
  }else if(Error2Perc == 0){
    pieData <- c(acc,Error1Perc)
    pieCol <- c("green","red")
    pieLabels <- c("Accuracy","Error 1. degree")
  }
  
  # Pie chart for results with parameters
  pieResults <- pie3D(pieData,
                      main="Accuracy vs. Errorpercentages",
                      col = pieCol,
                      radius = 1.5,
                      labels = pieLabels,
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
