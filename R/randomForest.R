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
#' @param block A string that holds a path to a cross-validation block file
#' @param feature1 A string that holds a path to a feature data file (hog or colorHist)
#' @param feature2 A string that holds a path to a second feature data file (the type that was not used in \code{feature1}).This parameter is optional
#' @param saveFile A string that holds a path to which the result should be stored as .rda file.
#' @param classLabel A string that holds a path to a file with classification labels for the feature data
#' @param numTrees A number that defines the number of trees randomForest should use (see package randomForest)
#' @param mtry A number that defines the number of variables randomly sampled as candidates at each split of randomForest (see package randomForest). This parameter is optional
#' @param nodesize A number that defines the minimum size of terminal nodes for randomForest (see package randomForest). This parameter is optional
#' @param cutoff A vector of length equal to number of classes that defines the ratio of proportion of votes to cutoff (see package randomForest). This parameter is optional
#' @return A table that holds the pairs of correct and not correct predicted images, the number of correct predicted images and the accuracy percentage
#' @examples \code{result <- d.a.randomForest.start("data/blocks2677IMG.rda", "data/hog_original_8_9_complete.rda",
#' "data/colorHistRGBNorm_original_255buckets_hog_original_8_9_nodes7_rf100_result.rda", "data/classesOrig.rda",
#' 100, feature2 = "data/colorHistOriginalRGBNorm255Buckets.rda",nodesize = 7)}
#' @author Vitali Friesen, Colin Juers, Tassilo Tobollik
d.a.randomForest.start <- function(block, feature1, saveFile, classLabel, numTrees, feature2 = NULL, mtry = NULL, nodesize = NULL, cutoff = NULL){

  options(warn=-1)
  
  remove(colorHist)
  remove(hogData)
  remove(pixelFeatureMatrix28Squared)
  remove(pixelFeatureMatrixEighths)
  remove(classesEights)
  remove(classesOrig)
  
  options(warn=0)

  load(block)
  load(classLabel)
  # load("data/classesEights.rda")
  load(feature1)
  if(!is.null(feature2)){
    load(feature2)
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
    rfModel <- d.b.step1(data[trainBlockIndexes,], numTrees, mtry, nodesize, cutoff)
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
  save(blocks, file = saveFile)

  result <- d.d.evaluation(overallResult[,1], overallResult[,2])
  
  # d.e.plotImportanceColorHist(blocks)
  
  return(result)
}

#' @title Classifier 1 - Step 1
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#' @author Vitali Friesen, Colin Juers, Tassilo Tobollik
#' @param trainData A data frame with number of images as rowsize and number of features as columnsize that holds the feature data
#' @param numTrees A number that defines the number of trees randomForest should use (see package randomForest)
#' @param pMtry A number that defines the number of variables randomly sampled as candidates at each split of randomForest (see package randomForest). This parameter is optional
#' @param pNodesize A number that defines the minimum size of terminal nodes for randomForest (see package randomForest). This parameter is optional
#' @param pCutoff A vector of length equal to number of classes that defines the ratio of proportion of votes to cutoff (see package randomForest). This parameter is optional
#' @return A randomForest model that has been trained on the feature data
#' @examples \code{rfModel <- d.b.step1(featureData, numTrees, mtry, nodesize, cutoff)}
d.b.step1 <- function(trainData, numTrees, pMtry = NULL, pNodesize = NULL, pCutoff = NULL){
  library(randomForest)
  set.seed(1337)

  if(is.null(pCutoff)){
    if(is.null(pMtry) && is.null(pNodesize)){
      rfModel <- randomForest(as.factor(P) ~ . - P,
                              data=trainData,
                              importance=TRUE,
                              #Parameter-Tuning
                              ntree=numTrees)
    }else if(!is.null(pMtry) && !is.null(pNodesize)){
      rfModel <- randomForest(as.factor(P) ~ . - P,
                              data=trainData,
                              importance=TRUE,
                              #Parameter-Tuning
                              ntree=numTrees,
                              mtry = pMtry,
                              nodesize = pNodesize)
    }else if(is.null(pNodesize)){
      rfModel <- randomForest(as.factor(P) ~ . - P,
                              data=trainData,
                              importance=TRUE,
                              #Parameter-Tuning
                              ntree=numTrees,
                              mtry = pMtry)
    }else{
      rfModel <- randomForest(as.factor(P) ~ . - P,
                              data=trainData,
                              importance=TRUE,
                              #Parameter-Tuning
                              ntree=numTrees,
                              nodesize = pNodesize)
    }
  }else{
    if(is.null(pMtry) && is.null(pNodesize)){
      rfModel <- randomForest(as.factor(P) ~ . - P,
                              data=trainData,
                              importance=TRUE,
                              #Parameter-Tuning
                              ntree=numTrees,
                              cutoff = pCutoff)
    }else if(!is.null(pMtry) && !is.null(pNodesize)){
      rfModel <- randomForest(as.factor(P) ~ . - P,
                              data=trainData,
                              importance=TRUE,
                              #Parameter-Tuning
                              ntree=numTrees,
                              mtry = pMtry,
                              nodesize = pNodesize,
                              cutoff = pCutoff)
    }else if(is.null(pNodesize)){
      rfModel <- randomForest(as.factor(P) ~ . - P,
                              data=trainData,
                              importance=TRUE,
                              #Parameter-Tuning
                              ntree=numTrees,
                              mtry = pMtry,
                              cutoff = pCutoff)
    }else{
      rfModel <- randomForest(as.factor(P) ~ . - P,
                              data=trainData,
                              importance=TRUE,
                              #Parameter-Tuning
                              ntree=numTrees,
                              nodesize = pNodesize,
                              cutoff = pCutoff)
    }
  }

  return(rfModel)
}




#' @title Classifier 1 - Step 2
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#' @author Vitali Friesen, Colin Juers, Tassilo Tobollik
#' @param testData A dataframe with number of images as rowsize and number of features as columnsize that holds the feature data which should be predicted
#' @param rfModel A randomForest classification model
#' @return A vector that holds the prediction for \code{testData} with length equal to the number of images in the dataset
#' @examples \code{pred <- d.c.step2(testData, rfModel)}
d.c.step2 <- function(testData, rfModel){

  pred <- predict(rfModel, testData)
  
  return(pred)
}

#' @title Classifier 1 - Evaluation
#' @description To get (back) to the overview of all steps and functions use this link:
#' \code{\link{a.a.main}}
#'
#' ...
#'
#' @author Colin Juers, Tassilo Tobollik
d.d.evaluation <- function(pred, testData){

  library(gridExtra)
  library(plotrix)
  
  # Compute result table
  result <- table(pred, testData)
  if(nrow(result) == 1)
    if (row.names(result) == "0")
      result = rbind(result, c(0,0))
    else
      result = rbind(c(0,0), result)
  if (ncol(result) == 1)
    if (colnames(result) == "0")
      result = cbind(result, c(0,0))
    else 
      result = cbind(c(0,0), result)
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
  correct_person <- result["Person predicted", "Person"]
  correct_no_person <- result["No person predicted", "No person"]
  correct <- correct_person+correct_no_person
  acc <- (correct)/sum(result)
  
  # Calculate error 1. and 2. degree (percentage)
  Error1 <- result["Person predicted","No person"]
  Error1Perc <- Error1/sum(result)
  Error2 <- result["No person predicted","Person"]
  Error2Perc <- Error2/sum(result)
  correct_person_Perc <- correct_person/sum(result)
  correct_no_person_Perc <- correct_no_person/sum(result)
  
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
    pieData <- c(correct_person_Perc,correct_no_person_Perc,Error1Perc,Error2Perc)
    pieCol <- c("green","green3","red3","red")
    pieLabels <- c("Person correct","No person correct","Error 1. degree","Error 2. degree")
  }else if(Error1Perc == 0 && Error2Perc == 0){
    pieData <- c(correct_person_Perc, correct_no_person_Perc)
    pieCol <- c("green", "green3")
    pieLabels <- c("Person correct","No person correct")
  }else if(Error1Perc == 0){
    pieData <- c(correct_person_Perc, correct_no_person_Perc, Error2Perc)
    pieCol <- c("green","green3","red")
    pieLabels <- c("Person correct","No person correct","Error 2. degree")
  }else if(Error2Perc == 0){
    pieData <- c(correct_person_Perc, correct_no_person_Perc, Error1Perc)
    pieCol <- c("green", "green3","red3")
    pieLabels <- c("Person correct", "No person correct","Error 1. degree")
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
