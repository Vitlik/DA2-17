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

load("data/blocks.rda")
load("data/blockNum.rda")
load("data/colorHistOriginal.rda")
load("data/classesOrig.rda")
load("data/hog_16.rda")

neueMatrix <- cbind(colorHistOriginal, hog16, P = classesOrig[, "P"])

library(mxnet)

train_x2 <- colorHistOriginal[0:2120, ]
test_x2 <- colorHistOriginal[2121:2650, ]

train_y2 <- classesOrig[0:2120, ]
test_y2 <- classesOrig[2121:2650,]
test_x2 <- data.matrix(test_x2)

train.y <- train_y2[,3]
train.x <- t(train_x2)
test.x <- t(test_x2)


data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=144)
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=72)
act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
fc3 <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=36)
act3 <- mx.symbol.Activation(fc3, name="relu3", act_type="relu")
fc4 <- mx.symbol.FullyConnected(act3, name="fc4", num_hidden=2)
softmax <- mx.symbol.SoftmaxOutput(fc4, name="sm")

devices <- mx.cpu()

mx.set.seed(1234)
model <- mx.model.FeedForward.create(softmax, X = train.x, y = train.y, 
                                     ctx = devices, num.round = 100, 
                                     array.batch.size = 20, learning.rate = 0.03, 
                                     momentum = 0.9, eval.metric = mx.metric.rmse
                                    )

mx.set.seed(1234)
model <- mx.model.FeedForward.create(softmax, X = train.x, y = train.y, 
                                     ctx = devices, num.round = 1000, 
                                     array.batch.size = 20, learning.rate = 0.0001, 
                                     momentum = 0.9, eval.metric = mx.metric.accuracy
)

preds <- predict(model, test.x)
dim(preds)

pred.label <- max.col(t(preds)) - 1
table(pred.label)

test.y <- t(test_y2$P)
View(test.y)
comparison <- rbind(preds, test.y)
View(comparison)
View(preds)

tPreds <- preds
rownames(tPreds) <- c(0,1)
View(tPreds)

tPreds2 <- t(tPreds)
View(tPreds2)

colnames(tPreds2)

predsV2 <- colnames(tPreds2)[max.col(tPreds2, ties.method = 'first')]
predsV3 <- data.frame(cbind(predsV2, test_y2$P))
colnames(predsV3) <- c("Preds", "Real")

result <- table(predsV3$Preds, predsV3$Real)
colnames(result)=c("No person","Person")
rownames(result)=c("No person predicted","Person predicted")

# Calculate accuracy
correct <- result["No person predicted","No person"]+result["Person predicted","Person"]
acc <- (correct)/sum(result)

  

## eighth 

load("data/colorHistEighth.rda")
load("data/classesEights.rda")

library(mxnet)

train_eighthX <- colorHistEighth[0:2120, ]
test_eighthX <- colorHistEighth[2121:2650, ]

train.EighthY <- classesEights[0:2120, ]
test_classY <- classesEights[2121:2650,]
test_eighthX2 <- data.matrix(test_eighthX)

train.EighthX <- t(train_eighthX)
test.EighthX <- t(test_eighthX2)

dataEighth <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(dataEighth, name="fc1", num_hidden=144)
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=72)
act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
fc3 <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=36)
act3 <- mx.symbol.Activation(fc3, name="relu3", act_type="relu")
fc4 <- mx.symbol.FullyConnected(act3, name="fc4", num_hidden=2)
softmax <- mx.symbol.SoftmaxOutput(fc4, name="sm")

devices <- mx.cpu()

mx.set.seed(1234)
modelEighth <- mx.model.FeedForward.create(softmax, X = train.EighthX, y = train.EighthY, 
                                     ctx = devices, num.round = 100, 
                                     array.batch.size = 20, learning.rate = 0.00003, 
                                     momentum = 0.9, eval.metric = mx.metric.accuracy
)

predsEighth <- predict(model, test.EighthX)
dim(preds)

predEighth.label <- max.col(t(predsEighth)) - 1
table(predEighth.label)

test.EighthY <- t(test_classY$P)
comparison <- rbind(predsEighth, test.EighthY)

rownames(predsEighth) <- c(0,1)
predsEighth2 <- t(predsEighth)

colnames(predsEighth2)

predsEighthV2 <- colnames(predsEighth2)[max.col(predsEighth2, ties.method = 'first')]
predsEighthV3 <- data.frame(cbind(predsEighthV2, test_classY$P))
colnames(predsEighthV3) <- c("Preds", "Real")

resultEighth <- table(predsEighthV3$Preds, predsEighthV3$Real)
colnames(resultEighth)=c("No person","Person")
rownames(resultEighth)=c("No person predicted","Person predicted")

# Calculate accuracy
correcteighth <- resultEighth["No person predicted","No person"]+resultEighth["Person predicted","Person"]
accEighth <- (correctEighth)/sum(resultEighth)
