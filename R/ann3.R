setwd("C:/dev/DA2/DA2-17/")
load("data/colorHistOriginal.rda")
load("data/clasAll.rda")

library(mxnet)

train_x2 <- colorHistOriginal[0:2120, ]
test_x2 <- colorHistOriginal[2121:2650, ]

train_y2 <- clasAll[0:2120, ]
test_y2 <- clasAll[2121:2650,]
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
                                     ctx = devices, num.round = 50, 
                                     array.batch.size = 20, learning.rate = 0.03, 
                                     momentum = 0.9, eval.metric = mx.metric.accuracy
)

preds <- predict(model, test.x)
dim(preds)

pred.label <- max.col(t(preds)) - 1
table(pred.label)



