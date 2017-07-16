load("data/pixelFeatureMatrixEighthsSquared.rda")
load("data/classesEights.rda")
load("data/blocks.rda")
load("data/blockNum.rda")

library(mxnet)

## load data matrixes
train <- pixelFeatureMatrixEighthsSquared[0:2120, ]
test <- pixelFeatureMatrixEighthsSquared[2121:2650, ]

train_classes <- classesEights[0:2120, ]
test_classes <- classesEights[2121:2650,]
##test_x2 <- data.matrix(test_x2)

##train.y <- train_y2
train_x <- t(train)

test_x <- t(test)

## img_size <- 28*28
train_array <- train_x
dim(train_array) <- c(28, 28, 1, ncol(train_x))

##den test datensatz hat er im R-Blogger nicht als matrix, nur die Tabelle verwendet nachher die MAtrix
test_array <- test_x
dim(test_array) <- c(28,28,1,ncol(test_x))

# Model
data <- mx.symbol.Variable('data')
# 1st convolutional layer 5x5 kernel and 20 filters.
conv_1 <- mx.symbol.Convolution(data= data, kernel = c(5,5), num_filter = 20)
tanh_1 <- mx.symbol.Activation(data= conv_1, act_type = "tanh")
pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2,2), stride = c(2,2))
# 2nd convolutional layer 5x5 kernel and 50 filters.
conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5,5), num_filter = 50)
tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
pool_2 <- mx.symbol.Pooling(data = tanh_2, pool_type = "max", kernel = c(2,2), stride = c(2,2))
# 1st fully connected layer
flat <- mx.symbol.Flatten(data = pool_2)
fcl_1 <- mx.symbol.FullyConnected(data = flat, num_hidden = 500)
tanh_3 <- mx.symbol.Activation(data = fcl_1, act_type = "tanh")
# 2nd fully connected layer
fcl_2 <- mx.symbol.FullyConnected(data = tanh_3, num_hidden = 2)
# Output
NN_model <- mx.symbol.SoftmaxOutput(data = fcl_2)

mx.set.seed(100)

device <- mx.cpu()

model <- mx.model.FeedForward.create(NN_model, X = train_array, y = train_classes,
                                     ctx = device,
                                     num.round = 20,
                                     array.batch.size = 20,
                                     learning.rate = 0.00000001,
                                     momentum = 0.9,
                                     wd = 0.00001,
                                     eval.metric = mx.metric.accuracy,
                                     epoch.end.callback = mx.callback.log.train.metric(100)
                                     )

  
preds <- predict(model, test_array)
dim(preds)

pred.label <- max.col(t(preds)) - 1
table(pred.label)

table(test_classes, pred.label)
sum(diag(table(test_classes, pred.label)))/530

#test.y <- t(test_y2)
#comparison <- rbind(preds, test.y)

tPreds <- t(preds)
colnames(tPreds) <- c(0,1)

#tPreds2 <- t(tPreds)
#View(tPreds2)

#colnames(tPreds2)

predsV2 <- colnames(tPreds)[max.col(tPreds, ties.method = 'first')]
predsV3 <- data.frame(cbind(predsV2, test_classes))
colnames(predsV3) <- c("Preds", "Real")


result <- table(predsV3$Preds, predsV3$Real)
colnames(result)=c("No person","Person")
rownames(result)=c("No person predicted","Person predicted")

# Calculate accuracy
correct <- result["No person predicted","No person"]+result["Person predicted","Person"]
acc <- (correct)/sum(result)

