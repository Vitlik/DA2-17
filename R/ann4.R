setwd("C:/Users/Sascha/Documents/DA2-17/")
load("data/colorHistOriginalEqual.rda")
load("data/classesOrig.rda")

library(neuralnet)

set.seed(1337)

## extract a set to train the NN
trainset <- colorHistOriginalEqual[0:2120, ]

## select the test set
testset <- colorHistOriginalEqual[2121:2650, ]


## build the neural network (NN)
csnet <- neuralnet(classesOrig$P ~ classesOrig$CT + classesOrig$T, trainset, hidden = 4, learningrate = 0.0003, lifesign = "full", 
                       linear.output = FALSE, threshold = 0.1)


## plot the NN
plot(csnet, rep = "best")

