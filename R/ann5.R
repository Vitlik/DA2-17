#install.packages('neuralnet')
library(neuralnet)
#require(neuralnet)

#BANK MARKETING Beispiel

# Daten laden
setwd("E:\\Benjamin's tresor\\Viadee\\NeuralNets\\dataset-bank-marketing")

trainingdata <- read.table("bank-complete.csv", header=TRUE, sep=";", strip.white=TRUE, dec=",")
#trainingdata <- read.table("bank-training.csv", header=TRUE, sep=";", strip.white=TRUE, dec=",")
testdata <- read.table("bank-validation.csv", header=TRUE, sep=";", strip.white=TRUE, dec=",")

# Mean Squared Error des Linearen Trennung berechnen
lm.fit <- glm(yout~., data=trainingdata)
summary(lm.fit)
pr.lm <- predict(lm.fit,testdata)
MSE.lm <- sum((pr.lm - testdata$yout)^2)/nrow(testdata)

# Konstruktion des Neuralen Netzes
n <- names(trainingdata)
f <- as.formula(paste("yout ~", paste(n[!n %in% "yout"], collapse = " + ")))
#nn <- neuralnet(f,data=trainingdata,hidden=10, threshold=0.01, linear.output = T)
nn <- neuralnet(f,
                data=trainingdata,
                hidden=c(10),
                #startweights = NULL,
                #learningrate.limit = NULL,
                #learningrate.factor = list(minus = 0.5, plus = 1.2),
                #learningrate=NULL,
                #lifesign = "none",
                #lifesign.step = 1000,
                #algorithm = "rprop+",
                #err.fct = "sse",
                act.fct = "logistic",
                #stepmax = 1e+05,
                rep=10,
                threshold=0.01)

# Ggf. Ausgabedes Neuronalen Netzes
#print(nn)
plot(nn)


pr.nn <- compute(nn,trainingdata[,1:23])
pr.nn_ <- pr.nn$net.result*(max(trainingdata$yout)-min(trainingdata$yout))+min(trainingdata$yout)
train.r <- (trainingdata$yout)*(max(trainingdata$yout)-min(trainingdata$yout))+min(trainingdata$yout)
MSE.nn <- sum((train.r - pr.nn_)^2)/nrow(trainingdata)
print(paste(MSE.lm,MSE.nn))


# Calculation of MSE to the testdata
pr.nn <- compute(nn,testdata[,1:23])
pr.nn_ <- pr.nn$net.result*(max(testdata$yout)-min(testdata$yout))+min(testdata$yout)
test.r <- (testdata$yout)*(max(testdata$yout)-min(testdata$yout))+min(testdata$yout)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(testdata)
print(paste(MSE.lm,MSE.nn))


# Grafische Analyse - einzeln
par(mfrow=c(1,2))
plot(testdata$yout,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
#abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
plot(testdata$yout,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
#abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

# Grafische Analyse - kombiniert
plot(testdata$yout,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(testdata$yout,pr.lm,col='blue',pch=18,cex=0.7)
#abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))