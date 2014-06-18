# load data
pregnancyData <- read.csv("data/Pregnancy.csv")
pregnancyData.Test <- read.csv("data/Pregnancy_Test.csv")
pregnancyData$PREGNANT <- factor(pregnancyData$PREGNANT)
pregnancyData.Test$PREGNANT <- factor(pregnancyData.Test$PREGNANT)
# load packages
install.packages("randomForest", dependencies=TRUE)
install.packages("ROCR", dependencies=TRUE)
library(randomForest)
library(ROCR)
# build the linear model using logistic regression using the family binomial
# train my model to predict the PREGNANT column using (~) all other columns (.) 
pregnancy.lm <- glm(PREGNANT ~ ., data=pregnancyData, family=binomial("logit"))
# analyze which variables are statisticaly significant by summarizing the model
summary(pregnancy.lm)
# train a random forest model using the randomForest() function
# The random forests technique examines a large ensemble of decision trees, 
# by first generating a random sample of the original data with replacement
# (bootstrapping), and using a user-defined number of variables selected at 
# random from all of the variables to determine node splitting.
# Multiple subsets of trees are built, and the support for the role of each 
# variable in each decision is noted.

pregnancy.rf <- randomForest(PREGNANT~., data=pregnancyData, importance=TRUE)
# Note: importance = TRUE allows us to graph variable importance using 
# another function varImpPlot(), which allows us to undersrand
# which variables are important and which are weak.
varImpPlot(pregnancy.rf, type=2)
# predict
pregnancyData.Test.lm.Preds <- 
    predict(pregnancy.lm, pregnancyData.Test, type="response")

pregnancyData.Test.rf.Preds <- 
    predict(pregnancy.rf, pregnancyData.Test, type="prob")

# Note: type="response" sets the values returned from the prediction 
# to be between 0 and 1 just like the original PREGNANT values.
# type="prob" ensures that you get back the class probabilities(two columns of data)
# one prob. of pregnancy and one prob. of no pregnancy.
summary(pregnancyData.Test.lm.Preds)
summary(pregnancyData.Test.rf.Preds)
# we can pull out the first record from the test set and print it
t(pregnancyData.Test[1,])
# Then, chk its prediction using logistic regression
t(pregnancyData.Test.lm.Preds[1])
# Then, chk its prediction using random Forest
t(pregnancyData.Test.rf.Preds[1, 2])
# now we can compare the models in terms of true positive rate and false positive rate
# Use the ROCR package: prediction() function which count up the positive
# and negative class predictions at various cut off levels in the class probablities.
# For lm (linear model):
pred.lm <- prediction(pregnancyData.Test.lm.Preds, pregnancyData.Test$PREGNANT)
# same for rf (random Forest):
pred.rf <- prediction(pregnancyData.Test.rf.Preds[,2], pregnancyData.Test$PREGNANT)
# Performance measure to use for the evaluation for both lm and rf
# Computing a simple ROC curve(x-axis: fpr, y-axis: tpr) using library(ROCR)

perf.lm <- performance(pred.lm, "tpr", "fpr") 
perf.rf <- performance(pred.lm, "tpr", "fpr")
# Note: trp = true pos (same as recall). rate, fpr= false pos. rate

plot(perf.lm, xlim=c(0,1), ylim=c(0,1))
# adding the rf plot using add=TRUE
plot(perf.rf, xlim=c(0,1), ylim=c(0,1), lty=2, add=TRUE)
# plot rec=recall and prec = precision
perf.lm <- performance(pred.lm, "prec", "rec")
perf.rf <- performance(pred.rf, "prec", "rec")
plot(perf.lm, xlim=c(0,1), ylim=c(0,1))
# adding the rf plot using add=TRUE
plot(perf.rf, xlim=c(0,1), ylim=c(0,1), lty=2, add=TRUE)