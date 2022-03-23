library(pROC)
library(nnet)
library(caret)

#Question 2
getwd()
setwd('/Users/Cassie Gill/OneDrive/SCMA 851/CGill TermProj/SCMA 851 TermProj')
midwest.df <- read.csv("MidWest_balanced.csv")
summary(midwest.df$Balance)
summary(midwest.df$Bonus_miles)
summary(midwest.df$Topflight)

#Question 3
midwestLR1.df <- midwest.df[, c(-1)]
set.seed(2)
train.index <- sample(c(1:dim(midwestLR1.df)[1]), dim(midwestLR1.df)[1]*0.8)  
train.df <- midwestLR1.df[train.index, ]
valid.df <- midwestLR1.df[-train.index, ]
logit.reg.train.LR0 <- glm(Phone_sale ~ ., data = train.df, family = binomial(link="logit")) 
options(scipen=999)
summary(logit.reg.train.LR0) 
midwestLR2.df <- midwest.df[ , c(3,8,9,10,12,13,15,16)]
set.seed(2)
train.index.LR2 <- sample(c(1:dim(midwestLR2.df)[1]), dim(midwestLR2.df)[1]*0.8)  
train.df.LR2 <- midwestLR2.df[train.index.LR2, ]
valid.df.LR2 <- midwestLR2.df[-train.index.LR2, ]
logit.reg.train.LR1 <- glm(Phone_sale ~ ., data = train.df.LR2, family =binomial(link="logit")) 
options(scipen=999)
summary(logit.reg.train.LR1)
logit.reg.pred.LR1 <- predict(logit.reg.train.LR1, valid.df.LR2[, -8], type = "response")
data.frame(actual = valid.df.LR2$Phone_sale[1:5], predicted = logit.reg.pred.LR1[1:5])
exp(coef(logit.reg.train.LR1))
exp(cbind(OR = coef(logit.reg.train.LR1), confint(logit.reg.train.LR1)))
test_prob = predict(logit.reg.train.LR1, newdata = valid.df.LR2[, -8], type = "response")
test_roc = roc(valid.df.LR2$Phone_sale ~ test_prob, plot = TRUE, print.auc = TRUE)
test_roc = roc(valid.df.LR2$Phone_sale ~ test_prob, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate", lwd=4)
par(pty="m")

#Question 4
midwestLR3.df <- midwestLR2.df[, c(-4)]
set.seed(2)
train.index.LR3 <- sample(c(1:dim(midwestLR3.df)[1]), dim(midwestLR2.df)[1]*0.8)  
train.df.LR3 <- midwestLR3.df[train.index.LR3, ]
valid.df.LR3 <- midwestLR3.df[-train.index.LR3, ]
logit.reg.train.LR2 <- glm(Phone_sale ~ ., data = train.df.LR3, family = "binomial") 
options(scipen=999)
summary(logit.reg.train.LR2)
logit.reg.pred.LR2 <- predict(logit.reg.train.LR2, valid.df.LR3[, -7], type = "response")
data.frame(actual = valid.df.LR3$Phone_sale[1:5], predicted = logit.reg.pred.LR2[1:5])
exp(coef(logit.reg.train.LR2))
exp(cbind(OR = coef(logit.reg.train.LR2), confint(logit.reg.train.LR2)))
test_prob = predict(logit.reg.train.LR2, newdata = valid.df.LR3[, -7], type = "response")
test_roc = roc(valid.df.LR3$Phone_sale ~ test_prob, plot = TRUE, print.auc = TRUE)
test_roc = roc(valid.df.LR3$Phone_sale ~ test_prob, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate", lwd=4)
par(pty="m")

#Question 5
network20<-nnet(as.factor(Phone_sale) ~ .,data = train.df,size =20)
test_probnet = predict(network20, newdata = valid.df[, -15])
test_roc = roc(valid.df$Phone_sale ~ test_probnet, plot = TRUE, print.auc = TRUE)
test_roc = roc(valid.df$Phone_sale ~ test_probnet, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate", lwd=4)
par(pty="m")

network50<-nnet(as.factor(Phone_sale) ~ .,data = train.df,size =50,MaxNWts=1000)
test_probnet1 = predict(network50, newdata = valid.df[, -15])
test_roc = roc(valid.df$Phone_sale ~ test_probnet1, plot = TRUE, print.auc = TRUE)
test_roc = roc(valid.df$Phone_sale ~ test_probnet1, plot = TRUE, print.auc = TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Rate", ylab="True Positive Rate", lwd=4)
par(pty="m")

#Question 6
threshold = .5
test_prob1= predict(logit.reg.train.LR1, newdata = valid.df.LR2[, -8], type = "response")
Test.pred1 = ifelse(test_prob1  > threshold, "1", "0")
CM.test1  <- confusionMatrix(factor(Test.pred1),  factor(valid.df.LR2$Phone_sale), positive="1")
CM.test1

test_prob2= predict(logit.reg.train.LR2, newdata = valid.df.LR3[, -7], type = "response")
Test.pred2 = ifelse(test_prob2  > threshold, "1", "0")
CM.test2  <- confusionMatrix(factor(Test.pred2),  factor(valid.df.LR3$Phone_sale), positive="1")
CM.test2

test_prob3= predict(network20, newdata = valid.df[, -15])
Test.pred3 = ifelse(test_prob3  > threshold, "1", "0")
CM.test3  <- confusionMatrix(factor(Test.pred3),  factor(valid.df$Phone_sale), positive="1")
CM.test3

test_prob4= predict(network50, newdata = valid.df[, -15])
Test.pred4 = ifelse(test_prob4  > threshold, "1", "0")
CM.test4  <- confusionMatrix(factor(Test.pred4),  factor(valid.df$Phone_sale), positive="1")
CM.test4

#Question 8
testdata.df <- read.csv("C:/Users/Cassie Gill/OneDrive/SCMA 851/CGill TermProj/SCMA 851 TermProj/Prediction Scoring.csv")
testdata.df <- testdata.df[, c(2,7,8,11,12,14)]
prob = predict(logit.reg.train.LR2, newdata = testdata.df, type = "response")
pred = ifelse(prob  > threshold, "1", "0")
pred
