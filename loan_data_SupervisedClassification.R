# This project explores publicly available data from LendingClub.com.
# This project predicts whether or not the borrower paid back their loan in full.

setwd('C:/Users/evana/OneDrive/Documents/Evan-DESKTOP-NPD6SEU/Data') # set working directory

loans <- read.csv("loan_data.csv")

anyNA(loans) # Check for missing data

summary(loans) 
str(loans) # 0/1 int type columns will need to be transformed into factors for random forest model
head(loans)

#VISUALIZATION

library(ggplot2)
pl1 <- ggplot(data=loans, aes(x=fico, fill=as.factor(not.fully.paid)))
pl1 <- pl1 + geom_histogram(bins=40, alpha=0.5, color='black') + theme_bw()
pl1+ scale_fill_manual(values = c('green','red')) + theme_bw()

library(tidyr)
library(dplyr)
paid <- loans %>% filter(not.fully.paid==0) #fully paid loans
mean(paid$fico) # mean fico score for fully paid loans is 713.33
not.paid <- loans %>% filter(not.fully.paid==1) #not fully paid loans
mean(not.paid$fico) # mean fico score for loans not fully paid is 697.83

pl2 <- ggplot(data=loans, aes(x=purpose, fill=as.factor(not.fully.paid)))
pl2 + geom_bar(alpha=0.5, position='dodge') + theme(axis.text.x 
                                   = element_text(angle = 90, hjust = 1))

pl3 <- ggplot(data=loans, aes(x=int.rate, y=fico))
pl3 + geom_point(aes(color=as.factor(not.fully.paid)), alpha=0.4)


#Train and Test Sets
library(caTools)
library(caret)
set.seed(101)

split = sample.split(loans$not.fully.paid, SplitRatio = 0.70)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

# Logistic Regression
train.logModel <- glm(not.fully.paid ~ ., family='binomial', data=train)
summary(train.logModel)
fitted.glm <- predict(train.logModel, newdata=test[1:13], type='response')
glm.CM <- table(fitted.glm > 0.5, test$not.fully.paid) #Confusion Matrix
(glm.CM[1]+glm.CM[4])/sum(glm.CM) # Logistic Regression Accuracy



# Random Forest Method

#CLEAN
#factor '0, 1' columns
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)
loans$credit.policy <- factor(loans$credit.policy)
str(loans)

split = sample.split(loans$not.fully.paid, SplitRatio = 0.70)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

library(randomForest)
RFmodel <- randomForest(not.fully.paid ~ .,data=train, importance=TRUE)

summary(RFmodel)
RFmodel$importance
RFpred.val <- predict(RFmodel, newdata=test[1:13])
RF.CM <- table(RFpred.val, test$not.fully.paid) #Confusion Matrix
(RF.CM[1]+RF.CM[4])/sum(RF.CM) # Random Forest Accuracy



# SVM Method
library(e1071)
SVM.model <- svm(not.fully.paid ~ .,data=train)

summary(SVM.model)
SVMpred.val <- predict(SVM.model, newdata=test[1:13])
SVM.CM <- table(SVMpred.val, test$not.fully.paid) #Confusion Matrix
(SVM.CM[1]+SVM.CM[4])/sum(SVM.CM) # SVM Accuracy


(glm.CM[1]+glm.CM[4])/sum(glm.CM) # Logistic Regression Accuracy
(RF.CM[1]+RF.CM[4])/sum(RF.CM) # Random Forest Accuracy
(SVM.CM[1]+SVM.CM[4])/sum(SVM.CM) # SVM Accuracy
