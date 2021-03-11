# This project explores publicly available data from LendingClub.com.
# This project predicts whether or not the borrower paid back their loan in full.

loans <- read.csv("C:/Program Files/R/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/loan_data.csv")
library(randomForest)
library(ggplot2)
library(caTools)
library("caret")
library("corrplot")
head(loans)
summary(loans)
str(loans)


#CLEAN
#factor '0, 1' columns
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)
loans$credit.policy <- factor(loans$credit.policy)
str(loans)

#VISUALIZATION
pl1 <- ggplot(data=loans, aes(x=fico, fill=not.fully.paid))
pl1 <- pl1 + geom_histogram(bins=40, alpha=0.5, color='black') + theme_bw()
pl1+ scale_fill_manual(values = c('green','red')) + theme_bw()

pl2 <- ggplot(data=loans, aes(x=purpose, fill=not.fully.paid))
pl2 + geom_bar(alpha=0.5, position='dodge') + theme(axis.text.x 
                                   = element_text(angle = 90, hjust = 1))

pl3 <- ggplot(data=loans, aes(x=int.rate, y=fico))
pl3 + geom_point(aes(color=not.fully.paid), alpha=0.4)


#Random Forest
set.seed(101)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.70)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)
RFmodel <- randomForest(not.fully.paid ~ .,data=train, importance=TRUE)

summary(RFmodel)
RFmodel$importance
RFpred.val <- predict(RFmodel, newdata=test[1:13])
RF.CM <- table(RFpred.val, test$not.fully.paid) #Confusion Matrix
(RF.CM[1]+RF.CM[4])/sum(RF.CM) # Random Forest Accuracy
