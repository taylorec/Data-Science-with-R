# This project explores publicly available data from LendingClub.com.
# This project predicts whether or not the borrower paid back their loan in full.

loans <- read.csv("C:/Program Files/R/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/loan_data.csv")
library(e1071)
library(ggplot2)
library(caTools)
library("caret")
library("corrplot")

head(loans)
summary(loans)
str(loans)

#CLEAN

# Convert categorical columns into factor columns
#factor '0, 1' columns
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)
loans$credit.policy <- factor(loans$credit.policy)
str(loans)

#VISUALIZATIONS

# histogram of Fico scores, colored by unpaid and paid off loans
pl1 <- ggplot(data=loans, aes(x=fico, fill=not.fully.paid))
pl1 <- pl1 + geom_histogram(bins=40, alpha=0.5, color='black') + theme_bw()
pl1+ scale_fill_manual(values = c('green','red')) + theme_bw()

# barplot of loan purpose by counts, colored by unpaid and paid off loans
pl2 <- ggplot(data=loans, aes(x=purpose, fill=not.fully.paid))
pl2 + geom_bar(alpha=0.5, position='dodge') + theme(axis.text.x 
                                   = element_text(angle = 90, hjust = 1))

# Scatterplot of fico score versus int.rate, colored by unpaid and paid off loans
pl3 <- ggplot(data=loans, aes(x=int.rate, y=fico))
pl3 + geom_point(aes(color=not.fully.paid), alpha=0.4)


#Train and Test Sets
set.seed(101)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.70)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

SVM.model <- svm(not.fully.paid ~ .,data=train)

summary(SVM.model)
SVMpred.val <- predict(SVM.model, newdata=test[1:13])
SVM.CM <- table(SVMpred.val, test$not.fully.paid) #Confusion Matrix
(SVM.CM[1]+SVM.CM[4])/sum(SVM.CM) # SVM Accuracy


# Tuning the Model
tune.results <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial',
                  ranges=list(cost=c(0.1,1,10), gamma=c(0.1,1)))
summary(tune.results) # find best tuning parameters, cost=0.1, gamma=0.1
model <- svm(not.fully.paid ~ .,data=train,cost=0.1,gamma = 0.1)
predicted.values <- predict(model,newdata=test[1:13])
SVM.t.CM <- table(predicted.values,test$not.fully.paid) # Tuned SVM Accuracy

(SVM.CM[1]+SVM.CM[4])/sum(SVM.CM) # SVM Accuracy
(SVM.t.CM[1]+SVM.t.CM[4])/sum(SVM.t.CM) # Tuned SVM Accuracy

