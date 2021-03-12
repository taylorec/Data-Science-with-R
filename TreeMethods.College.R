# This project classifies schools as Private or Public based off their features.

library(ISLR)
?College
head(College)

df<-College

# Exploratory Data Analysis
library(ggplot2)

# Scatterplot of Grad.Rate versus Room.Board, colored by the Private column
ggplot(df,aes(Room.Board,Grad.Rate)) + geom_point(aes(color=Private))

# Histogram of full time undergrad students, color by Private
ggplot(df,aes(F.Undergrad)) + geom_histogram(aes(fill=Private),color='black',bins=50)

# Histogram of Grad.Rate colored by Private
ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(fill=Private),
	color='black',bins=50) # a datapoint is above 100%

#Clean data
# What college had a Graduation Rate of above 100%?
subset(df,Grad.Rate > 100)

df['Cazenovia College','Grad.Rate'] <- 100 # Change it 100%

# Train Test Split
library(caTools)
set.seed(101) 
sample = sample.split(df$Private, SplitRatio = .70)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

#Decision Tree
library(rpart)
tree <- rpart(Private ~.,method='class',data = train)
tree.preds <- predict(tree,test)
head(tree.preds) # Check the Head of the predicted values.

# Create Yes/No Label for a Private column
tree.preds <- as.data.frame(tree.preds)
joiner <- function(x){
    if (x>=0.5){
        return('Yes')
    }else{
        return("No")
    }
}

tree.preds$Private <- sapply(tree.preds$Yes,joiner)
head(tree.preds)
tree.CM <- table(tree.preds$Private,test$Private) # confusion matrix

library(rpart.plot)
prp(tree) # tree plot


# Random Forest
library(randomForest)
rf.model <- randomForest(Private ~ . , data = train,importance = TRUE)
rf.model$confusion
rf.model$importance
rf.preds <- predict(rf.model,test)
rf.CM <- table(rf.preds,test$Private) #confusion matrix

#Accuracy
(tree.CM[1]+tree.CM[4])/sum(tree.CM) # Tree Accuracy
(rf.CM[1]+rf.CM[4])/sum(rf.CM) # Random Forest Accuracy



#XGB Method
library(xgboost)
label_indicator1 <- function(x){ 	# used to convert y variables on test and train data
	if (x == "Yes"){
	return (1)
}	else {
	return (0)
}
}
label_indicator2 <- function(x){ # used to convert xgb predictors to match y variables
	if (x > 0.5){
	return (1)
}	else {
	return (0)
}
}
x_test <- test
x_test$Private <- NULL # delete y variable from test set x
y_test <- test$Private # test y variable
y_test <- sapply(y_test, label_indicator1)
x_train <- train
x_train$Private <- NULL # delete y variable from test set x
y_train <- train$Private # test y variable
y_train <- sapply(y_train, label_indicator1)
xgb.model <- xgboost(data=as.matrix(x_train), label=as.matrix(y_train), nrounds=500, objective="binary:logistic", params=list(eta=0.001, subsample=0.8, lambda=600))
xgb.preds <- predict(xgb.model, as.matrix(x_test))
xgb.preds <- sapply(xgb.preds, label_indicator2)
xgb.CM <- table(xgb.preds,y_test) #confusion matrix


# Accuracy
(tree.CM[1]+tree.CM[4])/sum(tree.CM) # Tree Accuracy
(rf.CM[1]+rf.CM[4])/sum(rf.CM) # Random Forest Accuracy
(xgb.CM[1]+xgb.CM[4])/sum(xgb.CM) # XGB Accuracy



























