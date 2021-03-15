# This project predicts whether or not a Bank Note was authentic

df <- read.csv("C:/Program Files/R/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/bank_note_data.csv")

head(df)
str(df)


# Train Test Split
library(caTools)
set.seed(101)
split = sample.split(df$Class, SplitRatio = 0.70)
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

str(train) # Keep Class variable as an integer for Neural Net Method

library(neuralnet)
nn <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train,hidden=10,linear.output=FALSE)
predicted.nn.values <- compute(nn,test[,1:4])
head(predicted.nn.values$net.result)  # Predicted values are probabilities
predictions <- sapply(predicted.nn.values$net.result,round) # rounds redicted values to 0s and 1s

head(predictions, 10) # first 10 predictions
nn.cm <- table(predictions,test$Class) # confusion matrix


# Comparison to Random Forest Method
library(randomForest)
df$Class <- factor(df$Class) # randomForest uses factor levels instead of standardized variables
str(df)
set.seed(101)
split = sample.split(df$Class, SplitRatio = 0.70)
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

rf.model <- randomForest(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train)
rf.pred <- predict(rf.model,test)
rf.cm <- table(rf.pred,test$Class) # confusion matrix

# Accuracy for Neural Net
(nn.cm[1]+nn.cm[4])/sum(nn.cm)

#Accuracy for Random Forest
(rf.cm[1]+rf.cm[4])/sum(rf.cm)






