# This project predicts if Caravan owners purchased an insurance policy

library(ISLR)

str(Caravan) # 5822 observations with 86 variables
#?Caravan # run for more information on the Caravan data set

summary(Caravan$Purchase) # indicates whether the customer purchased a caravan insurance policy

any(is.na(Caravan))

# save the Purchase column in a separate variable
purchase <- Caravan[,86]

# Standarize the dataset
standardized.Caravan <- scale(Caravan[,-86])

# Set first 1000 rows as test set
test.index <- 1:1000
test.data <- standardized.Caravan[test.index,]
test.purchase <- purchase[test.index]

# Set rest of data for training
train.data <- standardized.Caravan[-test.index,]
train.purchase <- purchase[-test.index]

# KNN Method
library(class)

set.seed(101)
predicted.purchase <- knn(train.data,test.data,train.purchase,k=1)
head(predicted.purchase)

KNN.1.CM <- table(predicted.purchase, test.purchase) #KNN with k=1 confusion matrix
(KNN.1.CM[1]+KNN.1.CM[4])/sum(KNN.1.CM) #Accuracy


# Choosing K Value with the Elbow Method
library(ggplot2)
predicted.purchase = NULL
error.rate = NULL

for(i in 1:20){
    set.seed(101)
    predicted.purchase = knn(train.data,test.data,train.purchase,k=i)
    error.rate[i] = mean(test.purchase != predicted.purchase)
}

k.values <- 1:20
error.df <- data.frame(error.rate,k.values)
error.df

pl <- ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()
pl + geom_line(lty="dotted",color='red') # best parameter: k=9


# KNN Method with k=9
set.seed(101)
predicted.purchase <- knn(train.data,test.data,train.purchase,k=9)
KNN.9.CM <- table(predicted.purchase, test.purchase) #KNN with k=9 confusion matrix
(KNN.9.CM[1]+KNN.9.CM[4])/sum(KNN.9.CM) #Accuracy

