library(MASS)

# Boston - dataset records of house value for 506 neighborhoods around Boston
# medv - neighborhood's median house value
# rm - average number of rooms per house
# age - average age of house
# lsat - percent of households with low socioeconomic status
#?Boston #more info on data set

data <- Boston

head(data)
any(is.na(data))
str(data)
summary(data)

cor(data) #strongest (positive/negative) correlations 
# with medv are lstat, rm and ptratio; lstat and rm are correlated

qplot(lstat, medv, data=data) # non-linear relation
qplot(log(lstat), medv, data=data) #this is a better graph

qplot(rm, medv, data=data) # linear relation

qplot(ptratio, medv, data=data) # linear relation with high variation


# Neural Net Model
library(neuralnet)

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
head(scaled)

library(caTools)
split = sample.split(scaled$medv, SplitRatio = 0.70)
train = subset(scaled, split == TRUE)
test = subset(scaled, split == FALSE)

n <- names(train) # Get column names
n

# Paste variable names together
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
f
nn <- neuralnet(f,data=train,hidden=c(5,3),linear.output=TRUE) # 2 hidden layers with 5 and 3 neurons

plot(nn) # Neural Net Visualization

# Compute Predictions off Test Set
predicted.nn.values <- compute(nn,test[1:13])

str(predicted.nn.values)

# Convert back to non-scaled predictions
true.predictions <- predicted.nn.values$net.result*(max(data$medv)-min(data$medv))+min(data$medv)

# Convert the test data
test.r <- (test$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

(sum_errors <- sum(test.r - true.predictions)^2)

# Check the Mean Squared Error
(MSE.nn <- sum((test.r - true.predictions)^2)/nrow(test))

error.df <- data.frame(test.r,true.predictions)
head(error.df)

library(ggplot2)
ggplot(error.df,aes(x=test.r,y=true.predictions)) + geom_point() + stat_smooth()
