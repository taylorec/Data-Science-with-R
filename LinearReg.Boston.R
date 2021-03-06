library(MASS)
library(ISLR)

# Boston - dataset records of house value for 506 neighborhoods around Boston
# medv - neighborhood's median house value
# rm - average number of rooms per house
# age - average age of house
# lsat - percent of households with low socioeconomic status
?Boston #more info on data set

head(Boston)
any(is.na(Boston))
str(Boston)

cor(Boston) #strongest positive/negative correlation with medv is lstat
qplot(medv, lstat, data=Boston)

# Simple Linear Model of medv based on lstat
lm.fit <- lm(medv~lstat, data=Boston)
summary(lm.fit)
coef(lm.fit) # coefficients
confint(lm.fit) # confidence interval

# prediction interval of medv based on lstat values
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval='prediction')

# least squares regression plot of lstat and medv
plot(Boston$lstat, Boston$medv, col='red')
abline(lm.fit, lwd=3)

# plot of residuals
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit)) # evidence of non-linearity
which.max(hatvalues(lm.fit))

# Multiple Linear Regression
lm.fit <- lm(medv~., data=Boston)
summary(lm.fit)
lm.fit1 <- lm(medv~.-age, data=Boston) # age variable is not significant
summary(lm.fit1)

# Interaction Terms
lm.it <- lm(medv~lstat*rm, data=Boston)
summary(lm.it)

# Non-linear Transformation of the Predictors
lm.fit2 <- lm(medv~lstat+I(lstat^2), data=Boston)
summary(lm.fit2)

# ANOVA test between Simple Regression and Non-linear Transformation
lm.fit <- lm(medv~lstat, data=Boston)
anova(lm.fit, lm.fit2) # lm.fit2 is the better model
c(sum(abs(resid(lm.fit))), sum(abs(resid(lm.fit2)))) #lm.fit2 has smaller sum of residuals

# Sum of residuals of Non-linear Transformation and Interaction Terms
c(sum(abs(resid(lm.fit2))), sum(abs(resid(lm.it)))) #Interaction Terms has smaller sum of residuals


