library(MASS)

# Boston - dataset of housing values for 506 neighborhoods around Boston
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

cor(data)   #strongest correlations with medv are 
            #lstat, rm and ptratio; 
            #lstat and rm are correlated

qplot(lstat, medv, data=data) # non-linear relation
qplot(log(lstat), medv, data=data) #this is more linear

qplot(rm, medv, data=data) # linear relation

qplot(ptratio, medv, data=data) # linear relation with high variation

# Simple Linear Model of medv based on lstat
lm.fit1 <- lm(medv~lstat, data=Boston) 
summary(lm.fit1) # R-squared: 0.5441 

# Log Model of medv based on lstat
lm.fit2 <- lm(medv~log(lstat), data=Boston) 
summary(lm.fit2) # R-squared: 0.6643; better model

# plot of residuals
par(mfrow=c(2,2))
plot(lm.fit2)

qplot(rm, medv, data=Boston) # linear relation
lm.fit3 <- lm(medv~rm, data=Boston) 
summary(lm.fit3) # R-squared: 0.4835 

qplot(ptratio, medv, data=Boston) # linear relation
lm.fit4 <- lm(medv~ptratio, data=Boston) 
summary(lm.fit4) # R-squared: 0.2564

# Multiple Linear Regression
lm.fit5 <- lm(medv~., data=Boston)
summary(lm.fit5) # age variable is not significant;  Adjusted R-squared: 0.7338
lm.fit6 <- lm(medv~.-age, data=Boston) 
summary(lm.fit6) # Adjusted R-squared: 0.7343 

# Lasso Regression
library(lasso2)
lasso.fit1 <- l1ce(medv~., data=Boston)
summary(lasso.fit1)$coefficients; # eliminate zero coefficient variables
lasso.fit2 <- l1ce(medv~crim+chas+nox+rm+dis+ptratio+black+lstat, data=Boston)
summary(lasso.fit2)$coefficients; # eliminate zero coefficient variables
lasso.fit3 <- l1ce(medv~rm+ptratio+lstat, data=Boston)
summary(lasso.fit3)$coefficients

lm.fit7 <- lm(medv~rm+ptratio+lstat, data=Boston)
summary(lm.fit7) #Adjusted R-squared: 0.6767 

lm.fit8 <- lm(medv~rm+ptratio+log(lstat), data=Boston)
summary(lm.fit8) #Adjusted R-squared: 0.7318


# Interaction Terms
lm.fit9 <- lm(medv~lstat*rm, data=Boston)
summary(lm.fit9) #Adjusted R-squared:  0.7387

# Final Model
lm.fit10 <- lm(medv~rm+ptratio+log(lstat)+lstat*rm, data=Boston)
summary(lm.fit10) # Adjusted R-squared:  0.7591 

MSE <- c(((sum(resid(lm.fit1)))^2)/nrow(data), ((sum(resid(lm.fit2)))^2)/nrow(data), ((sum(resid(lm.fit6)))^2)/nrow(data),
                ((sum(resid(lm.fit8)))^2)/nrow(data), ((sum(resid(lm.fit10)))^2)/nrow(data)) 

names(MSE) <- c('Simple Reg Model', 'Log Model', 'Mult Reg Model', 
                       'Lasso Reg Model', 'Final Model')

MSE # Final Model is best


