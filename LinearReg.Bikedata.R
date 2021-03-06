bike <- read.csv('C:/Program Files/R/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/bikeshare.csv', sep=",")

library(ggplot2)
library(ggthemes)
library(dplyr)

head(bike)
any(is.na(bike)) #check for any missing values
str(bike) #check structure of bike data
bike$datetime <- as.POSIXct(bike$datetime) #convert the datetime column into POSIXct

# check correlations; 
numeric_var <- sapply(bike, is.numeric)
cor(bike[,numeric_var]) #humidity and weather have a strong correlation

# plot of count with temp
qplot(temp, count, data=bike, color=temp, alpha=0.6)
cor(bike[,c('temp','count')])

# Plot of count with datetime and temp
gp <- ggplot(data=bike,aes(x = datetime,y=count))
gp + geom_point(aes(color = temp),size=2, alpha=.4) + scale_colour_gradient(high='orange',low = "blue") + theme_bw()

# boxplot of count with season
bp <- ggplot(bike, aes(factor(season), count))
bp + geom_boxplot(aes(color = factor(season)))

# create column as integer of hours
time.stamp <- bike$datetime
time.stamp <- as.integer(format(time.stamp, "%H"))
bike$time.stamp <- time.stamp
head(bike)

# plot of count with hours for weekdays
wdsc <- ggplot(filter(bike, workingday==1), aes(time.stamp, count))
wdsc + geom_point(aes(color = temp),size=2, alpha=.4, position=position_jitter(w=1, h=0)) + scale_colour_gradient(high='orange',low = "blue")

#plot count with hours for non-weekdays
nwdsc <- ggplot(filter(bike, workingday==0), aes(time.stamp, count))
nwdsc + geom_point(aes(color = temp),size=2, alpha=.4, position=position_jitter(w=1, h=0)) + scale_colour_gradient(high='orange',low = "blue")

# create linear model; count ~ temp
temp.model <- lm(count ~ temp, data=bike)
summary(temp.model)

# predicted count with temp=25
predict(temp.model, newdata=data.frame(temp=25))

# linear model of count based on season, holiday, workingday, weather, temp, humidity, windspeed, time.stamp
lm <- lm(count~.-datetime-atemp-casual-registered, data=bike)
summary(lm) # Adjusted R-squared:  0.3339  
qplot(time.stamp,count,data=bike,alpha=0.8) #time.stamp vs count is non-normally distributed
qplot(windspeed,count,data=bike,alpha=0.8) #windspeed vs counts is normally distributed

library(mgcv)
# model with non-normal distr predictor;
lm_gam <- gam(count~s(time.stamp)+season+holiday+workingday+weather+temp+humidity+windspeed,data=bike)
summary(lm_gam) # Adjusted R-squared:  0.587