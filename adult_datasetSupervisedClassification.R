# This project attempts to predict if people in the data set 
# belong in a certain class by salary, either making <=50k or >50k per year.

setwd('C:/Users/evana/OneDrive/Documents/Evan-DESKTOP-NPD6SEU/Data')
adult <- read.csv("adult_sal.csv")
head(adult)

library(dplyr)
adult <- select(adult, -X) #Drop repeated index

head(adult)
str(adult)
summary(adult)

table(adult['type_employer']) #type_employer column has many levels

any(is.na(adult['type_employer'])) #check for any na values in column type_employer

#Create column Unemployed; combined with columns Never-worked and Without-Pay
Unem_conv <- function(job){
  job <- as.character(job)
  if (job == 'Never-worked' | job == 'Without-pay'){
    return ('Unemployed') 
  }		else {
    return (job)
  }
}
adult$type_employer <- sapply(adult$type_employer, Unem_conv)
table(adult['type_employer'])

#Create column SL-gov; combin columns Local-gov and State-gov
SL_gov_conv <- function(job){
  job <- as.character(job)
  if (job == 'Local-gov' | job == 'State-gov'){
    return ('SL-gov')
  } 	else {
    return (job)
  }
}
adult$type_employer <- sapply(adult$type_employer, SL_gov_conv)
table(adult['type_employer'])

#Create column Self-emp; combine columns Self-emp-inc and Self-emp-not-inc
SE_conv <- function(job){
  job <- as.character(job)
  if (job == 'Self-emp-inc' | job == 'Self-emp-not-inc'){
    return ('Self-emp')
  } 	else {
    return (job)
  }
}
adult$type_employer <- sapply(adult$type_employer, SE_conv)
table(adult['type_employer'])

table(adult['marital']) # marital column has 7 levels

#Reduce the number of levels in marital column to three
married_conv <- function(status){
  status <- as.character(status)
  if (status == 'Divorced' | status == 'Widowed'){
    return ('Not-Married')
  } 	else if (status == 'Never-married'){
    return ('Never-Married')
  }	else {
    return ('Married')
  }
}
adult$marital <- sapply(adult$marital, married_conv)
table(adult['marital'])
#country column has many countries listed
table(adult['country'])

#Group countries into groups of continents
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')
North.America <- c('Canada','United-States','Puerto-Rico' )
Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')
Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')
country_conv <- function(ctry){
  ctry <- as.character(ctry)
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}
adult$country <- sapply(adult$country, country_conv)
table(adult['country'])
str(adult)

str(adult)

#Change values of ? to NA
NA_convert <- function(value){
  value <- as.character(value)
  if (value == '?' | value == ' ?'){
    return (NA)
  }	else {
    return (value)
  }
}
adult$type_employer <- sapply(adult$type_employer, NA_convert)
table(adult$type_employer)

#change columns country, marital, income and type_employer into factors
adult$country <- sapply(adult$country, factor)
adult$marital <- sapply(adult$marital, factor)
adult$type_employer <- sapply(adult$type_employer, factor)
adult$income <- sapply(adult$income, factor)

adult <- na.omit(adult) #Omit na values from the data
any(is.na(adult))

str(adult)

library(ggplot2)
library(theme_bw)

# Histogram of ages, color by income
pl_ageVSinc <- ggplot(adult, aes(x=age, fill=income))
pl_ageVSinc + geom_histogram(binwidth=.8) + 
  xlab('Age')+ ylab('count')

# Histogram of hours worked per week
pl_hrsVSinc <- ggplot(adult, aes(x=hr_per_week , fill=income))
pl_hrsVSinc + geom_histogram(binwidth=1.2) + 
  xlab('hrs_per_week')+ ylab('count')

#Rename country column to region
str(adult)
names(adult)[names(adult)=="country"] <- "region"

str(adult)

#Barplot of region with the fill color defined by income class
Bp_regionVSinc <- ggplot(adult, aes(x=region , fill=income))
Bp_regionVSinc + geom_bar() + theme(axis.text.x 
                                    = element_text(angle = 45, hjust = 1))

#Split the data into a train and test set using the caTools library
library(caTools)
split = sample.split(adult$income, SplitRatio = 0.70)
adult.train = subset(adult, split == TRUE)
adult.test = subset(adult, split == FALSE)

# Logistic regression model on sampled data
adult.train.logModel <- glm(income ~ ., family='binomial', data=adult.train)
summary(adult.train.logModel)

#fitted glm model on test data
fitted.glm <- predict(adult.train.logModel, newdata=adult.test[1:15], type='response')

#check model accuracy
glm.cm <- table(fitted.glm > 0.5, adult.test$income)

#Accuracy
(glm.acc <- (glm.cm[1]+glm.cm[4])/sum(glm.cm))

#Recall
glm.cm[1]/sum(glm.cm[1]+glm.cm[3])

#Precision
glm.cm[1]/sum(glm.cm[1]+glm.cm[2])


# Random Forest Model
library(randomForest)
rf.train <- randomForest(income ~ .,data=adult.train, importance=TRUE)

summary(rf.train)
rf.train$confusion
rf.train$importance

predicted.rf <- predict(rf.train, adult.test[1:15])
RF.CM <- table(predicted.rf, adult.test$income) 
(rf.acc <- (RF.CM[1]+RF.CM[4])/sum(RF.CM)) #Random Forest accuracy


# Support Vector Machine 
library(e1071)
svmModel <- svm(income ~ .,data=adult.train)
summary(svmModel)
predicted.svm <- predict(svmModel,adult.test[1:15])
SVM.CM <- table(predicted.svm, adult.test$income) 
(svm.acc <- (SVM.CM[1]+SVM.CM[4])/sum(SVM.CM)) #Random Forest accuracy


#Logistic accuracy
glm.acc
#Random Forest accuracy
rf.acc
#SVM accuracy
svm.acc
