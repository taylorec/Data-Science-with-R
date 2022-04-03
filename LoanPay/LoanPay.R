setwd('C:/Users/evana/OneDrive/Documents/Evan-DESKTOP-NPD6SEU/Data') # set working directory

lp <- read.csv("LoanPay.csv")

library(caret)

head(lp)
summary(lp)
is.na(lp)
lp[is.na(lp)] = 0 #replace NA with 0

nameList=c("loan_status","Principal","terms","age","education","Gender","past_due_days")

df= lp[,colnames(lp) %in% nameList] 

head(df)
summary(df)
str(df)
df$loan_status <- factor(df$loan_status)
### 1) Data split

set.seed(99)
Train = createDataPartition(df$loan_status, p=0.75, list=FALSE)
#split data in 75%-25% ratio

training = df[ Train, ] #75% data for training 
testing = df[ -Train, ] #25% testing

# define training control
train_control <- trainControl(method="cv", number=10)
#10 fold cv



fit1= train(loan_status~., data=training, method="rf",
            trControl=train_control,importance=TRUE)


#see the performance on the training data

fit1

plot(fit1)


######## evaluate variable importance
varImp(fit1)

plot(varImp(fit1), top = 5)

########## predict on unseen data

pred1 = predict(fit1, testing)

confusionMatrix(pred1, testing$loan_status)





