library(ggplot2)
library(dplyr)
library(tidyr)
trainData<-read.csv("C:\\Users\\Subhada\\Desktop\\Project\\hr_train.csv")

#Structure of the Dataset
str(trainData)
attach(trainData)

#converting left variable to factor variable 
trainData$left<-as.factor(trainData$left)
table(trainData$left)

#Summary Statistics of the dataset
summary(trainData)

trainData$number_project<-as.factor(trainData$number_project)
trainData$Work_accident<-as.factor(trainData$Work_accident)
trainData$promotion_last_5years<-as.factor(trainData$promotion_last_5years)
trainData$sales<-as.factor(trainData$sales)
trainData$salary<-as.factor(trainData$salary)
trainData$time_spend_company<-as.factor(trainData$time_spend_company)

#Splitting train data into 2 parts 

set.seed(2)
s = sample(1:nrow(trainData), 0.8*nrow(trainData))
trainData_train1 = trainData[s,]
trainData_train2 = trainData[-s,]

logit_model1=glm(left~.,family=binomial(link='logit'),data=trainData_train1)
library(car)
library(MASS)

stepAIC(logit_model1, threshold = 5, verbose = FALSE)

logit_model2=glm(formula = left ~ satisfaction_level + last_evaluation + number_project + 
                   average_montly_hours + time_spend_company + Work_accident + 
                   sales + salary, family = binomial(link = "logit"), data = trainData_train1)
summary(logit_model2)
stepAIC(logit_model2, threshold = 5, verbose = FALSE)

logit_model3=glm(formula = left ~ satisfaction_level + last_evaluation + number_project + 
                   average_montly_hours + time_spend_company + Work_accident + salary, family = binomial(link = "logit"), data = trainData_train1)
summary(logit_model3)

#plotting the Model 3

par(mfrow=c(2,2))
plot(logit_model3)

library(pROC)
#View(bankdata_train2)
val.score = predict(logit_model3, newdata = trainData_train2, type= 'response')
val.score
auc_score = auc(roc(trainData_train2$left, val.score))

auc_score #0.81
mydata = data.frame(y = trainData_train2$left, val.score = val.score)
ggplot(mydata, aes(y = trainData_train2$left, x= val.score, color = factor(y))) + geom_point() + geom_jitter()

train2.score = predict(logit_model3, newdata = trainData_train2, type = 'response')
real = trainData_train2$left
cutoffs = seq(0.001, 0.999, 0.001)
cutoff_data = data.frame(cutoff = 99, Sn=99, Sp=99, KS=99, F5=99, F.1 = 99, M=99)

for (cutoff in cutoffs) {
  predicted = as.numeric(train2.score>cutoff)
  
  TP = sum(real ==1 & predicted ==1)
  TN = sum(real ==0 & predicted ==0)
  FP = sum(real ==0 & predicted ==1)
  FN = sum(real == 1 & predicted == 0)
  
  P = TP + FN 
  N = TN + FP 
  
  Sn = TP /P
  Sp = TN/N
  precision = TP/(TP+FP)
  recall = Sn
  
  KS = (TP/P) - (FP/N)
  F5 = (26*precision*recall)/((0.01*precision)+recall)
  F.1 = (1.01*precision*recall)/((0.1*precision)+recall)
  
  M = (4*FP+FN)/(5*(P+N))
  
  cutoff_data = rbind(cutoff_data, c(cutoff, Sn, Sp, KS, F5, F.1, M))
}
cutoff_data = cutoff_data[-1,]

#Plotting Specificity
ggplot(cutoff_data, aes(x=cutoff, y = Sp)) + geom_line()

#Plotting all 

library(tidyr)
cutoff_long = cutoff_data%>%
  gather(Measure,Value,Sn:M)


ggplot(cutoff_long, aes(x=cutoff, y = Value, color=Measure)) + geom_line()

my_cutoff = cutoff_data$cutoff[which.max(cutoff_data$KS)]
which.max(cutoff_data$KS)
my_cutoff # 0.353

##Building the model on entire training data

logit_model=glm(left~.,family=binomial(link='logit'),data=trainData)
stepAIC(logit_model, threshold = 5, verbose = FALSE)
logit_model=glm(formula = left ~ satisfaction_level + last_evaluation + number_project + 
      average_montly_hours + time_spend_company + Work_accident + 
      salary, family = binomial(link = "logit"), data = trainData)
summary(logit_model)
train.prob.score = predict(logit_model, newdata = trainData, type = 'response') 

## Here we compare the predicted probability with the actual output variable in training data
auc_score = auc(roc(trainData$left, train.prob.score))
auc_score

train2.score = predict(logit_model, newdata = trainData, type = 'response')
real = trainData$left
cutoffs = seq(0.001, 0.999, 0.001)

cutoff_data = data.frame(cutoff = 99, Sn=99, Sp=99, KS=99, F5=99, F.1 = 99, M=99)

for (cutoff in cutoffs) {
  predicted = as.numeric(train2.score>cutoff)
  
  TP = sum(real ==1 & predicted ==1)
  TN = sum(real ==0 & predicted ==0)
  FP = sum(real ==0 & predicted ==1)
  FN = sum(real == 1 & predicted == 0)
  
  P = TP + FN 
  N = TN + FP 
  
  Sn = TP /P
  Sp = TN/N
  precision = TP/(TP+FP)
  recall = Sn
  
  KS = (TP/P) - (FP/N)
  F5 = (26*precision*recall)/((0.01*precision)+recall)
  F.1 = (1.01*precision*recall)/((0.1*precision)+recall)
  
  M = (4*FP+FN)/(5*(P+N))
  
  cutoff_data = rbind(cutoff_data, c(cutoff, Sn, Sp, KS, F5, F.1, M))
}

cutoff_data = cutoff_data[-1,]

#Plotting Specificity
ggplot(cutoff_data, aes(x=cutoff, y = Sp)) + geom_line()

#Plotting all 

library(tidyr)

cutoff_long = cutoff_data%>%
  gather(Measure,Value,Sn:M)


ggplot(cutoff_long, aes(x=cutoff, y = Value, color=Measure)) + geom_line()

my_cutoff = cutoff_data$cutoff[which.max(cutoff_data$KS)]
which.max(cutoff_data$KS)
my_cutoff

#Now let's predict the precentage probability that a given response is 1/0 in test data
testData<-read.csv("C:\\Users\\Subhada\\Desktop\\Project\\hr_test.csv")#Test Data frame
str(testData)
testData$number_project<-as.factor(testData$number_project)
testData$Work_accident<-as.factor(testData$Work_accident)
testData$promotion_last_5years<-as.factor(testData$promotion_last_5years)
testData$sales<-as.factor(testData$sales)
testData$salary<-as.factor(testData$salary)
testData$time_spend_company<-as.factor(testData$time_spend_company)


test.prob.score = predict(logit_model, newdata = testData, type = 'response')
test.prob.score

## Converting 1/0 to Yes/No 
test.predicted.numeric = as.numeric(test.prob.score>my_cutoff)
test.predicted = ifelse(test.predicted.numeric ==0, 'No', 'Yes')

any(is.na(trainData))
any(is.na(testData))
any(is.na(test.prob.score))
any(is.na(test.predicted.numeric))
any(is.na(test.predicted))

#Writing the predicted file
getwd()
write.csv(test.predicted, 'Employee_Churn_Prediction.csv', row.names = FALSE)
ggplot(average_montly_hours, aes(x=cutoff, y = Value, color=Measure)) + geom_line()
