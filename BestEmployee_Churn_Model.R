
library(dplyr)
#library(ggplot2) #For visualization of data 
#library(plotly)

hr_train<-read.csv('hr_train.csv')
hr_test<-read.csv('hr_test.csv')
str(hr_train$left)
hr_train$data='train'
hr_test$data='test'

dim(hr_train)

str(hr_train$left)

#the data in the response variable is imbalanced.Hence, need to balance the data
#library(ROSE)
#hr_train_rose <- ROSE.eval(left~., data=hr_train, seed=3)$data
#adding the response column in test data

hr_test$left<-0
str(hr_train)
str(hr_test)
hr_full<-rbind(hr_train,hr_test)

#Feature Engineering
#employee ID variable, creating IDs for the each employee 
hr_full$Empid<-0
hr_full$Empid = sample(x=1:nrow(hr_full),size=nrow(hr_full))
head(hr_full)
str(hr_full)
#View(hr_full)

#Checking for NA values
sapply(hr_full, function(df)
{
  sum(is.na(df)==T)/length(df)
})
#No NA values in datasets

#checking for missing values
library("Amelia")
missmap(hr_full, main = "Missing Map")
#No missing values in datasets

#Encoding the target and response features as factor
hr_full$left <- as.factor(hr_full$left)


# Feature Scaling
hr_full[1:5] = scale(hr_full[1:5])

#Renaming sales column name since it is same as the sales row name
colnames(hr_full)[9] <- "Department"
glimpse(hr_full)

#Creating dummies for all the categorical variables

hr_full$Department = factor(hr_full$Department,levels = c('sales', 'technical', 'accounting', 'hr', 'IT', 'management', 'marketing', 'product_mng', 'RandD', 'support'),labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
hr_full$salary = factor(hr_full$salary, levels = c('high', 'low', 'medium'),labels = c(1, 2, 3))
hr_full$Work_accident = as.numeric(hr_full$Work_accident)
hr_full$promotion_last_5years = as.numeric(hr_full$promotion_last_5years)
glimpse(hr_full)

#Splitting data back to train and test data
hr_train = hr_full %>%
  filter(data == 'train') %>%
  select(-data)
str(hr_train)

hr_test = hr_full %>%
  filter(data == 'test') %>%
  select(-data)
str(hr_test)

#Splitting train data into train and validation set
library(caTools)
set.seed(390)
split <- sample.split(hr_train, SplitRatio = 0.8)
split

train.data <- subset(hr_train, split== "TRUE")
validation.data <- subset(hr_train, split== "FALSE")

str(train.data)
str(validation.data)

#Train model with logistics regression using glm function

logit_model=glm(left~.-Empid,family=binomial(link='logit'),data=train.data)
library(pedometrics)
stepVIF(logit_model, threshold = 5, verbose = FALSE)
#all predictor variables have a VIF lower than the threshold

logit_model1 <- glm(left ~ .,family=binomial(link='logit'),data = train.data)
summary(logit_model1)

logit_model1=step(logit_model1)

summary(logit_model1)

#AIC : 8243.7

formula(logit_model1)

logit_model2 <- glm(left ~ satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+salary, family=binomial(link='logit'), data = train.data)
summary(logit_model2)
formula(logit_model2)

anova(logit_model1, logit_model2, test = "Chisq")
anova(logit_model2, test = 'Chisq')

#Predicting on the validation set
fitted.results <- predict(logit_model2,newdata=validation.data,type='response')

#Changing probabilities to class (0 or 1/Yes or No)
# If prob > 0.5 then 1, else 0. Threshold can be set for better results
fitted.results <- ifelse(fitted.results > 0.5,1,0)

#Evauating Model Accuracy using Confusion matrix
library(caret)
confusionMatrix(table(validation.data$left, fitted.results))

library(ROCR)
ROCRPred <- prediction(fitted.results, validation.data$left)
ROCRPerf <- performance(ROCRPred, measure ="tpr", x.measure ="fpr")
par(mfrow = c(1, 1))
plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1),main = "ROC CURVE")
abline(a=0, b=1)

auc <- performance(ROCRPred, measure = "auc")
auc <- auc@y.values[[1]]
auc <- round(auc, 4)
legend (.6,.4,auc, title = "AUC", cex =1)

# Make predictions on the test set
my_prediction <- predict(logit_model2, hr_test, type = "response")

# If prob > 0.5 then 1, else 0. Threshold can be set for better results
my_prediction <- ifelse(my_prediction > 0.5,1,0)
my_solution <- data.frame(EmployeeId = hr_test$Empid, churn = my_prediction)
nrow(my_solution)
write.csv(my_solution, file = "my_solution_EmployeeChurn.csv", row.names = FALSE)
getwd()
