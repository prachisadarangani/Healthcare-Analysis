#loading data and creating a dataframe 
traindata<-read.csv(“https://intro-datascience.s3.us-east- 2.amazonaws.com/HMO_data.csv (https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv)”) View(traindata) str(traindata) sum(is.na(traindata)) library(imputeTS)
#remove 80 rows with hypertension with na
traindata <- traindata[!is.na(traindata
as expensive traindata 0.75),1,0)
traindata
traindata
location_type) traindata education_level) traindata
yearly_physical)) traindata
exercise) traindata gender) traindata
isexpensive)
hypertension),]
#na interpolation of bmi traindata
bmi <- na_interpolation(traindata
cost) #mark all records with cost more than 78 percentile
cost > quantile(traindata$cost, probs =
smoker==“yes”,1,0) traindata exercise==“Active”,1,0) #traindata
yearly_physical==“Yes”,1,0) #traindata married==“Married”,1,0) #traindata
gender==“female”,1,0) #traindata location_type==“Urban”,1,0) #traindata
education_level)) #traindata location))
#use lm with smoker,exercise,age,bmi,hypertension lm1 <- lm(formula = cost ~ issmoker+isexercise+age+bmi+hypertension ,data = traindata) summary(lm1)
library(kernlab) library(caret)
#creating factors for categorical variables traindata location) traindata
smoker) traindata
married)
trainList <- createDataPartition(y=traindata$isexpensive,p=.60,list=FALSE) trainSet <- traindata[trainList,] testSet <- traindata[-trainList,]
SVM_1 <- ksvm(isexpensive ~ age+bmi+smoker+exercise+hypertension, data=trainSet,C = 8,cross = 3, prob.model = TRUE) SVM_1
svmpredict <- predict(SVM_1, newdata=testSet, type = “response”)
Confusion matrix with test set data CM <- confusionMatrix(svmpredict, testSet$isexpensive) CM
