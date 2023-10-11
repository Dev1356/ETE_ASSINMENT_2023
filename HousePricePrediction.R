rm(list=ls(all=T))
library(dplyr)
library(glmnet)
library(imputeTS)
library(mice)
library(stats)
library(cluster)
library(factoextra)
library(caret)
library(DMwR2)
library(car)
library(tseries)
library(rpart)
library(rpart.plot)
D=read.csv("C:\\Users\\Shiv\\Documents\\data mining\\Assingment 1\\Houses\\Delhi.csv")
dim(D)
data.frame(colSums(is.na(D)))
colnames(D[,-1])
D[D==9]=NA
View(D)
D1=D[,-3]
D2=D[,-c(1:5)]
View(subset(updated_data2,Area < 700 & No..of.Bedrooms >3))



########## mice function
dataSet=D1[,-(1:4)]
head(dataSet)
mice_mdl=mice(dataSet,m=5,method=c("logreg.boot", "logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot"
                                   ,"logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot", "logreg.boot"
                                   ,"logreg.boot", "logreg.boot", "logreg.boot", "logreg.boot" ,"logreg.boot", "logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot"
                                   ,"logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot" ,"logreg.boot"))
mice_imptd=complete(mice_mdl,1)


updated_data1=cbind(D[,1:5],mice_imptd)
par(mfrow=c(1,2))
hist(updated_data1$Gymnasium,main = "Impuatated Data")
hist(D$Gymnasium,main="Missing Val Data")
#################################################

Table=(table(updated_data1$Location))
locations_to_replace <- names(Table[Table<11])

# Replace those locations with a new name, e.g., "Other"
updated_data1$Location[updated_data1$Location %in% locations_to_replace] <- "Other"
length(unique(updated_data1$Location))



############### Outlier detection and Removal 

model_outler=lm(Price~ ., data = updated_data1)

# Detect outliers using studentized residuals
outliers= abs(rstudent(model_outler)) > 1  # Adjust the threshold as needed


# Remove outliers from the dataset
D_clean=updated_data1[!outliers, ]
par(mfrow=c(1,3))
boxplot(D$Price,main="fig 1")
boxplot(D_clean$Price,main="fig 2")
dim(D_clean)

############## another method cook's distance for outlier
cooksmodel=lm(Price~.,data=D_clean)
cooksd =cooks.distance(cooksmodel)

# Set a threshold for Cook's Distance (e.g., 4 times the mean)
cook_threshold = 3* mean(cooksd)

# Identify and remove outliers
D_clean2 =D_clean[cooksd <= cook_threshold, ]
dim(D_clean2)
boxplot(D_clean2$Price,main="fig 3")
updated_data2=D_clean2
View(updated_data2)


############################    Variable selection method
library(leaps)
breg_full = regsubsets(Price ~ .-Location, data = updated_data2,really.big = T,nvmax =38 )
breg_summary = summary(breg_full)
breg_summary$rsq
par(mfrow=c(2,2))
plot(breg_summary$rsq,main="R-squred Plot",xlab="No of Variables",ylab="R-square Values",type="l",xlim=c(0,40))
plot(breg_summary$cp,main="cp plot",xlab="No of Variables",ylab="Cp Values",type="l",xlim=c(0,40))
plot(breg_summary$bic,main="BIC Plot",xlab="No of Variables",ylab="BIC Values",type="l",xlim=c(0,40))
plot(breg_summary$adjr2,main="adj R-squred Plot",xlab="No of Variables",ylab="adj R-square Values",type="l",xlim=c(0,40))



############ Variable selection
Var_M=lm(Price~.,data=updated_data2[,-3])
Var_select1=step(Var_M,direction = "both")
Sel_cof1=names(Var_select1$coefficients)
Sel_cof1=Sel_cof1[-1]
Sel_cof1
Var_data=updated_data2[,Sel_cof1]
summary(Var_select1)


############# dummy enconding for var selction 
Location1=updated_data2$Location
df11 <- dummyVars("~.", data=updated_data2)
df22 <- data.frame(predict(df11, newdata =updated_data2))
df22=df22[,c(1,3:102)]
dim(df22)
updated_data_var=cbind(Var_data,df22[,-67])
j=step(lm(Price~.,data=updated_data_var),direction = "both")
updated_data3=cbind(D_clean2,df22[,-67])
updated_data3=updated_data3[,-c(3,41)]
View(updated_data3)

summary(lm(D_clean2$Price~.,data = updated_data3))

############### Multiple Linear Regrssion
n=nrow(updated_data_var)
outputmatrix=matrix(,nrow=4,ncol=2)
k=5

####
Train_vector=Validaton_vector=c()
for(i in 1:k)
{
  sample_data=sample(1:n,size=n,replace = FALSE)
  Folds=matrix(sample_data,ncol=k)
  FirstFold=Folds[,i]
  validation_data=updated_data_var[FirstFold,]
  train_data=updated_data_var[-FirstFold,]
  M1=lm(Price~.,data=train_data)
  Train_vector[i]=(anova(M1)$"Sum Sq"[117])/nrow(train_data)
  Predictor_1=predict(M1,newdata=validation_data)
  Validaton_vector[i]=mean((validation_data$Price-Predictor_1)^2)
}
outputmatrix[1,1]=mean(Validaton_vector)
outputmatrix[1,2]=cor(Predictor_1,validation_data$Price)
outputmatrix
plot(Predictor_1,validation_data$Price)



######################### k folds in decision trees 
n1=dim(D_clean2)[1]
Train_vector1=Validaton_vector1=c()
for(i in 1:k)
{
  sample_data1=sample(1:n1,size=n1,replace = FALSE)
  Folds1=matrix(sample_data1,ncol=k)
  FirstFold1=Folds1[,i]
  validation_data1=D_clean2[FirstFold1,]
  train_data1=D_clean2[-FirstFold1,]
  M2=rpart(Price~.,data=train_data1,method="anova",cp=0.0001)
  Train_vector1[i]=mean((train_data1$Price-(predict(M2,newdata=train_data1)))^2)
  Predictor_2=predict(M2,newdata=validation_data1)
  Validaton_vector1[i]=mean((validation_data1$Price-Predictor_2)^2)
}
outputmatrix[2,1]=mean(Validation_vector1)
outputmatrix[2,2]=cor(Predictor_2,validation_data2$Price)
outputmatrix
(M2$variable.importance)
rpart.plot(M2)
plot(Predictor_2,validation_data1$Price)
abline(0,1)

###################  Random forest 

library(randomForest)
randomForest_sample=sample(1:n1,size=n1/3,replace = FALSE)
validation_data_forest=D_clean2[randomForest_sample,]
train_data_forest=D_clean2[-randomForest_sample,]
M3=randomForest(Price~.,data=train_data_forest,importance=TRUE)
MSE_Train=mean((train_data_forest$Price-(predict(M3,newdata=train_data_forest)))^2)
Predictor_forest=predict(M3,newdata=validation_data_forest)
MSE_Validation=mean((validation_data_forest$Price-Predictor_forest)^2)

outputmatrix[3,1]=(MSE_Validation)
outputmatrix[3,2]=cor(Predictor_forest,validation_data_forest$Price)
outputmatrix


################################## KNN 
n2=dim(updated_data_var)[1]
sample_data_knn=sample(1:n2,size=n2/3,replace = FALSE)
validation_data_knn=updated_data_var[sample_data_knn,]
train_data_knn=updated_data_var[-sample_data_knn,]
ctrl <- trainControl(method = "cv", number = 10)
model_knn <- train(Price~., data = train_data_knn, method = "knn", trControl = ctrl)
model_knn
MSE_Train_knn=mean((train_data_knn$Price-(predict(model_knn,newdata=train_data_knn)))^2)
Predictor_4=predict(model_knn,newdata=validation_data_knn)
MSE_val_knn=mean((validation_data_knn$Price-Predictor_4)^2)

outputmatrix[4,1]=MSE_val_knn
outputmatrix[4,2]=cor(Predictoe_4,validation_data_knn$Price)
outputmatrix
plot(model_knn)
qqnorm(Predictor_4-validation_data_knn$Price)
qqline(Predictor_4-validation_data_knn$Price)
