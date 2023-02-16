#Multiple Linear Regression

#Importing the data
data=read.csv('insurance.csv')
newdata=data[,c(-1)]

#Encoding categorical Variables
newdata$smoker=factor(newdata$smoker, levels=c('yes','no'),
                      labels=c(1,2))

newdata$sex=factor(newdata$sex, levels=c('male','female'),
                   labels=c(1,2))

newdata$region=factor(newdata$region, levels=c('southeast','southwest','northeast','northwest'),
                      labels=c(1,2,3,4))


#Transforming Age and BMI to fit a normal distribution
besttransform= function (datacolumn) {
  set.seed(123456)
  library(bestNormalize)
  BN_obj <- bestNormalize(datacolumn, allow_lambert_s = TRUE)
  # Perform transformation
  data_Transformed <- predict(BN_obj)
  return(list(data_Transformed = data_Transformed, BN = BN_obj)) 
}

newdatalr=newdata
output_bmi=besttransform(newdata$bmi)
newdatalr$bmi=output_bmi$data_Transformed
transformation=output_bmi$BN
transformation
shapiro.test(newdatalr$bmi)

output_age=besttransform(newdata$age)
transformation=output_age$BN
transformation
#Checking the p-value before transformation
shapiro.test(newdata$age)
#Checking the p-value after transformation
shapiro.test(output_age$data_Transformed)
newdatalr$age=output_age$data_Transformed

#Removing outliers from BMI
newdatalr=data[-c(117,848,1048,1318),]

#Test/Train split
set.seed(123456)
library(caTools)
split=sample.split(newdatalr$charges,SplitRatio=0.8)
training_set=subset(newdatalr,split==TRUE)
test_set=subset(newdatalr,split==FALSE)

#Creating a linear regression model. First we try to build it with all features.
regressor1=lm(charges~age+sex+smoker+bmi+children+region,training_set)
summary(regressor1)

#Eliminating features based on significance. Let's drop sex, since it is not significant.
regressor2=lm(charges~age+smoker+bmi+children+region,training_set)
summary(regressor2)

# Let's drop region and see whether adjusted R-squared increases or decreases. 
regressor3=lm(charges~age+smoker+bmi+children,training_set)
summary(regressor3)

#Evaluating model performance for regression models
regression_eval=function(reg_model,td)
{
  library(Metrics)
  RMSE=rmse(td$charges,predict(reg_model, newdata = td))
  NRMSE_minmax=rmse(td$charges,predict(reg_model, newdata = td))/(max(td$charges)-min(td$charges))
  return(c(RMSE,NRMSE_minmax))  
}

model_perf_main=regression_eval(regressor1,test_set)
model_perf_dropone=regression_eval(regressor2,test_set)
model_perf_droptwo=regression_eval(regressor3,test_set)
metric=c('RMSE','NRMSE_minmax')
Final=as.data.frame(cbind(metric,model_perf_main,model_perf_dropone,model_perf_droptwo))
colnames(Final)=(c('Metric','Regressor1','Regressor2','Regressor3'))
Final



