#Support Vector Regression

#Importing dataset
data=read.csv('insurance.csv')
newdata=data[,-c(1)]

#Encoding categorical data
newdata$smoker=factor(newdata$smoker, levels=c('yes','no'),
                      labels=c(1,2))

newdata$sex=factor(newdata$sex, levels=c('male','female'),
                   labels=c(1,2))

newdata$region=factor(newdata$region, levels=c('southeast','southwest','northeast','northwest'),
                      labels=c(1,2,3,4))

#Test/Train
library(caTools)
set.seed(1)
split=sample.split(newdata$charges,SplitRatio=0.8)
training_set=subset(newdata,split==TRUE)
test_set=subset(newdata,split==FALSE)

#Feature Scaling
training_set[,c(1,3)]=scale(training_set[,c(1,3)])
test_set[,c(1,3)]=scale(test_set[,c(1,3)])
#Drop features
#training_set=training_set[,-c(2)]
#test_set=test_set[,-c(2)]

#Support Vector regression
library(e1071)
regressordefault=svm(charges~.,training_set,type='eps-regression', kernel='radial')
summary(regressordefault)
preddefault <- as.data.frame(predict(regressordefault, test_set))

#RMSE and R-squared of the default model
library(caret)
print(postResample(preddefault, test_set$charges))


#Optimizing hyperparameters
#Preparing data for cross validation
newdata[,c(1,3)]=scale(newdata[,c(1,3)])
#Drop features
#newdata1=newdata[,-c(2)]

#Define the number of folds
k <- 5

#Define the hyperparameter grid
grid <- expand.grid(C = c(0.1, 1, 10),
                    gamma = c(0.1, 1, 10),
                    sigma = c(0.1, 1, 10),
                    epsilon = c(0.01, 0.1, 1))

#Divide the data into k folds
folds <- createFolds(newdata1$charges, k = k)

#Perform a grid search to find the best hyperparameters using k-fold cross-validation
best_params <- data.frame(RMSE = numeric(0))
for (i in 1:nrow(grid)) {
  params <- grid[i, ]
  rmse_sum <- 0
  for (j in 1:k) {
    #Get the indices of the jth fold
    test_indices <- folds[[j]]
    train_indices <- unlist(folds[-j])
    
    #Get the training and test sets for the jth fold
    train_set_j <- newdata1[train_indices, ]
    test_set_j <- newdata1[test_indices, ]
    
    #Train the model on the training set
    model <- svm(charges ~ ., data = train_set_j,
                 type = "eps-regression",
                 kernel = "radial",
                 cost = params$C,
                 gamma = params$gamma,
                 sigma = params$sigma,
                 epsilon = params$epsilon)
    
    #Make predictions on the test set
    pred <- predict(model, test_set_j)
    
    #Calculate the RMSE for the jth fold
    rmse_j <- sqrt(mean((test_set_j$charges - pred)^2))
    rmse_sum <- rmse_sum + rmse_j
  }
  #Calculate the average RMSE over all folds
  rmse <- rmse_sum / k
  rsquared <- cor(test_set_j$charges, pred) ^ 2
  best_params <- rbind(best_params, data.frame(RMSE = rmse, rsquared,params))
}

#Find the best hyperparameters with the lowest average RMSE
best_params <- best_params[order(best_params$RMSE), ]
best_params