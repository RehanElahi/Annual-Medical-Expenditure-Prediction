#Exploratory Data Analysis 

#Importing data
data=read.csv('insurance.csv')
EDAdata=data[,-1]

#Summarizing data
summary(EDAdata)

#Finding out number of invalid and unique values
library(funModeling)
library (flextable)
flextable(status(EDAdata))

#Finding out the distribution for categorical variables in the data
library(DataExplorer)
plot_bar(data,title='Distributions of categorical variables',nrow=3,ncol=1)

#Finding out the distribution for numerical variables in the data
tempdata=EDAdata[,-7]
plot_histogram(tempdata, title='Distribution of numerical variables')

#Normality check for bmi
shapiro.test(EDAdata$bmi)

#Inspecting relationship between independent numerical variables and dependent variable
# Plot the relationship between bmi and charges
library(ggplot2)
ggplot(EDAdata, aes(x = bmi, y = charges)) +
  geom_point(size = 5, alpha = 0.5, color = "blue") +
  theme_classic() +
  labs(x = "BMI", y = "Charges")

library(ggplot2)
ggplot(EDAdata, aes(x = age, y = charges)) +
  geom_point(size = 5, alpha = 0.5, color = "blue") +
  theme_classic() +
  labs(x = "Age", y = "Charges")

#Correlation Matrix
model_matrix <- model.matrix(~0+., data=EDAdata)

# Plotting the correlation matrix
library(ggcorrplot)
model_matrix <- model.matrix(~0+., data=EDAdata)
cor_matrix <- cor(model_matrix, use="pairwise.complete.obs")
ggcorrplot(cor_matrix, show.diag = FALSE, type="lower", lab=TRUE, lab_size=2,
           colors = c("#1B9E77", "white", "#D95F02"))

#Outlier check
library(performance)
check_outliers(EDAdata$age, method = "iqr")
check_outliers(EDAdata$bmi, method = "iqr")

