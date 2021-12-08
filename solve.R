#set working directory 
setwd("E:/simplilearn/R Studio/Project/college addmision")
getwd()

#import csv file 
data1<-read.csv("College_admission.csv")
View(data1)
str(data1)

#find na value if any one is 
which(complete.cases(data1))
is.na(data1)
sum(is.na(data1))
#there is no missing values in this data

#find outlers in the data set if any 
boxplot(data1)

summary(data1)
IQR_gre=587.7-520
upfen_gre=220+1.5*IQR_gre
upfen_gre

check<-data1[,-2]
boxplot(check)
IQR_gpa=3.390-3.130
upfen_gpa=2.260+1.5*IQR_gpa
upfen_gpa

# after removing the outliers 
my_data=subset(data1,admit<=1 & gre>=321.55 & gpa>=2.65 & ses<=3 & Gender_Male<=1 & Race<=3 & rank<=4)
boxplot(my_data)

##find the structure of the my_data
str(my_data)
class(my_data$gpa)
#no need to change the structure of the data 

plot(my_data)
summary(my_data)

##spliting the data into the test and training set
#install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(my_data$gpa, SplitRatio = 0.75)
training_set = subset(my_data, split == TRUE)
test_set = subset(my_data, split == FALSE)


# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting Kernel SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = gpa ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
regressor = rpart(formula = gpa ~ .,
                  data = my_data,
                  control = rpart.control(minsplit = 1))

# Predicting a new result with Decision Tree Regression
y_pred = predict(regressor, data.frame(Level = 6.5))


# Visualising the Decision Tree Regression results (higher resolution)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(my_data$gre), max(my_data$gpa), 0.01)
ggplot() +
  geom_point(aes(x = my_data$gre, y = my_data$gpa),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('gre') +
  ylab('gpa')

plot(regressor)


# Fitting SVR to the dataset
# install.packages('e1071')
library(e1071)
regressor = svm(formula = gpa ~ .,
                data = my_data,
                type = 'eps-regression',
                kernel = 'radial')

# Predicting a new result
y_pred = predict(regressor, data.frame(my_data,Level = 6.5))


# Visualising the SVR results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = my_data$gre, y = my_data$gpa),
             colour = 'red') +
  geom_line(aes(x = my_data$gre, y = predict(regressor, newdata = my_data)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (SVR)') +
  xlab('gre') +
  ylab('gpa')



# Visualising the SVR results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(my_data$gre), max(my_data$gre), 0.1)
ggplot() +
  geom_point(aes(x = my_data$gre, y = my_data$gpa),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(gre = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (SVR)') +
  xlab('gre') +
  ylab('gpa')

