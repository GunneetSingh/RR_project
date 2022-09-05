#############################################
#############################################
####### Task 2 Replication_Part  ############
#############################################
#############################################


# Loading the necessary Packages 

install.packages('caTools')
install.packages('class')
install.packages('ggplot2')
install.packages('reshape2')
install.packages('caret')
install.packages('mlbench')
install.packages('e1071')
library(caTools)
library(class)
library(ggplot2)
library(reshape2)
library(caret)
library(mlbench)
library(e1071)


# Loading the dataset and cheking the few rows.
data <- read.csv(file.choose())


head(data)
# our data looks a bit more complex with no clear headers assigned
# lets make handle it first.
# Adding headers to data to increase readability.
colnames(data) <- c('sample_code_number', 
                    'clump_Thickness', 
                    'uniformity_of_cell_size',
                    'uniformity_of_cell_shape',
                    'marginal_adhesion',
                    'single_epithelial_cell_size',
                    'bare_nuclei',
                    'bland_chromatin',
                    'normal_nucleoli',
                    'mitoses',
                    'class')


# checking structure of dataset
summary(data)

# Piloting the Class variable to see the distribution. 

ggplot(data = data, aes(class, fill = factor(class)))+
  geom_bar()

# So Benign cases is dominant here with around 65.5% which is 458 cases in total
# Malignant class is second with 34,5 % and in numbers 241 cases 

# Converting bare_nuclei from character datatype to int.
data$bare_nuclei <- as.integer(data$bare_nuclei)

# Dropping all na values from the dataset 
data <- na.omit(data)

# Changing class variable to binary 0 and 1 
data$class <- ifelse(data$class == 2,0,1)

# doing train test split
set.seed(100)
split <- sample.split(data)
train <- subset(data, split == 'TRUE')
train_features <- train[-11]
test <- subset(data, split == 'FALSE')
test_features <- test[-11]


# Applying Logistic Regression Model
log_model <- glm(class ~ ., data = train, family = 'binomial')

summary(log_model)

# Generating the Prediction for the model

predict_reg <- predict(log_model, 
                       test_features, type = "response")
predict_reg  

predict_reg <- ifelse(predict_reg >0.5, 1, 0)

table(test$class, predict_reg)

# Finalizing Accuracy 

missing_classerr <- mean(predict_reg != test$class)
print(paste('Accuracy =', 1 - missing_classerr))

# Accuracy is around 96 % which is high and was not utilized in the original study



####### KNN model   ##########

# Checking feature importance 

control <- trainControl(method="repeatedcv", number=10, repeats=3)

# Train the model

model <- train(class~., data=train, method="knn", preProcess="scale", trControl=control)

# Estimating variable importance

importance <- varImp(model, scale=FALSE)

# Summarizing importance

print(importance)

# plot importance

plot(importance)

# Checking which K value fits bets for our dataset.
for (k in c(1:10)) {
  # Creating K-Nearest Neighbor Model...
  knn_model <- knn(train = train_features[-10][-1],test = test_features[-10][-1], cl = train$class, k = k)
  # Confusion Matrix of KNN...
  cm <- table(test$class, knn_model)
  print(cm)
  
  # Accuracy of KNN..
  misClassError <- mean(knn_model != test$class)
  precision <- cm[2,2]/(cm[2,2]+cm[2,1])
  recall <- cm[2,2]/(cm[2,2]+cm[1,2])
  print(paste('k = ',k,' Accuracy =', 1-misClassError, ' precision = ',precision, ' recall = ', recall))
} 

# Creating K-Nearest Neighbor Model

knn_model <- knn(train = train_features[-10][-1],test = test_features[-10][-1], cl = train$class, k = 5 )

# Confusion Matrix of KNN.

cm <- table(test$class, knn_model)

# Accuracy of KNN..
precision <- cm[2,2]/(cm[2,2]+cm[2,1])
recall <- cm[2,2]/(cm[2,2]+cm[1,2])
print(paste(' Accuracy =', 1-misClassError, ' precision = ',precision, ' recall = ', recall))

# Using feature selection technique has improved accuracy of the model significantly 
# in comparison Reproduction of the original study.
# Further more precision and recall have a high positive change 

# Using Naive Bayes Algorithm

nb_model <- naiveBayes(class ~ ., data = train)
nb_model

prediction <- predict(nb_model, newdata = test[-11])

cm <- table(test$class, prediction)
cm

# Accuracy of Naive Bayes

correct_values <- cm[1,1]+cm[2,2]
total_values <- correct_values + cm[1,2] + cm[2,1]

acc <- correct_values/total_values
print(paste('Accuracy = ', acc))

# Accuracy is around 97 % which is almost equal to the one in Original Study.


#   Area of improvements
# - Used Cross Validation Techniques and Feature selection techniques which improved our results drastically.
# - For KNN model Not only accuracy improved but precision and recall took a major leap.
#   Suggestion
# - It is also suggested to use Logistic regression for this problem as that ML model also give good stats in this case.
# - Better if we use KNN model here with features selected for it. For Naive Bayes feature selection haven't put any change here

