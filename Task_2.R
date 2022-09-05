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
