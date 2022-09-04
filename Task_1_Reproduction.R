
## Reproducible Research ##

## Reproducibilty of the original study ##

## Installing necessary libraries

install.packages('e1071')
library(e1071)
install.packages('caTools')
library(caTools)
install.packages('class')
library(class)
library(ggplot2)

## Loading the dataset

data <- read.csv(file.choose(), header = FALSE)

## Adding headers to the columns

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

## Checking shape of data

dim(data)

# The dataset has 699 observations for 11 different features

## Checking structure of dataset

str(data)

# Out of the total 11 variables, only the seventh column named bare_nuclei has character 
# type values, while all others have integers.

## Target count chart

ggplot(data = data, aes(class, fill = factor(class)))+
  geom_bar()

# Clearly, the proportion of Benign cases is 65.5% corresponding to 458 of the total, whereas
# the rest 34.5%(241) belongs to Malignant class.

## Converting bare_nuclei from character datatype to integer

data$bare_nuclei <- as.integer(data$bare_nuclei)

# Before going further, we have to remove the missing values present in our data.

## Dropping all NA values

data <- na.omit(data)

dim(data)

# Clearly, there were missing values in 16 rows, which were removed from the dataset for 
# further analysis.

## Train-Test data split

set.seed(123)

split <- sample.split(data)   # random sampling of the data

train <- subset(data, split == 'TRUE')
test <- subset(data, split == 'FALSE')

## Creating K-Nearest Neighbor model

# We do not need the eleventh variable named Class for building the models

knn_model <- knn(train = train[-11],test = test[-11], cl = train$class, k = 3)

## Confusion Matrix of KNN

cm1 <- table(test$class, knn_model)
cm1

## Checking the accuracy of KNN

misClassError <- mean(knn_model != test$class)
print(paste('Accuracy =', 1-misClassError))

# Surely, the accuracy is not good enough here for any sort of predictions or further 
# analysis. However, what is even more interesting is the fact that in the research paper, the
# accuracy corresponding to KNN model is around 97% which is significantly higher than what we
# got here.

# The reason behind this stark difference has to be the unclarity in the article regarding
# various things including the exact proportion chosen for the modeling purposes, or any 
# additional tuning of the parameters of the algorithm.

## Naive Bayes Algorithm

nb_model <- naiveBayes(class ~ ., data = train)
nb_model

## Generating the predicts based on NB model

prediction <- predict(nb_model, newdata = test[-11])

## Confusion matrix based on the "predictions"

cm2 <- table(test$class, prediction)
cm2

## Assessing the accuracy of Naive Bayes

correct_values <- cm[1,1]+cm[2,2]
total_values <- correct_values + cm[1,2] + cm[2,1]

acc <- correct_values/total_values
print(paste('Accuracy = ', acc))

# Interestingly enough, the accuracy of the NB algorithm comes out to be really good. Even 
# though it is not the same as that in the original study, it surely is very close to the 
# actual number. This means that the unavailability of proper description about the use of
# algorithm does not have a huge implication here in comparison with the KNN model.




