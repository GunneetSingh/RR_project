
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

ggplot(data = data, aes(class))+
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
