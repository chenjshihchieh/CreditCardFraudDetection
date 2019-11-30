#loading in necessary library
library(tidyverse)

##Creating the testing and training data

#loading in data
dat <- read_csv("dat/creditcardfraud_data.csv")

# Creating training and testing data set
folds <- cut(seq(1, nrow(dat)), breaks = 10, labels = FALSE)
set.seed(35)
testindex <- sample(folds == 1, length(folds))
test <- dat[testindex,]
train <- dat[-testindex,]
rm(folds, testindex) #removing unnecessary items

#Saving the training and testing sets
write.csv(train, "dat/training_set.csv", row.names = FALSE)
write.csv(test, "dat/testing_set.csv", row.names = FALSE)
