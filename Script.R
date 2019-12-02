#loading necessary packages
library(tidyverse)
library(caret)

#loading in data
creditcard <- read_csv("dat/training_set.csv")

#getting a look at the data
nrow(creditcard) #284807 different data points
sum(creditcard$Class) #492 are found to be fradulent
summary(creditcard)
#the variables are tranformed through PCA for privacy
#Time = amount of time since first transaction in data set
#Amount = amount of transaction

#more focused on correctly detecting fraud than legitimate
(nrow(creditcard) - sum(creditcard$Class))/nrow(creditcard)
#If just purely guessing class as not fraud, accuracy is 0.9982725
# Maybe F1 score is best here. Sensitivity is important but,
# too many false positives can also be detrimental. So reduce F1

#### Creating training and validation data set
folds <- cut(seq(1, nrow(creditcard)), breaks = 10, labels = FALSE)
set.seed(86)
validationindex <- sample(folds == 1, length(folds))
validation <- creditcard[validationindex,]
train <- creditcard[-validationindex,]
rm(folds, validationindex, creditcard) #removing unnecessary items

####setting up calculator for F1
F1 <- function(TP, FP, FN){
  (2*TP/(2*TP + FP + FN))
}


#ways of approaching
#logistic regression

#Taking the Z scores of the data
mean_dat <- sweep(train[,1:30], 2, colMeans(train[,1:30]))
z_dat <- sweep(mean_dat, 2, apply(mean_dat, 2, sd), FUN = "/")
z_dat <- cbind(z_dat, train[,31])

#running logistic regression on the Z scores
model <- glm(Class ~ ., data = z_dat, family = binomial)
model_coefficient <- data.frame(summary(model)$coefficients)

#Taking out variables that are unimportant
sig_coefficients <- model_coefficient[model_coefficient$Pr...z.. <= 0.01,]

##running glm again with only the sig variables
Pattern <- paste(c(rownames(sig_coefficients), "Class"), collapse = "|")
index <- str_detect(colnames(z_dat), Pattern)


model_sig_variables <- glm(Class ~., data = z_dat[,index])
sigvar_coefficients <- data.frame(summary(model_sig_variables)$coefficients)


## creating predictions based on the estimates
Pattern <- paste(rownames(sigvar_coefficients), collapse = "|")
index <- str_detect(colnames(test), Pattern)

pred <- apply(validation, 1, function(x){
  beta <- sum(x[index]*sigvar_coefficients$Estimate[-1]) + sigvar_coefficients$Estimate[1]
  ifelse(beta > 700, beta <- 700, beta) 
  exp(beta)/(1+exp(beta))
})


y_hat <- map_dbl(pred, function(x){ifelse(x > 0.52, 1, 0)} )
table(y_hat, true = validation$Class)
F1(44, 13, 9)



#knn
#taking a look at the data with different plots
ggplot(data = train, aes(V1, V2)) + 
  geom_point(alpha = 0.5) + 
  geom_point(data = train %>% filter(Class == 1),  aes(V1, V2), colour = "Red")


ggplot(data = train, aes(V3, V4)) + 
  geom_point(alpha = 0.5) + 
  geom_point(data = train %>% filter(Class == 1),  aes(V3, V2), colour = "Red")

ggplot(data = train, aes(V27, V28)) + 
  geom_point(alpha = 0.5) + 
  geom_point(data = train %>% filter(Class == 1),  aes(V27, V28), colour = "Red")


#we'll be using knn3 from the caret package for our knn analysis
#defining the matrix of predictors and outcome
predictor_mat <- as.matrix(train[,1:30])
outcome <-factor(train$Class, levels = c(1, 0), ordered = TRUE)


knn_fit <- knn3(predictor_mat, outcome, k = 3)
y_hat_knn <- predict(knn_fit, as.data.frame(validation[,1:30]), type = "class")
confusionMatrix(data = y_hat_knn, reference = factor(validation$Class, levels = c(1, 0), ordered = TRUE))


