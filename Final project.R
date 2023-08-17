install.packages("tidyverse")
installed.packages("class")
install.packages("corrplot")
install.packages("Hmisc")
install.packages("caTools")
install.packages("caret")
library(tidyverse)
library(class)
library(corrplot)
library(Hmisc)
library(caTools)
library(caret)

mydata <- read.csv("C:/Users/USER/Desktop/Data Science/Final project/loan_approval_dataset.CSV", header = TRUE, sep = ",")
mydata
summary(mydata)
colSums(is.na(mydata))
#Remove loan_id
remove_loan_id <- "loan_id"
mydata_filtered <- mydata[, !names(mydata) %in% remove_loan_id]
mydata_filtered

mydata_filtered_final <- mydata_filtered 
mydata_filtered_final$education <- as.numeric(as.factor(mydata_filtered_final$education))
mydata_filtered_final$self_employed <- as.numeric(as.factor(mydata_filtered_final$self_employed))
mydata_filtered_final$loan_status <- as.numeric(as.factor(mydata_filtered_final$loan_status))
mydata_filtered_final


scale(mydata_filtered_final)

res2 <- rcorr(as.matrix(mydata_filtered_final))
res2


correlation_matrix <- cor(mydata_filtered_final[, sapply(mydata_filtered_final, is.numeric)])
corrplot(correlation_matrix, method = "color")

set.seed(255)
split <- sample.split(mydata_filtered_final$loan_status, SplitRatio = 0.8)
train <- subset(mydata_filtered_final, split == TRUE)
test <- subset(mydata_filtered_final, split == FALSE)

train_scaled <- scale(train[, -13]) 
test_scaled <- scale(test[, -13])    

k_value <- 10

fold_pred <- knn(
  train = train_scaled, 
  test = test_scaled,
  cl = train$loan_status, 
  k = k_value
)

conf_matrix <- confusionMatrix(as.factor(fold_pred), as.factor(fold_val$loan_status))
print(conf_matrix$table)

accuracy <- sum(diag(cm)) / sum(cm)
sprintf("Accuracy: %.2f%%", accuracy * 100)
print(cm)

num_folds <- 10
fold_accuracies <- numeric(length = num_folds)

for (fold in 1:num_folds) {
  fold_indices <- sample(1:nrow(train), size = floor(1 / num_folds * nrow(train)))
  fold_train <- train[-fold_indices, ]
  fold_val <- train[fold_indices, ]
  
  fold_train_scaled <- scale(fold_train[, -13])
  fold_val_scaled <- scale(fold_val[, -13])
  
  fold_pred <- knn(
    train = fold_train_scaled, 
    test = fold_val_scaled,
    cl = fold_train$loan_status, 
    k = k_value
  )
  #cm_with_cv <- confusionMatrix(data=fold_pred, reference=as.factor(fold_val$loan_status))
  
  #print(cm_with_cv)  
  
  conf_matrix <- confusionMatrix(as.factor(fold_pred), as.factor(fold_val$loan_status))
  print(conf_matrix$table)
  fold_accuracy <- sum(diag(cm)) / sum(cm)
  
  fold_accuracies[fold] <- fold_accuracy
}

average_accuracy <- mean(fold_accuracies)

sprintf("Average Accuracy across %d folds: %.2f%%", num_folds, average_accuracy * 100)
cm_with_cv

