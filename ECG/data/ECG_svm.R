#install.packages("e1071")
library("e1071")

#install.packages("tree")
library("tree")


train_data <- read.csv("proc_data_train.csv")
train_labels <- read.csv("proc_label_train.csv")
test_data <- read.csv("proc_data_test.csv")
test_separation <- read.csv("proc_datasep_test.csv")

set.seed(5)
shuffle <- sample(nrow(train_data))
train_data <- train_data[shuffle,]
train_labels <- train_labels[shuffle,]

dataset <- data.frame(train_data,train_labels[,1])
dataset$train_labels = factor(dataset$train_labels, levels = c(0, 1,2))



[ , namevector] <- NA


model <- svm(formula = train_labels ~ .,data = dataset, kernel='radial',cost=10)
summary(model)
predictions <- predict(model, test_data)

actual <- vector()

for (i in 2:length(test_separation)) {
  vec <- predictions[(i-1):i]
  if (length(which(vec==0))>length(which(vec==1)) & length(which(vec==0))>length(which(vec==2))) {
    actual <- c(actual,0)
  } else if (length(which(vec==1))>=length(which(vec==0)) & length(which(vec==1))>length(which(vec==2))) {
    actual <- c(actual,1)
  } else {
    actual <- c(actual,2)
  }
}

