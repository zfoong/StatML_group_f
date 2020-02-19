MSE <- function(truth, pred){
  return(sum((truth-pred)^2))
}

#read data
setwd(getwd())
training.data <- read.csv("Chemical\\data\\training_set.csv")
test.data <- read.csv("Chemical\\data\\test_set.csv")

#head(training.data)
#plot(training.data)

#cross-validation
set.seed(0)
sample <- sample.int(n = nrow(training.data), size = floor(.80*nrow(training.data)), replace = F)
trainset.data <- training.data[sample, ]
validationset.data  <- training.data[-sample, ]


#rename variable
perc <- trainset.data$Impurity.Percent
var1 <- trainset.data$I
var2 <- trainset.data$II
var3 <- trainset.data$III
var4 <- trainset.data$IV
var5 <- trainset.data$V
temp <- trainset.data$Temp

valid_perc <- validationset.data$Impurity.Percent
valid_var1 <- validationset.data$I
valid_var2 <- validationset.data$II
valid_var3 <- validationset.data$III
valid_var4 <- validationset.data$IV
valid_var5 <- validationset.data$V
valid_temp <- validationset.data$Temp

test_var1 <- test.data$I
test_var2 <- test.data$II
test_var3 <- test.data$III
test_var4 <- test.data$IV
test_var5 <- test.data$V
test_temp <- test.data$Temp

qd.model <- lm(perc ~ var1 + var2 + var3 + var4 + var5 + temp)
summary(qd.model)

#validation
validation_pred <- predict(qd.model, list(var1=valid_var1, var2=valid_var2, var3=valid_var3, var4=valid_var4, var5=valid_var5, temp=valid_temp))
error <- MSE(valid_perc, validation_pred)

#prediction on test set
prediction <- predict(qd.model, list(var1=test_var1, var2=test_var2, var3=test_var3, var4=test_var4, var5=test_var5, temp=test_temp))
#plot(qd.model)

#index of pure chemical in test data set
pure_chemical_index <- which(prediction<1.8)
