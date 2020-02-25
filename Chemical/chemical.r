library(MASS)
library(randomForest)
library(ridge)

MSE <- function(truth, pred){
  n.truth <- length(truth)
  return((sum((truth-pred)^2))/n.truth)
}

ClassificationError <- function(truth, pred){
  n.incorrect <- length(which(validationset.data.y != y_pred))
  n.truth <- length(truth)
  return(n.incorrect/n.truth)
}

#read data
setwd("C:\\Users\\Jane\\Desktop\\statml assignment\\StatML_group_f\\Chemical")
training.data <- read.csv("data\\training_set.csv")
test.data <- read.csv("data\\test_set.csv")

#centering and scale training data set
training.data.scaled <- as.data.frame(scale(training.data[-c(1,2)]))
training.data <- cbind(training.data[c(1,2)], training.data.scaled)

#centering and scale testing data set
test.data <- as.data.frame(scale(test.data))

#head(training.data)
#plot(training.data$Impurity.Percent, training.data$I)


#shuffle data
set.seed(0)
rows = sample(nrow(training.data))
training.data = training.data[rows,]

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

trainset.data.x <- trainset.data[-2]
trainset.data.y <- trainset.data$Impurity.Type

validationset.data.x  <- validationset.data[-2]
validationset.data.y  <- validationset.data$Impurity.Type

method <- "mlr"
y.hat <- c()
reg_error <- 0

if(method == "mlr"){
  #training multiple linear regression
  mlr.model <- lm(perc ~ var1 + var3 + var4 + var5)
  summary(mlr.model)
  
  #regression validation
  validation_pred <- predict(mlr.model, list(var1=valid_var1,
                                             var3=valid_var3, 
                                             var4=valid_var4, 
                                             var5=valid_var5))
  reg_error <- MSE(valid_perc, validation_pred)
  
  #regression prediction on test set
  y.hat <- predict(mlr.model, list(var1=test_var1,
                                   var3=test_var3, 
                                   var4=test_var4, 
                                   var5=test_var5))
  #plot(mlr.model)
}else{
  #ridge regression
  ridge.model <- linearRidge(perc ~ var1 + var2 + var3 + var4 + var5)
  summary(ridge.model)
  
  validation_pred <- predict(ridge.model, list(var1=valid_var1,
                                               var2=valid_var2,
                                             var3=valid_var3, 
                                             var4=valid_var4, 
                                             var5=valid_var5))
  reg_error <- MSE(valid_perc, validation_pred)
  
  y.hat <- predict(ridge.model, list(var1=test_var1,
                                     var2=test_var2,
                                   var3=test_var3, 
                                   var4=test_var4, 
                                   var5=test_var5))  
}

#index of pure chemical in test data set
pure_chemical_index <- which(y.hat<1.8)

#impurity type classification

# random forest for classification 
rf = randomForest(x = trainset.data.x, y = trainset.data.y, ntree = 50)

#classification validation
y_pred = predict(rf, newdata = validationset.data.x)
cls_error <- ClassificationError(validationset.data.y, y_pred)

#classification prediction on test set
final_rf_class = randomForest(x = training.data[-2], y = training.data$Impurity.Type,ntree = 50)
Impurity.Percent <- y.hat
new.data <- cbind(Impurity.Percent, test.data)
g.hat = predict(final_rf_class, newdata = new.data)

#classify item with impurity percentage below 1.8 as chemical type X
#adding 'X' as factor for g.hat class, otherwise adding 'x' to data will fail
g.hat <- `levels<-`(g.hat, c(levels(g.hat), "X"))
g.hat[pure_chemical_index] <- "X"

#performance metric
tot_error <- reg_error + cls_error

#combining regressiong and classification result
g.hat <- as.character(g.hat)
result <- cbind(g.hat,y.hat)

#output to required csv format
write.csv(result, file = "chemical_predictions_group_F_week_1.csv", row.names=FALSE)
