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
setwd("C:\\Users\\zfoong\\Desktop\\statml assignment\\StatML_group_f\\Chemical")
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
set.seed(10)
rows = sample(nrow(training.data))
training.data = training.data[rows,]

#cross-validation
set.seed(10)
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
train_mean <- rowMeans(trainset.data[3:7])

valid_perc <- validationset.data$Impurity.Percent
valid_var1 <- validationset.data$I
valid_var2 <- validationset.data$II
valid_var3 <- validationset.data$III
valid_var4 <- validationset.data$IV
valid_var5 <- validationset.data$V
valid_temp <- validationset.data$Temp
valid_mean <- rowMeans(validationset.data[3:7])

test_var1 <- test.data$I
test_var2 <- test.data$II
test_var3 <- test.data$III
test_var4 <- test.data$IV
test_var5 <- test.data$V
test_temp <- test.data$Temp
test_mean <- rowMeans(test.data[1:5])

trainset.data.x <- trainset.data[-c(1,2)]
trainset.data.y <- trainset.data$Impurity.Type

validationset.data.x  <- validationset.data
validationset.data.y  <- validationset.data$Impurity.Type

#-------------------------------------impurity type classification----------------------------------------------
# random forest for classification 
rf_class = randomForest(x = trainset.data.x[-1], y = trainset.data.y, ntree = 50)

#classification validation
y_pred = predict(rf_class, newdata = validationset.data.x[-1])
cls_error <- ClassificationError(validationset.data.y, y_pred)
#-------------------------------------end of impurity type classification---------------------------------------

#-------------------------------------training multiple linear regression---------------------------------------
model_list <- list()
for(lvl in levels(trainset.data$Impurity.Type)){
  index <- which(trainset.data$Impurity.Type == lvl)
  t_data = train_mean[index]
  mlr.model <- lm(perc[index] ~ t_data)
  print(summary(mlr.model))
  cat("summary for", lvl)
  model_list[[lvl]] <- mlr.model
}

plot(x = train_mean, y = perc, col=trainset.data$Impurity.Type, pch=16, main="Mean plot", xlim=c(-0.5,1.5), ylim =c(1,10))

#regression validation
col_index <- 1
total_reg_error <- 0
for(lvl in levels(validationset.data$Impurity.Type)){
  index <- which(validationset.data$Impurity.Type == lvl)
  if(length(index)==0){next()}
  validation_pred <- predict(model_list[[lvl]], list(t_data=valid_mean[index]))

  total_reg_error <- total_reg_error + MSE(valid_perc[index], validation_pred)
  
  plot(x = valid_mean[index], y = valid_perc[index], xlim=c(-0.5,1.5), ylim =c(1,10), col=col_index, pch=16)
  abline(model_list[[lvl]], col=col_index)
  col_index=col_index+1
}
reg_error <- total_reg_error/nrow(validationset.data)
#-------------------------------------end of training multiple linear regression--------------------------------

#performance metric
tot_error <- reg_error + cls_error

#-------------------------------------prediction on classification----------------------------------------------
g.hat = predict(rf_class, newdata = test.data)
#-------------------------------------end of prediction on classification---------------------------------------

#combine data
Impurity.Type <- g.hat
new.data <- cbind(Impurity.Type, test.data)

#-------------------------------------prediction on regression--------------------------------------------------
y.hat <- vector(mode = "list", length = length(test.data))
for(lvl in levels(new.data$Impurity.Type)){
  index <- which(new.data$Impurity.Type == lvl)
  y.hat[index] <- predict(model_list[[lvl]], list(t_data=test_mean[index]))
}
#-------------------------------------end of prediction on regression-------------------------------------------

#index of pure chemical in test data set
pure_chemical_index <- which(y.hat<1.8)

#classify item with impurity percentage below 1.8 as chemical type X
#adding 'X' as factor for g.hat class, otherwise adding 'x' to data will fail
g.hat <- `levels<-`(g.hat, c(levels(g.hat), "X"))
g.hat[pure_chemical_index] <- "X"

#combining regressiong and classification result
g.hat <- as.character(g.hat)
result <- cbind(g.hat,y.hat)

#output to required csv format
write.csv(result, file = "chemical_predictions_group_F_week_3.csv", row.names=FALSE)