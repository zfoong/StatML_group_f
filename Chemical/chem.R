set.seed(20)
data <- read.csv("training_set.csv", header=TRUE)
rows = sample(nrow(data))

split = round(nrow(data)*.75)
end = nrow(data)

data_shuff = data[rows,]

X_train = data_shuff[1:split, !(colnames(data) %in% c("Impurity.Percent","Impurity.Type"))]
X_test = data_shuff[(split+1):nrow(data), !(colnames(data) %in% c("Impurity.Percent","Impurity.Type"))]

y_train = data_shuff[1:split,'Impurity.Type']
y_test = data_shuff[(split+1):nrow(data),'Impurity.Type']

# plots of impurity percent against channel

plot(data$I,data$Impurity.Percent,col = factor(data$Impurity.Type), pch = 16)
legend(x = 'topright', 
       legend=levels(as.factor(data$Impurity.Type)),
       col=as.numeric(unique(data$Impurity.Type)),
       pch = 16)

plot(data$II,data$Impurity.Percent,col = data$Impurity.Type, pch = 16)
legend(x = 'topright', 
       legend=levels(as.factor(data$Impurity.Type)),
       col=as.numeric(unique(data$Impurity.Type)),
       pch = 16)

plot(data$III,data$Impurity.Percent,col = data$Impurity.Type, pch = 16)
legend(x = 'topright', 
       legend=levels(as.factor(data$Impurity.Type)),
       col= as.numeric(unique(data$Impurity.Type)),
       pch = 16)

plot(data$IV,data$Impurity.Percent,col = data$Impurity.Type, pch = 16)
legend(x = 'topright', 
       legend=levels(as.factor(data$Impurity.Type)),
       col= as.numeric(unique(data$Impurity.Type)),
       pch = 16)

plot(data$V,data$Impurity.Percent,col = data$Impurity.Type, pch = 16)
legend(x = 'topright', 
       legend=levels(as.factor(data$Impurity.Type)),
       col= as.numeric(unique(data$Impurity.Type)),
       pch = 16)

plot(data$Temp,data$Impurity.Percent,col = data$Impurity.Type, pch = 16)
legend(x = 'topright', 
       legend=levels(as.factor(data$Impurity.Type)),
       col= as.numeric(unique(data$Impurity.Type)),
       pch = 16)


# box plots of impurity types against each channel
plot(data$Impurity.Type,data$I, 
     xlab = 'Impurity Type', ylab = 'Channel 1')

plot(data$Impurity.Type,data$II, 
     xlab = 'Impurity Type', ylab = 'Channel 2')

plot(data$Impurity.Type,data$III, 
     xlab = 'Impurity Type', ylab = 'Channel 3')

plot(data$Impurity.Type,data$IV, 
     xlab = 'Impurity Type', ylab = 'Channel 4')

plot(data$Impurity.Type,data$V, 
     xlab = 'Impurity Type', ylab = 'Channel 5')

plot(data$Impurity.Type,data$Temp, 
     xlab = 'Impurity Type', ylab = 'Temp')


# number of instances in each class, farily balanced across each class.
summary(data$Impurity.Type)
# number of classes, add 1 for the pure type X
length(summary(data$Impurity.Type))

# from the graphs it looks like the temp feature does not have much correlation  
# with impurity type or percent


library(rpart)

# fitting descion tree to training set for classification
classifier = rpart(formula = y_train ~.,
                   data = X_train[-6])
# predictions on test set
y_pred = predict(classifier, newdata = X_test[-6], type = 'class')
# confusion matrix on test set
cm = table(y_test,y_pred)
# test accuracy 
acc = sum(diag(cm))/length(y_test)

# evaluation on training set
y_eval = predict(classifier, newdata = X_train[-6], type = 'class')
cm_eval = table(y_train,y_eval)
acc_eval = sum(diag(cm_eval))/sum(cm_eval)



library(randomForest)

# random forest for classification 
rf = randomForest(x = X_train[-6], y = y_train,
                  ntree = 50)

y_pred2 = predict(rf, newdata = X_test[-6])

cm2 = table(y_test,y_pred2)
acc2 = sum(diag(cm2))/length(y_test)


# evaluation on training set
y_eval2 = predict(rf, newdata = X_train[-6])
cm_eval2 = table(y_train,y_eval2)
acc_eval2 = sum(diag(cm_eval2))/sum(cm_eval2)



# random forest for impurity percent
y_train_reg = data_shuff[1:split,'Impurity.Percent']
y_test_reg = data_shuff[(split+1):nrow(data),'Impurity.Percent']

regRF = randomForest(x= X_train, y = y_train_reg, ntree = 50)
ypred_reg = predict(regRF, newdata = X_test)

rmse = function(p, t){
        sqrt(mean((p - t)^2))
}
rmse(ypred_reg,y_test_reg)


###### Cross Val on random forest for classification
k = 6

folds <- cut(seq(1,nrow(data_shuff)),breaks=k,labels=FALSE)
#Perform k fold cross validation
for(i in 1:k){
        #Segement data by fold using the which() function 
        testIndexes = which(folds==i,arr.ind=TRUE)
        
        X_testCV = data_shuff[testIndexes, !(colnames(data) %in% c("Impurity.Percent","Impurity.Type"))]
        X_trainCV = data_shuff[-testIndexes, !(colnames(data) %in% c("Impurity.Percent","Impurity.Type"))]
        y_testCV = data_shuff[testIndexes,'Impurity.Type']
        y_trainCV = data_shuff[-testIndexes,'Impurity.Type']
        
        rf = randomForest(x = X_trainCV[-6], y = y_trainCV,
                          ntree = 50)
        y_predCV = predict(rf, newdata = X_testCV[-6])
        cm = table(y_testCV,y_predCV)
        acc = sum(diag(cm))/length(y_testCV)
        print(acc)
}

# Cross val on random forest for regression

folds <- cut(seq(1,nrow(data_shuff)),breaks=k,labels=FALSE)
#Perform k fold cross validation
for(i in 1:k){
        #Segement data by fold using the which() function 
        testIndexes = which(folds==i,arr.ind=TRUE)
        
        X_testCV = data_shuff[testIndexes, !(colnames(data) %in% c("Impurity.Percent","Impurity.Type"))]
        X_trainCV = data_shuff[-testIndexes, !(colnames(data) %in% c("Impurity.Percent","Impurity.Type"))]
        y_testCV = data_shuff[testIndexes,'Impurity.Percent']
        y_trainCV = data_shuff[-testIndexes,'Impurity.Percent']
        
        rf = randomForest(x = X_trainCV[-6], y = y_trainCV,
                          ntree = 50)
        y_predCV = predict(rf, newdata = X_testCV[-6])
        errors = rmse(y_predCV,y_testCV)
        print(errors)
}


# predicting on test set
test <- read.csv("test_set.csv", header=TRUE)

X = data_shuff[,!(colnames(data) %in% c("Impurity.Percent","Impurity.Type"))]
y_class = data_shuff[,'Impurity.Type']
y_reg = data_shuff[,'Impurity.Percent']

final_rf_class = randomForest(x = X[-6], y = y_class,ntree = 50)
y_test_pred_c = predict(final_rf_class, newdata = test[-6])

final_rf_reg = randomForest(x = X, y = y_reg,ntree = 25)
y_test_pred_r = predict(final_rf_reg, newdata = test)

