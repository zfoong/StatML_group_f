library(dummies)
library(randomForest)

set.seed(25)

orig = read.csv("training_set.csv", header=TRUE)
data = read.csv("training_set.csv", header=TRUE)
test = read.csv("test_set.csv", header=TRUE)


rows = sample(nrow(data))

split = round(nrow(data)*.75)
end = nrow(data)

# scaled channels
channels = rbind(data[3:7],test[-6])
channels = scale(channels)

# replacing unscaled channels with scaled channels in data

data[3:7] = channels[1:92,]

# shuffle data
data_shuff = data[rows,]

# average of channels for each instance
means = data.frame(rowMeans(data_shuff[3:7]))

data_shuff = cbind(data_shuff,means)

names(data_shuff)[names(data_shuff) == "rowMeans.data_shuff.3.7.."] <- "Averages"

### for using impurity type as a feature ##
dum = data.frame(dummy(data_shuff$Impurity.Type))
data_shuff = cbind(data_shuff,dum)

X_train = data_shuff[1:split, !(colnames(data_shuff) %in% c("Impurity.Type","Impurity.Percent"))]
X_test = data_shuff[(split+1):nrow(data), !(colnames(data_shuff) %in% c("Impurity.Type","Impurity.Percent"))]

y_train = data_shuff[1:split,'Impurity.Type']
y_test = data_shuff[(split+1):nrow(data),'Impurity.Type']

y_train_reg = data_shuff[1:split,'Impurity.Percent']
y_test_reg = data_shuff[(split+1):nrow(data),'Impurity.Percent']


# zoom graphs, added lables to points because there isnt enough colours for each group
plot(data_shuff$I,data_shuff$Impurity.Percent,col = data_shuff$Impurity.Type, pch = 16)
text(data_shuff$I,data_shuff$Impurity.Percent, labels=data_shuff$Impurity.Type, cex=0.5, font=2, pos=4)

plot(data_shuff$II,data_shuff$Impurity.Percent,col = data_shuff$Impurity.Type, pch = 16)
text(data_shuff$II,data_shuff$Impurity.Percent, labels=data_shuff$Impurity.Type, cex=0.5, font=2, pos=4)

plot(data_shuff$III,data_shuff$Impurity.Percent,col = data_shuff$Impurity.Type, pch = 16)
text(data_shuff$III,data_shuff$Impurity.Percent, labels=data_shuff$Impurity.Type, cex=0.5, font=2, pos=4)

plot(data_shuff$IV,data_shuff$Impurity.Percent,col = data_shuff$Impurity.Type, pch = 16)
text(data_shuff$IV,data_shuff$Impurity.Percent, labels=data_shuff$Impurity.Type, cex=0.5, font=2, pos=4)


plot(data_shuff$V,data_shuff$Impurity.Percent,col = data_shuff$Impurity.Type, pch = 16)
text(data_shuff$V,data_shuff$Impurity.Percent, labels=data_shuff$Impurity.Type, cex=0.5, font=2, pos=4)


plot(data_shuff$Averages,data_shuff$Impurity.Percent,col = data_shuff$Impurity.Type, pch = 16)
text(data_shuff$Averages,data_shuff$Impurity.Percent, labels=data_shuff$Impurity.Type, cex=0.5, font=2, pos=4)




plot(data_shuff$Impurity.Type,data_shuff$I,xlab = 'Impurity Type', ylab = 'Channel 1')

plot(data_shuff$Impurity.Type,data_shuff$II,xlab = 'Impurity Type', ylab = 'Channel 2')

plot(data_shuff$Impurity.Type,data_shuff$III,xlab = 'Impurity Type', ylab = 'Channel 3')

plot(data_shuff$Impurity.Type,data_shuff$IV,xlab = 'Impurity Type', ylab = 'Channel 4')

plot(data_shuff$Impurity.Type,data_shuff$V,xlab = 'Impurity Type', ylab = 'Channel 5')

plot(data_shuff$Impurity.Type,data_shuff$Temp,xlab = 'Impurity Type', ylab = 'Temp')

plot(data_shuff$Impurity.Type,data_shuff$Averages,xlab = 'Impurity Type', ylab = 'Averages')


# multivariate regression 

rmse = function(p, t){
  sqrt(mean((p - t)^2))
}


mv = lm(formula = y_train_reg ~., data = X_train[c(4,5,7:20)])
y_pred_reg = predict(mv, newdata = X_test[c(4,5,7:20)])
rmse(y_pred_reg,y_test_reg)
y_pred_reg
y_test_reg

#


folds <- cut(seq(1,nrow(data_shuff)),breaks=k,labels=FALSE)
#Perform k fold cross validation
score = c()
for(i in 1:k){
  #Segement data by fold using the which() function 
  testIndexes = which(folds==i,arr.ind=TRUE)
  
  X_testCV = data_shuff[testIndexes, !(colnames(data_shuff) %in% c("Impurity.Percent","Impurity.Type"))]
  X_trainCV = data_shuff[-testIndexes, !(colnames(data_shuff) %in% c("Impurity.Percent","Impurity.Type"))]
  y_testCV = data_shuff[testIndexes,'Impurity.Percent']
  y_trainCV = data_shuff[-testIndexes,'Impurity.Percent']
  
  # 7:20 is using the average channal reading and the dummy variables for the impurity type
  mv_reg = lm(formula = y_trainCV ~., data = X_trainCV[c(8:20)])
  y_pred_reg = predict(mv_reg, newdata = X_testCV[c(8:20)])
  
  errors = rmse(y_pred_reg,y_testCV)
  print(errors)
  score = c(score,errors)
}

mean(score)


###### Cross Val on random forest for classification

#Perform k fold cross validation
k = 3
score = c()
for(i in 1:k){
  #Segement data by fold using the which() function 
  testIndexes = which(folds==i,arr.ind=TRUE)
  
  X_testCV = data_shuff[testIndexes, !(colnames(data_shuff) %in% c("Impurity.Percent","Impurity.Type"))]
  X_trainCV = data_shuff[-testIndexes, !(colnames(data_shuff) %in% c("Impurity.Percent","Impurity.Type"))]
  y_testCV = data_shuff[testIndexes,'Impurity.Type']
  y_trainCV = data_shuff[-testIndexes,'Impurity.Type']
  
  rf = randomForest(x = X_trainCV[c(-6,-7:-20)], y = y_trainCV,
                    ntree = 50)
  y_predCV = predict(rf, newdata = X_testCV[c(-6,-7:-20)])
  cm = table(y_testCV,y_predCV)
  acc = sum(diag(cm))/length(y_testCV)
  print(cm)
  print(acc)
  score = c(score,acc)
}

mean(score)

# looks like rf gets (D and M) mixed up and (H and B) mixed up 

# D&M and H&B ----------------------

df_DM = data_shuff[data_shuff$Impurity.Type == 'D' | data_shuff$Impurity.Type == 'M' ,]
df_HB = data_shuff[data_shuff$Impurity.Type == 'H' | data_shuff$Impurity.Type == 'B' ,]


plot(df_DM$I,df_DM$Impurity.Percent,col = df_DM$Impurity.Type, pch = 16)
text(df_DM$I,df_DM$Impurity.Percent, labels=df_DM$Impurity.Type, cex=0.5, font=2, pos=4)

plot(df_DM$II,df_DM$Impurity.Percent,col = df_DM$Impurity.Type, pch = 16)
text(df_DM$II,df_DM$Impurity.Percent, labels=df_DM$Impurity.Type, cex=0.5, font=2, pos=4)

plot(df_DM$III,df_DM$Impurity.Percent,col = df_DM$Impurity.Type, pch = 16)
text(df_DM$III,df_DM$Impurity.Percent, labels=df_DM$Impurity.Type, cex=0.5, font=2, pos=4)

plot(df_DM$IV,df_DM$Impurity.Percent,col = df_DM$Impurity.Type, pch = 16)
text(df_DM$IV,df_DM$Impurity.Percent, labels=df_DM$Impurity.Type, cex=0.5, font=2, pos=4)


plot(df_DM$V,df_DM$Impurity.Percent,col = df_DM$Impurity.Type, pch = 16)
text(df_DM$V,df_DM$Impurity.Percent, labels=df_DM$Impurity.Type, cex=0.5, font=2, pos=4)


plot(df_DM$Averages,df_DM$Impurity.Percent,col = df_DM$Impurity.Type, pch = 16)
text(df_DM$Averages,df_DM$Impurity.Percent, labels=df_DM$Impurity.Type, cex=0.5, font=2, pos=4)

# HB

plot(df_HB$I,df_HB$Impurity.Percent,col = df_HB$Impurity.Type, pch = 16)
text(df_HB$I,df_HB$Impurity.Percent, labels=df_HB$Impurity.Type, cex=0.5, font=2, pos=4)

plot(df_HB$II,df_HB$Impurity.Percent,col = df_HB$Impurity.Type, pch = 16)
text(df_HB$II,df_HB$Impurity.Percent, labels=df_HB$Impurity.Type, cex=0.5, font=2, pos=4)

plot(df_HB$III,df_HB$Impurity.Percent,col = df_HB$Impurity.Type, pch = 16)
text(df_HB$III,df_HB$Impurity.Percent, labels=df_HB$Impurity.Type, cex=0.5, font=2, pos=4)

plot(df_HB$IV,df_HB$Impurity.Percent,col = df_HB$Impurity.Type, pch = 16)
text(df_HB$IV,df_HB$Impurity.Percent, labels=df_HB$Impurity.Type, cex=0.5, font=2, pos=4)

plot(df_HB$V,df_HB$Impurity.Percent,col = df_HB$Impurity.Type, pch = 16)
text(df_HB$V,df_HB$Impurity.Percent, labels=df_HB$Impurity.Type, cex=0.5, font=2, pos=4)

plot(df_HB$Averages,df_HB$Impurity.Percent,col = df_HB$Impurity.Type, pch = 16)
text(df_HB$Averages,df_HB$Impurity.Percent, labels=df_HB$Impurity.Type, cex=0.5, font=2, pos=4)



# grouping together H&B and D&M into two new classes and using random forest
data_copy = data_shuff

data_copy$Type = data_copy$Impurity.Type
levels(data_copy$Type) = c(levels(data_copy$Type),"Y")
levels(data_copy$Type) = c(levels(data_copy$Type),"Z")


data_copy[data_copy$Impurity.Type == 'D' |data_copy$Impurity.Type == 'M' , 'Type'] = 'Y'
data_copy[data_copy$Impurity.Type == 'H' |data_copy$Impurity.Type == 'B' , 'Type'] = 'Z'

data_copy$Type = factor(data_copy$Type)


X_train_copy = data_copy[1:split, !(colnames(data_copy) %in% c("Impurity.Percent"))]
X_test_copy = data_copy[(split+1):nrow(data), !(colnames(data_copy) %in% c("Impurity.Percent"))]

y_train_copy = data_copy[1:split,'Type']
y_test_copy = data_copy[(split+1):nrow(data),'Type']

# set seed to 20
X_trainDM = X_train_copy[X_train_copy$Impurity.Type == 'D' |X_train_copy$Impurity.Type == 'M',]
X_testDM = X_test_copy[X_test_copy$Impurity.Type == 'D' |X_test_copy$Impurity.Type == 'M',]

y_trainDM = X_trainDM$Impurity.Type
y_testDM = X_testDM$Impurity.Type

y_trainDM = factor(y_trainDM)
y_testDM = factor(y_testDM)

rf_DM = randomForest(x = X_trainDM[c(-1,-6,-7:-21)], y = y_trainDM,
                       ntree = 50)

y_pred_DM = predict(rf_DM, newdata = X_testDM[c(-1,-6,-7:-21)])
cm_DM = table(y_testDM,y_pred_DM)
acc_DM = sum(diag(cm_DM))/length(y_testDM)
cm_DM

# cant seperate D from M. Not enough data

rf_copy = randomForest(x = X_train_copy[c(-1,-6,-7:-21)], y = y_train_copy,
                  ntree = 50)
y_pred_copy = predict(rf_copy, newdata = X_test_copy[c(-1,-6,-7:-21)])
cm_copy = table(y_test_copy,y_pred_copy)
acc_copy = sum(diag(cm_copy))/length(y_test_copy)
cm_copy

# cross val
k = 5
folds <- cut(seq(1,nrow(data_copy)),breaks=k,labels=FALSE)
score = c()
for(i in 1:k){
  #Segement data by fold using the which() function 
  testIndexes = which(folds==i,arr.ind=TRUE)
  
  X_testCV = data_copy[testIndexes, !(colnames(data_copy) %in% c("Impurity.Percent","Impurity.Type"))]
  X_trainCV = data_copy[-testIndexes, !(colnames(data_copy) %in% c("Impurity.Percent","Impurity.Type"))]
  y_testCV = data_copy[testIndexes,'Type']
  y_trainCV = data_copy[-testIndexes,'Type']
  
  rf = randomForest(x = X_trainCV[c(-6,-7:-21)], y = y_trainCV,
                    ntree = 50)
  y_predCV = predict(rf, newdata = X_testCV[c(-6,-7:-21)])
  cm = table(y_testCV,y_predCV)
  acc = sum(diag(cm))/length(y_testCV)
  
  print(cm)
  print(acc)
  score = c(score,acc)
}

mean(score)








## final predictions-----------------


# assigning scaled channels back to test
test = cbind(channels[93:192,],test$Temp)

# mean of channels for each instance
avg_test = data.frame(rowMeans(test[,1:5]))

test = cbind(test,avg_test)
names(test)[names(test) == "rowMeans.test...1.5.."] <- "Averages"

X = rbind(X_train,X_test)
y_class = data_shuff[,'Impurity.Type']
y_reg = data_shuff[,'Impurity.Percent']


# using channels 1-5 as features
final_class_model = randomForest(x = X[-6:-20], y = y_class, ntree = 50)


y_pred_final = predict(final_class_model, newdata = test[-6])


# appending classification prediction to test set
test = cbind(test,data.frame(y_pred_final))

# creating dummy variables for categorical feature impurity type
dum_predict = data.frame(dummy(test$y_pred_final))

# concat dummy variables and drop predictions
test_reg = cbind(test,dum_predict) 
test_reg_final = test_reg[c(4,5,6,8:20)]

X_reg = X[c(4,5,7:20)]

names(test_reg_final) = names(X_reg)

# using channels 4,5, average for all channels and predictions for classes for features 
final_reg_model = lm(formula = y_reg ~., data = X_reg)
y_pred_reg_final = predict(final_reg_model, newdata = test_reg_final)


reg_pred = data.frame(y_pred_reg_final)

# adding 'X' level to predictions
levels(y_pred_final) = c(levels(y_pred_final),"X")

# indexes of pure chemicals
pure_chemical_index = which(y_pred_reg_final<1.8)

y_pred_final[pure_chemical_index] = 'X'

final_prediction = cbind(data.frame(y_pred_final),y_pred_reg_final)

write.csv(final_prediction,file = 'predictions_Week_3.csv', row.names = TRUE)



  
  
  
