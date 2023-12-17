library(MASS)
library(rpart)

################ Ex 51 ###################
data(Boston)
?Boston
str(Boston)
arv=rpart(Boston[,14]~.,data=Boston[,-14])
plot(arv)
text(arv)

#introducing a complexity parameter

arv_cp=rpart(Boston[,14]~.,data=Boston[,-14],cp=0.001)
plot(arv_cp)
text(arv_cp)

################ Ex 51 applied to wine dataset ###################

setwd("C:/Users/bfpar/Documents/GitHub/Data-Mining/DataMiningExercises/wine")
wine=read.table("wine.data",sep=",")
summary(wine)
arv_wine=rpart(wine[,1]~.,data=wine[,-1])
plot(arv_wine)
text(arv_wine)
#introducing a complexity parameter

arv_wine_cp=rpart(wine[,1]~.,data=wine[,-1],cp=0.08)
plot(arv_wine_cp)
text(arv_wine_cp)


################### Ex 58 ###################
library(e1071)
singh.train=read.table("singh.train.data",sep=",")
singh.test=read.table("singh.test.data",sep=",")
singh.train$outcome <- ifelse(singh.train$outcome == "tumour", 1, 0)
singh.test$outcome <- ifelse(singh.test$outcome == "tumour", 1, 0)
model1 <- svm(singh.train[,1]~., data = singh.train[,-1],kernel="radial")
summary(model1)
# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(singh.train[,-1])), col = as.integer(singh.train[,1]),pch = c("o","+")[1:569%in% model1$index + 1],xlab="x",ylab="y")
predr1 = predict(model1, singh.test[,-1])
table(predr1, singh.test[,1])
erro.svmr1=sum(predr1!=singh.test[,1])/nrow(singh.test)
erro.svmr1
#Agora com kernel=linear
model2 = svm(singh.train[,1]~., data = singh.train[,-1],kernel="linear")
summary(model2)
plot(cmdscale(dist(singh.train[,-1])), col = as.integer(singh.train[,1]),pch = c("o","+")[1:569
                                                                                          %in% model2$index + 1],xlab="x",ylab="y")
predr2 = predict(model2, singh.test[,-1])
table(predr2, singh.test[,1])
erro.svmr2=sum(predr2!=singh.test[,1])/nrow(singh.test)
erro.svmr2
#Agora com kernel=sigmoid
model3 = svm(singh.train[,1]~., data = singh.train[,-1],kernel="sigmoid")
summary(model3)
plot(cmdscale(dist(singh.train[,-1])), col = as.integer(singh.train[,1]),pch = c("o","+")[1:569
                                                                                          %in% model3$index + 1],xlab="x",ylab="y")
predr3 = predict(model3, singh.test[,-1])
table(predr3, singh.test[,1])
erro.svmr3=sum(predr3!=singh.test[,1])/nrow(singh.test)
erro.svmr3














