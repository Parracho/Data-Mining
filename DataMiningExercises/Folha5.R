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















