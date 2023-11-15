# Set the working directory to a specific path
setwd("C:/Users/bfpar/Documents/Data-Mining/DataMiningExercises/wine")
library(MASS)
wine=read.table("wine.data",sep=",")
dim(wine)
pairs(wine[,2:14],col=wine[,1])
#Vamos escolher a 2 e a 13 e a 1 que cont´em a classe
wine1=wine[,c(2,13,1)]
plot(wine1[,1:2],col=wine1[,3],pch=20,cex=1.5,cex.lab=1.4)
#correr o LDA
wine.lda = lda(x=wine1[,1:2],grouping=wine1[,3])
#crear uma grelha para fazer as fronteiras da LDA 
x=seq(10,15,0.01)
y=seq(0.5,4.5,0.01)
z = as.matrix(expand.grid(x,y),0)
m =length(x)
n =length(y)
#como as classes s˜ao 1,2 e 3 vamos crias os contornos em 1.5 e 2.5
wine.ldp = predict(wine.lda,z)$class ### desenha as linhas e separa pela class
contour(x,y,matrix(wine.ldp,m,n),levels=c(1.5,2.5), add=TRUE, d=FALSE, lty=2) ### contour mete se por cima do plot
# Vamos fazer agora as fronteiras com QDA.
wine.qda = qda(x=wine1[,1:2],grouping=wine1[,3])
wine.qdp = predict(wine.qda,z)$class
plot(wine1[,1:2],col=wine1[,3],pch=20,cex=1.5,cex.lab=1.4)
contour(x,y,matrix(wine.qdp,m,n),levels=c(1.5,2.5), add=TRUE, d=FALSE, lty=2)
