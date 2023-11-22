setwd("C:/Users/bfpar/Documents/Data-Mining/Project/Code/")      # Change working directory
library(foreign)
higgs <- read.arff("dataset.arff")
data_frame <- data.frame(higgs)
# sample 10000 rows from the data set
sample_indices <- sample(1:nrow(data_frame), 10000, replace = FALSE)
data <- data_frame[sample_indices, ]
# Summary of the dataset
sum <- summary(data)
sum

target <- data$target
predictors <- data[, -which(names(data) == "target")]

pairs(predictors)


variable=predictors[,c(2,13)]
plot(variable[,1:2],col=variable[,3],pch=20,cex=1.5,cex.lab=1.4)
#correr o LDA
variable.lda = lda(x=variable[,1:2],grouping=variable[,3])
#crear uma grelha para fazer as fronteiras da LDA 
x=seq(10,15,0.01)
y=seq(0.5,4.5,0.01)
z = as.matrix(expand.grid(x,y),0)
m =length(x)
n =length(y)
#como as classes s˜ao 1,2 e 3 vamos crias os contornos em 1.5 e 2.5
variable.ldp = predict(variable.lda,z)$class ### desenha as linhas e separa pela class
contour(x,y,matrix(variable.ldp,m,n),levels=c(1.5,2.5), add=TRUE, d=FALSE, lty=2) ### contour mete se por cima do plot
# Vamos fazer agora as fronteiras com QDA.
variable.qda = qda(x=variable[,1:2],grouping=variable[,3])
variable.qdp = predict(variable.qda,z)$class
plot(variable[,1:2],col=variable[,3],pch=20,cex=1.5,cex.lab=1.4)
contour(x,y,matrix(variable.qdp,m,n),levels=c(1.5,2.5), add=TRUE, d=FALSE, lty=2)