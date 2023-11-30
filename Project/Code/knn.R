setwd("C:/Users/bfpar/Documents/GitHub/Data-Mining/Project/Code")      # Change working directory
library(factoextra)
library(foreign)
library(ggfortify)
library(lattice)
library(class)
higgs <- read.arff("dataset.arff")
data_frame<- data.frame(higgs)
sample_indices <- sample(1:nrow(data_frame), 11000, replace = FALSE)
data <- data_frame[sample_indices, ]

data.train = head(data,10000)

data.test = tail(data,1000)

for (n in 1:10){
  data[[n]]=data.train[1:nrow(data_frame),1000*(n-1):n*1000]
}
accuracy=c()

for (k in 1:25){
  for (n in 1:10){
    data.knn.k = knn(data.train[,-1], data.test[,-1],data.train[,1], k)
    misclass = sum(data.test$target != data.knn.k)
    test.error.knn.k = misclass/nrow(data.test)
    cm=table(data.test$target,data.knn.k)
    accuracy[k-1]=sum(diag(cm))/length(data.test$target)
  }
}

