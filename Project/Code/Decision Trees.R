setwd("C:/Users/bfpar/Documents/GitHub/Data-Mining/Project/Code")   
library(foreign)
library(rpart)
higgs <- read.arff("dataset.arff")
data_frame <- data.frame(higgs)
# sample 1000 rows from the data set
sample_indices <- sample(1:nrow(data_frame), 1000, replace = FALSE)
data1 <- data_frame[sample_indices, ]


arv_data=rpart(data1[,1]~.,data=data1[,-1],cp=0.05)
plot(arv_data)
text(arv_data)

