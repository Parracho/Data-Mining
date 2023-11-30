setwd("C:/Users/bfpar/Documents/GitHub/Data-Mining/Project/Code/")      # Change working directory
library(foreign)
library(MASS)
higgs <- read.arff("dataset.arff")
data_frame <- data.frame(higgs)
# sample 10000 rows from the data set
sample_indices <- sample(1:nrow(data_frame), 1000, replace = FALSE)
data <- data_frame[sample_indices, ]
sum=summary(data)
# Summary of the dataset


#################### Visualize #######################
variable=data[,c(27,28,1)]
plot(variable[,1:2],col=variable[,3],pch=20,cex=1.5,cex.lab=1.4)
#correr o LDA
variable.lda = lda(x=variable[,1:2],grouping=variable[,3])
#crear uma grelha para fazer as fronteiras da LDA 
x=seq(0,5,0.01)
y=seq(0,3.5,0.01)
z = as.matrix(expand.grid(x,y),0)
m =length(x)
n =length(y)
#como as targetes s˜ao 1,2 e 3 vamos crias os contornos em 1.5 e 2.5
variable.ldp = predict(variable.lda,z)$target ### desenha as linhas e separa pela target
contour(x,y,matrix(variable.ldp,m,n),levels=c(0.5), add=TRUE, d=FALSE, lty=2) ### contour mete se por cima do plot
# Vamos fazer agora as fronteiras com QDA.
variable.qda = qda(x=variable[,1:2],grouping=variable[,3])
variable.qdp = predict(variable.qda,z)$target
plot(variable[,1:2],col=variable[,3],pch=20,cex=1.5,cex.lab=1.4)
contour(x,y,matrix(variable.qdp,m,n),levels=c(0.5), add=TRUE, d=FALSE, lty=2)

######################## Performance ##############################

library(MASS)

# Assuming your target variable is in the first column (adjust the column index accordingly)
target_col <- 1
features <- data[, -target_col]  # Exclude the target column

# Extract the target variable
target <- data[, target_col]

# Perform Linear Discriminant Analysis
lda_model <- lda(features, target)

# Print the summary of the LDA model
print(lda_model)

# Make predictions on the original data
predictions <- predict(lda_model, features)

# Print the confusion matrix (assuming your target variable is categorical)
conf_matrix <- table(predictions$class, target)
print(conf_matrix)
