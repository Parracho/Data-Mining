setwd("C:/Users/telmo/OneDrive/Ambiente de Trabalho/MEDM Project 1/")      # Change working directory
library(factoextra)
library(foreign)
higgs <- read.arff("dataset.arff")
data_frame <- data.frame(higgs)
sample_indices <- sample(1:nrow(data_frame), 10000, replace = FALSE)
data <- data_frame[sample_indices, ]
# Summary of the dataset
summary(data)

# Separate the target variable (assuming 'target_column' is the name) from predictor variables
target <- data$target
predictors <- data[, -which(names(data) == "target")]

pca_result <- prcomp(predictors,scale=TRUE)  # Use scale = TRUE to standardize the variables
scores <- pca_result$x
eigenvalues <- pca_result$sdev^2
retain_components <- which(eigenvalues > 1)
pca_result_subset <- pca_result$x[, retain_components]



# K means clustering
cl= kmeans(pca_result_subset, 2, nstart=1, iter.max=10)
print(cl)
print(cl$size)

library(cluster)
clusplot(predictors, cl$cluster, color = TRUE, shade = TRUE,labels = 2:1, lines = 0)


length(cl)  # This should match the number of data points
nrow(pca_result_subset)  # Number of data points in pca_result_subset

# Create a table of the cluster assignments
cluster_table <- table(Cluster = cl)
# Rename the rows to match your cluster labels (0 and 1)
colnames(cluster_table) <- c("Cluster 0", "Cluster 1")
# Print the table
print(cluster_table)
count_cluster_0 <- cluster_table["Cluster 0"]
count_cluster_1 <- cluster_table["Cluster 1"]


table(cl$cluster,c(rep(1,10000),rep(2,10000)))
splom(pca_result_subset,col=cl$cluster+2, main="blue/green is cluster")
