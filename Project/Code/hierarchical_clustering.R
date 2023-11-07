setwd("C:/Users/bfpar/Documents/DataMining/Project/Code")      # Change working directory
library(factoextra); library(foreign); library(fields)
higgs <- read.arff("dataset.arff")
data_frame <- data.frame(higgs)
sample_indices <- sample(1:nrow(data_frame), 10000, replace = FALSE)
data <- data_frame[sample_indices, ]
# Separate the target variable (assuming 'target_column' is the name) from predictor variables
target <- data$target
predictors <- data[, -which(names(data) == "target")]

pca_result <- prcomp(predictors,scale=TRUE)
scores <- pca_result$x
eigenvalues <- pca_result$sdev^2
retain_components <- which(eigenvalues > 1)
PCA_13_first <- pca_result$x[, retain_components]
#------------------------------------------------------------------------------------------------------------
wardsmethod<-hclust(dist(PCA_13_first),method="ward.D")
print(wardsmethod)
plot(wardsmethod, main = "Hierarchical Clustering Dendrogram", xlab = "Data Points")
image.plot(1:ncol(PCA_13_first),1:nrow(PCA_13_first),t(PCA_13_first),
           col=tim.colors(200),xlab="Variables",ylab="Events",cex.lab=1.4)


# Calculate the dissimilarity matrix using predefined distance (euclidian)
# clusters the points
dist_matrix <- dist(PCA_13_first)
# Perform hierarchical clustering (using complete linkage in this example)
hierarchical_cluster <- hclust(dist_matrix, method = "complete")
# Plot the dendrogram
plot(hierarchical_cluster, main = "Hierarchical Clustering Dendrogram", xlab = "Data Points")
ord = hierarchical_cluster$order
image.plot(1:ncol(PCA_13_first),1:nrow(PCA_13_first),t(PCA_13_first),
           col=tim.colors(200),xlab="Variables",ylab="Events",cex.lab=1.4)

#repeat for average linkage
dist_matrix <- dist(PCA_13_first)
hierarchical_cluster <- hclust(dist_matrix, method = "complete")
# Plot the dendrogram
plot(hierarchical_cluster, main = "Hierarchical Clustering Dendrogram", xlab = "Data Points")
ord = hierarchical_cluster$order
image.plot(1:ncol(PCA_13_first),1:nrow(PCA_13_first),t(PCA_13_first),
           col=tim.colors(200),xlab="Variables",ylab="Events",cex.lab=1.4)

#clusters the principal components
dist_matrix1 = dist(t(PCA_13_first))
hierarchical_cluster1 = hclust(dist_matrix1,method="average")
ord1 = hierarchical_cluster1$order
plot(hierarchical_cluster1,main = "Hierarchical Clustering Dendrogram", xlab = "Data Points")
heatmap(PCA_13_first)



# Calculate the dissimilarity matrix using predefined distance
dist_matrix <- dist(PCA_13_first,method="euclidian")
# Perform hierarchical clustering (using complete linkage in this example)
hierarchical_cluster <- hclust(dist_matrix, method = "average")
# Plot the dendrogram
plot(hierarchical_cluster, main = "Hierarchical Clustering Dendrogram", xlab = "Data Points")
ord = hierarchical_cluster$order
image.plot(1:ncol(PCA_13_first),1:nrow(PCA_13_first),t(PCA_13_first),
           col=tim.colors(200),xlab="Variables",ylab="Events",cex.lab=1.4)

dist_matrix1 = dist(t(predictors),method="euclidian")
hierarchical_cluster1 = hclust(dist_matrix1,method="average")
ord1 = hierarchical_cluster1$order
plot(hierarchical_cluster1,main = "Hierarchical Clustering Dendrogram", xlab = "Data Points")
predictors_numeric <- as.matrix(predictors)
heatmap(x=PCA_13_first,hclustfun =hierarchical_cluster,main="Heatmap for 13 first PC",verbose=TRUE)


library(pheatmap)
pheatmap(PCA_13_first, main = "Heatmap of first 13 principal components",show_rownames=FALSE)



library(colorspace); library(dendextend)
distance <- dist(PCA_13_first)
hc <- hclust(distance, method = "average")
higgs_target <- rev(levels(target))
dend <- as.dendrogram(hc)
dend <- rotate(dend, 1:10000)
dend <- color_branches(dend, k = 2)
# Manually match the labels to the real classification
ordered_target <- as.numeric(target)[order.dendrogram(dend)]
target_colors <- rainbow_hcl(2)[ordered_target]
# Modify the labels to include target values and cluster memberships
cluster_labels <- paste(as.character(target), "(", labels(dend), ")", sep = " ")
labels(dend) <- cluster_labels
dend <- hang.dendrogram(dend, hang_height = 0.1)
dend <- set(dend, "labels_cex", 0.5)
par(mar = c(4, 4, 4, 6))
plot(dend, main = "Clustered Higgs data set", horiz = FALSE, nodePar = list(cex = 0.002))
legend("topright", legend = higgs_target, fill = target_colors)





distance <- dist(PCA_13_first)
hc <- hclust(distance, method = "complete")
higgs_target <- rev(levels(target))
dend <- as.dendrogram(hc)
# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:10000)
# Color the branches based on the clusters:
dend <- color_branches(dend, k=2)
# Manually match the labels, as much as possible, to the real classification:
labels_colors(dend) <-rainbow_hcl(2)[sort_levels_values(as.numeric(target)[order.dendrogram(dend)])]
# We shall add the type to the labels:
labels(dend) <- paste(as.character(target)[order.dendrogram(dend)],"(",labels(dend),")", sep = "")
# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
dend <- set(dend, "labels_cex", 0.5)
par(mar = c(3,3,3,7))
plot(dend, main = "Clustered Higgs data set", horiz =  FALSE,  nodePar = list(cex = .007))
legend("topleft", legend = higgs_target, fill = rainbow_hcl(3))
