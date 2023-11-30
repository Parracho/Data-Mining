setwd("C:/Users/bfpar/Documents/GitHub/Data-Mining/Project/Code")      # Change working directory
library(factoextra)
library(foreign)
library(ggfortify)
library(lattice)
higgs <- read.arff("dataset.arff")
data_frame <- data.frame(higgs)
sample_indices <- sample(1:nrow(data_frame), 10000, replace = FALSE)
data <- data_frame[sample_indices, ]
# Summary of the dataset
summary(data)

# Separate the target variable (assuming 'target_column' is the name) from predictor variables
target <- data$target
predictors <- data[, -which(names(data) == "target")]

min_max_normalize <- function(predictors) {
  min_val <- apply(predictors, 2, min)
  max_val <- apply(predictors, 2, max)
  normalized_predictors <- (predictors - min_val) / (max_val - min_val)
  return(normalized_predictors)
}

normalized_predictors <- min_max_normalize(predictors)
print(normalized_predictors)

pca_result <- prcomp(predictors,scale=TRUE)  # Use scale = TRUE to standardize the variables
scores <- pca_result$x
# Summary of the PCA results
summary(pca_result)


# variancias no Y, componentes no X
plot(pca_result)
plot(pca_result, type="l") #same but with a line instead of histogram
fviz_eig(pca_result,ncp = 28,addlabels=TRUE)

plot(scores[,2:3])
# unclass para mostrar divisao entre target 0 e 1 
plot(scores[,2:3],col=unclass(data[,1]))


# biplot good
biplot.higgs.pca <- biplot(pca_result,pc.biplot=TRUE, cex=0.5, font=1, expand=1) 
biplot.higgs.pca


#good graphic comp 2 x comp 1 with target
# loading library 
library(ggfortify) 
pca_result.plot <- autoplot(pca_result, data = data,colour = 'target') 
pca_result.plot


# heatmap of correlation between variables
library(tidyr)
# Compute the correlation matrix
cor_variables <- cor(predictors)
# Melt the correlation matrix into long format
cor_variables_long <- as.data.frame(as.table(cor_variables))
# Create a correlation heatmap
ggplot(cor_variables_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed(ratio = 1)

library(ggcorrplot)
ggcorrplot(cor_variables)

# K means clustering
cl= kmeans(predictors, 2, nstart=1, iter.max=10)
table(cl$cluster,c(rep(1,10000),rep(2,10000)))
splom(predictors,col=cl$cluster+2, main="blue/green is cluster")

pcp= princomp(predictors)
spc= pcp$scores %*% diag(1/pcp$sdev)
#splom( spc[,1:3],col=as.numeric(crabs[,1]),pch=as.numeric(crabs[,2]),
#       main="circle/triangle is gender, black/red is species")
cl= kmeans(spc, 2, nstart=1, iter.max=20)
splom( spc,col=cl$cluster+2, main="blue/green is cluster")

