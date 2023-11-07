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

#library(GGally)
#data_for_parallel_plot <- cbind(data.frame(target = target), scores[,1:14])
#parallel_plot <- ggparcoord(data_for_parallel_plot, columns = 2:ncol(data_for_parallel_plot), groupColumn = "target")
#parallel_plot <- parallel_plot + labs(title = "Parallel Coordinates Plot")
#print(parallel_plot)


# Summary of the PCA results
summary(pca_result)

# % variancias no Y, componentes no X
fviz_eig(pca_result,ncp = 28,addlabels=TRUE)


fviz_cos2(pca_result,choice="var",axes=1)

fviz_eig(pca_result,addlabels=TRUE)

#contribuiçao de cada variabel para as duas primeiras componentes principais
fviz_pca_var(pca_result,col.var="cos2",repel=TRUE)


#fviz_pca_ind(pca_result, label="none", habillage=target,addEllipses=TRUE, ellipse.level=0.95)
