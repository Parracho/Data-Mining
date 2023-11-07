setwd("C:/Users/telmo/OneDrive/Ambiente de Trabalho/MEDM Project 1/")  # Change working directory
library(foreign)
higgs <- read.arff("dataset.arff")
data_frame <- data.frame(higgs)
sample_indices <- sample(1:nrow(data_frame), 10000, replace = FALSE)
data <- data_frame[sample_indices, ]

# Summary of the dataset
sum <- summary(data)
sum

library(ggplot2)

# Create histograms for all variables using lapply
histograms <- lapply(data, function(column) {
  ggplot(data, aes(x = column)) +
    geom_histogram(binwidth = 1) +  # Adjust the binwidth as needed
    labs(title = paste("Histogram of", names(column)))
})

# Print the histograms
print(histograms)

