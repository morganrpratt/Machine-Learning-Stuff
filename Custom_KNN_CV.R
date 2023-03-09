library(tidyverse)
library(dplyr)
library(FNN)
library(glmnet)

# Custom cross-validation function for k-fold k nearest neighbors
cv_knn <- function(xdat, ydat, kseq, kfolds) {
  # Create a random list of ids (1 thru kfolds) equal in length to data
  ids <- rep(1:kfolds, length.out = length(xdat))   
  ids <- ids[sample(length(ids))]
  
  # Matrix to store results
  results <- matrix(NA, nrow = length(kseq), ncol = kfolds) 
  
  # Loop through folds
  for (k in kseq) {   
    for (i in 1:kfolds) {
      model <- knn.reg(train = scale(xdat[ids != i, ]), 
                       test = scale(xdat[ids == i, ]), y = ydat[ids != i], k = k)
      
      predicted_y <- model$pred
      results[k, i] <- mean((predicted_y - ydat[ids == i])^2)
    }
  }
  return(apply(results, 1, mean))
}

# Test with example data

# Load the data
prostate <- read.table("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.data")

# Drop 'train' column
prostate <- prostate[, -10]

# Some visuals of the data 
cor(prostate)
pairs(prostate, col = "purple")

# Putting it into custom cross-validation function
set.seed(222)
error1 <- cv_knn(xdat = prostate[, 1:8], ydat = prostate[, 9], 
                 kseq = seq(50), kfolds = 5)

# Plot MSEs from above test
plot(error1, type = "l", xlab = "k", ylab = "MSE")

# Find minimum MSE
which.min(error1)
