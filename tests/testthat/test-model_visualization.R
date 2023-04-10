library(testthat)
library(tidyverse)

accuracy_data <- read_csv('../data/modelling/heart_data_accuracies.csv')
test_that("Y axis is not what we expected", {
    expect_equivalent(knn_visualization(accuracy_data)$labels$y, "Accuracy Estimate")})
test_that("X axis is not what we expected", {
    expect_equivalent(knn_visualization(accuracy_data)$labels$x, "Neighbors")
})
