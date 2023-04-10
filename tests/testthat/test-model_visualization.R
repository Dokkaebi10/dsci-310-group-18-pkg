library(testthat)
library(tidyverse)

accuracy_data <- read_csv("https://raw.githubusercontent.com/Dokkaebi10/dsci-310-group-18/main/data/modelling/heart_data_accuracies.csv")
test_that("Y axis is not what we expected", {
    expect_equivalent(knn_visualization(accuracy_data)$labels$y, "Accuracy Estimate")})
test_that("X axis is not what we expected", {
    expect_equivalent(knn_visualization(accuracy_data)$labels$x, "Neighbors")
})
