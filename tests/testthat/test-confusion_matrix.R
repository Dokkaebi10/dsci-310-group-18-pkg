library(testthat)
library(tidyverse)

heart_data_predict <- read_csv("https://raw.githubusercontent.com/Dokkaebi10/dsci-310-group-18/main/data/modelling/predict_data.csv") %>%
    mutate(diagnosis_f = as.factor(diagnosis_f), .pred_class = as.factor(.pred_class))
test_that("Y axis of confusion matrix does not match", {
    expect_equivalent(confusion_matrix(heart_data_predict)$labels$y, "Prediction of diagnosis")})
test_that("X axis of confusion matrix does not match", {
    expect_equivalent(confusion_matrix(heart_data_predict)$labels$x, "Actual diagnosis")})
