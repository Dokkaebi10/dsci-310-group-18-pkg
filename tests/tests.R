library(testthat)
library(tidyverse)

source("../R/majority_classifier_function.R")
test_that("X-axis label is not the same!", {
          majority_classifier <- read_csv('../data/modelling/majority_classifier.csv')
          plot <- majority_classifier_vis_function(majority_classifier)
          expect_identical(plot$labels$x, 'Heart disease degree of severity')})
test_that("Y-axis label is not the same!", {
          majority_classifier <- read_csv('../data/modelling/majority_classifier.csv')
          plot <- majority_classifier_vis_function(majority_classifier)
          expect_identical(plot$labels$y, 'Percent of outcomes \n in training dataset')})

source("../R/selection_forward_function.R")
heart_data_subset <- read_csv('../data/modelling/forward_selection_subset.csv') %>%
    mutate(diagnosis_f = as.factor(diagnosis_f))
plot <- forwardSelection(heart_data_subset)
test_that("The number of variables is not the same!", {
          expect_equivalent(max(plot$size), 5)})
test_that("The results are not the same!", {
          expect_equal(min(plot$accuracy), 0.426789675597178, tolerance = 0.05)})

source("../R/place_data.R")
# test that there are the right number of columns
test_that("The number of columns for Switzerland is not the same!", {
    expect_equal(ncol(web_data('switzerland', url_switzerland)), 13)})
test_that("The number of columns for cleveland is not the same!", {
    expect_equal(ncol(web_data('cleveland', url_cleveland)), 13)})

# test that there are 200 rows
test_that("The number of rows in va are not the same!", {
    expect_equal(nrow(web_data('va', url_va)), 200)})
test_that("The number of rows in hungary are not the same!", {
    expect_equal(nrow(web_data('hungary', url_hungarian)), 200)})

source("../R/joining_data.R")
# testing the function clean_csv produces dataframes with correct data types
test_that("thal_f is not a factor (it should be)", {
    expect_true(is.factor(clean_csv('../data/raw/cleveland.csv')$thal_f))})
test_that("age is not numeric", {
    expect_true(is.numeric(clean_csv('../data/raw/va.csv')$age))})

source("../R/boxplots.R")
# Setup
heart_data <- read_csv('../data/processed/heart_data.csv')
boxplot_age <- grid_boxplot(heart_data, age, "Age (years)", "A. Boxplot of degree of heart \n disease in relation to patient's \nage")
boxplot_max_heart_rate <- grid_boxplot(heart_data, max_heart_rate, "Maximum heart rate (BPM)", "C. Boxplot of degree of heart \ndisease in relation to patient's \nmaximum heart rate")

# Check-expects
test_that("First boxplot check-expect is not the same!", {
    expect_equivalent(boxplot_age$labels$y, "Age (years)")
})
          
test_that("Second boxplot check-expect is not the same!", {
    expect_equivalent(boxplot_max_heart_rate$labels$y, "Maximum heart rate (BPM)")
})          

source("../R/classification_model.R")
# Test that there are 7 columns and 21 rows
heart_training <- read.csv('../data/modelling/training_split.csv') %>%
    mutate(diagnosis_f = as.factor(diagnosis_f))
test_that("Classifier data has different number of columns!", {
    expect_equal(ncol(classifier(heart_training)), 7)})
test_that("Classifier data has a different number of rows!", {
    expect_equal(nrow(classifier(heart_training)), 21)})

source("../R/model_visualization.R")
accuracy_data <- read_csv('../data/modelling/heart_data_accuracies.csv')
test_that("Y axis is not what we expected", {
    expect_equivalent(knn_visualization(accuracy_data)$labels$y, "Accuracy Estimate")})
test_that("X axis is not what we expected", {
    expect_equivalent(knn_visualization(accuracy_data)$labels$x, "Neighbors")
})

# Test that the estimate of our classifier is correct
source("../R/confusion_matrix.R")
heart_data_predict <- read_csv('../data/modelling/predict_data.csv') %>%
    mutate(diagnosis_f = as.factor(diagnosis_f), .pred_class = as.factor(.pred_class))
test_that("Y axis of confusion matrix does not match", {
    expect_equivalent(confusion_matrix(heart_data_predict)$labels$y, "Prediction of diagnosis")})
test_that("X axis of confusion matrix does not match", {
    expect_equivalent(confusion_matrix(heart_data_predict)$labels$x, "Actual diagnosis")})
