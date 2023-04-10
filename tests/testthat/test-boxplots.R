library(testthat)
library(tidyverse)

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
