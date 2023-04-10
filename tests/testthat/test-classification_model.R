# Test that there are 7 columns and 21 rows
heart_training <- read.csv("https://raw.githubusercontent.com/Dokkaebi10/dsci-310-group-18/main/data/modelling/training_split.csv") %>%
  mutate(diagnosis_f = as.factor(diagnosis_f))
test_that("Classifier data has different number of columns!", {
  expect_equal(ncol(classifier(heart_training)), 7)})
test_that("Classifier data has a different number of rows!", {
  expect_equal(nrow(classifier(heart_training)), 21)})
