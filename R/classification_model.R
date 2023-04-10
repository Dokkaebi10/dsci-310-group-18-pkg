set.seed(1)
#' K visualization
#'
#' @import caret
#' @import rsample
#' @import recipes
#' @import dplyr
#' @import parsnip
#' @import workflows
#' @import tune
#'
#' @param data is a a dataset used to be plot accuracy versus K
#' @param x is diagnosis_f
#' @param y is .metric
#' @param split is the number of diagnosis
#' @param gridFrom is the gridvals value
#' @param gridTo is the gridvals value
#'
#' @return a line graph with the accuracy estimates versus the number of neighbors
#'
#' @export
#'
#' @examples
#' classifier(heart_training, 5, 1, 21)
#'
classifier <- function(data, x, y, split =5 , gridFrom = 1, gridTo = 21) {
  set.seed(1)

  recipe <- recipes::recipe(x ~ ., data) %>%
    recipes::step_scale(all_predictors()) %>%
    recipes::step_center(all_predictors())

  heart_data_recipe <- recipe

  spec <- parsnip::nearest_neighbor(weight_func = "rectangular", neighbors = parsnip::tune()) %>%
    parsnip::set_engine("kknn") %>%
    parsnip::set_mode("classification")

  vfold <- rsample::vfold_cv(data, v = split, strata = x)

  gridvals <- dplyr::tibble(neighbors = seq(from = gridFrom, to = gridTo))

  results <- workflows::workflow() %>%
    workflows::add_recipe(recipe) %>%
    workflows::add_model(spec) %>%
    tune::tune_grid(resamples = vfold, grid = gridvals) %>%
    tune::collect_metrics()
  accuracies <- results %>%
    stats::filter(y == "accuracy")
}
