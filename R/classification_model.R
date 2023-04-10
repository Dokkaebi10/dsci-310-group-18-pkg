set.seed(1)
#' K visualization 
#' 
#' @param data is a a dataset used to be plot accuracy versus K
#' @param split is the number of diagnosis
#' @param gridFrom is the gridvals value
#' @param gridTo is the gridvals value
#'
#' @return a line graph with the accuracy estimates versus the number of neighbors
#' 
#' @examples 
#' classifier(heart_training, 5, 1, 21)
#' 
#' @export
classifier <- function(data, split =5 , gridFrom = 1, gridTo = 21) {
  set.seed(1)
  
  recipe <- recipe(diagnosis_f ~ ., data) %>%
      step_scale(all_predictors()) %>%
      step_center(all_predictors())
    
  heart_data_recipe <<- recipe
  
  spec <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) %>%
      set_engine("kknn") %>%
      set_mode("classification")
  
  vfold <- vfold_cv(data, v = split, strata = diagnosis_f)
  
  gridvals <- tibble(neighbors = seq(from = gridFrom, to = gridTo))
  
  results <- workflow() %>%
      add_recipe(recipe) %>%
      add_model(spec) %>%
      tune_grid(resamples = vfold, grid = gridvals) %>%
      collect_metrics()
  accuracies <- results %>%
      filter(.metric == "accuracy")
}
