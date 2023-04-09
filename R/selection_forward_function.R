library(tidyverse)
library(tidymodels)
#' Forward selection step in model
#'    
#' @param data a dataset that is intented to be turned into a bar graph
#' @param seedVar the number inputted into set.seed()
#' @param folds the number of folds for cross validation
#' 
#' @return a table with the accuracies based on variables added to the model
#' 
#' @examples
#' forwardSelection(diff_data_set0)
#' forwardSelection(diff_data_set1, seedVar = 2, folds = 10)
forwardSelection <- function(data , seedVar = 1, folds=5){
    #create an empty tibble to store the results
    names<-colnames(data%>% select(-diagnosis_f))
    accuracies <- tibble(size = integer(), 
                         model_string = character(), 
                         accuracy = numeric())

    # create a model specification
    knn_spec <- nearest_neighbor(weight_func = "rectangular", 
                                 neighbors = tune()) %>%
         set_engine("kknn") %>%
         set_mode("classification")

    # create a 5-fold cross-validation object
    heart_vfold <- vfold_cv(data, v = 5, strata = diagnosis_f)

    # store the total number of predictors
    n_total <- length(names)

    # stores selected predictors
    selected <- c()

    # for every size from 1 to the total number of predictors
    for (i in 1:n_total) {
        # for every predictor still not added yet
        accs <- list()
        models <- list()
        for (j in 1:length(names)) {
            # create a model string for this combination of predictors
            preds_new <- c(selected, names[[j]])
            model_string <- paste("diagnosis_f", "~", paste(preds_new, collapse="+"))

            # create a recipe from the model string
            heart_recipe <- recipe(as.formula(model_string), 
                                    data = data) %>%
                                    #step_upsample(diagnosis_f, over_ratio = 1, skip = FALSE) %>%
                              step_scale(all_predictors()) %>%
                              step_center(all_predictors())

            # tune the KNN classifier with these predictors, 
            # and collect the accuracy for the best K
            acc <- workflow() %>%
              add_recipe(heart_recipe) %>%
              add_model(knn_spec) %>%
              tune_grid(resamples = heart_vfold, grid = 10) %>%
              collect_metrics() %>%
              filter(.metric == "accuracy") %>%
              summarize(mx = max(mean))
            acc <- acc$mx %>% unlist()

            # add this result to the dataframe
            accs[[j]] <- acc
            models[[j]] <- model_string
        }
        jstar <- which.max(unlist(accs))
        accuracies <- accuracies %>% 
          add_row(size = i, 
                  model_string = models[[jstar]], 
                  accuracy = accs[[jstar]])
        selected <- c(selected, names[[jstar]])
        names <- names[-jstar]
    }
    accuracies
}