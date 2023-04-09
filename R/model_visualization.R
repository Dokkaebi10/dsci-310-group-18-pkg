library(ggplot2)

# this script builds the visualization for the 
# classification model. An elbow plot should be produced.

#' kNN visualization
#' 
#' @param data accuracy data created by kNN model 
#' in classification_model.R
#' 
#' @return Elbow plot for kNN models.
#' 
#' @example
#'  knn_visualization(heart_data_accuracies)
knn_visualization <- function(data) {
accuracy_versus_k <- ggplot(data, aes(x = neighbors, y = mean))+
    geom_point() +
   geom_line() +
    labs(x = "Neighbors", y = "Accuracy Estimate") +
    ggtitle("Plot of estimated accuracy versus the number of neighbors") +
    theme(text = element_text(size = 22)) +
    ylim(0.2, 0.8) +
    scale_x_continuous(breaks = 1:21)
accuracy_versus_k
}