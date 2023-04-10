# this script builds the visualization for the
# classification model. An elbow plot should be produced.

#' kNN visualization
#'
#' @import ggplot2
#'
#' @param data accuracy data created by kNN model
#' in classification_model.R
#' @param x is the horizontal variable for knn model
#' @param y is the vertical variable for knn model
#' @param xlab is the name of the x-axis
#' @param ylab is the name of the y-axis
#' @param title is the name of the model
#'
#' @return Elbow plot for kNN models.
#'
#' @export
#'
#'
knn_visualization <- function(data, x, y, xlab, ylab, title) {
accuracy_versus_k <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y))+
    ggplot2::geom_point() +
   ggplot2::geom_line() +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(text = element_text(size = 22)) +
    ggplot2::ylim(0.2, 0.8) +
    ggplot2::scale_x_continuous(breaks = 1:21)
accuracy_versus_k
}
