#' Testing accuracy of model
#'
#' @import caret
#' @import ggplot2
#' @import tidymodels
#' @import dplyr
#' @import parsnip
#'
#' @param data is the predicted data of our classifier
#' @param x is diagnosis_f
#' @param y is .pred_Class
#' @param hGraph is the height of the Confusion matrix
#' @param wGraph is the width of the Confusion matrix
#' @param textSize is size of text
#'
#' @return a Confusion Matrix heat map with the accuracies based on variables of the model
#'
#' @export
#'
#' @examples
#' confusion_matrix(data_predict, 7, 7, 22)
#'
confusion_matrix <- function(data, x, y, hGraph = 7, wGraph = 7, textSize = 22) {
  confusion_matrix <- data %>%
      caret::conf_mat(truth = x, estimate = y)

options(repr.plot.height = hGraph, repr.plot.width = wGraph)
confusion_vis <- ggplot2::autoplot(confusion_matrix, type = "heatmap") +
  ggplot2::theme(text = element_text(size = textSize)) +
  ggplot2::labs(y = "Prediction of diagnosis",
                x = "Actual diagnosis",
                title = "Confusion matrix plot: \n Number of diganoses that are correct \n and incorrect") +
  ggplot2::scale_fill_gradient(low = '#cce3ff', high = '#177fff')
confusion_vis
}
