#' Testing accuracy of model
#' 
#' @param data is the predicted data of our classifier
#' @param hGraph is the height of the Confusion matrix
#' @param wGraph is the width of the Confusion matrix
#' @param textSize is size of text
#' 
#' @return a Confusion Matrix heat map with the accuracies based on variables of the model 
#' @export
#'
#' @examples 
#' confusion_matrix(data_predict, 7, 7, 20, "Prediction of diagnosis", "actual Diagnosis", "Confusion Matrix Plot: \nNumber of diagnosis that are correct \nand incorrect")
#' 
confusion_matrix <- function(data, hGraph = 7, wGraph = 7, textSize = 22) {
  confusion_matrix <- data %>%
      conf_mat(truth = diagnosis_f, estimate = .pred_class)
  
options(repr.plot.height = hGraph, repr.plot.width = wGraph)
confusion_vis <- autoplot(confusion_matrix, type = "heatmap") +
  theme(text = element_text(size = textSize)) +
  labs(y = "Prediction of diagnosis",
       x = "Actual diagnosis",
       title = "Confusion matrix plot: \n Number of diganoses that are correct \n and incorrect") +
  scale_fill_gradient(low = '#cce3ff', high = '#177fff')
confusion_vis
}

