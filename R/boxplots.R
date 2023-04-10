# Sam Thorne
# 2023-03-10
# DSCI310 group 18 project

# this script contains a function that builds 3 box plots
# and displays them in a grid for initial visualizations
# of the data.

#' Boxplot Visualization
#'
#' Make a boxplot based on the given variables for data, y-axis, y-axis label, and the title of the boxplot and with x always as degree of the heart disease
#'
#' @import tidyverse
#' @import ggplot2
#' @import cowplot
#'
#' @param data this is a dataframe with 14 columns and every place
#'  this data frame is the output of joining_data.R
#' @param x is diagnosis_f of joining_data.R
#' @param yAxis is the variable input for the y-axis
#' @param yLabel is a string that indicates what the label for the y-axis should be
#' @param titleLabel is a string for the title of the graph
#'
#' @returns A boxplot given the variables that always has diagosis_f as the x variable
#'
#' @export
#'
#' @examples
#' grid_boxplots(heart_data, age,"ylabel", "Whatever title you'd like")
#'
grid_boxplot <- function(data, x, yAxis, yLabel, titleLabel){
    boxplot_age <- ggplot2::ggplot(data, ggplot2::aes(x=x, y= {{yAxis}})) +
    ggplot2::geom_boxplot(color="#033291", fill="#b0c8f7", alpha=0.2) +
    ggplot2::labs(x="Degree of heart disease", y=yLabel) +
    ggplot2::ggtitle(titleLabel) +
    ggplot2::theme(text = ggplot2::element_text(size = 12))
}
