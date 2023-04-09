library(ggplot2)

#' Converts dataset to a bar graph visualization
#'    
#' @param data a dataset that is intented to be turned into a bar graph
#' @param hGraph the height of the bar graph
#' @param wGraph the width of the bar graph
#' @param textSize the text size of the labels and title in the graph
#' @param colWidth the column width of the bars in the graph
#' @param xLabel the label on the x-axis
#' @param yLabel the label on the y-axis
#' @param titleLabel the title of the graph
#' 
#' @return a visualization of the majority classifier
#' 
#' @examples
#' majority_classifier_vis_function(diff_data_set0)
#' majority_classifier_vis_function(diff_data_set1, hgraph = 4, wGraph = 10, colWidth = 1)
majority_classifier_vis_function <- function(data, hGraph = 5, wGraph = 7, textSize = 20, colWidth = 0.5, xLabel = 'Heart disease degree of severity', yLabel = 'Percent of outcomes \n in training dataset', titleLabel = 'Label proportions of classifier in dataset'){
    options(repr.plot.height = hGraph, repr.plot_width = wGraph)
    total_rows <- nrow(read_csv('data/modelling/training_split_new.csv'))
    majority_classifier_vis <- data %>%
        mutate(percent_outcomes=percent_outcomes/100)%>%
        ggplot(aes(x = reorder(diagnosis_f,-percent_outcomes), y=percent_outcomes)) +
        geom_col(width = colWidth, fill = '#4362d1') +
        labs(x = xLabel,
             y = yLabel,
             title = titleLabel) +
        theme(text = element_text(size = textSize))+
        scale_y_continuous(labels=scales::percent, sec.axis=sec_axis(~.*total_rows,name="Count"))
}
