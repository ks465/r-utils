# Version 0.10.4.960320

# Revision 970821-dev


#' Draw a horizontal boxplot.
#' @family Plots
#' @details
#' Create horizontal boxplots based on given value and partitions by the x column.
#' @note In a horizontal plot, x and y axes are swapped. So x-axis is vertical and y-axis is horizontal.
#' @param dataSet data frame. Data to put in the plot.
#' @param x string. Name of column used as factoring elemnt and shown in the vertical axis.
#' @param y string. Name of column used for building the plot. This should be a numeric vector column.
#' @param title string. Main title of the graph.
#' @param xlab string. Label of the x-axis, which is shown vertically here.
#' @param ylab string. Label of the y-axis, which is shown horizontally here.
#' @param notch boolean. If notch is TRUE, a notch is drawn in each side of the boxes.
#' If the notches of two plots do not overlap this is ‘strong evidence’ that the two medians differ.
#' \code{\link{boxplot}} for more information and \code{\link[=boxplot]{stats}} for the calculations used.
#' @return Class of type ggplot. Use \code{\link{plot}} for really draw the plots.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' Plots.createBoxPlot(sbm.data, 'student_status', 'average_semester', 'توزیع میانگین ترمی دانشجویان',)
#' }
Plots.createBoxPlot <- function(dataSet, x, y, title, xlab = NULL, ylab = NULL, notch = FALSE) {
    alpha <- aes_string(x, y)
    q <- ggplot(dataSet, alpha) +
        geom_boxplot(color = 'red', notch = notch) +
        coord_flip() +
        ggtitle(title) +
        # xlab(KH.load(xlab, ..Latex.removeDots(x))) +
        # ylab(KH.load(ylab, ..Latex.removeDots(y))) +
        Plots.theme()

    return(q)
}
