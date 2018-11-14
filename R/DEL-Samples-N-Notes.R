# Version 0.1.0.970812

# Revision 970812-dev


#' Some dummy function to list selected ROxygen documentation tags
#' @encoding UTF-8
#'
#' @export
#' @keywords internals
#' @return data.table
#' @section Warning:
#' You must not call this function unless ...
#' \code{\link[magrittr]{\%>\%}}
#'\emph{italics} \strong{bold}
#' \code{r_function_call(with = "arguments")}: r_function_call(with = "arguments")
#' \preformatted{Ther
#' IS
#' <strong>text</strong>
#' HERE
#' }
#' \subsection{Exceptions}{
#'    Apart from the following special cases...
#' }
#' See `::is_falsy` for the definition of what is _falsy_
#' and what is _truthy_.

#' @references
#' Robert E Tarjan and Mihalis Yannakakis. (1984). Simple
#' @param DT data.table
#' @param ... Dots. This is usually \code{...}, although other types are accepted.
#' linear-time algorithms to test chordality of graphs, test acyclicity
#' of hypergraphs, and selectively reduce acyclic hypergraphs.
#' *SIAM Journal of Computation* **13**, 566-579.
#' @importFrom data.table as.data.table
#' @import data.table
#' @examples {
#' input_file <- system.file("extdata", "input_file.csv", package = "KHanSUtils")
#' DT <- KH.loadCSV(input_file)
#' }
test_function <- function(DT, ...){
    stopifnot(is.data.table(DT))

    assign("last.warning", NULL, envir = baseenv())

    DT_1 <- data.table(x=rep(c("b","a","c"),each=3), y=c(1,3,6), v=1:9)
    DT_2 <- data.table(x=rep(c("x","y","z"),each=3), y=c(2,4,7), v=11:19)
    DT <- rbind.data.frame(DT_1, DT_2)

    return('data.table')
}
