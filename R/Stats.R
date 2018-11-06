# Version 0.3.3.941113

# Revision 970812-dev


#' Get \code{sd} with rounded output
#' @seealso \code{\link{sd}}
#' @family Stats
#' @details
#' Just a wrapper for the generic \code{sd} function with extra rounding.
#' @param x numeric. Vector of numbers to calculate standard deviation.
#' @param  decimal integer. Set precision for resultant standard deviation. Default is \code{decimal} in package.
#' @param ... Dots. An arbitrary number and variety of arguments, passing arguments on to \code{sd} function.
#' @return numeric. Standard deviation of the vector rounded to the given precision.
#' @importFrom stats sd
#' @export
#' @examples
#' set.seed(100)
#' KH.sd(c(rnorm(10, mean = 0)))
#' #[1] 0.56
#' set.seed(100)
#' KH.sd(c(rnorm(10, mean = 0)), decimal = 3)
#' #[1] 1.561
KH.sd <- function(x, decimal = KH.get(variable = 'decimal', default = 1), ...)
    return(round(sd(x, ...), digits = decimal))


#' Get \code{mean} with rounded output
#' @seealso \code{\link{mean}}
#' @family Stats
#' @details Just a wrapper for the generic \code{mean} function with extra rounding.
#' @param x numeric. Vector of numbers to calculate mean.
#' @param decimal integer. Set precision for vectors in resultant mean. Default is \code{decimal} in package.
#' @param ... Dots. An arbitrary number and variety of arguments, passing arguments on to \code{mean} function.
#' @export
#' @return numeric. Mean of the vector rounded to the given precision.
#' @examples
#' set.seed(100)
#' KH.mean(c(rnorm(10, sd = 1)))
#' #[1] -0.02
#' set.seed(100)
#' KH.mean(c(rnorm(10, sd = 1)), decimal = 3)
#' #[1] -0.018
KH.mean <- function(x, decimal = KH.get(variable = 'decimal', default = 1), ...)
    return(round(mean(x, ...), digits = decimal))


#' Get \code{median} with rounded output
#' @seealso \code{\link{median}}
#' @family Stats
#' @details Just a wrapper for the generic \code{median} function with extra rounding.
#' @param x numeric. Vector of numbers to calculate median.
#' @param decimal integer. Set precision for vectors in resultant median. Default is \code{decimal} in package.
#' @param ... Dots. An arbitrary number and variety of arguments, passing arguments on to \code{median} function.
#' @importFrom stats median
#' @export
#' @return numeric. Median of the vector rounded to the given precision.
#' @examples
#' set.seed(100)
#' KH.median(c(rnorm(10, sd = 1)))
#' #[1] -0.02
#' set.seed(100)
#' KH.median(c(rnorm(10, sd = 1)), decimal = 3)
#' #[1] -0.018
KH.median <- function(x, decimal = KH.get(variable = 'decimal', default = 1), ...)
    return(round(median(x, ...), digits = decimal))


#' Get \code{min} with rounded output
#' @seealso \code{\link{min}}
#' @family Stats
#' @details Just a wrapper for the generic \code{min} function with extra rounding.
#' @param x numeric. Vector of numbers to calculate min
#' @param decimal integer. Set precision for vectors in resultant min Default is \code{decimal} in package.
#' @param ... Dots. An arbitrary number and variety of arguments, passing arguments on to \code{min} function.
#' @export
#' @return numeric. Min of the vector rounded to the given precision.
#' @examples
#' set.seed(100)
#' KH.min(c(rnorm(10, sd = 1)))
#' #[1] -0.02
#' set.seed(100)
#' KH.min(c(rnorm(10, sd = 1)), decimal = 3)
#' #[1] -0.018
KH.min <- function(x, decimal = KH.get(variable = 'decimal', default = 1), ...)
    return(round(min(x, ...), digits = decimal))


#' Get \code{max} with rounded output
#' @seealso \code{\link{max}}
#' @family Stats
#' @details Just a wrapper for the generic \code{max} function with extra rounding.
#' @param x numeric. Vector of numbers to calculate max.
#' @param decimal integer. Set precision for vectors in resultant max. Default is \code{decimal} in package.
#' @param ... Dots. An arbitrary number and variety of arguments, passing arguments on to \code{max} function.
#' @export
#' @return numeric. Max of the vector rounded to the given precision.
#' @examples
#' set.seed(100)
#' KH.max(c(rnorm(10, sd = 1)))
#' #[1] -0.02
#' set.seed(100)
#' KH.max(c(rnorm(10, sd = 1)), decimal = 3)
#' #[1] -0.018
KH.max <- function(x, decimal = KH.get(variable = 'decimal', default = 1), ...)
    return(round(max(x, ...), digits = decimal))


#' Get \code{IQR} with rounded output
#' @seealso \code{\link{IQR}}
#' @family Stats
#' @details
#' Just a wrapper for the generic \code{IQR} function with extra rounding.
#' @param x numeric. Vector of numbers to calculate interquartile range.
#' @param decimal integer. Set precision for vectors in resultant interquartile range. Default is \code{decimal} in package.
#' @param ... Dots. An arbitrary number and variety of arguments, passing arguments on to \code{IQR} function.
#' @return numeric. IQR of the vector rounded to the given precision.
#' @importFrom stats IQR
#' @export
#' @examples
#' set.seed(100)
#' KH.IQR(c(rnorm(10, sd = 1)))
#' #[1] 0.74
#' set.seed(100)
#' KH.IQR(c(rnorm(10, sd = 1)), decimal = 3)
#' #[1] 0.738
KH.IQR <- function(x, decimal = KH.get(variable = 'decimal', default = 1), ...)
    return(round(IQR(x, ...), digits = decimal))


#' Get \code{quantile} with rounded output
#' @seealso \code{\link{quantile}}
#' @family Stats
#' @details
#' Just a wrapper for the generic \code{quantile} function with extra rounding.
#' @param x numeric. Vector of numbers to calculate quantile.
#' @param p numeric. Vector or value of probabilities.
#' @param decimal integer. Set precision for vectors in resultant quantile. Default is \code{decimal} in package.
#' @param ... Dots. An arbitrary number and variety of arguments, passing arguments on to \code{quantile} function.
#' @return numeric. quantile of the vector rounded to the given precision.
#' @importFrom stats quantile
#' @export
#' @examples
#' set.seed(100)
#' KH.quant.n(c(rnorm(10, sd = 1)), 0.10)
#' #  10%
#' #-0.61
#' set.seed(100)
#' KH.quant.n(c(rnorm(10, sd = 1)), 0.10, decimal = 3)
#' #  10%
#' #-0.606
KH.quant.n <- function(x, p, decimal = KH.get(variable = 'decimal', default = 1), ...)
    return(round(quantile(x, p, ...), digits = decimal))


#' Get \code{quantile} with rounded output for probability of 0.10
#' @inherit KH.quant.n seealso return
#' @family Stats
#' @param x numeric. Vector of numbers to calculate quantile.
#' @param decimal integer. Set precision for vectors in resultant quantile. Default is \code{decimal} in package.
#' @param ... Dots. An arbitrary number and variety of arguments, passing arguments on to \code{quantile} function.
#' @return numeric. quantile of the vector rounded to the given precision.
#' @importFrom stats quantile
#' @export
#' @examples
#' set.seed(100)
#' KH.quant.10(c(rnorm(10, sd = 1)))
#' #  10%
#' #-0.61
#' set.seed(100)
#' KH.quant.10(c(rnorm(10, sd = 1)), decimal = 3)
#' #  10%
#' #-0.606
KH.quant.10 <- function(x, decimal = KH.get(variable = 'decimal', default = 1), ...)
    return(KH.quant.n(x, 0.10, decimal, ...))


#' Get \code{quantile} with rounded output for probability of 0.25
#' @inherit KH.quant.n seealso return
#' @family Stats
#' @param x numeric. Vector of numbers to calculate quantile.
#' @param decimal integer. Set precision for vectors in resultant quantile. Default is \code{decimal} in package.
#' @param ... Dots. An arbitrary number and variety of arguments, passing arguments on to \code{quantile} function.
#' @return numeric. quantile of the vector rounded to the given precision.
#' @importFrom stats quantile
#' @export
#' @examples
#' set.seed(100)
#' KH.quant.25(c(rnorm(10, sd = 1)))
#' #  25%
#' #-0.36
#' set.seed(100)
#' KH.quant.25(c(rnorm(10, sd = 1)), decimal = 3)
#' #  25%
#' #-0.467
KH.quant.25 <- function(x, decimal = KH.get(variable = 'decimal', default = 1), ...)
    return(KH.quant.n(x, 0.25, decimal, ...))


#' Get \code{quantile} with rounded output for probability of 0.75
#' @inherit KH.quant.n seealso return
#' @family Stats
#' @param x numeric. Vector of numbers to calculate quantile.
#' @param decimal integer. Set precision for vectors in resultant quantile. Default is \code{decimal} in package.
#' @param ... Dots. An arbitrary number and variety of arguments, passing arguments on to \code{quantile} function.
#' @return numeric. quantile of the vector rounded to the given precision.
#' @export
#' @examples
#' set.seed(100)
#' KH.quant.75(c(rnorm(10, sd = 1)))
#' # 75%
#' #0.27
#' set.seed(100)
#' KH.quant.75(c(rnorm(10, sd = 1)), decimal = 3)
#' # 75%
#' #0.272
KH.quant.75 <- function(x, decimal = KH.get(variable = 'decimal', default = 1), ...)
    return(KH.quant.n(x, 0.75, decimal, ...))


#' Produce table of nine-numbers grouped by a given column in the data.
#'
#' @family Stats
#' @details
#' For an appropriate insight into the data 9 numbers are consulted usually.
#' These nine numbers consist of \code{length, minimum, lower-hinge, median, upper-hinge, maximum, mean, sd}
#' of a data vector. This function creates a row for each grouped item.
#' @param DT data.table date.table of at least one column of numbers and at least one column of factoring element to calculate statistics.
#' @param x string. Name of vector containing the data to extract statistics for.
#' @param groupBy string. Name of vector containig grouping factors.
#' @param titles string. Vector of strings with 9 member as col.names of the results.
#' @param decimal integer. Set precision for vectors in resultant quantile. Default is \code{decimal} in package.
#' @param order string. Not implemented yet. Sort order of columns.
#' @importFrom stats quantile
#' @export
#' @import data.table
#' @examples {
#' set.seed(100)
#' library(data.table)
#' df <- data.table(
#'    x = rnorm(20),
#'    y = sample(LETTERS[1:3], 20, replace = TRUE)
#' )
#' x=KH.nineNumbers.by(df, 'x', 'y')
#' }
KH.nineNumbers.by <- function(DT, x, groupBy, titles = NULL, decimal = 'default', order = NULL) {
    stopifnot(is.data.table(DT))

    if('default' == decimal){
        decimal <- KH.get('decimal')
    }

    DT[, .(
        count = .N,
        min = lapply(.SD, KH.min),
        quartile1st = lapply(.SD, KH.quant.25),
        median = lapply(.SD, KH.median),
        quartile3st = lapply(.SD, KH.quant.75),
        max = lapply(.SD, KH.max),
        IQR = lapply(.SD, KH.IQR),
        mean = lapply(.SD, KH.mean),
        sd = lapply(.SD, KH.sd)
    ), keyby = groupBy, .SDcols = x]
}

#' Produce single row of nine-numbers for the given column in the data.
#' @family Stats
#' @details
#' For an appropriate insight into the data 9 numbers are consulted usually.
#' These nine numbers consist of \code{length, minimum, lower-hinge, median, upper-hinge, maximum, mean, sd}
#' of a data vector. This function creates a row for total data.
#' @param DT data.table. data.table of at least one column of numbers and at least one column of factoring element to calculate statistics.
#' @param x string. Name of vector containing the data to extract statistics for.
#' @param titles string. Vector of strings with 9 member as col.names of the results.
#' @param decimal integer. Set precision for vectors in resultant quantile. Default is \code{decimal} in package.
#' @param rowname string. A title for the row. This is handy for attaching the output to the table from \code{KH.nineNumbers.by}.
#' @import data.table
#' @export
#' @examples {
#' set.seed(100)
#' library(data.table)
#' df <- data.table(
#'    x = rnorm(20),
#'    y = sample(LETTERS[1:3], 20, replace = TRUE)
#' )
#' z=KH.nineNumbers.total(df, 'x')
#' }
KH.nineNumbers.total <- function(DT, x, titles = NULL, decimal = 'default', rowname = NULL) {
    stopifnot(is.data.table(DT))

    if('default' == decimal){
        decimal <- KH.get('decimal')
    }

    DT[, .(
        y = 'Total',
        count = .N,
        min = lapply(.SD, min),
        quartile1st = lapply(.SD, KH.quant.25),
        median = lapply(.SD, KH.median),
        quartile3st = lapply(.SD, KH.quant.75),
        max = lapply(.SD, max),
        IQR = lapply(.SD, IQR),
        mean = lapply(.SD, mean),
        sd = lapply(.SD, sd)
    ), .SDcols = x]
}
#' Produce complete table of nine-numbers grouped by a given column in the data plus a total row.
#' @family Stats
#' @details
#' For an appropriate insight into the data 9 numbers are consulted usually.
#' These nine numbers consist of \code{length, minimum, lower-hinge, median, upper-hinge, maximum, mean, sd}
#' of a data vector. This function creates both a table and a row for total data.
#' @param DT data.table. data.table of at least one column of numbers and at least one column of factoring element to calculate statistics.
#' @param x string. Name of vector containing the data to extract statistics for.
#' @param groupBy string. Vector containig name of grouping factors columns.
#' @param titles string. Vector of strings with 9 member as col.names of the results.
#' @param rowname string. A title for the row. This is handy for attaching the output to the table from \code{Stats.nineNumbers.by}.
#' @param decimal integer. Set precision for vectors in resultant quantile. Default is \code{decimal} in package.
#' @param order string. Not implemented yet. Sort order of columns.
#' @import data.table
#' @export
#' @examples {
#' set.seed(100)
#' library(data.table)
#' df <- data.table(
#'    x = rnorm(20),
#'    y = sample(LETTERS[1:3], 20, replace = TRUE)
#' )
#' y=KH.nineNumbers.table(df, 'x', 'y')
#' }
KH.nineNumbers.table <- function(DT, x, groupBy, titles = NULL, rowname = NULL, decimal = 'default', order = 'default') {
    stopifnot(is.data.table(DT))
    rbind.data.frame(
        KH.nineNumbers.by(DT, x, groupBy),
        KH.nineNumbers.total(DT, x)
        )
}
