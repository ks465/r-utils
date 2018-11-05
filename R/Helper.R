# Version 0.1.0.961205
# Version 0.1.1.970517

# Revision 970812-dev


#' Assign value to the variable.
#'
#' @details
#' Useful for times checking for \code{NULL} and decide the outcome of a value.
#' @family Utils
#' @param userSet Mixed The desired value to assign as output.
#' @param defaultSet Mixed. The default used when the \code{userSet} is \code{NULL}.
#' @return Mixed. New value of the variable.
#' @export
#' @examples
#' KH.ifNull(NULL, 'B Nazanin')
#' #[1] "B Nazanin"
#'
#' KH.ifNull('defaultFont', 'B Nazanin')
#' #[1] "defaultFont"
#'
KH.ifNull <- function(userSet, defaultSet) {
    if (is.null(userSet)) {
        lab <- defaultSet
    }else{
        lab <- userSet
    }

    return(lab)
}

#' Format numbers in the table so they are correct in Persian.
#' @encoding UTF-8
#' @keywords internals
#' @import data.table
#' @family Utils
#' @details
#' Decimal point in persian is different from that in English. This function replaces it so the
#' decimal numbers are read correctly
#' @param DT data.table. Numerical data required for changing the decimal point.
#' @param columns string|number. Columns' names or numbers in the data to change.
#' @examples{
#' KH.changeDecimal(1.23)
#' #[1] "1٫23"
#' KH.changeDecimal(12.3)
#' #[1] "12٫3"
#' KH.changeDecimal('a.23')
#' #[1] "a.23"
#'
#' columns <- c('dep_x','arr_x')
#' input_file <- system.file("extdata", "input_file.csv", package = "KHanSUtils")
#' DT <- KH.loadCSV(input_file)
#'
#' DT[, c('dep_x','arr_x') := list(dep_delay/10, arr_delay/10)]
#' DT
#' #   dep_delay arr_delay carrier origin dest air_time distance hour dep_x arr_x
#' #1:        14        13      AA    JFK  LAX      359     2475    9   1.4   1.3
#' #2:        -3        13      AA    JFK  LAX      363     2475   11  -0.3   1.3
#' #3:         2         9      AA    JFK  LAX      351     2475   19   0.2   0.9
#' #4:        -8       -26      AA    LGA  PBI      157     1035    7  -0.8  -2.6
#' #5:         2         1      AA    JFK  LAX      350     2475   13   0.2   0.1
#' #6:         4         0      AA    EWR  LAX      339     2454   18   0.4   0.0
#'
#' KH.changeDecimal(DT, columns)
#' #   dep_delay arr_delay carrier origin dest air_time distance hour dep_x arr_x
#' #1:        14        13      AA    JFK  LAX      359     2475    9   1٫4   1٫3
#' #2:        -3        13      AA    JFK  LAX      363     2475   11  -0.3   1٫3
#' #3:         2         9      AA    JFK  LAX      351     2475   19   0٫2   0٫9
#' #4:        -8       -26      AA    LGA  PBI      157     1035    7  -0.8  -2.6
#' #5:         2         1      AA    JFK  LAX      350     2475   13   0٫2   0٫1
#' #6:         4         0      AA    EWR  LAX      339     2454   18   0٫4     0
#' }
KH.changeDecimal <- function(DT, columns = c()) {
    persianDecimal <- "\u066B"
    persianReplacement <- paste0("\\1", persianDecimal, "\\3")

    DT[, lapply(.SD, gsub, pattern='^([0-9]+)([.])([0-9]+)$', replacement=persianReplacement, .SD)]

    # return(DT)
}


#' Select rows based on minimum a valuein a given column
#'
#' @family Datasets
#' @param DT data.table. Data object wich has at least one string / factor column and one numeric colum.
#' @param groupBy character or vector of characters. Name of column to use for grouping rows of data.
#' If this is set to \code{NULL}, a single row containing \code{MIN(selectByMinOf)} will be returned.
#' @param selectByMinOf character. Name of column to calculate \code{MIN()}.
#' @return data.table. data.table oject with the same columns as input data set and rows containing the minimum
#' of target column.
#' @details
#' This function groups rows of input data according to \code{groupBy} value and filter the
#' code{MIN(selectByMinOf)} for each group.
#' @importFrom data.table is.data.table
#' @importFrom utils head
#' @export
#' @examples {
#' input_file <- system.file("extdata", "input_file.csv", package = "KHanSUtils")
#' DT <- KH.loadCSV(input_file)
#' DT
#'
#' #    year month day dep_delay arr_delay carrier origin dest air_time distance hour
#' # 1: 2014    10  31        -4        15      MQ    LGA  DTW       75      502   11
#' # 2: 2014    10  31        -8        16      MQ    LGA  RDU       83      431   11
#' # 3: 2014    10  31        -5         1      MQ    LGA  SDF      110      659    8
#' # 4: 2014     1   1        -8       -26      AA    LGA  PBI      157     1035    7
#' # 5: 2014    10  31        -5       -14      UA    EWR  IAH      189     1400    8
#' # 6: 2014    10  31         1       -30      UA    LGA  IAH      201     1416   14
#' # 7: 2014     1   1         4         0      AA    EWR  LAX      339     2454   18
#' # 8: 2014     1   1         2         1      AA    JFK  LAX      350     2475   13
#' # 9: 2014     1   1         2         9      AA    JFK  LAX      351     2475   19
#' #10: 2014     1   1        14        13      AA    JFK  LAX      359     2475    9
#' #11: 2014     1   1        -3        13      AA    JFK  LAX      363     2475   11
#'
#' groupBy <- c('carrier','origin')
#' selectByExtOf <- 'air_time'
#'
#' KH.selectByMin(DT,groupBy, selectByExtOf)
#'
#' #    carrier origin year month day dep_delay arr_delay dest air_time distance hour
#' # 1:      AA    EWR 2014     1   1         4         0  LAX      339     2454   18
#' # 2:      AA    JFK 2014     1   1         2         1  LAX      350     2475   13
#' # 3:      AA    LGA 2014     1   1        -8       -26  PBI      157     1035    7
#' # 4:      MQ    LGA 2014    10  31        -4        15  DTW       75      502   11
#' # 5:      UA    EWR 2014    10  31        -5       -14  IAH      189     1400    8
#' # 6:      UA    LGA 2014    10  31         1       -30  IAH      201     1416   14
#' }
KH.selectByMin <- function(DT, groupBy, selectByMinOf) {
    stopifnot(is.data.table(DT))

    setorderv(DT, c(groupBy, selectByMinOf))
    DT[, head(.SD, 1), keyby = groupBy]
}


#' Select rows based on maximum a value in a given column
#' @family Datasets
#' @param DT data.table. Data object wich has at least one string / factor column and one numeric colum.
#' @param groupBy character or vector of characters. Name of column to use for grouping rows of data.
#' If this is set to \code{NULL}, a single row containing \code{MAX(selectByMaxOf)} will be returned.
#' @param selectByMaxOf character. Name of column to calculate \code{MAX()}.
#' @return data.table. data.table oject with the same columns as input data set and rows containing the maximum
#' of target column.
#' @details
#' This function groups rows of input data according to \code{groupBy} value and filter the
#' \code{MAX(selectByMaxOf)} for each group.
#' @importFrom data.table is.data.table
#' @importFrom utils tail
#' @export
#' @examples {
#' input_file <- system.file("extdata", "input_file.csv", package = "KHanSUtils")
#' DT <- KH.loadCSV(input_file)
#' DT
#'
#' #    year month day dep_delay arr_delay carrier origin dest air_time distance hour
#' # 1: 2014    10  31        -4        15      MQ    LGA  DTW       75      502   11
#' # 2: 2014    10  31        -8        16      MQ    LGA  RDU       83      431   11
#' # 3: 2014    10  31        -5         1      MQ    LGA  SDF      110      659    8
#' # 4: 2014     1   1        -8       -26      AA    LGA  PBI      157     1035    7
#' # 5: 2014    10  31        -5       -14      UA    EWR  IAH      189     1400    8
#' # 6: 2014    10  31         1       -30      UA    LGA  IAH      201     1416   14
#' # 7: 2014     1   1         4         0      AA    EWR  LAX      339     2454   18
#' # 8: 2014     1   1         2         1      AA    JFK  LAX      350     2475   13
#' # 9: 2014     1   1         2         9      AA    JFK  LAX      351     2475   19
#' #10: 2014     1   1        14        13      AA    JFK  LAX      359     2475    9
#' #11: 2014     1   1        -3        13      AA    JFK  LAX      363     2475   11
#'
#' groupBy <- c('carrier','origin')
#' selectByExtOf <- 'air_time'
#'
#' KH.selectByMax(DT,groupBy, selectByExtOf)
#'
#' #    carrier origin year month day dep_delay arr_delay dest air_time distance hour
#' # 1:      AA    EWR 2014     1   1         4         0  LAX      339     2454   18
#' # 2:      AA    JFK 2014     1   1        -3        13  LAX      363     2475   11
#' # 3:      AA    LGA 2014     1   1        -8       -26  PBI      157     1035    7
#' # 4:      MQ    LGA 2014    10  31        -5         1  SDF      110      659    8
#' # 5:      UA    EWR 2014    10  31        -5       -14  IAH      189     1400    8
#' # 6:      UA    LGA 2014    10  31         1       -30  IAH      201     1416   14
#' }
KH.selectByMax <- function(DT, groupBy, selectByMaxOf) {
    stopifnot(is.data.table(DT))

    setorderv(DT, c(groupBy, selectByMaxOf))
    DT[, tail(.SD, 1), keyby = groupBy]
}


#' Separate data set into separated output for each department
#' @param DT data.table
#' @param departmentColumn character
#' @param outputDir string
#' @param writeUnivToo logical
#' @param create.dir logical
#' @param output.format character
#' @param ... Dots. This is usually \code{...}, although other types are accepted.
#' @importFrom data.table is.data.table
#' @export
KH.depart <- function(DT, departmentColumn, outputDir, writeUnivToo = TRUE, create.dir = FALSE, output.format='xlsx', ...){
    stopifnot(is.data.table(DT))

}

