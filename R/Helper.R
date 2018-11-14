# Version 0.1.0.961205
# Version 0.1.1.970517

# Revision 970823-dev


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
#' @return DT data.table. All the decimal points in the given columns are replaced with Persian equivalent.
#' @export
#' @examples{
#' KH.changeDecimal(1.23)
#' #[1] "1٫23"
#' KH.changeDecimal(12.3)
#' #[1] "12٫3"
#' KH.changeDecimal('a.23')
#' #[1] "a.23"
#' KH.changeDecimal(c(1.23, 0.02))
#' #[1] "1٫23" "0٫02"
#'
#' columns <- c('dep_x','arr_x')
#' input_file <- system.file("extdata", "input_file.csv", package = "KHanSUtils")
#' DT <- KH.loadCSV(input_file)
#'
#' DT[, c('dep_x','arr_x') := list(dep_delay/10, arr_delay/10)]
#' DT
#' #    year month day dep_delay arr_delay carrier origin dest air_time distance hour dep_x arr_x
#' # 1: 2014     1   1        14        13      AA    JFK  LAX      359     2475    9   1.4   1.3
#' # 2: 2014    10  31         1       -30      UA    LGA  IAH      201     1416   14   0.1  -3.0
#' # 3: 2014     1   1        -3        13      AA    JFK  LAX      363     2475   11  -0.3   1.3
#' # 4: 2014    10  31        -5       -14      UA    EWR  IAH      189     1400    8  -0.5  -1.4
#' # 5: 2014     1   1         2         9      AA    JFK  LAX      351     2475   19   0.2   0.9
#' # 6: 2014    10  31        -8        16      MQ    LGA  RDU       83      431   11  -0.8   1.6
#' # 7: 2014     1   1        -8       -26      AA    LGA  PBI      157     1035    7  -0.8  -2.6
#' # 8: 2014    10  31        -4        15      MQ    LGA  DTW       75      502   11  -0.4   1.5
#' # 9: 2014     1   1         2         1      AA    JFK  LAX      350     2475   13   0.2   0.1
#' #10: 2014    10  31        -5         1      MQ    LGA  SDF      110      659    8  -0.5   0.1
#' #11: 2014     1   1         4         0      AA    EWR  LAX      339     2454   18   0.4   0.0
#'
#' KH.changeDecimal(DT, columns)
#' #    year month day dep_delay arr_delay carrier origin dest air_time distance hour dep_x arr_x
#' # 1: 2014     1   1        14        13      AA    JFK  LAX      359     2475    9   1٫4   1٫3
#' # 2: 2014    10  31         1       -30      UA    LGA  IAH      201     1416   14   0٫1    -3
#' # 3: 2014     1   1        -3        13      AA    JFK  LAX      363     2475   11  -0.3   1٫3
#' # 4: 2014    10  31        -5       -14      UA    EWR  IAH      189     1400    8  -0.5  -1.4
#' # 5: 2014     1   1         2         9      AA    JFK  LAX      351     2475   19   0٫2   0٫9
#' # 6: 2014    10  31        -8        16      MQ    LGA  RDU       83      431   11  -0.8   1٫6
#' # 7: 2014     1   1        -8       -26      AA    LGA  PBI      157     1035    7  -0.8  -2.6
#' # 8: 2014    10  31        -4        15      MQ    LGA  DTW       75      502   11  -0.4   1٫5
#' # 9: 2014     1   1         2         1      AA    JFK  LAX      350     2475   13   0٫2   0٫1
#' #10: 2014    10  31        -5         1      MQ    LGA  SDF      110      659    8  -0.5   0٫1
#' #11: 2014     1   1         4         0      AA    EWR  LAX      339     2454   18   0٫4     0
#' }
KH.changeDecimal <- function(DT, columns = c()) {
    persianDecimal <- "\u066B"
    persianReplacement <- paste0("\\1", persianDecimal, "\\3")

    if(is.data.table(DT)){
        DT[, lapply(.SD, gsub, pattern='^([0-9]+)([.])([0-9]+)$', replacement=persianReplacement, .SD)]
    }else{
        DT <- gsub('^([0-9]+)([.])([0-9]+)$', '\\1\u066B\\3', DT, perl = TRUE)
    }

    return(DT)
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


#' Round a data.table to different precision in various columns
#' @description
#' With only one line, change the precision of different data vectors in a data.table.
#' One group is casted into integer, and one group is rounded to the given precision.
#' @family Utils
#' @param DT data.table. Data object wich has at least one numeric colum.
#' @param integerList list of integers refering to columns' id or name. These columns are casted to integer -- and NOT rounded.
#' @param decimalList list of integers refering to columns' id or name. These columns are rounded to the given precision.
#' @param decimal integer. Set precision for vectors in decimalList. Default is \code{decimal} in package.
#' @param latex logic. Add math sections to decimalList items (e.g.:\code{n}, \code{NAN}, \code{NA}, \code{INF}, etc.)
#'  for Persian XePersain output. Decimal point also is changed XePersian. Default is \code{TRUE}
#' @param ... Dots. An arbitrary number and variety of arguments, passing arguments on to other functions.
#' @return data.table. The same data.table as iput with certain numeric values rounded.
#' @note This function has side effect and changes the input data.frame.
#' @export
#' @import data.table
#' @examples
#' set.seed(1000)
#' library(data.table)
#' d <- data.table(
#' group = sample(LETTERS[1:3], 10, replace = TRUE),
#' integer = rnorm(10, 5, 1),
#' decimal = rnorm(10, 5, 1)
#' )
#' d
#' #    group  integer  decimal
#' # 1:     A 4.614511 5.170057
#' # 2:     C 4.524132 5.155079
#' # 3:     A 5.719751 5.024932
#' # 4:     C 4.981494 2.953415
#' # 5:     B 3.626882 5.213154
#' # 6:     A 4.017572 7.670072
#' # 7:     C 4.445511 3.772984
#' # 8:     B 5.121381 5.834247
#' # 9:     A 4.879128 5.532572
#' #10:     A 3.663959 4.353175
#'
#' d1 <- KH.roundAll(copy(d), integerList = c(2), decimalList = c(3)) # default precision is presumably 2
#' d1
#' #    group integer decimal
#' # 1:     A       4    5.17
#' # 2:     C       4    5.16
#' # 3:     A       5    5.02
#' # 4:     C       4    2.95
#' # 5:     B       3    5.21
#' # 6:     A       4    7.67
#' # 7:     C       4    3.77
#' # 8:     B       5    5.83
#' # 9:     A       4    5.53
#' #10:     A       3    4.35
#'
#' d1 <- KH.roundAll(copy(d), integerList = c(2), decimalList = c(3), decimal = 3)
#' d1
#' #    group integer decimal
#' # 1:     A       4    5.170
#' # 2:     C       4    5.155
#' # 3:     A       5    5.025
#' # 4:     C       4    2.953
#' # 5:     B       3    5.213
#' # 6:     A       4    7.670
#' # 7:     C       4    3.773
#' # 8:     B       5    5.834
#' # 9:     A       4    5.533
#' #10:     A       3    4.353
#'
#' d1 <- KH.roundAll(copy(d), integerList = c('integer', 'decimal'))
#' d1
#' #    group integer decimal
#' # 1:     A       4       5
#' # 2:     C       4       5
#' # 3:     A       5       5
#' # 4:     C       4       2
#' # 5:     B       3       5
#' # 6:     A       4       7
#' # 7:     C       4       3
#' # 8:     B       5       5
#' # 9:     A       4       5
#' #10:     A       3       4
#'
#' KH.roundAll(d, decimalList = c('integer', 'decimal'), decimal = 3)
#' d
#' #    group integer decimal
#' # 1:     A   4.615   5.170
#' # 2:     C   4.524   5.155
#' # 3:     A   5.720   5.025
#' # 4:     C   4.981   2.953
#' # 5:     B   3.627   5.213
#' # 6:     A   4.018   7.670
#' # 7:     C   4.446   3.773
#' # 8:     B   5.121   5.834
#' # 9:     A   4.879   5.533
#' #10:     A   3.664   4.353
KH.roundAll <- function(DT, integerList = NULL, decimalList = NULL, decimal = KH.get(variable = 'decimal', default = 1), latex = TRUE, ...){
    stopifnot(is.data.table(DT))

    if(!is.null(integerList)){
        DT[, (integerList) := lapply(.SD, as.integer), .SDcols = integerList]
    }
    if(!is.null(decimalList)){
        DT[, (decimalList) := lapply(.SD, round, ..decimal), .SDcols = decimalList]

        # if(latex){
        #     for(i in decimalList){
        #         x[[i]] <- KH.changeDecimal(x[[i]])
        #
        #         x[is.infinite(x[[i]])
        #           | is.nan(x[[i]])
        #           | is.na(x[[i]])
        #           , i] <- '$--$'
        #         # x[[i]] <- paste0('$', x[[i]], '$')
        #     }
        # }
    }

    return(DT)
}


#' Separate data set into separated output for each department
#' @details After separating data based on the given column, each part is written to a separate file
#' in the given directory. If the directory does not exist, it will be created. If the directory exists
#' and contains data, they will be overwritten without notice.
#' @param DT data.table. Data object containing multiple rows and columns.
#' @param departingColumn string A single value which name of one of the columns in the data set.
#' This will be used to separate data.
#' @param outputDir string. Path to a directory to save the output. If it does not exist, it will be created.
#' Data inside the directory will be overwritten without notice.
#' @param writeWholeToo logical. If TRUE write whole data set into a file alongside the separated files.
#' Default is TRUE.
#' @param output.format string. Format of the output files. Default is CSV.
#' @param ... Dots. This is usually \code{...}, although other types are accepted.
#' @importFrom data.table is.data.table
#' @export
#' @examples {
#' input_file <- system.file("extdata", "input_file.csv", package = "KHanSUtils")
#' DT <- KH.loadCSV(input_file)
#' KH.depart(DT, 'origin', '/tmp/xXx')
#'
#' }
KH.depart <- function(DT, departingColumn, outputDir, writeWholeToo = TRUE, output.format='csv', ...){
    stopifnot(is.data.table(DT))

    if(!dir.exists(outputDir)){
        dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
    # }else{
        # unlink(paste0(outputDir , '/*.', output.format))
    }

    writer <- NULL
    if('csv' == output.format){
        writer <- KH.writeCSV
    }
    if('xlsx' == output.format){
        stop('This engine is not available yet.')
    }

    if(is.null(writer)){
        stop('I could not find the Writer engine.')
    }

    departList <- unique(DT[[departingColumn]])

    resultOK <- TRUE
    for(dept in departList){
        data <- DT[DT[[departingColumn]] == dept, !..departingColumn]
        # resultOK <- resultOK & KH.writeCSV(data, paste0(outputDir, '/', dept, '.csv'))
        resultOK <- resultOK & writer(data, paste0(outputDir, '/', dept, '.csv'))

    }
    if(!resultOK){
        print('Something went wrong')
        return(FALSE)
    }

    if(writeWholeToo){
        # resultOK <- resultOK & KH.writeCSV(DT, paste0(outputDir, '/', '00-', departingColumn, '.csv'))
        resultOK <- resultOK & writer(DT, paste0(outputDir, '/', '00-', departingColumn, '.csv'))
    }

    return(resultOK)
}


#' Write data set into a CSV file
#' @param DT data.table. Data set ready to be written.
#' @param csv_filename string. Full URL to the CSV file.
#' @note If the file already exists, it would be overwritten without notice.
#' @importFrom data.table fwrite
#' @export
#' @examples {
#' set.seed(1000)
#' library(data.table)
#' d <- data.table(
#' group = sample(LETTERS[1:3], 10, replace = TRUE),
#' integer = rnorm(10, 5, 1),
#' decimal = rnorm(10, 5, 1)
#' )
#'
#' KH.writeCSV(d, '/tmp/test_csv')
#' #[1] TRUE
#' KH.loadCSV('/tmp/test_csv')
#' #    group  integer  decimal
#' # 1:     A 4.614511 5.170057
#' # 2:     C 4.524132 5.155079
#' # 3:     A 5.719751 5.024932
#' # 4:     C 4.981494 2.953415
#' # 5:     B 3.626882 5.213154
#' # 6:     A 4.017572 7.670072
#' # 7:     C 4.445511 3.772984
#' # 8:     B 5.121381 5.834247
#' # 9:     A 4.879128 5.532572
#' #10:     A 3.663959 4.353175
#' }
KH.writeCSV <- function(DT, csv_filename){
    stopifnot(is.data.table(DT))

    fwrite(DT, file = csv_filename)
    return(TRUE)
}


#' Write data set into a Excel 2007 (XLSX) file
#' @param DT data.table. Data set ready to be written.
#' @param xls_filename string. Full URL to the excel file.
#' @note If the file already exists, it would be overwritten without notice.
#' @import openxlsx
#' @export
#' @examples {
#' set.seed(1000)
#' library(data.table)
#' d <- data.table(
#' group = sample(LETTERS[1:3], 10, replace = TRUE),
#' integer = rnorm(10, 5, 1),
#' decimal = rnorm(10, 5, 1)
#' )
#'
#' KH.writeXlsx(d, '/tmp/test_xlsx')
#' #[1] TRUE
#' KH.loadXlsx('/tmp/test_xlsx')
#' #    group  integer  decimal
#' # 1:     A 4.614511 5.170057
#' # 2:     C 4.524132 5.155079
#' # 3:     A 5.719751 5.024932
#' # 4:     C 4.981494 2.953415
#' # 5:     B 3.626882 5.213154
#' # 6:     A 4.017572 7.670072
#' # 7:     C 4.445511 3.772984
#' # 8:     B 5.121381 5.834247
#' # 9:     A 4.879128 5.532572
#' #10:     A 3.663959 4.353175
#' }
KH.writeXlsx <- function(DT, xls_filename){
    stopifnot(is.data.table(DT))

    sheetID <- 'pGrad_Data'
    wb <- createWorkbook()
    addWorksheet(wb = wb, sheetName = sheetID)
    writeData(wb = wb, sheet = sheetID, x = DT, rowNames = FALSE)
    saveWorkbook(wb, xls_filename, overwrite = TRUE)

    return(TRUE)
}
