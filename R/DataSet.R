# Version 0.1.0.970812

# Revision 970812-dev


#' Load data from a CSV file into `data.table`
#' @family DataSets
#' @details
#' Use only data of \code{data.table} type. Many functions of this package work with this type of data only.
#' @param csv_filename string. Full URL to the CSV file.
#' @param key atomic or vector of string. Name of fields used in the keys. \emph{\strong{NOT Used for now.}}
#' @param index atomic or vecotr of string. Name of fields used in indexing data. \emph{\strong{NOT Used for now.}}
#' @param colClasses list of named strings. Name and classes of fields used to read and load data. \emph{\strong{NOT Used for now.}}
#' @return data.table. An object of type \code{data.table}
#' @note All the functions in this package accept data.table only.
#' @importFrom data.table fread
#' @export
#' @examples {
#' input_file <- system.file("extdata", "input_file.csv", package = "KHanSUtils")
#' KH.loadCSV(input_file)
#'
#' #year month  day dep_delay arr_delay carrier origin dest air_time distance hour
#' #1: 2014  TRUE TRUE        14        13      AA    JFK  LAX      359     2475    9
#' #2: 2014  TRUE TRUE        -3        13      AA    JFK  LAX      363     2475   11
#' #3: 2014  TRUE TRUE         2         9      AA    JFK  LAX      351     2475   19
#' #4: 2014  TRUE TRUE        -8       -26      AA    LGA  PBI      157     1035    7
#' #5: 2014  TRUE TRUE         2         1      AA    JFK  LAX      350     2475   13
#' #6: 2014  TRUE TRUE         4         0      AA    EWR  LAX      339     2454   18
#' }
KH.loadCSV <- function(csv_filename, key=NULL, index=NULL, colClasses=NULL){
    DT <- fread(csv_filename
                # ,colClasses = c(B="character",C="character",D="character"))
                # ,colClasses = list(character=2:4))     # same using column numbers
                ,colClasses = colClasses
                ,key = key
                ,index = index
                ,encoding = 'UTF-8'
                ,stringsAsFactors = FALSE
                ,blank.lines.skip = TRUE
                ,showProgress = TRUE
                ,data.table = TRUE
                ,logical01 = TRUE
                ,check.names = TRUE
                )
    return(DT)
}


#' Drop column or columns from a data.table by using the names.
#' @family DataSets
#' @param DT data.table Two dimension data object which \code{colnames(x)} is meaningful.
#' @param currentTitles character. Vector of current column names.
#' @return data.table. The same object as input with columns removed.
#' @importFrom data.table is.data.table
#' @export
#' @examples
#' set.seed(1000)
#' library(data.table)
#' d <- data.table(
#' group = sample(LETTERS[1:3], 10, replace = TRUE),
#' integer = rnorm(10, 5, 1),
#' integer1 = rnorm(10, 5, 1),
#' integer2 = rnorm(10, 5, 1),
#' decimal = rnorm(10, 5, 1)
#' )
#' colnames(d)
#' #[1] "group" "integer" "integer1" "integer2" "decimal"
#' d <- KH.dropColsByName(d, 'integer')
#' colnames(d)
#' #[1] "group" "integer1" "integer2" "decimal"
#'
#' d <- KH.dropColsByName(d, c('integer1', 'integer2'))
#' colnames(d)
#' #[1] "group" "decimal"
KH.dropColsByName <- function(DT, currentTitles){
    stopifnot(is.data.table(DT))

    return(DT[,!..currentTitles])
}


#' Change name of columns in a data set
#' @family DataSets
#' @param DT data.table. Two dimension data object which \code{colnames(x)} is meaningful.
#' @param currentTitles character. Vector of current column names.
#' @param newTitles character. Vector of new names after renaming.
#' @importFrom data.table is.data.table
#' @export
#' @return data.table. The same data object as input with name of columns changed.
#' @examples {
#' set.seed(1000)
#' library(data.table)
#' d <- data.table(
#' group = sample(LETTERS[1:3], 10, replace = TRUE),
#' integer = rnorm(10, 5, 1),
#' decimal = rnorm(10, 5, 1)
#' )
#' colnames(d)
#' #[1] "group" "integer" "decimal"
#' d <- KH.changeColName(d, 'integer', 'zeroPrecision')
#' colnames(d)
#' #[1] "group" "zeroPrecision" "decimal"
#' d <- KH.changeColName(d, c('group', 'decimal'), c('newGroup', 'float'))
#' colnames(d)
#' #[1] "newGroup" "zeroPrecision" "float"
#' }
KH.changeColName <- function(DT, currentTitles, newTitles){
    stopifnot(is.data.table(DT))
    stopifnot(length(currentTitles) == length(newTitles))

    columns <- colnames(DT)

    for(i in 1:length(currentTitles)){
        id <- grep(currentTitles[i], columns, fixed = TRUE)
        columns[id] <- newTitles[i]
    }
    colnames(DT) <- columns

    return(DT)
}
