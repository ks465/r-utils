# Version 0.1.1.961123
# Version 0.2.1.970502
# Version 0.2.1.970516
# Version 0.3.0.971015
# Version 0.4.0.971127

# Revision 971127-dev

#' @family Database
#' @seealso  disconnect.default
#' @importFrom methods is
disconnect <- function(obj) {UseMethod("disconnect")}


#' @family Database
#' @seealso  listTables.default
listTables <- function(obj) {UseMethod("listTables")}


#' @family Database
#' @seealso  existsTable.default
existsTable <- function(obj, name) {UseMethod("existsTable")}


#' @family Database
#' @seealso  writeTable.default
writeTable <- function(obj, DT, tableName) {UseMethod("writeTable")}


#' @family Database
#' @seealso  sendSQLString.default
sendSQLString <- function(obj, sqlString) {UseMethod("sendSQLString")}


#' @family Database
#' @seealso  readSQL.default
readSQL <- function(obj, sqlString, key) {UseMethod("readSQL")}


#' @family Database
#' @seealso  readAll.default
readAll <- function(obj, tableName) {UseMethod("readAll")}


#' @family Database
#' @seealso  readSQLFile.default
readSQLFile <- function(obj, sqlFile) {UseMethod("readSQLFile")}


#' Disconnect database resource
#' @family Database
#' @aliases disconnect.Maria disconnect.Oracle disconnect.Postgre disconnect.Sqlite
#' @details Generic function for disconnecting active database resource
#' @param obj object. One of the four defined database connections.
#' @return boolean. Result of calling database \code{\link[=DBI]{dbDisconnect}}.
#' @export
disconnect.default <- function(obj) {stop("This is a generic function. You should not get here. Check your code.\n")}


#' List tables in the database
#' @family Database
#' @aliases listTables.Maria listTables.Oracle listTables.Postgre listTables.Sqlite
#' @details Generic function for getting list of tables in active database resource
#' @param obj object. One of the four defined database connections.
#' @return boolean. Result of calling database \code{\link[=DBI]{dblistTables}}.
#' @export
listTables.default <- function(obj) {stop("This is a generic function. You should not get here. Check your code.\n")}


#' Check Existance of a table
#' @family Database
#' @aliases existsTable.Maria existsTable.Oracle existsTable.Postgre existsTable.Sqlite
#' @details Checks if a table with the given name is present in connected database.
#' @param obj object. One of the four defined database connections.
#' @param name character. Name of the table for search.
#' @return boolean.
#' @export
existsTable.default <- function(obj, name) {stop("This is a generic function. You should not get here. Check your code.\n")}


#' Write and append data to the table.
#' @family Database
#' @aliases writeTable.Maria writeTable.Oracle writeTable.Postgre writeTable.Sqlite
#' @details Write a data.table to the table in chunks. Chunking the data frame is done for avoiding database
#' queue overflow.
#' @param obj object. One of the four defined database connections.
#' @param DT data.table Dataset ready to save in the database.
#' @param tableName string. Name of table.
#' @return boolean. Result of saving.
#' @export
writeTable.default <- function(obj, DT, tableName) {stop("This is a generic function. You should not get here. Check your code.\n")}


#' Send sequel from a string
#' @family Database
#' @details Execute queries without result sets on the server.
#' @param obj object. One of the four defined database connections.
#' @param sqlString string. Complete SQL string.
#' @return boolean. Result of execution.
#' @export
sendSQLString.default <- function(obj, sqlString) {stop("This is a generic function. You should not get here. Check your code.\n")}


#' Read sequel from string and return the data
#' @family Database
#' @details Send SQL to the databse server and read the data as a data.table.
#' @param obj object. One of the four defined database connections.
#' @param sqlString string. Complete SQL string.
#' @param key Optional key column(s).
#' @return data.table. Curser result set from server.
#' @import data.table
#' @export
readSQL.default <- function(obj, sqlString, key) {stop("This is a generic function. You should not get here. Check your code.\n")}


#' Read all the rows and columns in the given table.
#' @family Database
#' @details Read all of rows in the given table containing all the columns in the result set.
#' @param obj object. One of the four defined database connections.
#' @param tableName String. Name of a table in the database.
#' @return data.table Curser result set from server.
#' @import data.table
#' @export
readAll.default <- function(obj, tableName) {stop("This is a generic function. You should not get here. Check your code.\n")}


#' Read sequel from a file and return the data
#' @family Database
#' @details Send SQL saved in a file to the databse server and read the data as a data.table.
#' @param obj object. One of the four defined database connections.
#' @param sqlFile string. Full name of a text file containg a valid SQL query.
#' @return data.table Curser result set from server.
#' @import data.table
#' @export
readSQLFile.default <- function(obj, sqlFile) {stop("This is a generic function. You should not get here. Check your code.\n")}
