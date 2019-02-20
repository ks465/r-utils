# Version 0.1.0.971201

# Revision 971201-dev


#' Create a database connection object using Sqlite
#' @seealso \code{\link[=DBI]{dbConnect}} \code{\link{Database}}
#' @details Connect to a database and set \code{\link{con_obj}} in the object itself.
#' @param db_name string. Path to the file containing the database to load.
#' \code{:memory:} creates a temporary in-memory database.
#' @return Sqlite, an S3 object of type Sqlite containing the required parameter to act as
#' an interface to RSQLite.
#' @import RSQLite
#' @importFrom DBI dbConnect
#' @export
#' @examples
#' \dontrun{
#'con <- Sqlite(db_name = ':memory:')
#'con
#' #$db_name
#' #[1] ":memory:"
#'
#' #$con_obj
#' #<SQLiteConnection>
#' #  Path: :memory:
#' #  Extensions: TRUE
#'
#' #attr(,"class")
#' #[1] "Sqlite"
#' }
Sqlite <- function(db_name){
    value <- list(
        db_name = db_name,
        con_obj = DBI::dbConnect(RSQLite::SQLite(),
                                 dbname = db_name
        )
    )
    # class can be set using class() or attr() function
    attr(value, "class") <- "Sqlite"
    value
}


#' Test relationship between an object and DB object class
#' @seealso \code{\link{is}}
#' @details Check to see if the given object has the class \code{Sqlite}.
#' @param obj object. One of the four defined database connections.
#' @return boolean.
#' @export
#' @examples
#' \dontrun{
#' s1 <- Sqlite(db_name = ':memory:')
#' is.Sqlite(s1)
#' #[1] TRUE
#'
#' s2 <- list(db_name = ':memory:')
#' is.Sqlite(s2)
#' #[1] FALSE
#' }
is.Sqlite <- function(obj){
    return(attr(obj, 'class') == 'Sqlite')
}


#' @family Database
#' @seealso  listTables.default
listTables.Sqlite <- function(obj){
    if(!is.Sqlite(obj)){
        message('Connection object is not set. Nothing to do.')
        return(FALSE)
    }
    if(!is(obj$con_obj, 'DBIConnection')){
        message('Connection object is corrupted. Nothing to do.')
        return(FALSE)
    }
    return(dbListTables(obj$con_obj))
}


#' @family Database
#' @seealso  disconnect.default
#' @importFrom methods is
disconnect.Sqlite <- function(obj){
    if(!is.Sqlite(obj)){
        message('Connection object is not set. Nothing to do.')
        return(FALSE)
    }
    if(!is(obj$con_obj, 'DBIConnection')){
        message('Connection object is corrupted. Nothing to do.')
        return(FALSE)
    }
    w <- getOption('warn')
    options(warn = -1);
    dummy <- dbDisconnect(obj$con_obj)
    options(warn = w);
    obj$con_obj <- "NULL"
    attr(obj, "class") <- "NULL"

    return(dummy)
}


#' @family Database
#' @seealso  existsTable.default
existsTable.Sqlite <- function(obj, name){
    if(!is.Sqlite(obj)){
        message('Connection object is not set. Nothing to do.')
        return(FALSE)
    }
    if(!is(obj$con_obj, 'DBIConnection')){
        message('Connection object is corrupted. Nothing to do.')
        return(FALSE)
    }
    return(dbExistsTable(obj$con_obj, name))
}


#' @family Database
#' @seealso  writeTable.default
writeTable.Sqlite <- function(obj, DT, tableName){
    if(!is.Sqlite(obj)){
        message('Connection object is not set. Nothing to do.')
        return(FALSE)
    }
    if(!is(obj$con_obj, 'DBIConnection')){
        message('Connection object is corrupted. Nothing to do.')
        return(FALSE)
    }

    stopifnot(is.data.table(DT))

    dummy <- dbSendQuery(obj$con_obj, paste0('DELETE FROM ', tableName, ';'))
    result <- dbWriteTable(obj$con_obj, tableName, DT, row.names = FALSE, append = TRUE)

    return(result)
}


#' @family Database
#' @seealso  sendSQLString.default
sendSQLString.Sqlite <- function (obj, sqlString) {
    if(!is.Sqlite(obj)){
        message('Connection object is not set. Nothing to do.')
        return(FALSE)
    }
    if(!is(obj$con_obj, 'DBIConnection')){
        message('Connection object is corrupted. Nothing to do.')
        return(FALSE)
    }

    dta <- dbSendQuery(obj$con_obj, sqlString)
    dbClearResult(dta)

    return(TRUE)
}


#' @family Database
#' @seealso  readSQL.default
readSQL.Sqlite <- function (obj, sqlString, key = NULL) {
    if(!is.Sqlite(obj)){
        message('Connection object is not set. Nothing to do.')
        return(FALSE)
    }
    if(!is(obj$con_obj, 'DBIConnection')){
        message('Connection object is corrupted. Nothing to do.')
        return(FALSE)
    }

    DT <- setDT(dbGetQuery(obj$con_obj, sqlString), keep.rownames = FALSE, key = key, check.names = FALSE)

    return(DT)
}


#' @family Database
#' @seealso  readAll.default
readAll.Sqlite <- function(obj, tableName){
    return(readSQL(obj, paste('SELECT * FROM', tableName)))
}


#' @family Database
#' @seealso  readSQLFile.default
readSQLFile.Sqlite <- function (obj, sqlFile) {
    sqlString <- readLines(sqlFile)
    sqlString <- paste0(sqlString, collapse = ' ')
    DT <- readSQL(obj, sqlString)

    return(DT)
}
