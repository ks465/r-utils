# Version 0.1.1.961123
# Version 0.2.1.970502
# Version 0.2.1.970516
# Version 0.3.0.971015

# Revision 971015-dev


#' Create a database connection object using MariaDB DBMS.
#' @seealso \code{\link[=DBI]{dbConnect}} \code{\link{Database}}
#' @details Connect to a database and set \code{\link{con_obj}} in the object itself.
#' @param db_name string. Name of the database to load.
#' @param db_user string. User identification for the DBMS.
#' @param db_pass string. Password identifying the user.
#' @param db_host string. Name of the server hosting the database.
#' @param db_port integer. Port the server is listening.
#' @return Maria, an S3 object of type Maria containing the required parameter to act as
#' an interface to RMariaDB.
#' @import RMariaDB
#' @importFrom DBI dbConnect
#' @export
#' @examples
#' \dontrun{
#' s1 <- Maria(db_host = '127.0.0.1', db_name = 'test', db_user = 'root', db_pass = '123456')
#' s1
#' #$db_host
#' #[1] "127.0.0.1"
#'
#' #$db_port
#' #[1] 3306
#'
#' #$db_name
#' #[1] "test"
#'
#' #$db_user
#' #[1] "root"
#'
#' #$db_pass
#' #[1] "123456"
#'
#' #$con_obj
#' #<MariaDBConnection>
#' #  Host:    127.0.0.1
#' #  Server:  5.5.60-MariaDB
#' #  Client:  5.5.60-MariaDB
#'
#' #attr(,"class")
#' #[1] "Maria"
#'}
Maria <- function(db_name, db_user, db_pass, db_host, db_port = 3306){
    value <- list(
        db_host = db_host,
        db_port = db_port,
        db_name = db_name,
        db_user = db_user,
        db_pass = db_pass,
        con_obj = DBI::dbConnect(RMariaDB::MariaDB(),
                                 user = db_user,
                                 password = db_pass,
                                 host = db_host,
                                 port = db_port,
                                 dbname = db_name
        )
    )
    # class can be set using class() or attr() function
    attr(value, "class") <- "Maria"
    value
}


#' Test relationship between an object and DB object class
#' @seealso \code{\link{is}}
#' @details Check to see if the given object has the class \code{Maria}.
#' @param obj object. One of the four defined database connections.
#' @return boolean.
#' @export
#' @examples
#' \dontrun{
#' s1 <- Maria(db_host = '127.0.0.1', db_pass = '123456', db_name = 'test', db_user = 'root')
#' is.Maria(s1)
#' #[1] TRUE
#'
#' s2 <- list(db_host = '127.0.0.1', db_pass = '123456', db_name = 'test', db_user = 'root')
#' is.Maria(s2)
#' #[1] FALSE
#'}
is.Maria <- function(obj){
    return(attr(obj, 'class') == 'Maria')
}


#' @family Database
#' @seealso  listTables.default
listTables.Maria <- function(obj){
    if(!is.Maria(obj)){
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
disconnect.Maria <- function(obj){
    if(!is.Maria(obj)){
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
existsTable.Maria <- function(obj, name){
    if(!is.Maria(obj)){
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
writeTable.Maria <- function(obj, DT, tableName){
    if(!is.Maria(obj)){
        message('Connection object is not set. Nothing to do.')
        return(FALSE)
    }
    if(!is(obj$con_obj, 'DBIConnection')){
        message('Connection object is corrupted. Nothing to do.')
        return(FALSE)
    }

    stopifnot(is.data.table(DT))

    dummy <- dbSendQuery(obj$con_obj, paste0('TRUNCATE TABLE ', tableName, ';'))
    result <- dbWriteTable(obj$con_obj, tableName, DT, row.names = FALSE, append = TRUE)

    return(result)
}


#' @family Database
#' @seealso  sendSQLString.default
sendSQLString.Maria <- function (obj, sqlString) {
    if(!is.Maria(obj)){
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
readSQL.Maria <- function (obj, sqlString, key = NULL) {
    if(!is.Maria(obj)){
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
readAll.Maria <- function(obj, tableName){
    return(readSQL(obj, paste('SELECT * FROM', tableName)))
}


#' @family Database
#' @seealso  readSQLFile.default
readSQLFile.Maria <- function (obj, sqlFile) {
    sqlString <- readLines(sqlFile)
    sqlString <- paste0(sqlString, collapse = ' ')
    DT <- readSQL(obj, sqlString)

    return(DT)
}
