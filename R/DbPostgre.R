# Version 0.2.1.951008
# Version 0.2.2.971201

# Revision 971201-dev


#' Create a database connection object using PostgreSQL DBMS.
#' @seealso \code{\link[=DBI]{dbConnect}} \code{\link{Database}}
#' @details Connect to a database and set \code{\link{con_obj}} in the object itself.
#' @param db_name string. Name of the database to load.
#' @param db_user string. User identification for the DBMS.
#' @param db_pass string. Password identifying the user.
#' @param db_host string. Name of the server hosting the database.
#' @param db_port integer. Port the server is listening.
#' @return Postgre, an S3 object of type Postgre containing the required parameter to act as
#' an interface to PostgreSQL.
#' @import RPostgreSQL
#' @importFrom DBI dbConnect
#' @export
#' @examples
#' \dontrun{
#'con <- Postgre(db_host = 'localhost', db_name = 'test', db_user = 'root', db_pass = '123456')
#'con
#' #$db_host
#' #[1] "127.0.0.1"
#'
#' #$db_port
#' #[1] 5432
#'
#' #$db_name
#' #[1] "test"
#'
#' #$db_user
#' #[1] "keyhan"
#'
#' #$db_pass
#' #[1] "123456"
#'
#' #$con_obj
#' #<PostgreSQLConnection>
#'
#' #attr(,"class")
#' #[1] "Postgre"
#' }
Postgre <- function(db_name, db_user, db_pass, db_host, db_port = 5432){
    value <- list(
        db_host = db_host,
        db_port = db_port,
        db_name = db_name,
        db_user = db_user,
        db_pass = db_pass,
        con_obj = DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                                 user = db_user,
                                 password = db_pass,
                                 host = db_host,
                                 port = db_port,
                                 dbname = db_name
        )
    )
    # class can be set using class() or attr() function
    attr(value, "class") <- "Postgre"
    value
}


#' Test relationship between an object and DB object class
#' @seealso \code{\link{is}}
#' @details Check to see if the given object has the class \code{Postgre}.
#' @param obj object. One of the four defined database connections.
#' @return boolean.
#' @export
#' @examples
#' \dontrun{
#' s1 <- Postgre(db_host = '127.0.0.1', db_pass = '123456', db_name = 'test', db_user = 'root')
#' is.Postgre(s1)
#' #[1] TRUE
#'
#' s2 <- list(db_host = '127.0.0.1', db_pass = '123456', db_name = 'test', db_user = 'root')
#' is.Postgre(s2)
#' #[1] FALSE
#' }
is.Postgre <- function(obj){
    return(attr(obj, 'class') == 'Postgre')
}


#' @family Database
#' @seealso  listTables.default
listTables.Postgre <- function(obj){
    if(!is.Postgre(obj)){
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
disconnect.Postgre <- function(obj){
    if(!is.Postgre(obj)){
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
existsTable.Postgre <- function(obj, name){
    if(!is.Postgre(obj)){
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
writeTable.Postgre <- function(obj, DT, tableName){
    if(!is.Postgre(obj)){
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
sendSQLString.Postgre <- function (obj, sqlString) {
    if(!is.Postgre(obj)){
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
readSQL.Postgre <- function (obj, sqlString, key = NULL) {
    if(!is.Postgre(obj)){
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
readAll.Postgre <- function(obj, tableName){
    return(readSQL(obj, paste('SELECT * FROM', tableName)))
}


#' @family Database
#' @seealso  readSQLFile.default
readSQLFile.Postgre <- function (obj, sqlFile) {
    sqlString <- readLines(sqlFile)
    sqlString <- paste0(sqlString, collapse = ' ')
    DT <- readSQL(obj, sqlString)

    return(DT)
}
