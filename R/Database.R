# Version 0.1.1.961123
# Version 0.2.1.970502
# Version 0.2.1.970516
# Version 0.3.0.971015

# Revision 971015-dev


#' Connect to a selected database.
#' @family Database
#' @seealso \code{\link[=DBI]{dbConnect}} \code{\link{Maria.connect}}
#' @details
#' Connect to a database and set \code{\link{db_con}} package configuration to the returned connection object.
#' There is only one connection object for this package. So only one type of database could be used.
#' @return object, an S4 object that inherits from DBIConnection.
#' This object is used to communicate with the database engine.
#' @import RMariaDB
#' @export
#' @examples
#' KH.set('db_name', 'stats_center_4')
#' KH.set('db_user', 'root')
#' KH.set('db_pass', '123456')
#' KH.set('db_host', '127.0.0.1')
#' KH.connect()
#' # [1] TRUE
KH.connect <- function() {
    if(is.null(KH.get('db_con'))){
        KH.set('db_con',
            DBI::dbConnect(RMariaDB::MariaDB(),
                user = KH.get('db_user'),
                password = KH.get('db_pass'),
                host = KH.get('db_host'),
                dbname = KH.get('db_name')
            )
        )
    }

    return(KH.get('db_con'))
}


#' Disconnect database resource
#' @family Database
#' @details Generic function for disconnecting active database rewource.
#' @return boolean. Result of calling database \code{\link[=DBI]{dbDisconnect}}.
#' @export
#' @examples
#' KH.disconnect()
#' # [1] TRUE
KH.disconnect <- function(){
    KH.con <- KH.get('db_con')
    if(is.null(KH.con)){
        message('Connection object is not set. Nothing to do.')
        return(FALSE)
    }
    if(!is(KH.con, 'DBIConnection')){
        message('Connection object is corrupted. Nothing to do.')
        return(FALSE)
    }
    w <- getOption('warn')
    options(warn = -1);
    dummy <- dbDisconnect(KH.con)
    options(warn = w);
    KH.set('db_con', NULL)

    return(TRUE)
}


#' Check Existance of a table and optionally check if it is empty
#' @family Database
#' @details Check for existence of th given table. If it is found and contains any records.
#' If \code{populated} is set to \code{TRUE}, and the table is empty, the result would be \code{FALSE}.
#' @param collectionName string. Name of table
#' @param populated boolean. Check for records.
#' @export
#' @return boolean.
KH.existsTable <- function(collectionName, populated = FALSE){
    result <- KH.readSQL(
        paste0('SELECT ', '* ', 'FROM `information_schema`.`tables` ',
               'WHERE `table_schema` = "', KH.get('db_name'),
               '" AND `table_name` = "', collectionName, '" LIMIT 1;'
        )
    )
    if(0 == nrow(result)){
        return(FALSE)
    }
    if(!populated){
        return(TRUE)
    }
    result <- Db.readSQL(paste0('SELECT COUNT(*) FROM `', collectionName, '`;'))
    if(0 == nrow(result)){
        return(FALSE)
    }

    return(TRUE)
}


#' Write and append data to the table.
#' @family Database
#' @details Write a data.table to the table in chunks. Chunking the data frame is done for avoiding database
#' queue overflow.
#' @param DT data.table Dataset ready to save in the database.
#' @param tableName string. Name of table.
#' @param field.types list. Named list of field types
#' @return boolean. Result of saving.
#' @export
#' @examples
#' @do
#' \dontrun{
#' dt <- KH.loadCSV("/var/www/html/stats45/update/oci_csv/department.csv")
#' KH.writeTable(dt, 'dept')
#' }
KH.writeTable <- function(DT, tableName, field.types = NULL){
    stopifnot(is.data.table(DT))

    dbBegin(KH.get('db_con'))

    dummy <- dbSendQuery(KH.get('db_con'), paste0('TRUNCATE TABLE `', tableName, '`;'))
    dummy <- dbWriteTable(KH.get('db_con'), tableName, DT,
                          row.names = FALSE, overwrite = FALSE, field.types = field.types)

    if(!dbCommit(KH.get('db_con'))){
        message(paste('Failed to commit', tableName, '!'))

        return(FALSE)
    }

    return(TRUE)
}


#' Send sequel from a string
#' @family Database
#' @details Execute queries without result sets on the server.
#' @param sqlString string. Complete SQL string.
#' @return boolean. Result of execution.
#' @export
#' @examples
#' \dontrun{
#' KH.sendSQLString(paste0('TRUNCATE TABLE `a_table`;'))
#' }
KH.sendSQLString <- function (sqlString) {
    dta <- dbSendQuery(KH.get('db_con'), sqlString)
    dbClearResult(dta)

    return(TRUE)
}


#' Read sequel from string and return the data
#' @family Database
#' @details Send SQL to the databse server and read the data as a data.table. Presumably the SQL string is
#' quoted in MariaDB standards (i.e.: using back quotes).
#' @param sqlString string. Complete SQL string.
#' @param key Optional key column(s).
#' @return data.table. Curser result set from server.
#' @export
#' @examples
#' KH.connect()
#' KH.readSQL('SELECT * FROM oci_departments LIMIT 1;', key = c('department_id'))
#' # department_id              title short_name campus ruling_department
#' # 1            10 معارف وعلوم انسانی      معارف   سایر                 0
#' KH.disconnect()
#' # [1] TRUE
KH.readSQL <- function (sqlString, key = NULL) {
    DT <- dbGetQuery(KH.get('db_con'), sqlString)
    DT <- setDT(DT, keep.rownames = FALSE, key = key, check.names=FALSE)

    return(DT)
}


#' Read all the rows and columns in the given table.
#' @family Database
#' @seealso \code{\link{KH.readSQL}}
#' @details #' Read all of rows in the given table containing all the columns in the result set.
#' @param collectionName String. Name of a table in the database.
#' @return data.table Curser result set from server.
#' For example see \code{\link{Db.readSQL}}
KH.readAll <- function(collectionName){
    return(KH.readSQL(paste('SELECT * FROM', collectionName)))
}


#' Read sequel from a file and return the data
#' @family Database
#' @seealso \code{\link{KH.readSQL}}
#' @details Send SQL saved in a file to the databse server and read the data as a data.table.
#' Presumably the SQL string is quoted in MariaDB standards (i.e.: using back quotes).
#' For example see \code{\link{KH.readSQL}}
#' @param sqlFile string. Full name of a text file containg a valid SQL query.
#' @return data.table Curser result set from server.
#' For example see \code{\link{Db.readSQL}}
KH.readSQLFile <- function (sqlFile) {
    sqlString <- readLines(sqlFile)
    sqlString <- paste0(sqlString, collapse = ' ')
    DT <- KH.readSQL(sqlString)

    return(DT)
}
