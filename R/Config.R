# Version 0.1.0.970814
# Version 0.2.0.971127

# Revision 971127-dev


#' List of all the configuration settings in this application, which are adjustable by the user.
#'
#' @name Configuration
#' @title Configuration
#' @family Config
#' @details Configuration Object has no methods. In order to set or get configuration values, use \code{\link{KH.set}} or use \code{\link{KH.get}}
#' @importFrom methods new
#' @section Fields:
#' \describe{
#' \item{\strong{defaultFont}:}{character.
#' Default font used for output for general use; mainly Xetex. \code{defaultFont} defaults to \code{Nazli}.
#' }
#' \item{\strong{winFont}:}{character.
#' Default font used for english text in Xetex. \code{latinFont} defaults to \code{DejaVu Sans}.
#' }
#' \item{\strong{latinFont}:}{character.
#' Default font used for output for MSWord documents; mainly Docx and Xlsx. \code{winFont} defaults to \code{B Nazanin}.
#' }
#' \item{\strong{decimal}:}{integer.
#' Default precission for rounding numbers. \code{decimal} defaults to \code{2}.
#' }
#' \item{\strong{workingDirectory}:}{character.
#' Default working directory for a given script. \code{workingDirectory} defaults to \code{NULL}.
#' }
#' \item{\strong{authorName}:}{character.
#' Name of the package author.
#' }
#' \item{\strong{authorEmail}:}{character.
#' eMail of the package author.
#' }
#' }

KHanConfig <- methods::setRefClass(
    "KHanConfig",
    fields = list(
        defaultFont = 'character',
        winFont = 'character',
        latinFont = 'character',
        decimal = 'integer',
        workingDirectory = 'character',
        authorName = 'character',
        authorEmail = 'character'
    )
)


KHanConfig$methods(
    initialize = function() {
        print('initialized')
        defaultFont <<- 'Nazli'
        winFont <<- 'B Nazanin'
        latinFont <<- 'DejaVu Sans'
        decimal <<- 2L
        workingDirectory <<- ''
        authorName <<- 'Keyhan Sedaghat'
        authorEmail <<- 'keyhansedaghat@aut.ac.ir'
    }
)


KH.config <- KHanConfig$new()

# KH.config$winFont
# KH.config$set_winFont('alpha')
# KH.config$winFont <- 'beta'

#' Get module configurations.
#'
#' @details Critical package-wide variables are saved in a module environment.
#' This function reads a given variable from this environment only.
#' It stops if a foreign key is requested.
#' @family Config
#' @keywords internals
#' @param variable String. Name of the variable to search for.
#' @param default Mixed. Default value if the object is not present, defaults to \code{NULL}.
#' @return Mixed. Based on the requested variable.
#' @export
#' @examples
#' KH.get('defaultFont')
#' #[1] "Nazli"
#'
#' KH.set('defaultFont', 'B Nazanin')
#' KH.get('defaultFont')
#' # "B Nazanin"
KH.get <- function(variable, default = NULL){
    if(!hasName(KH.config, variable)){
        stop(paste('Requested configuration field,', variable, ' does not exist.'))
    }
    out <- KH.config[[variable]]
    if(is.null(out)){
        out <- default
    }

    return(out)
}

#' Save a new value for module environment.
#' @details In order to change a default value or function definition, set the value using this method.
#' It stops if a foreign key is requested
#' @family Config
#' @note Use this only to redefine a variable. It could not be used to create a variable.
#' @param variable string. Name of the variable or function to change.
#' @param value mixed. New value of the object.
#' @return mixed. New value of the variable.
#' @export
#' @examples
#' KH.set('defaultFont', 'B Nazanin')
#' #[1] "B Nazanin"
#'
#' KH.set('decimal', 3)
#' #[1] 3
#' \dontrun{
#' KH.set('new_var', 2)
#'
#' KH.set('hello', function(){print('hello')})
#' # function(){print("hello")}
#' }
KH.set <- function(variable, value){
    if(!hasName(KH.config, variable)){
        stop(paste('Requested configuration field,', variable, ' does not exist.'))
    }
    KH.config[[variable]] <- value

    return(KH.get(variable))
}

