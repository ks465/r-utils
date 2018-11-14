# Version 0.1.1.970812

# Revision 970812-dev

options(encoding = "UTF-8")


# library(methods, quietly = TRUE)
# library(stats, quietly = TRUE)
# library(data.table, quietly = TRUE)


# .onLoad <- function(libname, pkgname){
# }


.onAttach <- function(libname, pkgname){
    # KH.set('db_name', 'stats_center_4')
    # KH.set('db_user', 'root')
    # KH.set('db_pass', '123456')
    # KH.set('db_host', '127.0.0.1')

    # KH.set('Excel.style.default', createStyle(
    #     fontName = KH.get('winFont'),
    #     fontSize = 11,
    #     border = 'TopBottom',
    #     borderColour = '#4F81BD',
    #     fontColour = '#006100',
    #     fgFill = '#C6EFCE'
    # ))
    packageStartupMessage("is still in development stage;)")
}


.onDetach <- function(libpath){
    KH.disconnect()

    message("KHan detached.")
}


.onUnload <- function(libpath){
    message("KHan unloaded.")
}
