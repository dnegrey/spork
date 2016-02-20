#' @title Read a configuration file into a named list
#' @description \code{readConfig} reads a configuration file of key value pairs 
#' and stores the result in a named list
#' @param f file path to the configuration file
#' @return A named list of character values where the names are the keys from 
#' the configuration file and the values are the values from the configuration 
#' file.
#' @examples 
#' writeLines(c("COLOR=blue", "ANIMAL=dog"), "test")
#' readConfig("test")
#' @seealso \code{\link{readLines}, \link{strsplit}}
#' @export 
readConfig <- function(f) {
    x <- readLines(f)
    x <- strsplit(x, split = "=", fixed = TRUE)
    y <- lapply(x, function(x){x[[1]]})
    z <- lapply(x, function(x){x[[2]]})
    names(z) <- unlist(y)
    return(z)
}