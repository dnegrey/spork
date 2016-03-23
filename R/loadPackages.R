#' @title Load the packages specified in a file
#' @description \code{loadPackages} attempts to load all of the packages listed 
#' in the specified file
#' @param file character value; path/name of the file containing the package list
#' @return A logical vector describing the success of each \code{library} call.
#' @examples 
#' writeLines(c("dplyr", "ggplot2", "rmarkdown"), "PACKAGES")
#' loadPackages("PACKAGES")
#' @seealso \code{\link{library}, \link{readLines}}
#' @export 
loadPackages <- function(file) {
    sapply(
        readLines(file),
        function(x){
            cat(sprintf("Loading package: %s\n", x))
            invisible(
                library(x, character.only = TRUE, logical.return = TRUE)
            )
        }
    )
}