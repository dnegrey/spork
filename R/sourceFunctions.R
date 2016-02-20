#' @title Source a directory of R functions
#' @description \code{sourceFunctions} sources all of the \code{*.R} files 
#' found in the specified directory
#' @param dir path containing the files to source
#' @param recursive logical value; recurse into sub-directories?
#' @return The \code{source} function is applied to any {*.R} files found in 
#' \code{dir}.
#' @examples 
#' write("foo <- function(){'Hello, world!'}", "foo.R")
#' sourceFunctions(".")
#' @seealso \code{\link{source}, \link{list.files}}
#' @export 
sourceFunctions <- function(dir, recursive = TRUE) {
    f <- list.files(
        dir,
        pattern = ".R$",
        full.names = TRUE,
        recursive = recursive
    )
    invisible(
        lapply(f, source)
    )
}