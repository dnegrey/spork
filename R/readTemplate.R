#' @title Read in a template file and apply optional replacements
#' @description \code{readTemplate} reads \code{file} and uses \code{sprintf} 
#' to make string replacements when specified
#' @param file character value; path/name of the template file
#' @param addLineBreaks logical value; include line breaks (\code{\\n}) in 
#' result?
#' @param ... objects to use as string replacements
#' @return A character value containing the contents of \code{f} after any 
#' specified string replacements have been made.
#' @examples 
#' x <- c("select %s", "from %s", "where %s")
#' write(x, "x.sql")
#' readTemplate("x.sql", FALSE, "*", "Table", "1=1")
#' y <- readTemplate("x.sql", TRUE, "*", "Table", "1=1")
#' print(y)
#' write(y, "x2.sql")
#' @seealso \code{\link{readLines}, \link{sprintf}}
#' @export 
readTemplate <- function(file, addLineBreaks = FALSE, ...) {
    cl <- ifelse(addLineBreaks, "\n", " ")
    x <- readLines(file, warn = FALSE)
    x <- paste(x, collapse = cl)
    x <- sprintf(x, ...)
    return(x)
}