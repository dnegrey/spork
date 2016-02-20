#' @title Convert numbers to text while applying pretty formatting
#' @description \code{cleanNumberFormat} converts numeric values to character 
#' and can apply various pretty formatting
#' @param x numeric vector on which to apply formatting/conversion
#' @param type the type of formatting to use; valid values are "int", "dlr" and 
#' "pct"
#' @param digits non-negative integer value indicating the number of decimal 
#' places to use
#' @return A character vector of the converted/formatted numeric input.
#' @examples 
#' x <- rnorm(5)
#' print(x)
#' cleanNumberFormat(x, "pct", 1)
#' cleanNumberFormat(x*10000, "int")
#' cleanNumberFormat(x*10000, "dlr", digits = 2)
#' @seealso \code{\link{round}, \link{trimws}}
#' @export 
cleanNumberFormat <- function(x, type, digits = 0) {
    if (type == "int") {
        y <- format(round(x, digits), nsmall = digits, big.mark = ",")
        y <- trimws(y)
    } else if (type == "dlr") {
        y <- format(round(x, digits), nsmall = digits, big.mark = ",")
        y <- trimws(y)
        y <- paste0("$", y)
    } else if (type == "pct") {
        y <- format(100*round(x, digits + 2), nsmall = digits, big.mark = ",")
        y <- trimws(y)
        y <- paste0(y, "%")
    } else {
        y <- x
    }
    return(y)
}