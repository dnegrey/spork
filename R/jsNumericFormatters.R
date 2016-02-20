#' @title Return JavaScript functions that format numeric data
#' @description \code{jsNumericFormatters} returns a character string 
#' containing a JavaScript function for formatting numeric data. This is useful 
#' in HTML widget packages that use JavaScript and allow for customization. The 
#' package \code{dygraphs} is a good example.
#' @param type character string describing the type of formatter to return; 
#' valid values are "int", "dlr" and "pct"
#' @return A character string containing a JavaScript function.
#' @examples 
#' jsNumericFormatters("int")
#' jsNumericFormatters("dlr")
#' jsNumericFormatters("pct")
#' library(dygraphs)
#' fmtr <- jsNumericFormatters("int")
#' dygraph(mdeaths) %>% 
#'     dyAxis("y",
#'            axisLabelFormatter = fmtr,
#'            valueFormatter = fmtr)
#' @export 
jsNumericFormatters <- function(type) {
    if (type == "int") {
        y <- paste0("function(x){",
                    "var y = Math.round(x);",
                    "var z = y.toString().replace(/",
                    "\\B(?=(\\d{3})+(?!\\d))/g, ',');",
                    "return z;}")
    } else if (type == "dlr") {
        y <- paste0("function(x){",
                    "var y = Math.round(x);",
                    "var dlr = '$';",
                    "var z = y.toString().replace(/",
                    "\\B(?=(\\d{3})+(?!\\d))/g, ',');",
                    "return dlr.concat(z);}")
    } else if (type == "pct") {
        y <- paste0("function(x){",
                    "var y = Math.round(x*1000)/10;",
                    "var pct = '%';",
                    "return y.toString().concat(pct);}")
    } else {
        stop('type must be c("int", "dlr", "pct")')
    }
    return(y)
}