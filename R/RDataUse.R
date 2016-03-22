#' @title Return a specific RData file/object
#' @description \code{RDataUse} returns the specified RData file/object. The 
#' file and object must have the same name.
#' @param dir character value; path to the RData file
#' @param x character value; file/object name to retrieve
#' @return Returns the object \code{x} from the RData file with the path/name: 
#' \code{paste0(dir, "/", x, ".RData")}
#' @examples 
#' RDataCreate(".", "test", names, list(x = mtcars), TRUE)
#' load("test.RData")
#' print(test)
#' x <- RDataUse(".", "test")
#' print(x)
#' identical(test, x)
#' @seealso \code{\link{RDataCreate}, \link{new.env}, \link{load}, \link{get}}
#' @export 
RDataUse <- function(dir, x) {
    f <- paste0(dir, "/", x, ".RData")
    if (!file.exists(f)) {
        stop(sprintf("The file [%s] does not exist", f))
    } else {
        nv <- new.env()
        load(file = f, envir = nv)
        if (!any(ls(nv) == x)) {
            stop(sprintf("The object [%s] was not found in [%s]", x, f))
        } else {
            y <- get(x, pos = nv)
            return(y)
        }
    }
}