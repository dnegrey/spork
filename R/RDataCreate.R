#' @title Create an RData file containing the result of a function call
#' @description \code{RDataCreate} stores the result of a function call in an 
#' RData file using the specified directory and file/object name
#' @param dir character value; directory in which to store the output RData file
#' @param x character value; name to use for the file/object
#' @param fun name of the function to call (non-quoted)
#' @param args named list of arguments to supply to the function; the 
#' \code{names} attribute of \code{args} gives the argument names
#' @param replace logical value; replace the output RData file if it already 
#' exists?
#' @return The result of the evaluated function call is stored in an RData 
#' file using the path/name: \code{paste0(dir, "/", x, ".RData")}
#' @examples 
#' RDataCreate(".", "test", names, list(x = mtcars), TRUE)
#' load("test.RData")
#' print(test)
#' @seealso \code{\link{RDataUse}, \link{do.call}, \link{assign}, \link{save}}
#' @export 
RDataCreate <- function(dir, x, fun, args, replace = FALSE) {
    f <- paste0(dir, "/", x, ".RData")
    if (file.exists(f) && !replace) {
        print(sprintf("The file [%s] already exists and replace is FALSE", f))
    } else if ((file.exists(f) && replace) || !file.exists(f)) {
        if (file.exists(f)) {
            print(sprintf("The file [%s] already exists and replace is TRUE", f))
            print(sprintf("The file [%s] will now be replaced", f))
            file.remove(f)
        } else  {
            print(sprintf("The file [%s] does not exist", f))
            print(sprintf("The file [%s] will now be created", f))
        }
        y <- do.call(fun, args)
        assign(x, y)
        save(list = x, file = f)
    }
}