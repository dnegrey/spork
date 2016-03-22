#' @title copy RmdHTMLdoc template
#' @description copy RmdHTMLdoc template
#' @param dir 
#' @param logoFile 
#' @param width 
#' @param height 
#' @param moreLogoCSS 
#' @param recursive 
#' @return A logical value indicating the success (or failure) of copying the 
#' template files. If \code{dir} does not exist, its creation will be 
#' attempted. Template files are then copied to \code{dir}. Appropriate 
#' modifications are then made to template files based on \code{logoFile}, 
#' \code{width}, \code{height} and \code{moreLogoCSS}.
#' @seealso \code{\link{file.copy}, \link{readTemplate}, \link{image_uri}}
#' @examples 
#' dir.create("test")
#' download.file("http://cran.us.r-project.org/Rlogo.svg", "test/Rlogo.svg")
#' draftRmdHTML("test", "test/Rlogo.svg", 148, 115, "margin-top: 20px;")
#' rmarkdown::render("test/template.Rmd")
#' dir.create("test2")
#' download.file(
#'     "https://www.rstudio.com/wp-content/uploads/2014/06/RStudio-Ball.png",
#'     "test2/RStudio-Ball.png"
#' )
#' draftRmdHTML("test2", "test2/RStudio-Ball.png", 150, 150)
#' rmarkdown::render("test2/template.Rmd")
#' @importFrom knitr image_uri
#' @export 
draftRmdHTML <- function(dir, logoFile, width, height,
                         moreLogoCSS = "",
                         recursive = FALSE) {
    if (!dir.exists(dir)) {
        cat(sprintf("\nThe directory [%s] does not exist; %s",
                    dir,
                    "attempting to create..."))
        dir.create(dir, recursive = recursive)
    }
    if (!dir.exists(dir)) {
        stop(sprintf("\nThe directory [%s] cannot be created at this time",
                     dir))
    } else {
        cat(sprintf("\nThe directory [%s] exists; ready to proceed",
                    dir))
    }
    inDir <- system.file("extdata/templates/RmdHTMLdoc",
                         package = getPackageName())
    x <- list.files(inDir, full.names = TRUE)
    z <- invisible(sapply(x, file.copy, to = dir, overwrite = TRUE))
    st <- paste(dir, "styles.css", sep = "/")
    hd <- paste(dir, "header.html", sep = "/")
    x <- readTemplate(
        st,
        TRUE,
        width,
        height,
        moreLogoCSS
    )
    write(x, st)
    x <- readTemplate(
        hd,
        TRUE,
        image_uri(logoFile)
    )
    write(x, hd)
    z <- all(as.logical(z))
    cat("\n")
    return(z)
}