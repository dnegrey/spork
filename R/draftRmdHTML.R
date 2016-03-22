#' @title copy RmdHTMLdoc template
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