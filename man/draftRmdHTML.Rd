% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draftRmdHTML.R
\name{draftRmdHTML}
\alias{draftRmdHTML}
\title{Draft a new R Markdown document with an embedded image}
\usage{
draftRmdHTML(dir, logoFile, width, height, moreLogoCSS = "",
  recursive = FALSE)
}
\arguments{
\item{dir}{character value; directory to store template files}

\item{logoFile}{character value; path to image file to embed}

\item{width}{integer; styling width (in pixels) to use on embedded image}

\item{height}{integer; styling height (in pixels) to use on embedded image}

\item{moreLogoCSS}{character value; additional CSS to use on embedded image}

\item{recursive}{logical value; should elements of \code{dir} other than the 
last be created? If \code{TRUE}, like the UNIX command \code{mkdir -p}.}
}
\value{
A logical value indicating the success (or failure) of copying the 
template files. If \code{dir} does not exist, its creation will be 
attempted. Template files are then copied to \code{dir}. Appropriate 
modifications are then made to template files based on \code{logoFile}, 
\code{width}, \code{height} and \code{moreLogoCSS}.
}
\description{
\code{draftRmdHTML} copies a standard R Markdown (to HTML) 
document template while allowing for dynamic insertion and styling of an 
image (logo)
}
\examples{
dir.create("test")
download.file("http://cran.us.r-project.org/Rlogo.svg", "test/Rlogo.svg")
draftRmdHTML("test", "test/Rlogo.svg", 148, 115, "margin-top: 20px;")
rmarkdown::render("test/template.Rmd")
dir.create("test2")
download.file(
    "https://www.rstudio.com/wp-content/uploads/2014/06/RStudio-Ball.png",
    "test2/RStudio-Ball.png"
)
draftRmdHTML("test2", "test2/RStudio-Ball.png", 150, 150)
rmarkdown::render("test2/template.Rmd")
}
\seealso{
\code{\link{file.copy}, \link{readTemplate}, \link{image_uri}}
}

