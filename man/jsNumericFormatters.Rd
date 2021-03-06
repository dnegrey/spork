% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jsNumericFormatters.R
\name{jsNumericFormatters}
\alias{jsNumericFormatters}
\title{Return JavaScript functions that format numeric data}
\usage{
jsNumericFormatters(type)
}
\arguments{
\item{type}{character string describing the type of formatter to return; 
valid values are "int", "dlr" and "pct"}
}
\value{
A character string containing a JavaScript function.
}
\description{
\code{jsNumericFormatters} returns a character string 
containing a JavaScript function for formatting numeric data. This is useful 
in HTML widget packages that use JavaScript and allow for customization. The 
package \code{dygraphs} is a good example.
}
\examples{
jsNumericFormatters("int")
jsNumericFormatters("dlr")
jsNumericFormatters("pct")
library(dygraphs)
fmtr <- jsNumericFormatters("int")
dygraph(mdeaths) \%>\% 
    dyAxis("y",
           axisLabelFormatter = fmtr,
           valueFormatter = fmtr)
}

