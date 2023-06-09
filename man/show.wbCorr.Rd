% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wbCorr.R
\name{show,wbCorr-method}
\alias{show,wbCorr-method}
\alias{show.wbCorr}
\title{Show Method for the wbCorr Class}
\usage{
\S4method{show}{wbCorr}(object)
}
\arguments{
\item{object}{A \code{wbCorr} object.}
}
\description{
Shows a summary of the \code{wbCorr} object, equivalent to the print method.
}
\examples{
# Example using the iris dataset
cors <- wbCorr(iris, iris$Species, weighted_between_statistics = TRUE)
show(cors)
}
\seealso{
\code{\link[=wbCorr]{wbCorr}}, \code{\link[=print.wbCorr]{print.wbCorr}}
}
