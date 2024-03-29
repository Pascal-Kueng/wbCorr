% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_post_02_accessor_functions.R, R/wbCorr.R
\name{get_matrix}
\alias{get_matrix}
\alias{get_matrices}
\alias{summary,wbCorr-method}
\alias{summary.wbCorr}
\title{Return matrices for within- and/or between-cluster correlations.}
\usage{
get_matrix(object, which = c("within", "between", "merge"), ...)

get_matrices(object, which = c("within", "between", "merge"), ...)

\S4method{summary}{wbCorr}(object, which = c("within", "between", "merge"), ...)
}
\arguments{
\item{object}{A wbCorr object, created by the wbCorr() function.}

\item{which}{A string or a character vector indicating which summaries to return.
Options are 'within' or 'w', 'between' or 'b', and various merge options
like 'merge', 'm', 'merge_wb', 'wb', 'merge_bw', 'bw'.
Default is c('within', 'between', 'merge').}

\item{...}{Additional arguments passed to the base summary method}
}
\value{
A list containing the selected matrices of within- and/or between-cluster correlations, and ICCs on the diagonals for merged matrices.
}
\description{
You can use summary(), get_matrices(), or get_matrix() interchangeably.
Merged matrices include the ICC on the diagonal.
For more detailed statistics, use get_table().
}
\examples{
# importing our simulated example dataset with pre-specified within- and between- correlations
data("simdat_intensive_longitudinal")

# create object:
correlations <- wbCorr(data = simdat_intensive_longitudinal,
                      cluster = 'participantID')

# returns a correlation matrix with stars for p-values:
matrices <- summary(correlations) # the get_matrix() and get_matrices() functions are equivalent
print(matrices)

# Access specific matrices by:
# Option 1:
matrices$within
# Option 2:
within_matrix <- summary(correlations, which = 'w') # or use 'within'
merged_within_between <- summary(correlations, which = 'wb')
print(within_matrix) # could be saved to an excel or csv file (e.g., write.csv)

}
\seealso{
\code{\link[=get_table]{get_tables}}, \code{\link[=wbCorr]{wbCorr}}
}
