% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcComparison.R
\name{calcComparison}
\alias{calcComparison}
\title{Compares two numeric vectors by passing comparison in a string}
\usage{
calcComparison(v_n_1, v_n_2, s_comp)
}
\arguments{
\item{v_n_1}{a numeric vector. Length must be the same as the length of \code{v_n_2} or length 1.}

\item{v_n_2}{a numeric vector. Length must be the same as the length of \code{v_n_1} or length 1.}

\item{s_comp}{type of comparison must be one of the following \code{"==","!=", "<=", ">=", "<" ,">"}.}
}
\value{
a logical vector of the comparison of v_n_1 and v_n_2.
}
\description{
Compares two numeric vectors by passing comparison in a string
}
\examples{
v_n_1 <- rep(0,3)
v_n_2 <- c(0, -1.1, 1.1)
calcComparison( v_n_1, v_n_2, "==")
calcComparison( v_n_1, v_n_2, ">")
calcComparison( v_n_1, v_n_2, ">=")


}

