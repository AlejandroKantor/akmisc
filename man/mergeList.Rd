% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mergeList.R
\name{mergeList}
\alias{mergeList}
\title{Merge a list of data.table}
\usage{
mergeList(l_dt_data, v_s_keys, all = TRUE)
}
\arguments{
\item{l_dt_data}{a list of data.tables which we want to merge}

\item{v_s_keys}{character vector of columns used for merging}

\item{all}{logical default \code{TRUE}. If \code{TRUE} keep all unique \code{v_s_keys} cases. If \code{FALSE} we only want to keep cases on all the data.tables.}
}
\value{
a data.table which is the merger of \code{l_dt_data}. \code{v_s_keys} are set as keys of returned object.
}
\description{
Merge a list of data.table
}
\examples{
library(data.table)
dt_1 <- data.table(let = letters[1:3], num = 1:3)
dt_2 <- data.table(let= letters[2:4], num = 1:3)
l_len_2 <- list(dt_1, dt_2)
mergeList(l_len_2, v_s_keys= "let")

mergeList(l_len_2, v_s_keys= "let", all=FALSE )
}
\seealso{
\code{\link{merge.data.table}}
}

