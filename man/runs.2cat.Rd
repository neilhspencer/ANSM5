% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runs.2cat.R
\name{runs.2cat}
\alias{runs.2cat}
\title{Perform Runs test for two categories}
\usage{
runs.2cat(
  x,
  alternative = c("two.sided", "less", "greater"),
  cont.corr = TRUE,
  do.asymp = FALSE,
  do.exact = TRUE
)
}
\arguments{
\item{x}{Vector with two unique values}

\item{alternative}{Type of alternative hypothesis (defaults to \code{two.sided})}

\item{cont.corr}{Boolean indicating whether or not to use continuity correction (defaults to \code{TRUE})}

\item{do.asymp}{Boolean indicating whether or not to perform asymptotic calculations (defaults to \code{FALSE})}

\item{do.exact}{Boolean indicating whether or not to perform exact calculations (defaults to \code{TRUE})}
}
\value{
An ANSMtest object with the results from applying the function
}
\description{
\code{runs.2cat()} performs the Runs test for two categories and is used in chapters 4, 5 and 6 of "Applied Nonparametric Statistical Methods" (5th edition)
}
\examples{
# Example 4.14 from "Applied Nonparametric Statistical Methods" (5th edition)
runs.2cat(ch4$tosses1, do.exact = FALSE, do.asymp = TRUE)

# Exercise 6.17 from "Applied Nonparametric Statistical Methods" (5th edition)
runs.2cat(ch6$twins, alternative = "greater")

}
