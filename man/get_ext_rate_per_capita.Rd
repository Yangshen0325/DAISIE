% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DAISIE_rates.R
\name{get_ext_rate_per_capita}
\alias{get_ext_rate_per_capita}
\title{Function to describe per-capita changes in extinction rate through time}
\usage{
get_ext_rate_per_capita(mu, x, extcutoff = 1000, A = 1)
}
\arguments{
\item{mu}{A numeric with the per capita extinction rate.}

\item{x}{Numeric defining the exponent for calculating extinction rate.}

\item{extcutoff}{A numeric with the cutoff for the the maximum extinction
rate preventing it from being too large and slowing down simulation.}

\item{A}{A numeric value for island area at a given point in time.}
}
\value{
Numeric with per capita extinction rate, given A(t), x, and mu0.
}
\description{
This function is only called directly inside the RHS of the ontogeny
likelihood functions. In all other cases \code{\link{get_ext_rate}()} is to
be called instead.
}
\examples{
ext_rate_per_capita <- DAISIE:::get_ext_rate_per_capita(
  mu = 0.5,
  x = 1,
  A = 1000
)
}
\keyword{internal}
