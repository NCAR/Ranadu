% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wetbulbT.R
\name{wetbulbT}
\alias{wetbulbT}
\alias{wetbulbT,wetBulbT,wetbulb}
\title{wetbulb Temperature}
\usage{
wetbulbT(P, AT, DPT)
}
\arguments{
\item{P}{A numeric representing ambient pressure in hPa}

\item{AT}{A numeric representing air temperature in deg. C}

\item{DP}{A numeric representing dewpoint temperature in deg. C.}
}
\value{
A numeric representing the wet-bulb temperature in deg. C
}
\description{
Calculates the wet-bulb temperature given the pressure, 
temperature, and dewpoint.
}
\details{
An interative solution is used, as described here:
https://rpubs.com/cooperwilliama/797335 
If the known quantities are pressure, temperature, and vapor pressure
you can use DPfromE() to find the dewpoint. The
input parameters may be vectors, all of the same size, and in that
case the result will also be a vector. Be aware, however, that large
vectors (like whole flights even at 1 Hz) are likely to give an error
resulting from the attempt by nleqslv to allocate too much memory.
}
\author{
William Cooper
}
