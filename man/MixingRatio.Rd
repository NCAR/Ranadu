% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MixingRatio.R
\name{MixingRatio}
\alias{MixingRatio}
\title{MixingRatio}
\usage{
MixingRatio(EoverP)
}
\arguments{
\item{EoverP}{A numeric representing the ratio of water vapor pressure to total pressure.}
}
\value{
A numeric representing the mixing ratio in dimensionless units (*NOT* g/kg)
}
\description{
Calculate the mixing ratio of water vapor
}
\details{
The dimensionless mixing ratio of water vapor is calculated from the ratio of 
water vapor pressure to total pressure. CAVEAT: Note the units below (dimensionless, not g/kg).
}
\examples{
MR <- MixingRatio (3./800.)
MR <- MixingRatio (RAFdata$EWX / RAFdata$PSXC)
}
\author{
William Cooper
}
