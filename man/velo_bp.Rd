% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/velo_by_pitch_type.R
\name{velo_bp}
\alias{velo_bp}
\title{Take a statcast data set and compare release speed densities by pitch type}
\usage{
velo_bp(file = NULL)
}
\arguments{
\item{file}{statcast file for specified player}
}
\value{
ggplot graphic
}
\description{
Take a statcast data set and compare release speed densities by pitch type
}
\examples{
velo_bp(file = deGrom)
}
