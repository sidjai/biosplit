% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrubValidation.R
\name{scrubTrap}
\alias{scrubTrap}
\title{Scrub the trap data from PestWatch}
\usage{
scrubTrap(pathXlsx, year, pathCsvOut = "", pestType = "faw",
  minTotCatch = 10, verboseRawTrap = FALSE)
}
\arguments{
\item{pathXlsx}{The path to the xlsx pestwatch dump}

\item{year}{The year you want to select}

\item{pathCsvOut}{The path for the indivdual horizontal report file that you 
want printed out}

\item{pestType}{The insect 3 letter identifier that you want}

\item{minTotCatch}{The minimum total catch for the year to be counted}

\item{verboseRawTrap}{Should the raw captures for each valid record be outputed?}
}
\value{
A list of the records that are filtered (in a named list) and the 
csv document written if you have pathCsv supplied
}
\description{
Scrub the trap data from PestWatch
}

