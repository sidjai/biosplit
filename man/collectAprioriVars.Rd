% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aprioriVars.R
\name{collectAprioriVars}
\alias{collectAprioriVars}
\title{A wrapper for aprioriVars}
\usage{
collectAprioriVars(config)
}
\arguments{
\item{config}{The configuration object returned by loadConfig 
or from loading the cfg.RData in the config.txt location}
}
\value{
Works as a byproduct to save the ncdf file
}
\description{
Abstracts away the folder selection which allows a developer
to insert more variables with easy to use names along with 
the equations directly in aprioriVars
}

