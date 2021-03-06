% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/rxes.R
\name{xes.getWorkload}
\alias{xes.getWorkload}
\title{Calculate hourly workload - number of seconds spend each our by resource
doing events from event log}
\usage{
xes.getWorkload(xes, ...)
}
\arguments{
\item{xes}{reference to xes class object created via \code{\link{xes.init}}}

\item{...}{parameters to be passed to filter creation function \code{\link{xes.processfilter}}}
}
\value{
data.frame with workloads
}
\description{
Calculate hourly workload - number of seconds spend each our by resource
doing events from event log
}

