% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/rxes.R
\name{xes.getFullSubTraceList}
\alias{xes.getFullSubTraceList}
\title{Return list of all trace segments limited by event names with attributes}
\usage{
xes.getFullSubTraceList(xes, start_event, end_event, ...)
}
\arguments{
\item{xes}{reference to xes class object created via \code{\link{xes.init}}}

\item{start_event}{name of first event in trace segment}

\item{end_event}{name of last event in trace segment}

\item{...}{parameters to be passed to filter creation function \code{\link{xes.processfilter}}}
}
\value{
data.frame with traces
}
\description{
Return list of all trace segments limited by event names with attributes
}

