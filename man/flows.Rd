% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flows.R
\name{flows}
\alias{flows}
\alias{flows.petrinet}
\alias{flows.marked_petrinet}
\title{Flows}
\usage{
flows(PN)

\method{flows}{petrinet}(PN)

\method{flows}{marked_petrinet}(PN)
}
\arguments{
\item{PN}{\code{\link{petrinet}} or \code{\link{marked_petrinet}}}
}
\value{
A data.frame containing the flows of the petri net.
}
\description{
Extracts the flows from a (marked) Petri Net
}
\section{Methods (by class)}{
\itemize{
\item \code{flows(petrinet)}: Flow of petrinet

\item \code{flows(marked_petrinet)}: Flow of marked petrinet

}}
