% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/simulateBinnningProcess.R
\name{rebin}
\alias{rebin}
\title{Function to simulate re-binned counts when each count of a data a bin is considered to come from a uniform distribution
and we want to simulate the data as if it comes from a less coarse bin}
\usage{
rebin(counts, lowerBounds, upperBounds, cutSize = 1, repNum = 10,
  seed = 1234, simplify = FALSE)
}
\arguments{
\item{counts}{a matrix of counts of objects placed in bins where each column is a bin and each row is on abservation}

\item{lowerBounds}{A vector of lower cut points of the original binned data}

\item{upperBounds}{A vector of the upper cutponts of the original binnned data}

\item{cutSize}{A scalar that is the the length of the new (less coarse) bin for simulated data}

\item{repNum}{The number of replications to carry out}

\item{seed}{Seed}
}
\value{
If \code{simplify=TRUE}, Returns a matrix the same size as the original data however, each cell of the matrix contains a list of simulations.
If \code{simplify=FALSE} then the function returns a list where each bin is a list element. Inside each bin there is another list that represents each observation.
For all non-empty bins each observation contains \bold{\code{n}} rows, which represent objects places in the bin and \bold{\code{p}} coulmns, were each column represents a simulation. This
data structure is designed to be passed to  \code{\link{formatSimsOutput} in order} to get the data in the format you desire for analysis.
}
\description{
Function to simulate re-binned counts when each count of a data a bin is considered to come from a uniform distribution
and we want to simulate the data as if it comes from a less coarse bin
}
\details{
The function uses \code{mapply} to simulate the placing of object within each bin into less coarse bins. It returns an unusual
two different unusual data structures. This is good for checking the simulation process but is not useful. Depending on the usage
you may want the data in different structures. Use \code{\link{formatSimsOutput}} to get the data in the format you desire
}

