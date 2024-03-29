% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hsLibTimeshift.R
\name{hsIntSumGeThreshold}
\alias{hsIntSumGeThreshold}
\title{Interval Sum >= Threshold}
\usage{
hsIntSumGeThreshold(
  tSeries,
  threshold,
  forward,
  maxTDiff,
  tsField = names(tSeries)[1],
  valField = names(tSeries)[2],
  valFactor = 1,
  includeIndices = TRUE,
  dbg = FALSE
)
}
\arguments{
\item{tSeries}{data.frame with timestamps in first column and values in
second column.}

\item{threshold}{Threshold that shall be reached/exceeded by the sum of
successive elements of \emph{values} of which the maximum time difference
is below or equal \emph{maxTDiff}.}

\item{forward}{If TRUE, the algorithm looks \code{forward}, else backwards,
i.e. when looking \code{forward} (backwards), the start indices <iStart>
are always less or equal (greater or equal) the assigned indices <iStop>.}

\item{maxTDiff}{Maximum allowed time difference in seconds between two
related timestamps.}

\item{tsField}{Name of time stamp field; default: name of first column}

\item{valField}{Name of value field containing the values of which the sum
shall reach the \code{threshold}; default: name second column}

\item{valFactor}{Factor to be applied to column \emph{valField} before
calculating value sums.}

\item{includeIndices}{if TRUE, two columns \emph{iStart} and \emph{iStop} are
included in the output data frame indicating the indices in \emph{tSeries}
corresponding to the timestamps \emph{tStart} and \emph{tStop}.}

\item{dbg}{If \code{TRUE}, debug messages are shown}
}
\value{
data frame with columns \emph{iStart} and \emph{iStop} being the
  indices of \emph{tSeries} that represent the beginning and the end of the
  time interval in which the value field sums up to at least
  \emph{threshold}, \emph{tStart}, \emph{tStop} and \emph{tDiff} representing
  the first timestamp, last timestamp and duration of the corresponding time
  intervals and the column \emph{sumWithin} being the sum of values that was
  actially reached within the interval.
}
\description{
For each index <iStart> of vector \emph{values}, this function tries to find
a corresponding index <iStop> in such a way that the sum of the vector
elements at indices between <iStart> and <iStop> reaches the given
\code{threshold}. For each possible start index i, the algorithm starts
either looking \code{forward} at indices i+1, i+2, ... or backwards at
indices i-1, i-2, ..., accumulating the values at these indices. Once the
accumulated sum reached the given \code{threshold} or if the difference
between the indices exceeds the maximum allowed index difference
\emph{maxDist} the algorithm stops and continues with the next start index.
}
