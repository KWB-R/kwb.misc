% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hsLibCalib.R
\name{hsAvailCalibs}
\alias{hsAvailCalibs}
\title{Available Calibrations}
\usage{
hsAvailCalibs(
  moniPoint = NULL,
  parAcronym = NULL,
  skipCur = FALSE,
  mdbCal = NULL,
  dbg = FALSE
)
}
\arguments{
\item{moniPoint}{acronym of monitoring point}

\item{parAcronym}{acronym of parameter}

\item{skipCur}{if TRUE, the name of the current specification (<parAcronym>_<moniPoint>)
is excluded from the list of available calibrations}

\item{mdbCal}{Path to database containing queries that getting calibrated
data according to the currently active calibration setting}

\item{dbg}{If \code{TRUE}, debug messages are shown}
}
\value{
Return character vector of (filtered) calibration names
}
\description{
Return names of available calibrations according to calibration database
}
