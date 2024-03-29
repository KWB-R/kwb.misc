% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hsLibRainDist.R
\name{plotRainOverview}
\alias{plotRainOverview}
\title{Plot Rain Overview}
\usage{
plotRainOverview(
  rain,
  minDate = "",
  maxDate = "",
  strTimestamp = "Zeitstempel",
  boolMaxDateIncluded = FALSE,
  Nmax_mm = NA,
  cex = 0.9
)
}
\arguments{
\item{rain}{data frame containing rain data}

\item{minDate}{minimum date}

\item{maxDate}{maximum date}

\item{strTimestamp}{name of timestamp column. Default: "Zeitstempel"}

\item{boolMaxDateIncluded}{logical. Should the maximum date be included?}

\item{Nmax_mm}{Default: NA}

\item{cex}{character expansion factor. Default: 0.9}
}
\description{
\code{minDate} and \code{maxDate} must be given as "yyyy-mm-dd"
  Timestamps of the day given in \code{minDate} are included, whereas timestamps of
  the day given in maxDay are excluded! This way it is easy to select whole
  months or whole years by selecting the first day of the next month/year as
  \code{maxDate}.
  It is assumed that all but the first columns contain \code{rain} heights!
}
