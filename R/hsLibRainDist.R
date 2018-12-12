#
# Author: Hauke Sonnenberg
# Created: 2011-12
#

# na_checksum
# read_rain_from_mdb (old: hsGetRain)
# prepare_rain_data (old: hsPrepRain)
# create_and_plot_rain_events (old: hsRainStudy1)
# plot_rain_events
# plot_given_rain_events (old: hsRainPlot2All)
# plot_one_rain_event (old: hsRainPlot1)
# plot_one_rain_event_2 ***
# plot_one_rain_gauge_event ***
# plotRainOverview ***
# get_rain_stat *** 
# plotDataAvailability ***
# plotTotalPrecipitation ***
# plot_rain_subevents_by_checksum (old: hsPlotAll)
# plot_rain_with_subevent_lines (old: hsPlotRain)
# plot_one_rain_event_3 (old: hsPlot1)
# plot_rain_events_to_pdf (old: hsPrintPdf)

if (FALSE)
{
  rain <- example_rain_data(1)
  
  rainStat <- kwb.misc:::get_rain_stat(rain)
  
  kwb.misc:::plotDataAvailability(rainStat)
  kwb.misc:::plotTotalPrecipitation(rainStat)
  kwb.misc:::plotRainOverview(rain)
  
  kwb.misc:::plot_one_rain_gauge_event(rain, i = 3)
  kwb.misc:::plot_one_rain_event_2(rain, 1)
}

# example_rain_data ------------------------------------------------------------
example_rain_data <- function(version = 1)
{
  if (version == 1) {
    
    times <- seq(as.POSIXct("2010-01-01"), as.POSIXct("2010-02-01"), 3600)
    
    rain <- data.frame(Zeitstempel = times, a = NA, b = NA, c = NA)
    
    n <- nrow(rain)
    
    n1 <- sample(n, 1)
    n2 <- sample(n, 1)
    n3 <- sample(n, 1)
    
    rain$a[sample(1:n, n1)] <- 0.1 * sample(1:5, n1, replace = T)
    rain$b[sample(1:n, n2)] <- 0.1 * sample(1:5, n2, replace = T)
    rain$c[sample(1:n, n3)] <- 0.1 * sample(1:5, n3, replace = T)
    
    rain
  } else if (version == 2) {
    
  }
}

# na_checksum ------------------------------------------------------------------
na_checksum <- function # number representing combination of gauges with NA-failure 
### calculate checksum for each row of a data frame or matrix \emph{x}. 
### Each combination of NA-occurrence in the row gets a different checksum.
### Therefore, each column of \emph{x} is represented as a power of two
### and for the columns in which the row contains NA the corresponding powers
### of two are added.
#@2011-11-29: moved from hsRainDist.r
(
  x
) 
{
  # Prepare a matrix with as many rows as there are rows in the rain series
  # and with each row containing a series of powers of 2: 1, 2, 4, 8...
  m <- matrix(rep(2^(0:(ncol(x) - 1)), nrow(x)), nrow = nrow(x), byrow = TRUE)
  
  # Calculate a checksum for each row by interpreting the sequence of booleans
  # (TRUE where the value in x is missing) as a binary coded number
  rowSums(is.na(x) * m)
}

# read_rain_from_mdb (old: hsGetRain) ------------------------------------------
read_rain_from_mdb <- function # Read Rain Data from Database
### Read rain data from database
(myVersion, strDb)
{
  if (myVersion == 1) {
    
    # Get data, version 1
    hsGetTimeSeries(
      strDb, "tbl_Regen_alleEZG_05min", "Zeitstempel", "*",
      minDate = "01/01/2011", maxDate = "01/01/2012"
    )
  } else {
    
    # Get data, version 2
    hsGetTable(
      strDb, "tbl_Regen_alleEZG_05min", "Zeitstempel > #12/31/2010#"
    )
  }
}

# prepare_rain_data (old: hsPrepRain) ------------------------------------------
prepare_rain_data <- function # Prepare Rain Data
### prepare rain data
(
  rain, myVersion, event_sep_time = 60 * 60 * 6, signal_width = 300
)
{
  if (myVersion == 1) {
    
    # Preparation of rain, version 1
    
    # Filter for rows where signals from all gauges are available and where the
    # sum of signals is greater than zero.
    n <- ncol(rain)
    rs <- rowSums(rain[, 4:n])
    idx <- ! is.na(rs) & rs > 0
    
    frmY <- rain[idx, 4:n]
    vecX <- rain$myDate[idx] + round(60 * rain$myTime_min[idx])
    frmR <- cbind(ts = vecX, frmY)
    
    # Create events
    evt <- kwb.event::hsEvents(as.integer(vecX), event_sep_time, signal_width)
    
  } else {
    
    # Preparation of rain, version 2
    
    # Filter for rows with a rain signal at any station
    rain <- rain[rowSums(rain[, -1], na.rm = TRUE) != 0, ]
    
    # Create rain events that are separated by at least 6 h of no rain
    evt <- hsEvents(rain$Zeitstempel, event_sep_time, signal_width)
    
    # Add checksum column (number indicating the combination of stations
    # providing NA values
    rain$chksum <- na_checksum(rain[, -1])
  }
  
  # Return events
  evt
}

# create_and_plot_rain_events (old: hsRainStudy1) ------------------------------
create_and_plot_rain_events <- function # plot rain events from given rain data
### plot rain events from given rain data.
### Rain data is filtered for rows where signals from all gauges are available 
### and where the sum of signals is greater than zero.
#@2011-11-29: moved from hsRainDist.r
(
  frmRain, 
  ### data frame containing rain data
  strTimestamp = "Zeitstempel", 
  ### name of timestamp field, default: "Zeitstempel"
  strPdf = NULL,
  ### optional. full path to pdf output file
  dbg = FALSE
) 
{  
  # Filter for rows where signals from all gauges are available 
  # and where the sum of signals is greater than zero.
  rs  <- rowSums(frmRain[, -1])
  idx <- ! is.na(rs) & rs > 0

  frmY <- frmRain[idx, -1]
  vecX <- frmRain[idx, strTimestamp]
  frmR <- cbind(ts = vecX, frmY)

  ## return if there is no data
  if (length(vecX) == 0) {
    
    cat("\n*** Nothing to plot in create_and_plot_rain_events.\n\n")
    return()
  }
  
  # Create events
  evt <- hsEvents(vecX, evtSepTime = 60 * 60 * 6, signalWidth = 60 * 5)
  
  kwb.utils::printIf(dbg, head(evt), "First events")
  kwb.utils::printIf(dbg, tail(evt), "Last events")

  to_pdf <- ! is.null(strPdf)
  
  kwb.utils::preparePdfIf(to_pdf, strPdf, landscape = FALSE)
  
  on.exit(kwb.utils::finishAndShowPdfIf(to_pdf, strPdf))
  
  for (i in seq_len(nrow(evt))) {
    
    plot_one_rain_event(frmR, evt, i)
  }  
}

# plot_rain_events -------------------------------------------------------------
plot_rain_events <- function # plot rain events from given rain data
### plot rain events from given rain data. Either one plot per day or one plot
### per rain event (created within this function). This function calls 
### \code{\link{plot_given_rain_events}}
(
  rain, 
  ### data frame containing rain data
  strTimestamp = "Zeitstempel", 
  ### name of timestamp field, default: "Zeitstempel"
  strPdf = NULL,
  ### optional. path to output pdf file
  irng = c(1, -1), 
  ### vector of two integer values determining the first and last index,
  ### respectively, in the data frame of events (created within this function) 
  ### to be plotted. Second element can be -1 to indicate the index of the
  ### last event. Default: c(1, -1)
  myCex = 0.7, 
  ### default: 0.7
  myMinN, 
  ### minimum precipitation in mm. Events with a total depth less than this 
  ### value are not plotted
  evtSepTime_s = 60 * 60 * 6, 
  ### event separation time in seconds. Default: 6*3600 = 6 hours
  sigWidth_s = 300,
  ### rain signal width in seconds. Default: 300 = 5 minutes
  events = TRUE
  ### one plot per event (events = TRUE) or one plot per day (events = FALSE)?
  ### default: TRUE
) 
{
  t0 <- Sys.time()
  
  # Number of (columns)
  n <- ncol(rain)
  
  timestamps <- kwb.utils::selectColumns(rain, strTimestamp)
  
  if (events) {

    # Filter for rows with a rain signal at any station
    rain <- rain[rowSums(rain[, 2:n, drop = FALSE], na.rm=TRUE) != 0, ]
    
    # Create rain events that are separated by at least <evtSepTime_s> seconds 
    # of no rain
    evt <- kwb.event::hsEvents(
      tseries = timestamps, 
      evtSepTime = evtSepTime_s, 
      signalWidth = sigWidth_s
    )
    
  } else {
    
    strDays <- unique(intervalKey(timestamps, "d"))
    tBeg <- hsToPosix(strDays)
    tEnd <- hsToPosix(as.Date(strDays) + 1)
    evt <- data.frame(tBeg = tBeg, tEnd = tEnd)
  }
  
  # If the path to a PDF file is given, prepare a PDF file
  if (! is.null(strPdf)) {
    
    hsPrepPdf(strPdf, boolLandscape = FALSE)
    on.exit(kwb.utils::finishAndShowPdf(strPdf))
  }
  
  # Make all plots
  plot_given_rain_events(
    rain = rain, evt = evt, imin = irng[1], imax = irng[2], 
    sigWidth_s = sigWidth_s, myCex = myCex, myMinN = myMinN
  )

  # Show run-time
  cat("Runtime:", Sys.time() - t0, "\n")
}

# plot_given_rain_events (old: hsRainPlot2All) ---------------------------------
plot_given_rain_events <- function # plot rain events from rain data and event info 
### plot all rain events from given rain data and event information. 
### This function calls \code{\link{plot_one_rain_event_2}} in a loop over all events
### with indices between \emph{imin} and \emph{imax}.
(
  rain, 
  ### data frame containing rain data
  evt, 
  ### event information as returned by \code{kwb.event::hsEvents}
  imin = 1, 
  ### row index in \emph{evt} of first event to be considered. Default: 1 
  imax = -1, 
  ### row index in \emph{evt} of last event to be considered. Set to -1 
  ### (default) to consider all events.
  sigWidth_s, 
  ### rain signal width in seconds
  myCex = 0.7, 
  ### default: 0.7
  myMinN
  ### minimum total precipitation in mm. Rain events with less precipitation are
  ### not plotted
) 
{
  # Set minimum and maximum event indices
  if (imax == -1) {
    
    imax <- nrow(evt)
  }
  
  stopifnot(imax >= imin)  
  
  # For each rain event with index between imin and imax...
  for (i in (imin:imax)) {
    
    plot_one_rain_event_2(
      rain = hsGetEvent(rain, evt, i), rainEventIndex = i, 
      sigWidth_s = sigWidth_s, boolBarplot = TRUE, myCex = myCex, 
      myMinN = myMinN
    )
  }
}

# plot_one_rain_event (old: hsRainPlot1) ---------------------------------------
plot_one_rain_event <- function
### plot_one_rain_event
#@2011-11-29: moved from hsRainDist.r
(
  frmR, evt, i
) 
{
  ib <- evt$iBeg[i]
  ie <- evt$iEnd[i]  
  n  <- ncol(frmR) - 1
  
  myCols <- rainbow(n)
  yma <- max(frmR[ib:ie, 2:n])

  opar <- par(mfrow = c(n, 1), oma = c(0, 0, 1.2, 0), mar = c(0, 0, 2, 0))
  on.exit(par(opar))
  
  for (j in seq_len(n)) {
    
    plot(
      frmR[ib:ie, 1], frmR[ib:ie, j + 1], type = "s", col = myCols[j], 
      xlab = "", xaxt = "n", ylab = "", lwd = 2, bty = "n", ylim = c(0, yma)
    )
    
    abline(h = 0)
    
    if (j == 1) {
      
      mtext(
        paste0(
          "Rain #", i, ": ",
          format(frmR[["ts"]][ib], "%d/%m/%Y %H:%M"), " to ",
          format(frmR[["ts"]][ie], "%d/%m/%Y %H:%M")
        ), 
        side = 3, line = 0.8
      )
    }
    
    legend("bottomleft", legend = colnames(frmR)[j + 1], bty = "n")
  }
}

# plot_one_rain_event_2 (old: hsRainPlot2) -------------------------------------
plot_one_rain_event_2 <- function # plot one rain event
### plot one rain event by calling \code{\link{plot_one_rain_gauge_event}}
(
  rain,
  rainEventIndex = 1, 
  sigWidth_s = NULL, 
  ### rain signal width in seconds
  boolBarplot = TRUE,
  myCex = 1, 
  myMinN = 0
) 
{
  # Constants
  dateFormat <- "%Y-%m-%d %H:%M" # to be used in timestamp labels

  stopifnot(length(dim(rain)) == 2, ncol(rain) > 1)
  
  # Number of last rain data column
  n <- ncol(rain)

  # For calculation of maximum/sum: remove first column (timestamp)
  rainDataOnly <- rain[ , -1, drop = FALSE]

  # Get global maximum of rain signal within time period and over all gauges
  Nmax <- max(rainDataOnly, na.rm = TRUE)

  # Calculate total precipitation
  Ntotal <- colSums(rainDataOnly, na.rm = TRUE)

  if (is.null(sigWidth_s)) {
    
    sigWidth_s <- kwb.datetime::minTimeStep(rain[, 1])
  }
  
  # Get minimum and maximum of time period; the minimum is <sigWidth_s> before
  # the time indicated by the first timestamp, as the timestamp represents the
  # end of a period of <sigWidth_s> seconds.
  minTimestamp <- rain[1, 1] - sigWidth_s
  maxTimestamp <- rain[nrow(rain), 1]

  # Calculate the duration of the event in number of hours and minutes
  tdiff_min <- as.integer(diff(as.integer(c(minTimestamp, maxTimestamp))) / 60)
    
  tdiff_h <- as.integer(tdiff_min / 60)
  
  tdiff_min <- tdiff_min - 60 * tdiff_h
  
  # Prepare plot matrix and margins.
  # The matrix contains n rows (one more than rain gauges). The last plot
  # will contain no data and only be used for showing the timestamp labels
  par(mfrow = c(n, 1), oma = c(4, 3, 4, 0), mar = c(0, 4, 1.6, 3))

  main <- kwb.utils::resolve(list(
    main = "Rain event #<i> from <from> to <to> (<h>:<min> h), <max_info>",
    max_info = "max. <max> mm in <gauge>",
    from = format(minTimestamp, dateFormat),
    to = format(maxTimestamp, dateFormat),
    i = as.character(rainEventIndex),
    h = as.character(tdiff_h),
    min = sprintf("%02d", tdiff_min),
    max = format(x = max(Ntotal), digits = 1, nsmall = 1),
    gauge = colnames(rain)[1 + which.max(Ntotal)]
  ))$main
  
  # Print the plot title as part of the progress information
  cat(main, "\n")

  # Plot only if max N > myMinN
  if (max(Ntotal) > myMinN) {
  
    # Loop through the rain gauges
    for (i in 2:n) {
      
      cat("Rain gauge #", i - 1, ":", colnames(rain)[i], "\n")
      
      plot_one_rain_gauge_event(
        rain[, seq_len(n)], sigWidth_s, i, Nmax = Nmax, boolLabels = FALSE
      )
 
      # Add a label with the sum of precipitation
      myN <- sum(rain[,i], na.rm = TRUE) # total prec within the event

      caption <- paste(format(myN, digits = 1, nsmall = 1), "mm")
      
      mtext(caption, side = 4, line = 0, cex = myCex)
      
      # Put the title above the first plot
      if (i == 2) {
        
        caption <- paste(
          "Rain heights h, measuered in a temporal resolution of 5 minutes"
        )
        
        mtext(main, side = 3, line = 3, cex = 0.9)
        mtext(caption, side = 3, line = 1.5, cex = 0.8)
      }
      
      mtext(colnames(rain)[i], 4, line = 1.5)
    }
    
    # Just another (empty) plot for the labels
    plot_one_rain_gauge_event(
      rain[, seq_len(n)], sigWidth_s, i, Nmax = Nmax, boolLabels = TRUE
    )
  }
}

# plot_one_rain_gauge_event (old: hsRainPlot3) ---------------------------------
plot_one_rain_gauge_event <- function # plot rain event
### plot rain event
(
  rain, 
  sigWidth_s = NULL, 
  ### signal width in seconds
  i = 2, 
  Nmax = 1, 
  ### default: 1
  boolLabels = FALSE,   
  boolDebug = FALSE, 
  myBlue = rainbow(8)[6], # nice blue colour
  nMaxLabels = 60, # max number of labels per page
  dbg = FALSE
) 
{
  stopifnot(is.data.frame(rain), ncol(rain) >= i)
  
  # Create all timestamps
  n <- nrow(rain)
  
  t0 <- rain[1, 1]
  tn <- rain[n, 1]
  
  if (is.null(sigWidth_s)) {
    
    sigWidth_s <- kwb.datetime::minTimeStep(rain[, 1])
  }
                                         
  tAll <- seq(t0, tn, sigWidth_s)
  
  N <- length(tAll)

  # Is this a regular plot or the last plot (for the timestamp labels)?
  if (! boolLabels) {

    # bool vector representing non-NA values and NA values in rain
    bNoNA <- ! is.na(rain[, i])
    bIsNA <-   is.na(rain[, i])
  
    # Timestamps of non-NA values and NA values, respectively
    tNoNA <- rain[bNoNA, 1]
    tIsNA <- rain[bIsNA, 1]
  
    # Indices in tAll corresponding to the timestamps of non-NA/NA values
    iNoNA <- (as.integer(tNoNA) - as.integer(t0)) / sigWidth_s + 1
    iIsNA <- (as.integer(tIsNA) - as.integer(t0)) / sigWidth_s + 1
   
    # Prepare two height vectors: one for non-NA, one for NA values
    hNoNA <- numeric()
    hIsNA <- numeric()
  
    # Span vectors of needed length
    hNoNA[N] <- NA 
    hIsNA[N] <- NA
  
    # Set the heights to the non-NA values
    hNoNA[iNoNA] <- rain[bNoNA, i]
    hIsNA[iIsNA] <- Nmax
    
    kwb.utils::printIf(dbg, head(rain))
    kwb.utils::printIf(dbg, head(tAll))
    kwb.utils::printIf(dbg, head(bNoNA))
    kwb.utils::printIf(dbg, head(bIsNA))
    kwb.utils::printIf(dbg, head(iNoNA))
    kwb.utils::printIf(dbg, head(iIsNA))
    kwb.utils::printIf(dbg, head(tNoNA))
    kwb.utils::printIf(dbg, head(tIsNA))
    kwb.utils::printIf(dbg, head(hNoNA))
    kwb.utils::printIf(dbg, head(hIsNA))
    
    # Show horizontal grid lines
    barplot(NA, xlim = c(0, N), ylim = c(0, Nmax), yaxt = "n")
    
    grid(nx = NA, ny = NULL, lty = 3, lwd = 0.5, col = "gray")     

    common_args <- list(
      xlim = c(0, N), ylim = c(0, Nmax), space = 0, border = NA
    )
    
    # Plot the non-NA values
    kwb.utils::callWith(
      barplot, common_args, height = hNoNA, col = myBlue, cex.axis = 0.7, 
      add = TRUE
    )

    # Plot the NA values
    kwb.utils::callWith(
      barplot, common_args, height = hIsNA, col = "indianred", add = TRUE, 
      yaxt = "n"
    )

    # Label the y axis
    mtext(expression(mm(5*min)^{-1}), 2, line = 2, cex = 0.5, adj = 1)
    
    # Draw an x-axis manually
    abline(h = 0)

    # Draw vertical lines at every 8 hours
    idx <- seq_len(N)[as.integer(tAll) %% (86400 / 4) == 0]
    
    abline(v = idx, xpd = TRUE, col = "gray", lty = 2, lwd = 0.5)
    
  } else {
    
    # Draw an empty barplot, not showing anything but with the same y scale
    # as in the previous regular plots
    barplot(NA, xlim = c(0, N), ylim = c(0, Nmax), border = NA, yaxt = "n")

    # Show only every <n_th> label
    n_th <- as.integer((N - 1) / nMaxLabels) + 1
    cat("N:", N, ", I will show every", n_th, "-th label.\n")

    # Calculate indices of labels to be shown by modulo division
    # idx <- (1:N)[(1:N) %% n_th == 0]
    idx <- seq_len(N)[as.integer(tAll) %% (n_th * sigWidth_s) == 0]

    # Put the timestamp labels
    text(
      idx, 1.3 * Nmax, format(tAll[idx], "%d/%m %H:%M"), srt = 90, adj = 1, 
      xpd = TRUE, cex = 0.8
    )

    # Put a label for the x-axis  
    mtext("Timestamp (UTC+1)", side = 1, line = 1)
  }
}

# plotRainOverview (old: hsPlotRainOverview) -----------------------------------
plotRainOverview <- function
### minDate and maxDate must be given as "yyyy-mm-dd"
### Timestamps of the day given in minDate are included, whereas timestamps of
### the day given in maxDay are excluded! This way it is easy to select whole
### months or whole years by selecting the first day of the next month/year as
### maxDate.
### It is assumed that all but the first columns contain rain heights!
(
  rain, 
  minDate = "", 
  maxDate = "", 
  strTimestamp = "Zeitstempel", 
  boolMaxDateIncluded = FALSE, 
  Nmax_mm = NA, 
  cex = 0.9
) 
{
  stopifnot(length(dim(rain)) == 2, dim(rain)[2] >= 2)

  # Define texts
  texts <- kwb.utils::resolve(list(
    main_1 = "Availability of rain data<origin> <interval_info>",
    main_2 = "Total precipitation<origin> <interval_info>",
    origin = " monitored by BWB rain gauges",
    interval_info = "within <date_from> and <date_to>"
  ))
  
  # If minDate and/or maxDate are given, filter rain data for the given 
  # period between minDate and maxDate
  if ((minDate != "") || (maxDate != "")) {
    
    rain <- hsFilterPeriod(
      rain, minDate, maxDate, tsField = strTimestamp, maxIncluded = FALSE, 
      dbg = TRUE
    )
  }

  ## Return if there is no data
  if (nrow(rain) == 0) {
    
    cat(sprintf(
      "\n*** No rain data found between %s and %s.\n\n", minDate, maxDate
    ))
    
    return()
  }
  
  # Prepare main titles for the plots
  time_range <- range(kwb.utils::selectColumns(rain, strTimestamp))
  
  texts <- kwb.utils::resolve(
    texts, 
    date_from = format.POSIXct(time_range[1], "%Y-%m-%d"), 
    date_to = format.POSIXct(time_range[2], "%Y-%m-%d")
  )
  
  # Overview on data availability
  rainStat <- get_rain_stat(rain)

  # Prepare 2x1 plot matrix
  old_par <- par(mfrow = c(2, 1), oma = c(0, 1, 1, 0)) 
  on.exit(par(old_par))
  
  # First bar plot: data availability
  plotDataAvailability(rainStat, main = texts$main_1, cex = cex)
  
  # Second bar plot: total precipitation  
  plotTotalPrecipitation(
    rainStat, main = texts$main_2, Nmax_mm = Nmax_mm, cex = cex
  )
}

# get_rain_stat ----------------------------------------------------------------
get_rain_stat <- function(rain, strTimestamp = names(rain)[1])
{
  stopifnot(is.data.frame(rain), strTimestamp %in% names(rain))
  
  is_na <- is.na(rain)
  basis <- nrow(rain)
  
  rainStat <- data.frame(
    "isntNA" = kwb.utils::percentage(colSums(! is_na), basis), 
    "isNA"   = kwb.utils::percentage(colSums(  is_na), basis)
  )
  
  rainStat <- kwb.utils::roundColumns(rainStat, names(rainStat), digits = 2)
  
  rainStat$total <- rainStat$isNA + rainStat$isntNA

  # Calculate the total precipitaion for each rain gauge (skip timestamp column)
  sums <- colSums(kwb.utils::removeColumns(rain, strTimestamp), na.rm = TRUE)
  
  rainStat$N_mm <- c(NA, round(sums))
  
  rainStat
}

# plotDataAvailability ---------------------------------------------------------
plotDataAvailability <- function # plotDataAvailability
### plotDataAvailability
(
  rainStat,
  n = nrow(rainStat),
  main = "Data Availability",
  cex = 0.9,
  barColours = c("lightgreen", "indianred")
)
{  
  stopifnot(is.data.frame(rainStat), n >= 2, n <= nrow(rainStat))
  stopifnot(nrow(rainStat) >= 2, ncol(rainStat) >= 2)
  columns <- c("isntNA", "isNA")
  
  graphicalParameters <- par(mar = c(4, 4, 5, 1))
  on.exit(par(graphicalParameters))
  
  # Plot first bar plot
  x <- barplot(
    t(kwb.utils::selectColumns(rainStat[2:n, ], columns)), ylim = c(0, 100), 
    cex.names = cex, ylab = "Percentage of Rows", col = barColours
  )
  
  # Add title, grid, legend and text labels showing the bar values 
  # to first bar plot
  title(main, line = 3)
  grid(nx = NA, ny = NULL)
  
  legend(
    "top", legend = c("data available", "data not available (NA)"), horiz = TRUE, 
    box.col = NA, fill = barColours, inset = -0.25, xpd = TRUE
  )

  y <- kwb.utils::selectColumns(rainStat, "isntNA")[2:n]
  
  add_value_labels(x, y, format(y, digits = 1, nsmall = 1), cex = cex)
}

# add_value_labels--------------------------------------------------------------
add_value_labels <- function(x, y, labels = y, cex = 0.9)
{
  delta_y <- kwb.plot::cmToUserWidthAndHeight(0.1)$height

  text(
    x = x, y = y + delta_y, labels = labels, adj = c(0.5, 0), cex = cex, 
    xpd = TRUE
  )
}

# plotTotalPrecipitation -------------------------------------------------------
plotTotalPrecipitation <- function # plotTotalPrecipitation
### plotTotalPrecipitation
(
  rainStat,
  ### data frame with column \emph{N_mm} and row names indicating rain gauge
  ### names
  n = nrow(rainStat),
  ### number of rain series
  main = "Total Precipitation",
  Nmax_mm = NA,
  cex = 0.9
)
{
  N_mm <- kwb.utils::selectColumns(rainStat, "N_mm")
  
  stopifnot(n > 1, n <= nrow(rainStat))
  
  graphicalParameters <- par(mar = c(4, 4, 3, 1))  
  on.exit(par(graphicalParameters))
  
  y <- N_mm[2:n]
  
  x <- barplot(
    y, names.arg = rownames(rainStat)[2:n], ylab = "Total precipitation in mm", 
    ylim = c(0, kwb.utils::defaultIfNA(Nmax_mm, max(N_mm[2:n]))),
    cex.names = cex
  )
  
  # Add title, grid and text labels showing the bar values
  title(main, line = 2)
  
  grid(nx = NA, ny = NULL)

  add_value_labels(x, y, cex = cex)
}

# plot_rain_subevents_by_checksum (old: hsPlotAll) -----------------------------
plot_rain_subevents_by_checksum <- function
### plot_rain_subevents_by_checksum
(
  rain, evt, n = nrow(evt)
) 
{
  # For each rain event
  for (i in seq_len(n)) {
    
    # If the event consists of more than one data row, create subevents by
    # grouping those rows together that have the same combination of stations
    # that provide a (non-NA) signal
    ib <- evt$evtBeg_i[i]
    ie <- evt$evtEnd_i[i]
    
    subevt <- if (ib != ie) {
      
      hsEventsOnChange(rain$chksum[ib:ie])
      
    } else {
      
      data.frame(begi = 1, endi = 1)
    }
    
    kwb.utils::printIf(TRUE, subevt, paste0("Sub-events of event #", i))

    plot_rain_with_subevent_lines(rain[ib:ie, ], subevt, i)
  }
}

# plot_rain_with_subevent_lines (old: hsPlotRain) ------------------------------
plot_rain_with_subevent_lines <- function # plot_rain_with_subevent_lines
### plot_rain_with_subevent_lines
(
  rain, 
  subevt, 
  rainEventIndex
) 
{
  n <- ncol(rain) - 2 # without timestamp, checksum
  ma <- max(rain[, 2:(n + 1)], na.rm = TRUE)
  
  par(mfrow = c(n, 1), oma = c(2, 2, 2, 2), mar = c(0.5, 0, 0.5, 0))
  
  for (i in seq_len(n)) {
    
    plot(rain[, 1], rain[, 1 + i], type = "s", lwd = 2, ylim = c(0, ma),
      xaxt = ifelse(i == n, "s", "n"), col = "red"
    )
    
    if (i == 1) {
      
      text <- paste0(
        "Rain event #", rainEventIndex, " from ", rain[1,1], " to ", 
        rain[nrow(rain), 1]
      )
      
      mtext(text, side = 3, line = 1)
    }

    # Loop through sub-events
    for (sei in seq_len(nrow(subevt))) { # sei = sub-event index
      
      abline(v = rain[subevt$begi[sei], 1], lty = 2) # dashed
      abline(v = rain[subevt$endi[sei], 1], lty = 2)
    }
    
    #legend("topleft", legend=colnames(rain,)[1 + i], bty = "n")
    mtext(colnames(rain)[1 + i], side = 4, line = 1)
  }
}

# plot_one_rain_event_3 (old: hsPlot1) -----------------------------------------
plot_one_rain_event_3 <- function
### plot_one_rain_event_3
(
  frmR, evt, i, myCols
) 
{
  ib <- evt$evtBeg_i[i]
  ie <- evt$evtEnd_i[i]
  n  <- ncol(frmR) - 1
  yma <- max(frmR[ib:ie, 2:n])
  
  par(mfrow = c(n, 1), oma = c(0, 0, 1.2, 0), mar = c(0, 0, 2, 0))

  for (j in seq_len(n)) {
    
    plot(
      frmR[ib:ie, 1], frmR[ib:ie, j + 1], type = "s", col = myCols[j],
      xlab = "", xaxt = "n", ylab = "", lwd = 2, bty = "n", ylim = c(0, yma)
    )

    abline(h = 0)
    
    if (j == 1) {
      
      mtext(
        paste0(
          "Rain #", i, ": ",
          format(frmR[["ts"]][ib], "%d/%m/%Y %H:%M"), " to ",
          format(frmR[["ts"]][ie], "%d/%m/%Y %H:%M")
        ), 
        side = 3, line = 0.8
      )
    }
    
    legend("bottomleft", legend = colnames(frmR)[j + 1], bty = "n")
  }
}

# plot_rain_events_to_pdf (old: hsPrintPdf) ------------------------------------
plot_rain_events_to_pdf <- function
### plot_rain_events_to_pdf
(
  frmR, evt, strPdf, myCols, myVersion = 1
) 
{
  kwb.utils::preparePdf(strPdf, landscape = FALSE)

  if (myVersion == 1) {
    
    # Print pdf file, version 1
    myCols <- rainbow(ncol(frmR) - 1)
    
    for (i in seq_len(nrow(evt))) {
      
      plot_one_rain_event_3(frmR, evt, i, myCols)
    }
  } else {
    
    # Print pdf file, version 2
    plot_rain_subevents_by_checksum(frmR, evt)
  }
  
  dev.off()
}
