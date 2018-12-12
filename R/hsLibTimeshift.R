# hsAllTimeshiftPlots ----------------------------------------------------------
hsAllTimeshiftPlots <- function
### For one water quality parameter, all overflow events given in "evt" are
### plotted in different scales given by "fracts" (fractions of interval 
### length). The time-series of the water quality parameter at the container, 
### upstream and downstream are plotted over time as well as the flow.
###@2011-11-25: moved to here
###@2011-12-13: renamed from hsPlotAll; new parameters
(
  frmOrig, 
  ### Original time-series 
  frmUs, 
  ### upstream shifted time-series
  frmDs, 
  ### downstream shifted time-series
  evt, 
  ### event list
  fieldNames,
  ### vector containing the relevant table field names as named elements:
  ### tsOrig: timestamp in Q time-series), 
  ### tsUsDs: timestamp in us/ds time-series),
  ### fieldQ: name of field containing Q,
  ### fieldP: name of field containing wq parameter
  plusOverview = FALSE, 
  ### If true, one plot comprising the whole time-series in frmOrig is added
  type1 = "l", 
  ### Plot type (default: "l" = line)
  fracts = c(0, 0.75,0.5,0.25,0.1,0.05),
  ### Vector of scaling factors. For each factor a plot is generated representing
  ### the corresponding fraction of the whole time interval of the event. 
  ### A factor of zero will plot the whole event.
  fieldPrefix = ""
) 
{  
  # Overview plot around the whole time interval in frm Orig
  if (plusOverview) {
    hsTimeshiftPlot(frmOrig, frmUs, frmDs, evt, fieldNames, 
		  i, fract, type1, boolOverview = TRUE, fieldPrefix = fieldPrefix)
  } 
  
  # One plot per event and zoom factor
  for (i in 1:(nrow(evt))) {
    for (fract in fracts) {
      hsTimeshiftPlot(frmOrig, frmUs, frmDs, evt, fieldNames, 
        i, fract, type1, fieldPrefix = fieldPrefix)
    }
  }
}

# hsTimeshiftPlot --------------------------------------------------------------
hsTimeshiftPlot <- function
### A plot is generated, containing flow measurements as well as 
### measurements (original, upstream/downstream time-shifted) of one 
### water quality parameter and for one overflow events i contained in the
### event list "evt".
# @2011-11-25: moved here
(
  frmOrig, 
  frmUs, 
  frmDs, 
  evt, 
  fieldNames, 
  i, 
  evtDurFract, 
  type1, 
  boolOverview = FALSE,
  fieldPrefix = ""
) 
{

	# Extract field names
	strTsOrig <- as.character(fieldNames$tsOrig) # timestamp in Q time-series
	strTsUsDs <- as.character(fieldNames$tsUsDs) # timestamp in us/ds time-series
	strQ      <- as.character(fieldNames$fieldQ)   # name of field containing Q
	strP      <- as.character(fieldNames$fieldPar) # name of field containing wq parameter
  strP2     <- paste(fieldPrefix, fieldNames$fieldPar, sep = "") # in us/ds  
  
	# Enlarge right margin
#  par(oma = c(1,1,1,2)) 
	
	if (boolOverview) {
		myTitle = "All events"

    # Plot the whole range of values
		ib <- 1
		ie <- nrow(frmOrig)

		# Calculate x limits
		myXlim <- c(frmOrig[[strTsOrig]][ib], frmOrig[[strTsOrig]][ie])
	}
	else {

    # Include event and zoom factor information in title
		myTitle <- sprintf("Event #%d from %s to %s" , i, evt$tBeg[i], evt$tEnd[i])
		if (evtDurFract != 0) {
		  myTitle <- sprintf("%s\n+/- %0.2f * event duration around t(Q = Qmax)", 
                         myTitle, evtDurFract)
		}  

		# Centre the plot around the maximum Q within the event
		ib    <- evt$iBeg[i] # begin index
		ie    <- evt$iEnd[i] # end index
		ed    <- evt$dur[i]
		tsb   <- frmOrig[[strTsOrig]][ib] # begin timestamp
		tse   <- frmOrig[[strTsOrig]][ie] # end timestamp
		iQmax <- which.max(frmOrig[[strQ]][ib:ie])
		tsmid <- frmOrig[[strTsOrig]][ib:ie][iQmax]
		
		# Calculate x limits according to the zoom factor
		if (evtDurFract != 0) {
		  myXlim <- c(tsmid - evtDurFract * ed, tsmid + evtDurFract * ed)
		}	else {
		  myXlim <- c(frmOrig[[strTsOrig]][ib], frmOrig[[strTsOrig]][ie])
		}
	}	

	# Calculate maximum values for Q and wq parameter
  parVals <- frmOrig[[strP]][ib:ie]
  if (all(is.na(parVals))) {
    cat(sprintf("Event #%d: All parameter values are NA in the original data!\n", i)    )
    maxP <- 100
  } else {
    maxP <- max(parVals, na.rm = TRUE)
  }
  
  maxQ <- max(frmOrig[[strQ]][ib:ie], na.rm = TRUE)

  # Calculate y limits
	if (boolOverview) {
		myYlim  <- maxP * c(-1.6, 1.2)
		myYlim2 <- maxQ * c(-0.6, 2.6)
	}
	else {
		myYlim  <- maxP * c(-1.2,   1)
		myYlim2 <- maxQ * c(-0.1, 2.2)
  }

	# Plot water quality parameter over time
  plot(
		frmOrig[[strTsOrig]][ib:ie], # x values
		frmOrig[[strP]][ib:ie],      # y values
		xaxt = "n", # suppress x axis
		yaxt = "n", # suppress y axis
    type = type1, col  = "red", pch  = 20, # plot style
		xlim = myXlim, 
		ylim = myYlim, 
		main = myTitle, 
    xlab = "",
		ylab = paste(strP, "in mg/l")
	)
	  	  
	# Add a y axis to the left
	axis(2, seq(0, maxP, 50))

	# Convert x limits to POSIXct
    myXlim2 <- as.POSIXct(3600 * as.integer(as.integer(myXlim)/3600), 
		origin = "1970-01-01 01:00:00 CET")
    tseq <- seq(myXlim2[1], myXlim2[2], 3600)

	# Add an x axis at the bottom
    axis(1, at = tseq, labels = format(tseq, "%Y-%m-%d %H:%M"))
    
	if (boolOverview) {
		idus <- 1:nrow(frmUs)
		idds <- 1:nrow(frmDs)
	}
	else {
		# Indices in us/ds data corresponding to this event
		idus <- c(1:nrow(frmUs))[frmUs[[strTsUsDs]] >= tsb & frmUs[[strTsUsDs]] <= tse]
		idds <- c(1:nrow(frmDs))[frmDs[[strTsUsDs]] >= tsb & frmDs[[strTsUsDs]] <= tse]
	}

	# Add upstream time-series of wq parameter in blue
  lines(frmUs[[strTsUsDs]][idus], frmUs[[strP2]][idus], col = "blue",  lty = 1, pch = 20)
	
	# Add downstream time-series of wq parameter in green
  lines(frmDs[[strTsUsDs]][idds], frmDs[[strP2]][idds], col = "green", lty = 1, pch = 20)
	
	# Add a new plot showing Q over time
    par(new=TRUE)
    plot(
		frmOrig[[strTsOrig]][ib:ie], # x values
		frmOrig[[strQ]][ib:ie],      # y values
    type = type1, pch = 20,      # plot style
		xaxt = "n",                  # suppress x axis
		yaxt = "n",                  # suppress y axis
		xlim = myXlim, 
    ylim = myYlim2, 
		xlab = "",                   # suppress x axis label
		ylab = ""                    # suppress y axis label
	)

	# Add a y axis to the right
	axis(4, pretty(maxQ * seq(-0.4,1,0.2)))

	# Add a y axis label
  mtext("Q in m3/s", 4, line = 2)

	# Add a legend
  legend(
		"topleft", 
		c(strP, paste(strP, "(us)"), paste(strP, "(ds)"), strQ),
		horiz  = FALSE, 
		lty    = c(1,1,1,1), 
		col    = c("red", "blue", "green", "black"), 
		border = NULL
	) 
  
}

# hsTimeshift ------------------------------------------------------------------
hsTimeshift <- function
### upstream or downstream "timeshift" of water quality data given time-series
### of hydraulic and water quality data in one data frame
(
  hq,
  ### hydraulic and water quality data in one data frame
  threshold,
  ### Threshold that shall be reached/exceeded by the sum of successive
  ### values in column \emph{valField} of which the maximum time difference is
  ### below or equal \emph{maxTDiff}.
  upstream = TRUE,
  ### if TRUE, the algorithm \dQuote{looks} upstream, else downstream
  tsField = names(hq)[1],  
  ### name of timestamp field in \emph{hq}
  valField = names(hq)[2],
  ### name of column in \emph{hq} containing the values to be summed up until
  ### the threshold is reached
  quaFields = names(hq)[-c(1, 2)],
  ### vector containing column names of water quality parameters, 
  ### e.g. c("AFS", "CSB", "CSBf")
  maxTDiff = 3600,
  ### Maximum allowed time difference in seconds between two related timestamps.
  valFactor = 1,
  ### factor to be applied to column \emph{valField} before calculating value
  ### sums.
  dbg = FALSE
) 
{
  ## Return if tsField is not a column in hq
  hqn <- names(hq)  
  if (! tsField %in% hqn)
    stop("No such timestamp field: ", tsField)
  
  ## Return if valField is not a column of hq
  if (! valField %in% hqn)
    stop("No such value field: ", valField)
  
  ## Return if quaFields are not all columns of hq
  if (! all(quaFields %in% hqn))
    stop("No such water quality field(s): ", 
         paste(quaFields[!quaFields %in% hqn], collapse = ", "))
  
  # Find timestamps related to timestamps of measurement in such a way that
  # the sum of values in \emph{valField} within the time interval between 
  # measurement timestamp and related timestamp is greater or equal the given 
  # threashold.  
  tShift <- hsIntSumGeThreshold(
    tSeries   = hq, 
    threshold = threshold, 
    forward   = !upstream,
    maxTDiff  = maxTDiff,
    tsField   = tsField, 
    valField  = valField, 
    valFactor = valFactor,
    dbg       = dbg)
    
  # Append related timeshift (tStop) to hydraulic and water quality data
  hqShift <- merge(hq, tShift[, c("tStart", "tStop", "tDiff.s")], 
    by.x = tsField, by.y = "tStart", all.x = TRUE)
  
  # Filter for non-NA values in column "tStop"
  hqShift <- hqShift[!is.na(hqShift$tStop), ]
  
  # Calculate mean values of water quality parameters for which the same related
  # timestep "tStop" has been found
  agg1 <- aggregate(hqShift[, quaFields], by = list(tRelated = hqShift$tStop), mean)

  ## Rename columns
  #if (isTRUE(indicateMean))
    names(agg1)[-1] <- paste("mean", quaFields, sep = ".")
  
  # Calculate min values of time difference between timestamp of measurement
  # and related timestamp; drop = FALSE prevents from reducing the data frame
  # to a vector when selecting a single column (here: tDiff) 
  agg2 <- aggregate(hqShift[, "tDiff.s", drop = FALSE], by = list(tRelated = hqShift$tStop), min)

  # Rename column "tDiff.s" with "tDiffMin_s"
  names(agg2)[names(agg2) == "tDiff.s"] <- "min.tDiff.s"
    
  # Calculate max values of time difference between timestamp of measurement
  # and related timestamp; drop = FALSE prevents from reducing the data frame
  # to a vector when selecting a single column (here: tDiff) 
  agg3 <- aggregate(hqShift[, "tDiff.s", drop = FALSE], by = list(tRelated = hqShift$tStop), max)

  # Rename column "tDiff.s" with "tDiffMax_s"
  names(agg3)[names(agg3) == "tDiff.s"] <- "max.tDiff.s"
  
  # Count values that have been used for averaging
  agg4 <- aggregate(hqShift[, "tDiff.s", drop = FALSE], by = list(tRelated = hqShift$tStop), length)
  
  # Rename column "tDiff.s" with "tDiffMax_s"
  names(agg4)[names(agg4) == "tDiff.s"] <- "nRows"
  
  # Merge agg2 with agg3 and the result with agg4
  agg <- merge(agg2, agg3, by = "tRelated")
  agg <- merge(agg,  agg4, by = "tRelated")
  
  # Merge agg1 with result of merging agg2, agg3 and agg4
  agg <- merge(agg1, agg, by = "tRelated")
  
  # Merge hydraulic data
  merge(hq[, setdiff(names(hq), quaFields)], agg, 
    by.x = tsField, by.y = "tRelated", all.y = TRUE)
}

# .hsCumToSum ------------------------------------------------------------------
.hsCumToSum <- function # experimental!!! 
### I was looking for an easier function than hsIntSumGeThreshold
(
  x,
  ### data frame. First column is assumed to contain timestamps, second column 
  ### to contain values to be cumulated  
  threshold = 500
)
{
  if (class(x) != "data.frame" || (ncol(x)) < 2)
    stop("x must be a data frame of at least two columns.")

  ## Calculate the cumulated sum of the value column
  cs <- cumsum(x[[2]])

  ## number of rows
  n <- nrow(x)
  
  ## Prepare vector storing indices of assigned timestamps
  ia <- integer(0)
  sumSoFar <- double(0)
  
  ## Calculate sums of values within time intervals by calculating the
  ## difference of corresponding cumulated sums.
  for (i in 1:(n-1)) {

    ## Show progress
    if (i %% 100 == 0)
      cat(i, "/", n-1, "\n")
    
    ## adapt range of indices
    rng <- (1+i):n
    
    ## indices with no assignment yet but where the sum of values 
    ## reaches/overtops the threshold now
    csum <- cs[rng] - cs[rng-i]
    thhr <- (csum >= threshold) # threshold reached?
    bsel <- thhr & (is.na(ia[rng] | csum < sumSoFar[rng]))
    sumSoFar[rng[bsel]] <- csum[bsel]
    idx <- rng[bsel]
    ia[idx] <- (1:n)[idx-i]
    
  }
  cat("\n")
  
  ## get assigned timestamps from assigned indices
  tassign <- x[ia, 1]
  
  ## create result data frame
  data.frame(ia, v = x[[2]], cs, sumSoFar, t=x[[1]], tassign, td = x[[1]]-tassign)
}

# hsIntSumGeThreshold ----------------------------------------------------------
hsIntSumGeThreshold <- function
### \dQuote{Interval sum greater or equal threshold}. 
### For each index <iStart> of vector \emph{values}, this function tries to find
### a corresponding index <iStop> in such a way that the sum of the
### vector elements at indices between <iStart> and <iStop> reaches
### the given threshold. For each possible start index i, the algorithm starts
### either looking forward at indices i+1, i+2, ... or backwards at indices
### i-1, i-2, ..., accumulating the values at these indices. Once the
### accumulated sum reached the given threshold or if the difference between
### the indices exceeds the maximum allowed index difference \emph{maxDist}
### the algorithm stops and continues with the next start index.
#@2011-12-29: created
#@2012-01-09: renamed from hsIntervalsWithSumReachingThreshold4
#             renamed from hsIntervalSumAboveThreshold
(
  tSeries,
  ### data.frame with timestamps in first column and values in second column.
  threshold,
  ### Threshold that shall be reached/exceeded by the sum of successive
  ### elements of \emph{values} of which the maximum time difference is
  ### below or equal \emph{maxTDiff}.
  forward,
  ### If TRUE, the algorithm looks forward, else backwards, i.e. when looking
  ### forward (backwards), the start indices <iStart> are always
  ### less or equal (greater or equal) the assigned indices <iStop>.
  maxTDiff,
  ### Maximum allowed time difference in seconds between two related timestamps.
  tsField = names(tSeries)[1],
  ### Name of time stamp field; default: name of first column
  valField = names(tSeries)[2],
  ### Name of value field containing the values of which the sum shall reach the
  ### threshold; default: name second column
  valFactor = 1,
  ### Factor to be applied to column \emph{valField} before calculating value
  ### sums.
  includeIndices = TRUE,
  ### if TRUE, two columns \emph{iStart} and \emph{iStop} are included in the
  ### output data frame indicating the indices in \emph{tSeries} corresponding
  ### to the timestamps \emph{tStart} and \emph{tStop}.
  dbg = FALSE
) 
{

  ## sub-function next.i
  next.i <- function(st) {
    if (dbg) cat("-> Next i.")

    if (forward) {
      if (st$i <= n) st$s <- st$s - v[st$i]
      st$i <- st$i + 1
    }
    else {
      st$i <- st$i + 1
      if (st$i <= n) st$s <- st$s + v[st$i]
    }

    # Return new state
    st
  }

  ## sub-function grow
  grow <- function(st) {
    if (dbg) cat("-> grow.")
  
    # condition for continuation
    if (forward) continue <- (st$j < n)
    else         continue <- (st$j > 1)
    if (continue) {
      if (forward) st$j <- st$j + 1
      else         st$j <- st$j - 1
      st$s <- st$s + v[st$j]
    }
    else {
      if (dbg) cat("Cannot grow as j reached n.")
      st <- next.i(st)
    }

    # Return new state
    st
  }

  ## sub-function shrink
  shrink <- function(st) {
    if (dbg) cat("-> shrink.")
  
    # condition for continuation
    if (forward) continue <- (st$j > st$i)
    else         continue <- (st$j < st$i)
    if (continue) {
      st$s <- st$s - v[st$j]
      if (forward) st$j <- st$j -1
      else         st$j <- st$j +1
    }
    else {
      if (dbg) cat("Cannot shrink as j reached i.")
      st <- next.i(st)
    }

    # Return new state
    st
  }

  ## sub-function statestr
  statestr <- function(st) {
    sprintf("i: %6d, j: %6d, s: %6.2f, tdiff: %6d; ", st$i, st$j, st$s, timeDiff(st))
  }

  ## sub-function timeDiff
  timeDiff <- function(st) {
    abs(ti[st$i] - ti[st$j])
  }
  
  timeStart <- Sys.time()
  cat(sprintf("Start: %s\n", timeStart))
  
  # Prepare vector of timestamps as integer to speed up time diff calculation
  ti <- as.integer(tSeries[[tsField]])
  v <- valFactor * tSeries[[valField]] # vector of values
  v <- ifelse(is.na(v), 0, v) # Replace NA with 0

  # Initialise vectors containing indices that represent the beginning and
  # end of the intervals in which the sum of values reaches the threshold.
  iStart <- iStop <- sumWithin <- NULL

  # Total number of values
  n <- nrow(tSeries)

  # Prepare initial state
  st <- list(i = 1, j = ifelse(forward, 0, 2), s = 0)

  # Loop through all indices of the input vector <values>
  while (st$i <= n) {
    if (st$i %% 1000 == 0) cat("i:", st$i, "\n")
    
    # THRR: threshold reached, MTDR: maximum time difference reached
    THRR <- MTDR <- FALSE
    
    while(st$i <= n && ! THRR && ! MTDR) {
      st <- grow(st)

      if (st$i <= n) {
        if (dbg) cat(sprintf("\n%s", statestr(st)))

        # threshold reached (THRR) or maximal time difference reached (MTDR)?
        THRR <- (st$s >= threshold)
        MTDR <- (timeDiff(st) > maxTDiff)

        if (dbg) cat(sprintf("%4s %4s", ifelse(THRR, "THRR", ""),
                                        ifelse(MTDR, "MTDR", "")))
      }
    }

    # Append state if threshold was reached within max. time diff.
    if (THRR && ! MTDR) {

      # Append i to iStart, j to iStop and s to sumWithin
      if (dbg) cat("-> appending current state.")
      iStart    <- c(iStart,    st$i)
      iStop     <- c(iStop,     st$j)
      sumWithin <- c(sumWithin, st$s)
    }

    # Continue with next i
    st <- next.i(st)
    
    # If we are not yet at the end...
    if (! st$i > n) {

      # shrink back below  max. time diff. and below threshold
      while (st$i != st$j 
        && (st$s >= threshold || timeDiff(st) >= maxTDiff)) {
        st <- shrink(st)
      }
    }

  } # end of while-loop

  cat(sprintf("\n\nRuntime: %0.2f s\n", Sys.time() - timeStart))

  # Return index vectors, index differences (= interval lengths in minutes)
  # and sums reached in a data frame
  res <- data.frame(
    iStart,
    iStop,
    tStart = tSeries[iStart, 1],
    tStop  = tSeries[iStop,  1],
    tDiff.s  = ti[iStop] - ti[iStart],
    sumWithin)
  
  ## Rename column "sumWithin" with "sum.<valField>"
  names(res)[6] <- paste("sum", valField, sep = ".")

  ## Remove index columns if not desired
  if (! isTRUE(includeIndices))
    res <- res[, -c(1, 2)]

  res
  ### data frame with columns \emph{iStart} and \emph{iStop} being the indices of 
  ### \emph{tSeries} that represent the beginning and the end of the time interval 
  ### in which the value field sums up to at least \emph{threshold}, \emph{tStart}, 
  ### \emph{tStop} and \emph{tDiff} representing the first timestamp, last timestamp 
  ### and duration of the corresponding time intervals and the column 
  ### \emph{sumWithin} being the sum of values that was actially reached within 
  ### the interval.
}

