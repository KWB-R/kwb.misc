# hsDataAvailability -----------------------------------------------------------
hsDataAvailability <- function # data availability in time-series data
### data availability in time-series data
(
  data,
  ### data frame with timestamp in first column
  tstep = minTimeStep(data[[1]], dbg = dbg),
  ### expected timestep between consecutive timestamps in seconds. Default:
  ### minimum time difference occurring in timestamps of \emph{data}.
  interval = 60*60*24, 
  ### length of time intervals to which data is grouped, in seconds. Default:
  ### 60*60*24 = one day intervals; data availability is calculated separately
  ### for each time interval.
  includeCount = TRUE,
  ### if TRUE, not only the data availability in percent but also the number
  ### of records per interval from which the percentage has been calculated
  ### are included as separate columns in the result data frame.
  dbg = FALSE
) 
{  
  #@2012-04-15;HSB;just call hsGroupInterval with the appropriate function
  ## duplicate the first column (timestamp) so that the timestamps are counted,
  ## too
  res <- hsGroupByInterval(data[, c(1, 1:ncol(data))], interval, 
                           FUN = function(x){sum(!is.na(x))}, offset2 = 0)
  
  ## Append columns containing availabilities in percent
  baseCount <- as.integer(interval / tstep)
  
  ## offset for generation of target column indices; 
  ## trick: offs = 0 if includeCount = FALSE (=0)
  offs <- includeCount * (ncol(res) - 1)
  res[, (2+offs):(ncol(res)+offs)] <- 100 * res[, 2:ncol(res)]/ baseCount

  ## Rename columns; trick: repeat "n"-column-names "includeCount"-times
  ## (TRUE = 1, FALSE = 0)
  names(res) <- c("intervalBeg", 
                rep(paste("n", names(data), sep = ""), includeCount), 
                paste("p", names(data), sep = ""))  
  
  ## Set attribute interval in result data frame
  attr(res, "interval.length") <- interval
  res
}

# hsDataAvailability.old -------------------------------------------------------
hsDataAvailability.old <- function # data availability of time series data
### Data availability of time series data
(
  info,
  ### list with the named elements (\emph{mdb}: full path to Access database, 
  ### \emph{tbl}: table name, \emph{tsField}: name of timestamp field, 
  ### \emph{parField}: name of parameter field)
  dateFirst = NULL, 
  ### Date object representing first date to be considered
  dateLast = NULL, 
  ### Date object representing last date to be considered
  tstep = NULL, 
  ### expected time step between time stamps in seconds. Default: minimum 
  ### time difference found between consecutive timestamps in given interval
  dbg = FALSE
  ### If TRUE, debug messages will be shown  
)
{
  # Build the WHERE-clause for an SQL query filtering for non-NA datasets 
  # in the given time interval
  tCond <- hsSqlExTimeCond(info$tsField, dateFirst, dateLast) # time condition
  cond <- sprintf("NOT IsNull(%s) AND %s", info$parField, tCond)
  
  # Build SQL query that gets the number of datasets per day ("d") 
  # considering the condition <cond> created above.
  sql <- hsSqlExTimeGroup(info$tbl, info$tsField, "d", cond)
  
  # Run the query
  res <- hsSqlQuery(info$mdb, sql, dbg)
  
  # Did we get data?
  if (length(res) == 0) {
    #@2011-12-19: stop instead of "print" and "return"
    stop("The returned recordset is empty.")
  }
  
  ## Find base timestep if not given
  if (is.null(tstep)) {

    ## Get timestamps within time interval
    mind <- NULL; if(! is.null(dateFirst)) mind <- as.character(dateFirst)
    maxd <- NULL; if(! is.null(dateLast))  maxd <- as.character(dateLast)    
    tmp <- hsMdbTimeSeries(info$mdb, info$tbl, info$tsField, info$parField, 
                           minDate = mind, maxDate = maxd)

    tstep <- minTimeStep(tmp[[info$tsField]], dbg = TRUE)
  }
  
  # Calculate availability
  baseCount <- as.integer(60*60*24 / tstep) # 24*60
  res$myAvail <- res$myCount / baseCount * 100
  
  ## Rename columns
  names(res) <- c("intervalBeg", paste(c("n", "p"), info$parField, sep = ""))
  
  # Return result data frame
  res
  ### data.frame with each row representing a day within the specified time
  ### interval and columns \eqn{intervalBeg} (day), \eqn{n<Par>} 
  ### (number of non-NA-values in column <Par> within the interval)
  ### and \eqn{p<Par>} (data availability of parameter <Par> in percent 
  ### = number of available non-NA-values divided by maximum possible 
  ### number of non-NA-values per day (= 86400 / \emph{tstep}).  
}                                   

# hsPlotDataAvailability -------------------------------------------------------
hsPlotDataAvailability <- function
### barplot showing data availability in (e.g. daily) time intervals.
(
  avail, 
  ### data frame containing the availibility information as returned by
  ### hsDataAvailability
  colNames = NULL,
  ### name of column containing availabilities 
  firstIntBeg = NULL,
  ### timestamp indicating the begin of the first interval to be plotted
  lastIntBeg = NULL,
  ### timestamp indicating the begin of the last interval to be plotted
  main = "hsPlotDataAvailability",
  ### main title of barplot
  barCols = NULL,
  ### bar colour(s). If avail is a list of data frames, each data frame is
  ### shown in its own colour as given here in barCols
  labelStep = 2, 
  ### if set to <n>, only every n-th date label will be shown in the plot
  firstPlot = TRUE,
  ### if TRUE, barplot is replotted, else plot is added to existing plot
  dbg = FALSE,
  ...
  ### further arguments to be passed to R's barplot() function.
) 
{
  ## avail must be a list of data frames or a data frame
  if (! (class(avail) %in% c("list", "data.frame"))) {
    stop("avail must be a list of data frames or a data frame.")
  }
  
  ## Get vector of available column names
  if (class(avail) == "data.frame") {
    availColNames <- names(avail)[-1]      
  }
  else {
    availColNames <- names(avail[[1]])[-1]      
  }
  
  ## if NULL, set colNames to all available column names 
  if (is.null(colNames)) {
    colNames <- availColNames
  }
  else {
    # Stop if any given column does not exist
    if (! all(colNames %in% availColNames))
      stop("At least one of the given column names could not be found in avail.")
  }

  if (dbg)
    cat("colNames:", paste(colNames, collapse = ", "), "\n")
  
  # Run this function for each column if more than one column is requested
  if (length(colNames) > 1) {
    for (colName in colNames) {
      hsPlotDataAvailability(avail, colNames = colName, firstIntBeg, lastIntBeg, 
                             paste(main, substr(colName, 2, nchar(colName)), 
                                   sep = ": "), barCols = barCols, 
                             labelStep = labelStep, dbg = dbg, ...)
    }
    return()
  }
  
  ## Here, we know that we have to deal with only one column...

  # mar: original margins plus 2 more lines at the bottom and 1 more on top
  # xpd: to allow legend to be outside of the plot region
  par(mar = c(7, 5, 4, 5), xpd = TRUE)
  
  # If avail is not a data frame but a list of data frames, call this
  # function for each data frame within the list. 
  if (class(avail) == "list") {
    
    if (dbg) {
      cat("You gave me a list of data frames: \n")
      print(names(avail))
    }
    
    ## We have to find the overall minimum and maximum interval begin if
    ## they are not given:
    if (is.null(firstIntBeg)) {
      firstIntBeg <- min(avail[[1]][[1]])
      for (i in seq(1, by = 1, along.with = avail)) {
        firstIntBeg <- min(firstIntBeg, min(avail[[i]][[1]]))
      }
      #firstIntBeg <- min(sapply(sapply(avail, FUN = "[", i = 1), FUN = "min"))
    }
    if (is.null(lastIntBeg)) {
      lastIntBeg <- max(avail[[1]][[1]])
      for (i in seq(1, by = 1, along.with = avail)) {
        lastIntBeg <- max(lastIntBeg, max(avail[[i]][[1]]))
      }
      #lastIntBeg <- max(sapply(sapply(avail, FUN = "[", i = 1), FUN = "max"))    
    }
    
    for (i in seq(1, by = 1, along.with = avail)) {
      hsPlotDataAvailability(avail[[i]], colNames = colNames, firstIntBeg, lastIntBeg,
             main, barCols = barCols[i], labelStep = labelStep, 
                             firstPlot = (i == 1), dbg = dbg, ...)
    }

    # Add a legend to the plot...
    # inset=-0.1: 10% of plot height above the plot
    #@2011-12-19: adapt the legend to the type of the data status
    if (is.null(barCols))
      barCols <- rainbow(length(avail))
    
    legend("top", legend = names(avail), fill = barCols, 
           border = "black", box.col = NA, horiz = TRUE) #, inset = -0.15)
    
    return()
  }

  if (dbg) {
    cat("You gave me a data frame:\n")
    print(head(avail))
  }

  ## if NULL, set firstIntBeg/lastIntBeg
  if (is.null(firstIntBeg))
    firstIntBeg <- min(avail[[1]])
  if (is.null(lastIntBeg))
    lastIntBeg <- max(avail[[1]])
  
  # Generate sequence of dates between first and last date
  interval <- attr(avail, "interval.length")

  if (dbg) {
    cat("interval:   ", interval , "\n",
        "firstIntBeg:", firstIntBeg, "\n",
        "lastIntBeg: ", lastIntBeg, "\n")
  }

  intBegs <- seq(firstIntBeg, lastIntBeg, by = interval)  
  
  if (dbg) {
    cat("intBegs:\n")
    print(intBegs)  
  }
  
  # merge sequence of all intervals with intervals in data frame avail
  pAvail <- merge(data.frame(intervalBeg = intBegs), avail,
                  by.x = "intervalBeg", by.y = names(avail)[1], all.x = TRUE)
  
  if (dbg) {
    cat("pAvail:\n")
    print(head(pAvail))
    cat("pAvail[[colNames]]:\n")
    print(pAvail[[colNames]])
  }
  
  # Prepare bar labels
  lbls <- niceLabels(intBegs, labelStep)
  
  # Prepare plot title
  main <- sprintf("%s\n%s to %s", main, 
                  format.Date(as.Date(firstIntBeg)), 
                  format.Date(as.Date(lastIntBeg)))

  ## Plot availabilities
  if (firstPlot) {
    barplot(pAvail[[colNames]], 
            names.arg = lbls, 
            space     = 0, 
            col       = barCols,
            las       = 3, 
            main      = main, 
            xlab      = "", 
            ylab      = "Data avaliability in %", 
            ylim      = c(0,115), 
            ...)    
  }
  else {
    barplot(pAvail[[colNames]], space = 0, col = barCols, add = TRUE)
  }

  # Add horizontal lines at 0% and 100%
  # xpd=FALSE: only show inside plot area
  abline(h = c(0,100), xpd = FALSE, lty = 2) # , col = "darkgrey" 
}


