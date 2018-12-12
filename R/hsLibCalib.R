# hsCalibAna -------------------------------------------------------------------
hsCalibAna <- function 
### Try all different combinations of events for calibration
(
  data,
  dbg = TRUE,
  doplot = TRUE,
  plot.main = "",
  pdf
)
{
  ## Outer loop: number n of events of which data is to be used 
  N <- max(data$evtid) # Number of available events
  ntot <- 0
  maxEval <- -Inf
  
  if (! missing(pdf)) 
    hsPrepPdf(pdf)
  
  ## Prepare the result structure
  res <- list()
  
  for (n in 1:N) {
    
    if (dbg) cat("Taking into account", n, "out of", N, "events...\n")
    
    ## build combinations of n events 
    combis <- combn(1:N, n)

    ## Inner loop: all possible combinations of n events to be taken from all
    ## available events
    for (i in 1:ncol(combis)) {
      ntot <- ntot + 1
      combi <- combis[, i]
      if (dbg) cat("Combi", i, ":", paste(combi, collapse = ", "), "...\n")
      
      ## Let's do something with this combination...
      res.stdev <- hsTestCombi(data, combi, plot = doplot, plot.main = plot.main)    

      if (res.stdev > maxEval) {
        maxEval <- res.stdev
        cat("\n*** maxEval:", maxEval, "\n")
      }
      
      cat("Residual standard deviation:", res.stdev, "\n")
      
      ## List element
      le <- list(evtids = combi, eval = res.stdev) 
      res <- c(res, list(le))
    }
  }
  if (dbg) cat ("altogether", ntot, "combinations have been performed.\n")
  
  ## Prepare an empty plot
  plot(NULL, NULL, xlim = c(1, N), ylim = c(0, ceiling(maxEval)), 
       xlab = "Number of events taken for calibration",
       ylab = "Residual standard deviation")

  for (ele in res) {
    n <- length(ele$evtids)
    points(n, ele$eval)    
  }  

  ## Finish pdf file and open it in viewer if pdf was given
  if (! missing(pdf)) {
    dev.off()
    cmd <- paste(getOption("pdfviewer"), ' "', pdf, '"', sep="")
    system(cmd)
  }
  
  res
}

# hsTestCombi ------------------------------------------------------------------
hsTestCombi <- function
### evaluate a specific combination of events used for calibration
(
  data, 
  combi, 
  plot = TRUE, 
  plot.main = "Lab values vs probe values",
  COLS = rainbow(length(unique(data$evtid)))
) 
{

  ## Calculate x and y limits
  xlim <- c(floor(min(data$prbval)), ceiling(max(data$prbval)))
  ylim <- c(floor(min(data$labval)), ceiling(max(data$labval)))
  
  ## Create sub-dataset according to combi
  sdat <- data[data$evtid %in% combi, ]
  
  ## Find a "sub" linear model fitting to the sub dataset 
  slm <- lm(labval ~ prbval, data = sdat)
  
  ## Calculate prediction for complete dataset
  prd <- predict(slm, data, se.fit = TRUE)
  
  ## Return residual standard deviations
  #sqrt(sum((prd$fit - data$labval)^2) / (nrow(data) - 2))
  my.eval <- sum((prd$fit - data$labval)^2) / (nrow(data) - 2)
  
  ## Plot lab values vs probe values and straight line representing the 
  ## linear model
  if (plot) {
    txt <- paste("Linear model taking into account event(s):", 
                 paste(combi, collapse = ", "))
    
    ## Plot all points of this combi
    plot(sdat$prbval, 
         sdat$labval, 
         #main = txt, 
         xlab = "probe value", 
         ylab = "lab value",
         xlim = xlim,
         ylim = ylim,
         main = plot.main)
    
    ## Replot points of different events in different colours
    for (evtid in combi) {
      idx <- sdat$evtid == evtid
      points(sdat$prbval[idx], 
             sdat$labval[idx], 
             pch = 16, 
             col = COLS[evtid])
    }
    cf.sub <- coef(slm)
    abline(cf.sub[1], cf.sub[2], lwd = 2) 

    ## Plot also straight line representing the "total" linear model
    cf.tot <- coef(lm(labval ~ prbval, data = data))
    abline(cf.tot[1], cf.tot[2], col = "gray")
    
    ## Add a legend
    leg.txt <- c(paste("Event ", combi, sep = "#"), 
                 sprintf("linear(shown events): y = %0.2f * x + (%0.2f); var. of resid. = %0.2f", 
                         cf.sub[2], cf.sub[1], my.eval), 
                 sprintf("linear(all events): y = %0.2f * x + (%0.2f)", 
                         cf.tot[2], cf.tot[1]))
    #print(leg.txt)
    legend("topleft", 
           legend = leg.txt,
           pch = c(rep(16, length(combi)), NA, NA),
           lty = c(rep(NA, length(combi)), 1, 1),
           lwd = c(rep(NA, length(combi)), 2, 1), 
           col = c(COLS[combi], "black", "gray")
           )
  }
  
  my.eval
}

# hsPlotCalOverview ------------------------------------------------------------
hsPlotCalOverview <- function
### plot overview of result of calibration analysis
(
  moniPoint,
  parAcronym,
  mdbCal,
  pdf,
  ... ##<< arguments given to hsPlotEventOverview
  )
{
  ## Put all available calibrations into one data frame
  cdat <- hsMergeAvailCalibs(moniPoint, parAcronym, mdbCal = mdbCal)

  ## Open PDF file if path to pdf file has been given
  if (! missing(pdf)) {
    hsPrepPdf(pdf)
    cat ("plotting to", pdf, "... ")
  }

  ## Plot an overview of the calibrated data
  hsPlotEventOverview(cdat, myTitle = sprintf("%s at %s (all available calibrations)",
                                    parAcronym, moniPoint), ...)    
  
  ## If path to pdf file has been given finish PDF file and open it
  ## in the pdf viewer
  if (! missing(pdf)) {
    dev.off()
    cat ("ok.\n")
    cmd <- paste(getOption("pdfviewer"), ' "', pdf, '"', sep = "")
    cat ("Opening", pdf, "... ")
    system(cmd)
    cat ("ok.\n")
  }
}

# hsMergeAvailCalibs -----------------------------------------------------------
hsMergeAvailCalibs <- function # merge data in available calibrations
### merge data in available calibrations
(
  moniPoint,
  parAcronym,
  mdbCal,
  ... ##<< arguments given to hsPlotEventOverview
  )
{
  ## Get the current  calibration with selected and reordered columns:
  ## myDateTime, par_global, par_curCal
  cdat <- hsCurCal(moniPoint, parAcronym, mdbCal = mdbCal)[, c(1, 3, 2)]  
  
  ## Rename parameter column into name of current calibration:
  ## <parAcronym>_<moniPoint>
  names(cdat)[3] <- sprintf("%s_%s", parAcronym, moniPoint)   
  
  ## Loop through available calibrations of monitoring point/parameter 
  ## combinations and skip the current calibration
  for (cname in hsAvailCalibs(moniPoint, parAcronym, skipCur = TRUE, 
                              mdbCal = mdbCal)) {
    cat("Calibration", cname, "...\n")
    
    ## Get calibrated data according to specific calibration named <cname>
    cdat.i <- hsSpecCal(moniPoint, parAcronym, cname, mdbCal = mdbCal)[, 1:2]
    
    ## Rename parameter column in cdat.i
    names(cdat.i)[2] <- cname
    
    ## merge datasets of different calibrations (only essential columns)
    ## - reorder columns of current calibration: myDateTime, par_global, par_current
    cdat <- merge(cdat, cdat.i, by = "myDateTime", all = TRUE)
  }  

  ## Replace underscore with dot in all column names
  names(cdat) <- gsub("_", ".", names(cdat))

  ## Return the merged data frame
  cdat
  ### data frame with the first two columns representing the timestamp and the
  ### global calibration, respectively and the following columns representing
  ### available calibrations, beginning with the "current" calibration that
  ### is stored in the calibration database under the name 
  ### <parAcronym>_<moniPoint>, e.g. "AFS_STA"
}

# hsCurCal ---------------------------------------------------------------------
hsCurCal <- function # Current calibration
### Get calibrated data according to current calibration
(
  moniPoint, 
  parAcronym,
  globOnly = FALSE,
  mdbCal = NULL
  ## Path to database containing queries that getting calibrated data according
  ## to the currently active calibration setting  
) 
{
  if (is.null(mdbCal))
    mdbCal <- "\\\\moby/miacso$/Daten/ACCESS/KwbMonitoring/3CAL/KWB_CAL.mdb"
  
  ## Build query name
  qry <- sprintf("qry_%s_CAL_%s", moniPoint, parAcronym)
  cat("Query:", qry, "\n")
  
  if (isTRUE(globOnly)) fields = sprintf("%s_global", parAcronym)
  else                  fields = "*"
  
  # Return calibrated data of currently active calibration setting
  hsMdbTimeSeries(mdbCal, qry, "myDateTime", fields = fields)  
}

# hsSpecCal --------------------------------------------------------------------
hsSpecCal <- function # Special calibration
### Get calibrated data according to a special calibration given by its name.
### Instead of running the prepared query in \dQuote{KWB_CAL.mdb} the specific
### SQL query respecting the given calibratio name is built and run here.
(
  moniPoint, 
  parAcronym, 
  calName = NULL,
  mdbCal = NULL
  ## Path to database containing queries that getting calibrated data according
  ## to the currently active calibration setting  
)
{
  ## Take care to have set the time zone to "UTC" when calling hsSqlQuery!!! 
  tzone <- Sys.getenv("tz")
  Sys.setenv(tz = "UTC")
  on.exit(Sys.setenv(tz = tzone))

  if (is.null(mdbCal))
    mdbCal <- "\\\\moby/miacso$/Daten/ACCESS/KwbMonitoring/3CAL/KWB_CAL.mdb"
  
  ## Return the result of the corresponding SQL query
  hsSqlQuery(mdbCal, hsSqlExCal(moniPoint, parAcronym, calName))
}

# hsSqlExCal -------------------------------------------------------------------
hsSqlExCal <- function
### Generate SQL expression needed to get calibrated data
(
  moniPoint, parAcronym, calName = NULL
)
{
  calName <- defaultIfNULL(calName, sprintf("%s_%s", parAcronym, moniPoint))
  
  # Query as stored in KWB_CAL.mdb:
  sql <- sprintf(paste(
    "SELECT v.myDateTime, v.%s_A*pcSlope+pcOffset AS %s,",
    "v.%s_A AS %s_global, pcSlope, pcOffset"), 
                 parAcronym, parAcronym, parAcronym, parAcronym)
  sql <- paste(
    sql, sprintf(paste(
      "FROM KWB_%s_ScanPar_%s_VAL AS v,",
      "tblPartialCalib, tblCalib, tblCalibPartialCalib"),
                 moniPoint, parAcronym))
  sql <- paste(
    sql, sprintf(paste( ## single quotes needed for text comparison in Jet SQL
      "WHERE cName='%s' And cpcCalibID=cID And pcID=cpcPartialCalibID",
      "And (v.myDateTime<pcTo) And (v.myDateTime>=pcFrom)"), calName))
  sql <- paste(sql, "ORDER BY v.myDateTime")  
  
  return(sql)
}

# hsAvailCalibs ----------------------------------------------------------------
hsAvailCalibs <- function
### Return names of available calibrations according to calibration database
(
  moniPoint = NULL, 
  parAcronym = NULL,
  skipCur = FALSE,
  ### if TRUE, the name of the current specification (<parAcronym>_<moniPoint>)
  ### is excluded from the list of available calibrations
  mdbCal = NULL,
  ## Path to database containing queries that getting calibrated data according
  ## to the currently active calibration setting
  dbg = FALSE
) 
{
  if (is.null(mdbCal))
    mdbCal <- "\\\\moby/miacso$/Daten/ACCESS/KwbMonitoring/3CAL/KWB_CAL.mdb"
  
  # Get all calibration names
  cnames <- as.character(
    hsGetTable(mdbCal, "tblCalib", fields = "cName", dbg = dbg)$cName)

  if (dbg) cat("all available calibrations:\n ", 
               paste(cnames, collapse = "\n  "), "\n")
  
  ## Filter for parameter if parAcronym is given
  if (! is.null(parAcronym)) 
    cnames <- grep(sprintf("^%s_", parAcronym), cnames, value = TRUE)
  
  ## Filter for monitoring point if moniPoint is given
  if (! is.null(moniPoint)) 
    cnames <- grep(sprintf("^[^_]+_%s", moniPoint), cnames, value = TRUE) 
  
  ## Exclude the name of the "current calibration" if desired
  if (! is.null(parAcronym) && ! is.null(moniPoint) && isTRUE(skipCur))
    cnames <- setdiff(cnames, sprintf("%s_%s", parAcronym, moniPoint))
  
  ## Return character vector of (filtered) calibration names
  cnames
}

# hsPlotEventOverview ----------------------------------------------------------
hsPlotEventOverview <- function
### plot overview on events... (?)
(
  dat,
  ### data frame with at least two columns and the timestamps being in the first
  ### column
  evts = NULL,
  ### data frame with columns \emph{tBeg} and \emph{tEnd} containing first and
  ### last timestamp, respectively, of the events. If NULL (default) the
  ### events are generated by calling hsEvents (using all timestamps in dat, 
  ### this is maybe not what we want!!!)
  evtSepTime = 3600,
  myTitle = "Event Overview", 
  plotTypes = rep("l", ncol(dat) - 1),
  ### vector containing the plot type (cp. type argument of plot function)
  ### for each data column to be plotted
  dbg = TRUE, 
  ... 
  ### e.g. inset = ...
) 
{
  ## Stop if there are not at least two columns in the data frame.
  nc <- ncol(dat)  # number of columns in dat  
  if (nc < 2) stop("At least one data column required.\n")  
  
  ## If no event information has been given then get it now
  if (is.null(evts)) 
    evts <- hsEvents(dat[[1]], evtSepTime = evtSepTime, signalWidth = 60, dbg = TRUE)
  
  ne <- nrow(evts) # number of events
  
  if (dbg) cat(sprintf("\n%d events found.\n", ne))
  
  ## Prepare some nice colours
  COLS <- rainbow(nc - 1)
  
  plotsPerRow <- ceiling(sqrt(ne))
  #par(mfrow = c(hsMfRows(ne, plotsPerRow), plotsPerRow), mar = c(2,2,2,5))
  par(ask = FALSE, oma = c(1,1,1,1), mar = c(5,4,4,12), xpd = TRUE)
  
  ## Loop through events by row index in data frame <evts>
  for (i in 1:ne) {
    #evtDat <- dat[evts$iBeg[i]:evts$iEnd[i], , drop = FALSE]

    #@2012-01-20 Select rows by time interval not by index
    rowinds <- (1:nrow(dat))[(dat[[1]] >= evts$tBeg[i]) & (dat[[1]] <= evts$tEnd[i])]
    if (length(rowinds) < 1) {
      cat(sprintf("\n*** No data for event #%d (%s - %s) found.\n\n", 
          i, evts$tBeg[i], evts$tEnd[i]))
    }
    else {
      if (dbg) cat("interval from index", min(rowinds), "to index", max(rowinds), "\n")
      
      evtDat <- dat[rowinds, , drop = FALSE]
      
      if (dbg) cat(sprintf("evtDat has %d rows and %d columns\n", 
                           nrow(evtDat), ncol(evtDat)))
      
      # Get min and max in this data block over all data columns
      # By catching the case of all values being NA the warning given by min or
      # max is suppressed.
      ymin <- min(as.numeric(lapply(evtDat[, 2:nc], 
                                    function(x) {if (all(is.na(x))) Inf 
                                                 else min(x, na.rm = TRUE)})))
      ymax <- max(as.numeric(lapply(evtDat[, 2:nc], 
                                    function(x) {if (all(is.na(x))) -Inf 
                                                 else max(x, na.rm = TRUE)})))
      
      if (dbg) cat(sprintf("ymin: %0.2f, ymax: %0.2f\n", ymin, ymax))
      
      ## init vectors holding line types and plot characters for the legend
      leg.lty = c()
      leg.pch = c()
      
      ## Loop through columns 2:n
      for (j in 2:nc) {
        ptype <- plotTypes[j-1] # plot type      
        if (ptype == "l") {
          leg.lty <- c(leg.lty, 1) # 1 = solid
          leg.pch <- c(leg.pch, NA)
        }
        else {
          leg.lty <- c(leg.lty, 0) # 0 = blank
          leg.pch <- c(leg.pch, 16)
        }
        if (j == 2)
          plot(evtDat[[1]], evtDat[[j]], ylim = c(ymin, ymax),
               type = ptype,
               pch  = 16,
               col  = COLS[1],
               xlab = "date and time", 
               ylab = names(dat)[2],
               main = sprintf("%s\n%s - %s", myTitle, evts$tBeg[i], evts$tEnd[i]))
        else {
          points(evtDat[[1]], evtDat[[j]], type = ptype, pch = 16, 
                 col = COLS[j - 1])
          
        }        
      }
    }
    
    ## Add a legend; the combination "left" and inset = 1 leads to a legend
    ## directly right of the plot window
    legend("left", inset = 1, legend = names(dat)[2:nc], col = COLS, 
           lty = leg.lty, pch = leg.pch, cex = 1, ...)
  }  
}
