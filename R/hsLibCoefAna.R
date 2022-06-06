# hsExampleCoefData ------------------------------------------------------------

#' hsExampleCoefData
#' 
#' provide example dataset for coefficient analysis with \code{\link{hsCoefAna}}
#' 
#' @param nevts number of gaps to be produced in continuous examle data in order to split
#'   events. default: 5
#' @param step timestep in seconds. default: 30
#' @param ex.avail example data available? default: FALSE
#' @param dev deviation. default: 0.01
#' @param dbg If \code{TRUE}, debug messages are shown  
hsExampleCoefData <- function
(
  nevts = 5, 
  step = 30,
  ex.avail = FALSE,
  dev = 0.01,
  dbg = FALSE
) 
{  
  ## Generate example data
  if (ex.avail) {
    tsx <- hsExampleTSeries(step = step)
    smpcands <- seq(3, nrow(tsx) - 2, by = 3)
    smp <- sort(sample(smpcands, nevts - 1, FALSE))
    cat("smpcands:", paste(smpcands), "smp:", paste(smp), "\n")
    dat <- tsx[-smp, ]
    names(dat) <- c("tstamp", "pval")
    dat$lval <- dat$pval + rnorm(nrow(dat), mean = dat$pval)    
  
    ## Append event information
    evts <- hsEvents(dat$t, evtSepTime = step, signalWidth = step)

    if (dbg) {
      cat("Events:\n")
      print(evts)      
    }
    
    dat$evtID <- hsEventNumber(dat$t, evts)
  }
  else {
    dat <- data.frame(tstamp = seq(as.POSIXct("2010-01-01"), 
                                   as.POSIXct("2010-01-01 08:00:00"), by = step))
    nr <- nrow(dat)
    dat$pval <- rnorm(nr)
    dat$lval <- dat$pval * (1 + dev) * rnorm(nr)
    
    ## Append event information
    dat$evtID <- sort(rep(1:nevts, length.out = nr))      
  }

  if (dbg) {
    cat("Data set:\n")
    print(dat)    
  }
  
  dat
}

# hsEqualUntilPos --------------------------------------------------------------

#' hsEqualUntilPos
#' 
#' returns the first index at which elements in \code{combi1} and \code{combi2} differ
#' 
#' @param combi1 vector 
#' @param combi2 vector of same mode as \code{combi1}
#' 
hsEqualUntilPos <- function(combi1, combi2) 
{
  ### Index until which elements in combi1 and combi2 are the same
  ifelse(combi1[1] != combi2[1], 0, hsEventsOnChange(combi1 == combi2)$iEnd[1])
}

# hsAllCombis ------------------------------------------------------------------

#' hsAllCombis
#' 
#' Generate all possible combinations of elements in \code{x} with order mattering
#' 
#' @param x vector of elements of which to create combinations
hsAllCombis <- function(x) 
{  
  ## stop condition
  if (length(x) == 1) {
    return(x)
  }
  res <- NULL
  for (e in x) {
    res <- rbind(res, cbind(e, hsAllCombis(setdiff(x, e))))
  }
  colnames(res) <- paste("pos", 1:ncol(res), sep = "")
  rownames(res) <- paste("combi", 1:nrow(res), sep = "")
  res
}

# hsNextCoefAnaCombi -----------------------------------------------------------

#' hsNextCoefAnaCombi
#' 
#' given a current combination the next combination of events as used for 
#'   coefficient analysis is provided
#' 
#' @param combi current combination of which the "successor" combination is to be 
#'   provided
#' @param n number of elements in the combination (= length of combination)
#' 
hsNextCoefAnaCombi <- function(combi, n)
{
  nextCombi <- NULL
  l <- length(combi)
  
  ## the very first combination is 1
  if (l == 0) {
    nextCombi <- 1  
  } 
  ## if all elements in the current combination are in sorted order and 
  ## if the combination has not yet the maximum length append the smallest
  ## element that is not yet contained in the current combination
  else if (l < n && all(c(combi, NA) > c(NA, combi), na.rm = TRUE)) {
    nextCombi <- c(combi, setdiff(1:n, combi)[1])
  } 
  else {
    
    ## start at the position (p) of the last element of the current combination
    p <- l
    cand <- NULL
    
    ## look for the position p at which the combination has to change its number
    ## and for the possible numbers (candidates cand) to be placed at this
    ## position 
    while (p >= 1 && length(cand) == 0) {
      cand <- 1:n
      
      ## numbers at previous positions are not allowed as candidates
      if (p > 1) {
        cand <- setdiff(cand, combi[1:(p-1)])
      }
      
      ## number must be greater than the number at current position in the
      ## current combination to keep the sorted order of elements
      cand <- cand[cand > combi[p]]
      p <- p - 1
    }
    
    ## if we found candidate elements update the current combination at the 
    ## position for which the candidates were found with the first candidate
    if (length(cand) > 0) {
      combi[p + 1] <- cand[1]
      nextCombi <- combi[1:(p+1)]
    } 
  }
  return(nextCombi)
}

# hsAllCoefAnaCombis -----------------------------------------------------------

#' hsAllCoefAnaCombis
#' 
#' Generates all possible combinations of events as used for coefficient
#'   analysis
#' 
#' @param n number of elements to be combined
#' @param dbg.level if not 0, combinations are shown when dbg.level-th element just changed
#' 
hsAllCoefAnaCombis <- function(n, dbg.level = n)
{
  combi <- 1
  combis <- list()
  i <- 1
  while(!is.null(combi)) {
    combis[[i]] <- combi
    if (dbg.level > 0 && length(combi) <= dbg.level) {
      cat("Combi", i, ":", paste(combi, collapse = ","), "\n")
    }
    combi <- hsNextCoefAnaCombi(combi, n)
    i <- i + 1
  }
  combis  
}

# hsCombiLinReg ----------------------------------------------------------------

#' Linear Regression for Event Combination
#' 
#' Calculation of linear regressions for given combination of events
#' 
#' @param data \code{data} frame containing columns \emph{tstamp} (time stamp), \emph{pval}
#'   (probe value), \emph{lval} (lab value), \emph{evtID} (event ID)
#' @param combi combination of events for which linear regressions are to be calculated
#'   in the following way: the first event numbers in \code{combi}, at positions 
#'   1:(length(\code{combi}) - 1), are considered to be "base" events, i.e. events
#'   of which all \code{data} points are considered for the linear regression.  
#'   The \code{data} points belonging to the event given at the last position
#'   of \code{combi} are added "point by point" to these "base points" and each time
#'   a separate regression is calculated
#' @param uselm if TRUE, the lm function is used to calculate the linear regression,
#'   otherwise (\code{uselm} == FALSE) the regression is calculated "manually" 
#'   which is much faster. default: FALSE
#' @param clever if TRUE, sums and means are updated by knowledge of previous values with
#'   the current \code{data} point, otherwise they are always recalculated for all
#'   datapoints to be considered
#' @param prep if TRUE, \code{data} is expected to contain columns \emph{x2} (squares of x),
#'   \emph{xy} (product of x and y values). Unfortunately, this does not give
#'   a better performance...
#' @param calc.rmse if TRUE, the root mean square error (RMSE) is calculated
#' @param dbg If \code{TRUE}, debug messages are shown  
hsCombiLinReg <- function(
  data,
  combi,
  uselm = FALSE,
  clever = FALSE,
  prep = FALSE,
  calc.rmse = TRUE,
  dbg = FALSE  
) 
{
  if (dbg) {
    cat(sprintf("in hsCombiLinReg(data, c(%s), uselm=%s, clever=%s, prep=%s)...\n",
                paste(combi, collapse = ","), 
                as.character(uselm), as.character(clever),
                as.character(prep)))
  }
  lp <- length(combi)

  # indices (IDs) of points belonging to the "path of events", 
  # excluding the last event (which is considered "point by point")
  ids <- which(data$evtID %in% combi[-lp])
  
  # indices (IDs) of points belonging to the last event in the "path of events". 
  # This event is considered "point by point".
  ids.new <- which(data$evtID == combi[lp])
  
  # for each new point, calculate the linear regression considering all the
  # old points and the growing set of new points
  ab <- NULL
  
  # loop through points of last event in the "path of events"
  for (id.new in ids.new) {
    ids <- c(ids, id.new)
    
    # Calculate coefficients of linar regression
    if (uselm) {
      #cat("lm ")
      
      lmres <- lm(lval ~ pval, data = data, subset = ids, qr = FALSE, model = FALSE)
      coefs <- coef(lmres)      
      slope  <- as.numeric(coefs[2])
      offset <- as.numeric(coefs[1])
      
      if (calc.rmse) {
        tmp <- lmres$residuals
        n <- length(tmp)
        if (n > 2) {
          rmse <- sqrt(sum(tmp*tmp)/(n-2))
        } else {
          rmse <- NA
        }
      }
    } 
    else {
      #cat("man ")
      if (clever) {
        first <- id.new == ids.new[1]
        if (first) {
          n <- length(ids)
          xnews <- data$pval[ids]
          ynews <- data$lval[ids]
          if (prep) {
            x2news <- data$x2[ids]
            xynews <- data$xy[ids]            
          }
          sx <- sum(xnews)
          sy <- sum(ynews)
        }
        else {
          n <- n + 1
          xnew <- data$pval[id.new]
          ynew <- data$lval[id.new]
          if (prep) {
            x2new <- data$x2[id.new]
            xynew <- data$xy[id.new]            
          }
          sx <- sx + xnew
          sy <- sy + ynew
        }
        mx <- sx / n # mean(x)
        my <- sy / n # mean(y)
        if (first) {
          if (prep) {
            sxxs <- sum(x2news)
            sxys <- sum(xynews)                      
          }
          else {
            sxxs <- sum(xnews * xnews)
            sxys <- sum(xnews * ynews)            
          }
        }
        else {
          if (prep) {
            sxxs <- sxxs + x2new
            sxys <- sxys + xynew            
          }
          else {
            sxxs <- sxxs + xnew * xnew
            sxys <- sxys + xnew * ynew            
          }
        }
        sxx <- sxxs - n * mx * mx
        sxy <- sxys - n * mx * my        
      }
      else {
        x <- data[ids, "pval"]
        y <- data[ids, "lval"]      
        mx <- mean(x)
        my <- mean(y)
        xmmx <- x - mx
        sxx <- sum(xmmx * xmmx)
        sxy <- sum(xmmx * (y - my))
      }
      #cat(sprintf("id.new: %d, mx=%f, my=%f, sxx=%f, sxy=%f\n",
      #            id.new, mx, my, sxx, sxy))
      if (sxx != 0) {
        slope  <- sxy / sxx
        offset <- my - slope * mx
      } else {
        slope <- NA
        offset <- my
      }
      
      # if requested and not clever, calculate RMSE
      if (calc.rmse && !clever) {        
        valPred <- (slope* x + offset)
        rsd <- valPred - y
        SQrsd <- rsd^2
        SUMSQrsd <- sum(SQrsd)
        n <- length(x)
        if (n > 2) {
          rmse <- sqrt((SUMSQrsd / n-2))
        } else {
          rmse <- NA
        }        
      }      
    }    
    
    ab <- rbind(ab, data.frame(np     = length(ids), 
                               offset = offset,
                               slope  = slope,
                               rmse   = rmse))
  }    
  ab
}

# hsCoefAna --------------------------------------------------------------------

#' hsCoefAna
#' 
#' regression coefficient analysis
#' 
#' @param data \code{data} frame containing columns \emph{tstamp} (time stamp), \emph{pval}
#'   (probe value), \emph{lval} (lab value), \emph{evtID} (event ID)
#' @param recursive if TRUE, the \code{recursive} version hsCoefAnaRes of the regression coefficient 
#'   analysis is used, otherwise the non-recursive version. default: TRUE
#' @param evtNums event numbers to be considered for the analysis. Only considered when
#'   \emph{recursive} == TRUE. default: all distinct values provided in 
#'   column \emph{evtID} of \emph{data})
#' @param aslist default: boolean value given in \emph{recursive}
#' @param uselm if TRUE, the lm function is used to calculate the linear regression,
#'   otherwise (\code{uselm} == FALSE) the regression is calculated "manually" 
#'   which is much faster. Default: FALSE
#' @param prep Default: FALSE
#' @param \dots further arguments passed to \code{\link{hsCombiLinReg}}, e.g. \emph{clever}
#' @param dbg.level default: max(2, length(\code{evtNums}) - 8)
#' 
hsCoefAna <- function(
  data, 
  recursive = TRUE,
  evtNums = unique(data$evtID), 
  aslist = recursive,
  uselm = FALSE,
  prep = FALSE,
  ...,
  dbg.level = max(2, length(evtNums) - 8)
) 
{
  ## prepare columns containing precalculated products
  if (prep) {
    data$x2 <- data$pval * data$pval
    data$xy <- data$pval * data$lval
  }
  if (recursive) {
    hsCoefAnaRec(data      = data, 
                 evtNums   = evtNums, 
                 combi     = NULL, 
                 tree      = aslist, 
                 resframe  = NULL, 
                 uselm     = uselm, 
                 prep      = prep,
                 ...,
                 dbg.level = dbg.level)
  }
  else {
    hsCoefAnaNonRec(data      = data, 
                    uselm     = uselm,
                    aslist    = aslist,
                    prep      = prep,
                    ...,
                    dbg.level = dbg.level)
  }
}

# hsCoefAnaRec -----------------------------------------------------------------

#' hsCoefAnaRec
#' 
#' recursive version of regression coefficient analysis
#' 
#' @param data \code{data} frame containing columns \emph{tstamp} (time stamp), \emph{pval}
#'   (probe value), \emph{lval} (lab value), \emph{evtID} (event ID)
#' @param evtNums event numbers to be considered for the analysis (default: all distinct
#'   values provided in column \emph{evtID} of \emph{data})
#' @param combi current combination to be evaluated and to be the base for the next 
#'   combinations to be determined
#' @param tree if TRUE, result is given in a \code{tree} structure, otherwise as a \code{data} frame
#' @param resframe if \emph{tree} is FALSE, this argument contains the results that have
#'   been found so far in a \code{data} frame
#' @param uselm if TRUE, the lm function is used to calculate the linear regression,
#'   otherwise (\code{uselm} == FALSE) the regression is calculated "manually" 
#'   which is much faster. default: FALSE
#' @param \dots further arguments passed to \code{\link{hsCombiLinReg}}, e.g. \emph{clever}
#' @param dbg.level debug level
#' @return Recursive list representing a \code{tree} structure. At the top level the list
#'   contains elements \emph{e<i>} where <i> are the event IDs to be considered 
#'   (elements in \emph{evtNums}).
#'   The sub lists below the top level (but not the "leafs" of the \code{tree}) also 
#'   contain elements \emph{e<j>} where <j> are the "remaining" event IDs, 
#'   i.e. the IDs that do not yet occur in the "path" of event IDs
#'   leading to the respective sub \code{tree}. These sub lists also have elements
#'   \emph{combi} (vector of event IDs representing the respective event 
#'   combination) and \emph{linreg} containing the results from linear 
#'   regression. In fact, \emph{linreg} is a \code{data} frame with each line 
#'   representing the \emph{slope} and \emph{offset} of the linear regression 
#'   through \emph{np} number of points, taken from the events in \emph{combi}.
#' 
hsCoefAnaRec <- function
(
  data, 
  evtNums = unique(data$evtID), 
  combi = NULL, 
  tree = TRUE,
  resframe = NULL,
  uselm = FALSE,
  ...,
  dbg.level = 1
) 
{  
  # prepare result list
  if (tree) {
    restree <- list()
  } 
  else {
    # set option "stringsAsFactors" to FALSE
    strAsFac <- getOption("stringsAsFactors")
    options(stringsAsFactors = FALSE)
  }
  
  # length of current "path of events"
  lp <- length(combi)
  
  # if a "path of events" is given calculate regressions for each point of the
  # last event in the path, with all points of all other events in the path
  # being the base set of points
  if (lp > 0) {
    
    if (lp <= dbg.level) {
      cat("path: ", paste(combi, collapse = ", "), "\n")
    }
    
    # Calculate coefficients of linear regression
    ab <- hsCombiLinReg(data, combi, uselm = uselm, ...)    
    
    if (tree) {
      # add information on the combination of event numbers to the
      # result list
      restree$combi <- combi
      
      # save the result data frame to element "linreg" of the result list
      restree$linreg <- ab          
    } 
    else {
      ab$combi <- paste(combi, collapse = ",")
      resframe <- rbind(resframe, ab)
    }
    
    # if the result list of combinations does not yet contain combinations of
    # length lc init the lc-th element of the result list with the current
    # combination, otherwise add the current combination to the existing 
    # combinations of length lc.
  }
  
  # Loop through event numbers; if evtNums is empty the loop is never executed
  for (evtNum in evtNums) {
    
    # check here if the new combination exists        
    if (is.null(combi) 
        || all(c(combi, NA) > c(NA, combi), na.rm = TRUE)) {
      
      # Call hsCoefAna recursively with evtNums reduced by the current event
      # number in evtNum
      resframe <- hsCoefAnaRec(data      = data, 
                               evtNums   = setdiff(evtNums, evtNum), 
                               combi     = c(combi, evtNum), 
                               tree      = tree,
                               resframe  = resframe,
                               uselm     = uselm,
                               ...,
                               dbg.level = dbg.level)
      if (tree) {
        restree[[paste("e", evtNum, sep = "")]] <- resframe
      } 
    } 
  }    
  
  if (tree) {
    restree    
  } 
  else {
    # reset option "stringsAsFactors"
    options(stringsAsFactors = strAsFac)
    resframe
  }
}

# hsCoefAnaNonRec --------------------------------------------------------------

#' hsCoefAnaNonRec
#' 
#' non-recursive version of regression coefficient analysis
#' 
#' @param data \code{data} frame containing columns \emph{tstamp} (time stamp), \emph{pval}
#'   (probe value), \emph{lval} (lab value), \emph{evtID} (event ID)
#' @param uselm if TRUE, the lm function is used to calculate the linear regression,
#'   otherwise (\code{uselm} == FALSE) the regression is calculated "manually" 
#'   which is much faster. default: FALSE
#' @param aslist if TRUE the result is retunred in forms of a list with each list element
#'   representing one combination. otherwise in forms of a database with
#'   columns \emph{np} (number of points), \emph{offset}, \emph{slope},
#'   \emph{combi}. Default: FALSE.
#' @param \dots further arguments passed to \code{\link{hsCombiLinReg}}, e.g. \emph{clever}
#' @param dbg.level debug level
hsCoefAnaNonRec <- function(
  data, uselm = FALSE, aslist = FALSE, ..., dbg.level = 1
)
{  
  # set option "stringsAsFactors" to FALSE
  strAsFac <- getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)    

  evtNums = unique(data$evtID)
  
  # prepare result list/data frame
  result <- NULL
  
  # Generate first combination
  combi <- 1
  
  while (! is.null(combi)) {
    
    combitxt <- paste(combi, collapse = ",")
      
    if (length(combi) <= dbg.level) {
      cat("combi: ", combitxt, "\n")
    }

    # Calculate linear regression
    ab <- hsCombiLinReg(data, combi, uselm = uselm, ...)    
    
    if (aslist) {
      result[[combitxt]] <- ab
    }
    else {
      ab$combi <- combitxt
      result <- rbind(result, ab)
    }
    
    # Get next combination
    combi <- hsNextCoefAnaCombi(combi, length(evtNums))
  }

  # reset option "stringsAsFactors"
  options(stringsAsFactors = strAsFac)    
  
  result
}

# hsPlotCoefAnaRes -------------------------------------------------------------

#' hsPlotCoefAnaRes
#' 
#' Plot function to visualise the regression lines calculated by \code{\link{hsCoefAna}}.
#' 
#' @param data \code{data} frame containing columns \emph{tstamp} (time stamp), \emph{pval}
#'   (probe value), \emph{lval} (lab value), \emph{evtID} (event ID)
#' @param res result tree as returned by \code{\link{hsCoefAna}}.
#' @param recursive Default: FALSE
hsPlotCoefAnaRes <- function(data, res, recursive = FALSE)
{
  col.all  <- "lightgrey"
  col.base <- "darkgrey"
  
  if ("combi" %in% names(res)) {
    # plot all points in grey
    plot(data$pval, 
         data$lval,
         xlab = "probe value", 
         ylab = "lab value",
         main = sprintf("Event combination: %s", paste(res$combi, collapse = ", ")),
         col = col.all)
    
    # number of events in event combination
    lc <- length(res$combi)
    
    # Add points belonging to event combination without last event (= base events)
    ids <- which(data$evtID %in% res$combi[-lc])  
    if (length(ids) > 0) {
      points(data$pval[ids], 
             data$lval[ids], 
             pch = 16,
             col = col.base)
    }  
    
    # Add points belonging to last event in separate colours
    ids <- which(data$evtID == res$combi[lc])
    rbcols <- rainbow(length(ids))
    points(data$pval[ids], 
           data$lval[ids], 
           pch = 16,
           col = rbcols)
    
    # Add lines representing linear regressions
    sapply(seq(1, nrow(res$linreg)), FUN = function(i) {
      abline(a = res$linreg$offset[i], 
             b = ifelse(is.na(res$linreg$slope[i]), 0, res$linreg$slope[i]),
             col = rbcols[i])})
    
    legend("topleft", cex = 0.8, 
           pch = c(1, 16, rep(16, length(rbcols))), 
           col = c(col.all, col.base, rbcols), 
           legend = c("all events", 
                      sprintf("base events (%s)", 
                              paste("#", res$combi[-lc], 
                                    sep = "", collapse = ", ")), 
                      sprintf("p%d of new event (#%d)", 
                              seq_along(rbcols), 
                              res$combi[lc])))    
  }
  
  # if requested, call function recursively
  if (recursive) {
    for (element in grep("^e", names(res), value = TRUE)) {
      hsPlotCoefAnaRes(data, res[[element]], recursive = recursive)
    }
  }
}

# hsPlotCoefAnaRes2 ------------------------------------------------------------

#' hsPlotCoefAnaRes2
#' 
#' Plot function to visualise the distribution of slopes and offsets of 
#'   regression lines through possible combinations of events.
#' 
#' @param data \code{data} frame containing columns \emph{tstamp} (time stamp), \emph{pval}
#'   (probe value), \emph{lval} (lab value), \emph{evtID} (event ID)
#' @param res result tree as returned by \code{\link{hsCoefAna}}.
#' @param olim limits of offset values to be used for plotting the offsets
#' @param slim limits of slope values to be used for plotting the offsets
#' 
hsPlotCoefAnaRes2 <- function(data, res, olim = NULL, slim = NULL)
{
  opar <- par(mfcol = c(2, 1))
  
  # number of events
  ne <- length(unique(data$evtID))
  
  for (combilen in c(seq(1, ne), -1)) {
    xy <- hsBrowseCoefAnaRes(res, combilen = combilen)
    
    plot(xy$np, 
         xy$slope, 
         ylim = slim,
         xlab = "number of samples", ylab = "slope",
         main = ifelse(combilen == -1, 
                       "All event combinations considered",
                       sprintf("Number of considered events: %d", combilen)))
    plot(xy$np, 
         xy$offset,
         ylim = olim,
         xlab = "number of samples", ylab = "offset")    
  }
  
  par(opar)
}

# hsBrowseCoefAnaRes -----------------------------------------------------------

#' hsBrowseCoefAnaRes
#' 
#' Browse through result \code{tree} of regression coefficient analysis and "rbind"
#'   data frames \emph{linreg}
#' 
#' @param tree list representing a \code{tree} structure as returned by \code{\link{hsCoefAna}}
#' @param combilen length of combinations. Default: -1
hsBrowseCoefAnaRes <- function(tree, combilen = -1)
{
  if (combilen == -1) {
    # set option "stringsAsFactors" to FALSE
    strAsFac <- getOption("stringsAsFactors")
    options(stringsAsFactors = FALSE)    
  }
  res <- NULL
  if (combilen == -1 || length(tree$combi) == combilen) {    
    if (!is.null(tree$combi)) {
      res <- cbind(tree$linreg, 
                   combi = paste(tree$combi, collapse = ","))
    }
  }
  if (combilen == -1 || length(tree$combi) < combilen) {
    for (element in grep("^e", names(tree), value = TRUE)) {
      res <- rbind(res, hsBrowseCoefAnaRes(tree[[element]], combilen = combilen))
    }    
  }
  if (combilen == -1) {
    # reset option "stringsAsFactors"
    options(stringsAsFactors = strAsFac)    
  }
  res
}

# hsBrowseCoefAnaResList -------------------------------------------------------

#' hsBrowseCoefAnaResList
#' 
#' Browse through result tree of regression coefficient analysis and "rbind"
#'   data frames \emph{linreg}
#' 
#' @param reslist result list as returned by hsCoefAna(..., recursive = FALSE, aslist = TRUE))
#' @param dbg.level debug level
hsBrowseCoefAnaResList <- function(reslist, dbg.level = 10) 
{ 
  # set option "stringsAsFactors" to FALSE
  strAsFac <- getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)    
  
  res <- NULL
  ll <- length(reslist)  
  dbg.div <- as.integer(ll/ifelse(dbg.level == 0, 1, dbg.level))
  i <- 0
  for (ename in names(reslist)) {
    i <- i + 1
    if (dbg.level != 0 && (i == 1 || i == ll || i %% dbg.div == 0)) {
      cat(sprintf("  %3.0f %%: combi = %s\n", 100*i/ll, ename))
    }
    res <- rbind(res, cbind(reslist[[i]], combi = ename))
  }

  # reset option "stringsAsFactors"
  options(stringsAsFactors = strAsFac)    
  
  res
}

# hsBrowseCombis ---------------------------------------------------------------

#' Browse Combinations
#' 
#' browses through result \code{tree} and collects all combinations
#' 
#' @param tree list representing a \code{tree} structure as returned by \code{\link{hsCoefAna}}
#' @param combis List of combinations. Default: \code{list()}
#' @return list with first element containing matrix of combinations of length 1, 
#'   second element containing matrix of combinations of length 2, and so on.
#' 
hsBrowseCombis <- function(tree, combis = list())
{
  # if the root of the current (sub-)tree contains an element "combi" add this
  # combination to the result list of combinations
  if (! is.null(tree$combi)) {
    
    # if the result list of combinations does not yet contain combinations of
    # length lc init the lc-th element of the result list with the current
    # combination, otherwise add the current combination to the existing 
    # combinations of length lc.
    if (length(combis) < (lc <- length(tree$combi))) {
      combis[[lc]] <- matrix(tree$combi, nrow = 1, byrow = TRUE)
    } else {
      combis[[lc]] <- rbind(combis[[lc]], tree$combi)      
    }
  }
  # Loop through names of list elements starting with "e"
  for (ele in grep("^e", names(tree), value = TRUE)) {
    combis <- hsBrowseCombis(tree[[ele]], combis)
  }
  combis
}
