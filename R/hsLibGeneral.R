# documentPackageFunctionDependencies ------------------------------------------

#' plot graphs showing the dependencies between package functions
#' 
#' @param packagenames vector of character containing the names of the packages of which
#'   functional dependencies are to be documented.
#' @param to.pdf if TRUE, graphical output is written to a pdf file
#' 
#' @examples 
#' # Show names of all installed KWB-packages
#' grep("^kwb\\.", library()$results[, "Package"], value = TRUE)
#'   
#' # Document one of the installed packages
#' \dontrun{
#' documentPackageFunctionDependencies("kwb.plot")
#' }
documentPackageFunctionDependencies <- function(packagenames, to.pdf = FALSE)
{  
  for(packagename in packagenames) {
    
    print(packagename)
    
    library(packagename, character.only = TRUE)
    
    packageExpression <- paste0("package:", packagename)
    
    file.pdf <- preparePdfIf(
      to.pdf, file.path(tempdir(), paste0(packagename, ".pdf"))
    )
    
    foodweb(where = packageExpression, cex = 0.7)
    
    mtext(packageExpression, line = 2)
    
    for (functionname in sort(ls(packageExpression))) {
      
      foodweb(where = packageExpression, prune = functionname)
      
      mtext(paste0(packageExpression, "::", functionname), line = 2)
    }
    
    finishAndShowPdfIf(to.pdf, file.pdf)
  }  
}

# pageAndPlot ------------------------------------------------------------------

#' output object to plots of same row number
#' 
#' capture the output of printing an object, split this output into blocks of 
#'   equal size (row per page) and print these blocks as plots using
#'   \code{\link{hsPrintToPlot}}
#' 
#' @param data \code{data} frame to plot to pdf
#' @param rpp rows per page
#' @param to.pdf if TRUE (default) the output goes into a temporary PDF file, otherwise
#'   to the standard plot device
#' @param \dots arguments to be passed to \code{\link{hsPrintToPlot}}, e.g. main, cex
#' 
pageAndPlot <- function(data, rpp = 60, to.pdf = TRUE, ...)
{
  pdfFile <- preparePdfIf(to.pdf, landscape = FALSE)
  
  subsets <- splitIntoFixSizedBlocks(data, rpp)
  
  for (subset in subsets) {
    hsPrintToPlot(subset, ...)
  }
  
  finishAndShowPdfIf(to.pdf, pdfFile)
}

# hsPrintToPlot ----------------------------------------------------------------

#' hsPrintToPlot
#' 
#' prints content of an object to a plot
#' 
#' @param data object to print
#' @param main plot title
#' @param addLines additional lines
#' @param \dots additional arguments passed to legend, e.g. cex
#' 
hsPrintToPlot <- function
(
  data, 
  main = "Printed by hsPrintToPlot", 
  addLines = NULL,
  ...
) 
{
  .hsReset <- function(oopts, opar) {
    options(oopts)  
    par(opar)
  }
  
  ## modify options and graphical parameters and reset on exit
  ## - prevent line breaks in following print
  oopts <- options()
  options(width = 1000)
  
  ## - use Courier-type font
  opar <- par(family = "mono")
  on.exit(.hsReset(oopts, opar))
  
  ## get lines as if they were printed to the console
  msgLines <- capture.output(print(data))
  msgLines <- c(msgLines, addLines)
  
  ## prepare an empty plot
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = main)    
  
  ## Add legend containing the text
  legend("top", msgLines, ...)      
}

# hsUnionSqls ------------------------------------------------------------------

#' hsUnionSqls
#' 
#' Creates UNION-queries of given SQL queries, respecting the maximum number
#'   of subqueries to be used in one and the same UNION query.
#' 
#' @param sqls vector of SQL statements
#' @param maxUnions number of maximum UNIONs allowed
#' @return vector of UNION queries
#' 
hsUnionSqls <- function(sqls, maxUnions) 
{
  # number of queries 
  n <- length(sqls)
  
  # Prepare result vector containing UNION queries
  usqls <- NULL
  
  # Get maximum number of SQL-block
  maxi <- as.integer((n-1)/maxUnions)
  
  # Loop through blocks of queries
  for (i in 0:maxi) {
    
    iFrom <- (i*maxUnions + 1)
    iTo <- ifelse(i == maxi, n, (i+1)*maxUnions)
    #cat(sprintf("maxi: %d, i: %d, iFrom: %d, iTo: %d\n", maxi, i, iFrom, iTo))
    
    # Create UNION query out of single queries in group
    sql <- paste(sqls[iFrom:iTo], collapse = " UNION ")
    
    # Append sql to result vector
    usqls <- c(usqls, sql)
  }
  
  usqls
}

# hsDropExistingTable ----------------------------------------------------------

#' Drop an existing table (user interaction)
#' 
#' @param channel database connection
#' @param strTable table name
#' @param boolAsk logical indication whether the user should be asked before 
#'   dropping the table
#' @return table name of created table. 
#' 
hsDropExistingTable <- function(channel, strTable, boolAsk = TRUE)
{
  newName <- strTable
  
  # Does the database contain a table of that name?
  if (strTable %in% sqlTables(channel)$TABLE_NAME) {
    
    # Shall the user decide?
    if (boolAsk) {
      
      # Prepare message
      strMsg <- paste("Table ", strTable, " exists! ",
                      "Ok: overwrite; new name and Ok: new table;", 
                      "Cancel: skip this block.", sep = "")
      
      # Ask the user to confirm or enter a new name
      newName <- winDialogString(strMsg, strTable)
      if (is.null(newName)) {
        newName <- ""
      }
    }
    
    # Delete the table if the user did not change the table name
    if (newName == strTable) {
      cat(paste("Dropping table", strTable, "... "))
      strMsg <- sqlDrop(channel, strTable)
      if (!is.null(strMsg)) {
        cat(sprintf("Could not drop table %s: %s", strTable, strMsg))
        newName <- ""
      }
      else {
        cat("ok.\n")
      }
    }
  }
  
  newName
}
