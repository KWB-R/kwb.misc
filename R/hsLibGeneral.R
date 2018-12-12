# documentPackageFunctionDependencies ------------------------------------------
documentPackageFunctionDependencies <- structure(
  function # plot graphs showing the dependencies between package functions
  ### plot graphs showing the dependencies between package functions
  (
    packagenames, 
    ### vector of character containing the names of the packages of which
    ### functional dependencies are to be documented.
    to.pdf = FALSE
    ### if TRUE, graphical output is written to a pdf file
  )
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
  }, ex = function() {
    # get names of all installed KWB-packages
    kwb.packages <- grep("^kwb\\\\.", library()$results[, "Package"], value = TRUE)
    
    # document the first package found
    documentPackageFunctionDependencies(kwb.packages[1])      
  })

# pageAndPlot ------------------------------------------------------------------
pageAndPlot <- function # output object to plots of same row number
### capture the output of printing an object, split this output into blocks of 
### equal size (row per page) and print these blocks as plots using
### \code{\link{hsPrintToPlot}}
(
  data,
  ### data frame to plot to pdf
  rpp = 60,
  ### rows per page
  to.pdf = TRUE,
  ### if TRUE (default) the output goes into a temporary PDF file, otherwise
  ### to the standard plot device
  ...
  ### arguments to be passed to hsPrintToPlot, e.g. main, cex
)
{
  pdfFile <- preparePdfIf(to.pdf, landscape = FALSE)
  
  subsets <- splitIntoFixSizedBlocks(data, rpp)
  
  for (subset in subsets) {
    hsPrintToPlot(subset, ...)
  }
  
  finishAndShowPdfIf(to.pdf, pdfFile)
}

# hsPrintToPlot() --------------------------------------------------------------
hsPrintToPlot <- function
### prints content of an object to a plot
(
  data, 
  ### object to print
  main = "Printed by hsPrintToPlot", 
  ### plot title
  addLines = NULL,
  ### additional lines
  ...
  ### additional arguments passed to legend, e.g. cex
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
hsUnionSqls <- function
### Creates UNION-queries of given SQL queries, respecting the maximum number
### of subqueries to be used in one and the same UNION query.
(sqls, maxUnions) 
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
  ### vector of UNION queries
}

# hsDropExistingTable ----------------------------------------------------------
hsDropExistingTable <- function
### drop an existing table (user interaction)
(channel, strTable, boolAsk = TRUE) {
  
  newName <- strTable
  
  # Does the database contain a table of that name?
  
  #@2011-12-19: use %in% instead of hsContains!
  #if (hsContains(sqlTables(channel)$TABLE_NAME, strTable)) {
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
  ### table name of created table. 
}
