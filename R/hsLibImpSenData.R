# hsGetParID -------------------------------------------------------------------

#' hsGetParID
#' 
#' Lookup name of water quality parameter and return its ID or NA if
#'   the parameter is not yet contained in the data frame \code{wqpNames}
#' 
#' @param parName parameter name
#' @param wqpNames data frame describing (water quality) parameters
hsGetParID <- function(parName, wqpNames) 
{
  # Select dataset 
  ds <- wqpNames[wqpNames$wqpName == parName, ]
  
  if (nrow(ds) == 0)
    NA
  else
    ds$wqpID
}

# hsGetOrCreateParID -----------------------------------------------------------

#' hsGetOrCreateParID
#' 
#' Lookup a water quality parameter defined by (SEN-ID, name, unit) 
#' 
#' @param parInfo parameter definition
#' @param parDefs list of parameter definitions
#' @return List with two elements \emph{myParID} and \emph{parDefs}.
#'   If the parameter defined in \emph{parInfo} was found in the list of all 
#'   parameters \emph{parDefs} that have been found so far, the unique
#'   parameter ID used in \code{parDefs} is returned in \emph{myParID}. If the
#'   parameter was not found it is added to \code{parDefs} and 
#' 
hsGetOrCreateParID <- function(parInfo, parDefs) 
{
  list(myParID = 1, parDefs = parDefs)
}

# .hsCheckParlist --------------------------------------------------------------
.hsCheckParlist <- function(df)
{
  agg <- aggregate(df, by = list(df$parid), FUN = length)
  dupids <- agg[agg$parid > 1, 1]
  cat("\n*** Parameter ids with duplicates:\n")
  print(df[df$parid %in% dupids, ])
  agg <- aggregate(df, by = list(df$parid, df$parname, df$unit), 
                   FUN = length)
  dupids <- agg[agg$parid > 1, ]
  cat("\n*** Parameters with duplicate combination of id,name,unit:\n")
  print(df[df$parid %in% dupids, ])
}

# hsGsDataListToMdb ------------------------------------------------------------

#' hsGsDataListToMdb
#' 
#' write table containing all grab sample data in  "all-in-one-table"-format
#'   to \code{mdb} database
#' 
#' @param gsDataList List of grab sample data sets
#' @param mdb path to MS Access database
#' @param \dots arguments passed to hsPutTable
#' 
hsGsDataListToMdb <- function(gsDataList, mdb, ...)
{
  ## Get vector of sorted unique parameter names
  fParNames <- as.factor(gsDataList$parName)
  lParNames <- levels(fParNames)
  
  ## Fill parameterID column with indices in parNames as ID!
  gsDataList$parID <- as.integer(fParNames)
  
  ## Write parameter table
  hsPutTable(
    mdb = mdb, 
    myData = data.frame(
      wqp2ID = seq_along(lParNames),
      wqp2Name = lParNames
    ), 
    tbl = "tblWqPar",
    ...
  )
  
  ## Write value table
  selCols <- setdiff(names(gsDataList), "parName")
  
  hsPutTable(
    mdb = mdb, 
    myData = gsDataList[, selCols],    
    tbl =  "tblGrabSample", 
    ...
  )
}

# hsWriteBlockToTable ----------------------------------------------------------

#' write data block to database table
#' 
#' write data \code{block} to database table
#' 
#' @param channel database connection
#' @param block block
#' @param blockName block name
#' @param tblNamePtrn name of table to be written in database. @M is replaced with the name 
#'   of the monitoring point
#' @param boolAsk logical. Ask before tropping existing tables?
#' @param dbg If \code{TRUE}, debug messages are shown
hsWriteBlockToTable <- function(
  channel, 
  block, 
  blockName, 
  tblNamePtrn = "tblSenGrabSmp_@M",
  boolAsk,
  dbg = FALSE
  ) 
{
  ### table name pattern must contain "%M"
  if (! grepl("@M", tblNamePtrn)) {
    stop(paste("tblNamePtrn must contain \"@M\" placeholder",
               "to be replaced with name of monitoring point."))
  }
  
  # MS Access does not allow more than 255 fields per table.
  # Two fields are required for the ID (automatically created by sqlSave())
  # and the timestamp. So, 253 additional data fields are possible. As we need
  # an even number of data fields (always pairs of value/info on detection 
  # limit exceedance), the maximum number of data fields is 252.
  nMaxDataCol <- 252
  
  # Number of sub-tables to be created
  nSubTables <- as.integer( (ncol(block)-2) / nMaxDataCol) + 1
  
  for (i in seq_len(nSubTables)) {  
    
    # Generate a table name
    #tbl <- paste("tblSenGrabSmp", blockName, sep = "_")
    tbl <- sub("@M", blockName, tblNamePtrn)
    if (i > 1) {
      tbl <- paste(tbl, i, sep = "_")
    }
    
    # Drop existing table
    tbl <- hsDropExistingTable(channel, tbl, boolAsk)
    
    # Continue if the user did not cancel
    if (tbl != "") {
      cat(sprintf("Writing table %s... ", tbl))  
      
      # number of data columns
      if (nSubTables > 1 && i < nSubTables) {
        len <- nMaxDataCol
      }
      else {
        len <- ncol(block)-1 - (i-1) * nMaxDataCol
      }
      
      # Begin of column index (cib) and end of column index (cie)
      cib <- (2+(i-1) * nMaxDataCol)
      cie <- cib + len - 1
      
      # Cut sub-block out of block
      subBlock <- block[, c(1,cib:cie)]
      
      if (dbg) {
        cat(sprintf("len: %d, cib: %d, cie: %d\n", len, cib, cie))
        cat(sprintf("subBlock has %d rows and %d columns.\n", nrow(subBlock),
                    ncol(subBlock)))
      }
      
      # Create a specification vector describing the data types 
      # of the table fields
      varSpec <- c("DATETIME", rep(c("DOUBLE", "TEXT"), length.out = len))
      names(varSpec) = colnames(subBlock)
      
      # Save table
      intResult <- sqlSave(channel, subBlock, varTypes = varSpec, 
                           tablename = tbl)
      if (intResult == 1) {
        cat("ok.\n")  
      }
      else {
        cat(sprintf("\n!!! Error when writing table %s.\n", tbl))
      }
    }
    else {
      cat("Table skipped.\n")
    }
  }
}

# hsImpGsData ------------------------------------------------------------------

#' import SENATE's grab sample data from \code{csv}
#' 
#' @param csv full path to \code{csv} file
#' @param mdb full path to MS Access database (*.mdb)
#' @param sep separator in \code{csv} file
#' @param dateFormat date format in \code{csv} file (default: \%d/\%m/\%Y)
#' @param blockBeginPtrn pattern indicating the begin of a data block in the
#'   file (default: Messstelle)
#' @param tblNamePtrn table name pattern. Default: "tblSenGrabSmp_@@M" with the
#'   placeholder @@M being replaced with the acronym of the monitoring point
#' @param boolAsk logical passed to \code{\link{hsWriteBlockToTable}}
#' @param dbg If \code{TRUE}, debug messages are shown  
hsImpGsData <- function(
  csv, 
  mdb, 
  sep = ",", 
  dateFormat = underscoreToPercent("_d/_m/_Y"), 
  blockBeginPtrn = "Messstelle", 
  tblNamePtrn = "tblSenGrabSmp_@M",
  boolAsk = TRUE, 
  dbg = FALSE
) 
{ 
  ## Get list of data blocks
  res <- hsGetGsData2(csv, sep = sep, dateFormat = dateFormat,
                     blockBeginPtrn = blockBeginPtrn, dbg = dbg)  
  
  # Create a table for each block within the file...

  ## Connect to the database
  channel <- kwb.db::hsOpenDb(mdb)

  # Loop through blocks
  for (moniPoint in names(res)) {

    # Get the data block
    block <- res[[moniPoint]]

    catIf(dbg, sprintf(
      "Data block of monitoring point %s contains %d columns.\n", 
      moniPoint, ncol(block)
    ))
    
    # Write data block to table(s)
    hsWriteBlockToTable(channel, block, moniPoint, 
                        tblNamePtrn = tblNamePtrn, boolAsk, dbg)
  }

  # Close database connection
  hsCloseMdb(channel)
}

# hsGetSqls --------------------------------------------------------------------

#' hsGetSqls
#' 
#' Returns vector of SQL strings each of which selects the values of one
#'   water quality parameter (in one column) from table \dQuote{tbl} 
#'   giving general column names
#' 
#' @param mdb path to MS Access database
#' @param tbl table name
#' @param mpID monitoring point ID
#' @param belowAbove Default: FALSE
#' @param bis2007 Default: FALSE
hsGetSqls <- function(mdb, tbl, mpID, belowAbove = FALSE, bis2007 = FALSE)
{
  cat(sprintf("\n\n*** Creating SQL queries for table %s and mpID = %d...\n\n",
              tbl, mpID))
  
  # Reset vector of sql sub queries
  sqls <- NULL
  
  # Load assignment of water quality parameter IDs and names 
  wqpNames <- hsSqlQuery(mdb, "SELECT wqpID, wqpName FROM tblWQParameter")
  
  #@2012-03-19;HSB
  if (isTRUE(bis2007)) {
    ## bis2007
    wqpars <- hsFields(mdb, tbl)[-c(1:2)]     
  }
  else {
    ## ab2008
    wqpars <- hsFields(mdb, tbl)[-1]     
    if (isTRUE(belowAbove)) {
      wqpars <- wqpars[grep("_ng$", wqpars)]
    }
    else {
      wqpars <- wqpars[grep("_ng$", wqpars, invert = TRUE)]
    }
  }
  
  for (wqpar in wqpars) {
    
    #@2012-03-19;HSB
    if (isTRUE(bis2007)) {
      parid <- hsGetParID(wqpar, wqpNames)      
    }
    else {
      parid <- hsGetParID(sub("_ng$", "", wqpar), wqpNames)      
    }
    
    if (is.na(parid)) 
      stop("Could not find ID of parameter: ", wqpar)
    
    if (isTRUE(belowAbove)) {
      if (isTRUE(bis2007)) {
        cond <- "NOT isNull(%s)"
      }
      else {
        cond <- "%s <> ''"
      }
      formatstr <- paste("SELECT Datum AS gsbaDate,",
                         "%d AS gsbaMoniPointID,",
                         "%d AS gsbaWQParameterID,",
                         "%s AS gsbaBelowAbove",
                         "FROM %s", 
                         "WHERE", cond)
      sql <- sprintf(formatstr, mpID, parid, wqpar, tbl, wqpar)            
    }
    else {
      formatstr <- paste("SELECT Datum AS gsDate,",
                         "%d AS gsMoniPointID,",
                         "%d AS gsWQParameterID,",
                         "%s AS gsValue,",
                         "NULL AS gsBelowAbove",
                         "FROM %s", 
                         "WHERE NOT isNull(%s)")    
      sql <- sprintf(formatstr, mpID, parid, wqpar, tbl, wqpar)      
    }
    sqls <- c(sqls, sql)    
  }
  sqls
}
