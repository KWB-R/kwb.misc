#===============================================================================
#
# Functions related to grab samples of water quality parameters ====
#
#===============================================================================

#===============================================================================
#
# Reading grab sample data from csv file(s) ====
#
#===============================================================================

# hsGetParID -------------------------------------------------------------------
hsGetParID <- function
### Lookup name of water quality parameter and return its ID or NA if
### the parameter is not yet contained in the data frame wqpNames
(parName, wqpNames) 
{
  
  # Select dataset 
  ds <- wqpNames[wqpNames$wqpName == parName, ]
  if (nrow(ds) == 0)
    return(NA)
  else
    return(ds$wqpID)
}

# hsGetOrCreateParID -----------------------------------------------------------
hsGetOrCreateParID <- function
### Lookup a water quality parameter defined by (SEN-ID, name, unit) 
(
  parInfo, 
  parDefs
) 
{
  return(list(myParID = 1, parDefs = parDefs))
  ### List with two elements \emph{myParID} and \emph{parDefs}.
  ### If the parameter defined in \emph{parInfo} was found in the list of all 
  ### parameters \emph{parDefs} that have been found so far, the unique
  ### parameter ID used in parDefs is returned in \emph{myParID}. If the
  ### parameter was not found it is added to parDefs and 
}

# .hsCheckParlist --------------------------------------------------------------
.hsCheckParlist <- function
### check parameter information for ambiguities
(df)
{
  ## parameters with more than one kinds
  agg <- aggregate(df, by = list(df$parid), FUN = length)
  dupids <- agg[agg$parid > 1, 1]
  
  ## Find corresponding names by id
  cat("\n*** Parameter ids with duplicates:\n")  
  print(df[df$parid %in% dupids, ])
  
  ## Is the combination of parid, parname and unit unambiguous? 
  agg <- aggregate(df, by = list(df$parid, df$parname, df$unit), FUN = length)
  dupids <- agg[agg$parid > 1, ]
  
  cat("\n*** Parameters with duplicate combination of id,name,unit:\n")
  print(df[df$parid %in% dupids, ])
  
}

#===============================================================================
##
## Transforming grab sample data within R objects ====
##
#===============================================================================

#===============================================================================
##
## Writing grab sample data to database ====
##
#===============================================================================

# hsGsDataListToMdb ------------------------------------------------------------
hsGsDataListToMdb <- function
### write table containing all grab sample data in  "all-in-one-table"-format
### to mdb database
(
  gsDataList, 
  mdb,
  ...
  ### arguments passed to hsPutTable
)
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
      wqp2ID = seq_len(length(lParNames)),
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
hsWriteBlockToTable <- function # write data block to database table
### write data block to database table
(
  channel, 
  block, 
  blockName, 
  tblNamePtrn = "tblSenGrabSmp_@M",
  ### name of table to be written in database. @M is replaced with the name 
  ### of the monitoring point
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
  
  for (i in 1:nSubTables) {  
    
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

#===============================================================================
##
## Reading grab sample data from csv file(s) and writing to database ====
##
#===============================================================================

# hsImpGsData ------------------------------------------------------------------
hsImpGsData <- function # import SENATE's grab sample data from csv
### import SENATE's grab sample data from csv
(
  csv, 
  ### full path to csv file
  mdb, 
  ### full path to MS Access database (*.mdb)
  sep = ",", 
  ### separator in csv file
  dateFormat = underscoreToPercent("_d/_m/_Y"), 
  ### date format in csv file (default: %d/%m/%Y)
  blockBeginPtrn = "Messstelle", 
  ### pattern indicating the begin of a data block in the file 
  ### (default: Messstelle)
  tblNamePtrn = "tblSenGrabSmp_@M", # @M will be replaced with monitoring point
  boolAsk = TRUE, 
  dbg = FALSE
) 
{ 
  ## Get list of data blocks
  res <- hsGetGsData2(csv, sep = sep, dateFormat = dateFormat,
                     blockBeginPtrn = blockBeginPtrn, dbg = dbg)  
  
  # Create a table for each block within the file...

  ## Connect to the database
  channel <- hsOpenMdb(mdb)

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

#===============================================================================
##
## Functions for reorganizing grab sample data within existing database ====
##
#===============================================================================

# hsGetSqls --------------------------------------------------------------------
hsGetSqls <- function(mdb, tbl, mpID, belowAbove = FALSE, bis2007 = FALSE) 
  ### Returns vector of SQL strings each of which selects the values of one
  ### water quality parameter (in one column) from table \dQuote{tbl} 
  ### giving general column names
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
