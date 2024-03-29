# mmdb -------------------------------------------------------------------------

#' Path to Metadata Database
#' 
#' @param id id of record. Currently, only 0 allowed (the default)
mmdb <- function(id = 0) 
{
  # Path to R meta database
  if (id == 0) {
    system.file("extdata", "RMeta.mdb", package = "kwb.db")
  }
}

# hsdbg ------------------------------------------------------------------------

#' Call browser() in Debug Mode
#' 
hsdbg <- function() 
{
  # in general: call "options(error=recover)" to let R enter the debug mode 
  #   on error.
  # idea taken from http://stackoverflow.com/questions/1882734/what-is-your-favorite-r-debugging-trick
  browser(expr = isTRUE(getOption("hsdbg")))
}

# hsAttribMatrix ---------------------------------------------------------------

#' Attribute Strings to Matrix
#' 
#' Converts a vector of attribute strings to a matrix with as many columns as
#'   there are different attributes occurring in the vector and each row
#'   representing an element of the vector for which the values of assigned
#'   attributes will occur in the corresponding attribute column.
#' 
#' @param attribs Vector containing strings of the form "<key1>=<val1>,<key2>=<val2>,..."
#' 
hsAttribMatrix <- function(attribs) 
{ 
  ## Split each element of the attribs vector at commas
  assilist <- strsplit(attribs, ",")
  
  ## Generate vector of unique assignments
  assigns <- character()
  #for (i in seq_along(assilist)) assigns <- union(assigns, assilist[[i]])
  for (assi in assilist) assigns <- union(assigns, assi)
  
  ## Split assignments at the equal sign and collect all different key names
  kvlist <- strsplit(assigns, "=")
  keys <- character()
  #for (i in seq_along(kvlist)) keys <- union(keys, kvlist[[i]][1])
  for (kv in kvlist) keys <- union(keys, kv[1])
  
  ## Prepare a matrix with the keys as colum names
  res <- matrix(nrow = length(assilist), ncol = length(keys), 
    dimnames = list(NULL, c(keys)))

  ## Loop through original list elements again and fill the result matrix
  for (i in seq_along(assilist)) {

    ## Loop through assignments
    for (assi in assilist[[i]]) {

      ## Split assignment into key/value pair
      kvp <- strsplit(assi, "=")
      
      ## Save value in corresponding key column 
      key <- kvp[[1]][1]
      val <- kvp[[1]][2]
      res[i, key] <- val
    }    
  }
  
  ## Return the result matrix
  res
}

# hsDirStructure ---------------------------------------------------------------

#' Read Directory Structure from Metadata Database
#' 
#' Gets recursively defined directory structure from RMeta.mdb
#' 
#' @param asMatrix logical indicating whether to return a matrix or not
#' @param dbg If \code{TRUE}, debug messages are shown  
hsDirStructure <- function(asMatrix = FALSE, dbg = FALSE) 
{
  on.exit(options(stringsAsFactors = getOption("stringsAsFactors")))
  options(stringsAsFactors = FALSE)

  # hsGetTable is called with as.is = TRUE which lets R remain
  # text fields as text (character). Otherwise columns that are 
  # completely empty (only NULL values) will be converted to 
  # logical at that is not what I want.
  ds <- hsGetTable(mmdb(), "tblDirStruct", dbg = dbg, as.is = TRUE)

  # Replace NAs with ""
  if (asMatrix) {
    ds <- as.matrix(ds) # otherwise assigning "" does not work
    ds[is.na(ds)] <- ""
  }
  ds
}
