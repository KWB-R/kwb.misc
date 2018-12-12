# defaultDictionary ------------------------------------------------------------
defaultDictionary <- function # Get Default Dictionary from Meta Database
### Get a default dictionary from the meta database
(
  mdb = mmdb(),
  ### full path to meta database, default: \code{\link{mmdb}()}
  dbg = FALSE
) 
{
  ## Get key-value table from mdb and transform to dictionary (list)
  data <- kwb.db::hsGetTable(mdb, tbl = "tblGrammarDir", 
                             fields = "grdKey,grdValue", dbg = dbg)

  kwb.utils::toLookupList(data = data)
}

# hsFileCands ------------------------------------------------------------------
hsFileCands <- function
### find file candidates according to paths defined in dictionary
(
  mdb, 
  dict.lst,
  dbg = FALSE
) 
{
  mdb.res <- hsResolve(mdb, dict.lst) # fully resolved path
  dir.name <- dirname(mdb.res)        # directory name
  base.name <- basename(mdb.res)      # file name
  
  ## directory must not contain <tags>
  if (length(kwb.utils::getTagNames(dir.name, expected.length = 1)[[1]])) {
    stop(sprintf("Directory must not contain tags (directory is '%s')\n", 
                 dir.name))
  }

  ## Find <tags> in file name
  tags <- kwb.utils::getTagNames(base.name, expected.length = 1)[[1]]
  
  ## Create file name pattern
  ptrn <- sprintf("^%s$", gsub("<[^>]+>", "(\\[^_\\]\\+)", base.name))
  
  catIf(dbg, sprintf("file name pattern: '%s'\n", ptrn))
  
  ## Get names of matching files in directory
  cands <- dir(dir.name, ptrn) # cadidates

  ## If there is no candidate stop
  if (length(cands) == 0) {
    stop(sprintf("No database file matching '%s' found in '%s'.\n", 
                 ptrn, dir.name))
  }
  
  ## If there is only one candidate then return its full path
  if (length(cands) == 1) {
    return(file.path(dir.name, cands))
  }
  
  catIf(dbg, "Candidate files:\n ", collapsed(cands, "\n  "), "\n")
  
  ## Get the different combinations of tag values
  ## - Create replacement string
  replacement <- paste(tags, "=\\", seq_along(tags), sep = "", collapse = ",")
  
  catIf(dbg, sprintf("replacement: '%s'\n", replacement))
    
  ## - Reduce file names to attribute values
  attribAssigns <- gsub(ptrn, replacement, cands)    

  cat("More than one possible database. Specify one of the following attributes:\n")

  ## Return matrix containing all different combinations of attribute values
  print(hsAttribMatrix(attribAssigns))
}

# hsSourceList -----------------------------------------------------------------
hsSourceList <- function
### Return data frame containing ids, properties and paths of mdb databases
### matching criteria given in ... argument list
(
  keyptrn = "^DB_", 
  dbg = FALSE, 
  ...
) 
{

  ## Get definition of grammar
  dict.lst <- c(defaultDictionary(dbg = dbg), list(...))
  
  ## Get unresolved database keys
  mdbs.unres <- grep(keyptrn, names(dict.lst), value = TRUE)
  
  printIf(dbg, mdbs.unres, "unresolved mdb paths")

  ## Resolve all database keys
  mdbs.res <- hsResolve(mdbs.unres, dict.lst, dbg = dbg)  
  printIf(dbg, mdbs.res, "fully resolved mdb paths")

  ## Extract attribute lists at the beginning of the resolved paths
  attribs <- character()
  
  for (tag in kwb.utils::getTagNames(mdbs.res, bt = "[]")) {
    if (! is.null(tag)) {
      attribs <- c(attribs, kwb.utils::commaCollapsed(tag))
    }
  }
  
  printIf(dbg, mdbs.res, 
          "Resolved database paths (including properties in brackets)")

  ## Remove attribute lists from resolved paths
  mdbs.res <- gsub("\\[[^]]+\\]","", mdbs.res)
  
  ## Return non-empty attributes
  data.frame(id = mdbs.unres, hsAttribMatrix(attribs), dir = mdbs.res)
}

# hsMiaDir ---------------------------------------------------------------------
hsMiaDir <- function
### Return directory paths containing mdb databases matching the criteria
### defined by the ... parameter list
(
  dbg = FALSE, 
  ...
)
{
  dirs <- hsSourceList(keyptrn = "^DIR_", dbg = dbg)

  printIf(dbg, dirs, "Relevant directories")
  
  assigns <- list(...)
  
  for (argname in names(assigns)) {
    
    argval <- assigns[[argname]]
    
    catIf(dbg, sprintf("attrib = '%s', value = '%s'\n", argname, argval))

    ## Filter for assigned attribute value
    dirs <- dirs[! is.na(dirs[[argname]]) & dirs[[argname]] == argval, ]

    ## Find indices of columns in which not all values are NA and exclude
    ## the index of the column which was subject to the filter
    colinds <- 1 # id column will be selected in any case
    
    # start with 2nd column
    for (i in 2:ncol(dirs)) {
      
      if (! all(is.na(dirs[[i]])) && names(dirs)[i] != argname) {
        colinds <- c(colinds, i)
      }
    }

    ## Remove columns in which all values are NA
    dirs <- dirs[, colinds, drop = FALSE]
    
    printIf(dbg, dirs)
  }

  if (nrow(dirs) > 1) {
    
    cat(sprintf(
      "Specify one of the attributes %s in order to select a distinct directory:\n",
      stringList(setdiff(names(dirs), "dir"))
    ))
    
    print(dirs)
    
    return (NULL)
  }
  
  return (dirs)
}
