# hsSel ------------------------------------------------------------------------
hsSel <- function # filter timeseries interactively
### filter timeseries interactively for interval or parallel intervals
(
  tseries,
  interval = FALSE, 
  tsfield = names(tseries)[1],
  dbg = FALSE
)
{
  .catVariableOnDebug("Timestamp column:", tsfield, dbg)  
  if (interval) {
    hsSelectTimeInterval(tseries, tsfield)
  }
  else {
    hsSelectParallelTimeIntervals(tseries, tsfield)
  }
}

# .catVariableOnDebug -----------------------------------------------------------
.catVariableOnDebug <- function(name, variable, debug)
{
  if (debug) {
    cat(name, variable, "\n")
  }  
}

# hsSelectTimeInterval ---------------------------------------------------------
hsSelectTimeInterval <- function # interactively select time interval 
### interactively select time interval from data.frame containing timeseries 
(
  tseries, 
  ### data.frame containing timeseries (at least one timestamp column and one
  ### additional numeric column)
  tsfield = names(tseries)[1]
  ### name of timestamp column; default: name of first column in tseries  
)
{
  tseries <- tseries[.selectYear(tseries[[tsfield]], TRUE), ]
  tseries <- tseries[.selectMonth(tseries[[tsfield]], TRUE), ]
  tseries <- tseries[.selectDay(tseries[[tsfield]], TRUE), ]  
  tseries
}

# hsSelectParallelTimeIntervals ------------------------------------------------
hsSelectParallelTimeIntervals <- function # interactively select parallel time intervals
### interactively select parallel time intervals from data.frame containing timeseries 
(
  tseries, 
  ### data.frame containing timeseries (at least one timestamp column and one
  ### additional numeric column)
  tsfield = names(tseries)[1]
  ### name of timestamp column; default: name of first column in tseries  
)
{
  tseries <- tseries[.selectYear(tseries[[tsfield]], FALSE), ]
  tseries <- tseries[.selectMonth(tseries[[tsfield]], FALSE), ]
  tseries <- tseries[.selectWeekday(tseries[[tsfield]], FALSE), ]   
  tseries <- tseries[.selectDay(tseries[[tsfield]], FALSE), ]
  tseries <- tseries[.selectHour(tseries[[tsfield]], FALSE), ]
  tseries <- tseries[.selectMinute(tseries[[tsfield]], FALSE), ]
  tseries  
}

# .select... -------------------------------------------------------------------
.selectYear <- function(timestamps, interval) {
  .userSelectedTimestampIndices(timestamps, "Year", interval)
}
.selectMonth <- function(timestamps, interval) {
  .userSelectedTimestampIndices(timestamps, "Month", interval)
}
.selectWeekday <- function(timestamps, interval) {
  .userSelectedTimestampIndices(timestamps, "Weekday", interval)
}
.selectDay <- function(timestamps, interval) {
  .userSelectedTimestampIndices(timestamps, "Day", interval)
}
.selectHour <- function(timestamps, interval) {
  .userSelectedTimestampIndices(timestamps, "Hour", interval)
}
.selectMinute <- function(timestamps, interval) {
  .userSelectedTimestampIndices(timestamps, "Minute", interval)
}

# .userSelectedTimestampIndices: selection of time period -----------------------
.userSelectedTimestampIndices <- function
(
  timestamps, 
  unit.name, 
  interval
) 
{ 
  format <- .getFormatSpec(interval, unit.name)
  timestampProperties <- .getTimestampProperties(timestamps, format)
  
  if (length(timestampProperties$unique.t.formatted) > 1) {
    row.indices <- .filteredByUserSelectedOptions(interval, unit.name, 
                                                 timestampProperties)    
  }
  else {
    row.indices <- .allIndices(timestamps)
  }
  row.indices
}

# .getFormatSpec ----------------------------------------------------------------
.getFormatSpec <- function(interval, unit)
{
  if (interval) {
    return (.hsUniqueFormatSpec(unit))
  }
  else {
    return (.hsNonUniqueFormatSpec(unit))
  }  
}

# .getTimestampProperties -------------------------------------------------------
.getTimestampProperties <- function(timestamps, formatstring)
{  
  t.formatted <- format(timestamps, formatstring)
  list(t.formatted = t.formatted, 
       unique.t.formatted = unique(t.formatted))
}

# .filteredByUserSelectedOptions ------------------------------------------------
.filteredByUserSelectedOptions <- function
(
  interval, unit.name, timestampProperties
)
{
  unique.t.formatted <- timestampProperties$unique.t.formatted
  t.formatted <- timestampProperties$t.formatted
  
  choice <- .getChoice(interval, unit.name, unique.t.formatted)  
  answer <- readline(prompt=.messageToAskForChoice(interval, unit.name, choice))
  
  if (answer == "") {
    row.indices <- .allIndices(t.formatted)
  }
  else {
    row.indices <- NULL
    for (index in .hsParse(answer)) {
      rows <- .getRows(interval, unit.name, t.formatted, unique.t.formatted, index)
      row.indices <- c(row.indices, rows)
    }
  }
  return (sort(row.indices))
}

# .getRows ----------------------------------------------------------------------
.getRows <- function(interval, unit.name, t.formatted, unique.t.formatted, idx)
{
  if (! interval && .unitIsDayHourOrMinute(unit.name)) {
    rows <- (as.integer(t.formatted) == idx)
  }
  else {
    rows <- (t.formatted == unique.t.formatted[idx])
  }  
  which(rows)
}

# .allIndices ----------------------------------------------------------------
.allIndices <- function(vector) 
{
  seq(1, by=1, along.with=vector)  
}

# .hsUniqueFormatSpec: format specification needed to identify given unit ------------
.hsUniqueFormatSpec <- function(unit) 
{
  format <- .unitToUniqueFormat(unit)
  return (.getDefaultIfNull(format, .defaultFormat()))
}

# .hsNonUniqueFormatSpec: format specification needed to identify given unit ------------
.hsNonUniqueFormatSpec <- function(unit) 
{
  format <- .unitToNonUniqueFormat(unit)
  return (.getDefaultIfNull(format, .defaultFormat()))
}

# .getDefaultIfNull -------------------------------------------------------------
.getDefaultIfNull <- function(object, default)
{
  if (is.null(object)) {
    object <- default
  }
  object
}

# .hsParse: parse user input ---------------------------------------------------
.hsParse <- function(text) {    
  sequence <- NULL
  for (range in .getRangesFromText(text)) {
    sequence <- union(sequence, .rangeToSequence(as.integer(range)))
  }
  sequence
}

# .getSpaceOrEndOfLineAsWordDelimiters ------------------------------------------
.getSpaceOrEndOfLineAsWordDelimiters <- function(length) {
  eol <- rep("", length)
  if (length > 5) {
    eol[seq(5, length, by = 5)] <- "\n  "
  }
  return (eol)
}

# .getChoice --------------------------------------------------------------------
.getChoice <- function(interval, unit.name, unique.t.formatted)
{
  if (!interval && .unitIsDayHourOrMinute(unit.name)) {
    choice <- paste(sort(as.integer(unique.t.formatted)), collapse = ",")        
  }
  else {
    choice <- .getChoiceBasedOnInterval(interval, unique.t.formatted)
  }  
  return (choice)
}

# .getChoiceBasedOnInterval -----------------------------------------------------
.getChoiceBasedOnInterval <- function(interval, options)
{
  num.options <- length(options)
  if (interval) {
    eol <- .getSpaceOrEndOfLineAsWordDelimiters(num.options)
    choice <- paste0(sprintf("%3d", 1:num.options), ": ", options, ",", eol, 
                     collapse = "")          
  }
  else {
    choice <- paste0(c(1:num.options, "RET"), ":", c(options, "all"), 
                     collapse = ",") 
  }
  return (choice)
}

# .messageToAskForChoice --------------------------------------------------------
.messageToAskForChoice <- function(interval, unit.name, choice) 
{
  if (interval) {
    cat(sprintf("%s?\n  %s\n", unit.name, choice))        
    msg <- "Your choice (RET: all)? "
  }
  else {
    msg <- sprintf("%s (%s)? ", unit.name, choice)
  }
  return (msg)
}

# .rangeToSequence --------------------------------------------------------------
.rangeToSequence <- function(range)
{
  from <- range[1]
  to <- range[length(range)]
  seq(from, to, by=1)
}

# .getRangesFromText ------------------------------------------------------------
.getRangesFromText <- function(text)
{
  # "<from>-<to>" ranges, separated by comma
  strsplit(strsplit(text, ",")[[1]], "-")
}  

# .unitIsDayHourOrMinute --------------------------------------------------------
.unitIsDayHourOrMinute <- function(unit)
{
  unit %in% c("Day", "Hour", "Minute")  
}

# .unitToUniqueFormat -----------------------------------------------------------
.unitToUniqueFormat <- function(unit)
{
  list("Year"    = "%Y",
       "Month"   = "%b %Y",
       "Weekday" = "%a",
       "Day"     = "%d %b %Y",
       "Hour"    = "%d %b %Y %Hh",
       "Minute"  = "%d %b %Y %H:%M")[[unit]]
}

# .unitToNonUniqueFormat -----------------------------------------------------------
.unitToNonUniqueFormat <- function(unit)
{
  list("Year"    = "%Y",
       "Month"   = "%b",
       "Weekday" = "%a",
       "Day"     = "%d",
       "Hour"    = "%H",
       "Minute"  = "%M")[[unit]]
}

# .defaultFormat -------------------------------------------------------------
.defaultFormat <- function()
{
  "%Y-%m-%d %H:%M:%S"
}