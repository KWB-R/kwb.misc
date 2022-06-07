get_servername <- function() {
  
  servername <- Sys.getenv("SERVERNAME")
  if (servername == "") {
    stop('Please define "SERVERNAME" with Sys.setenv("SERVERNAME")')
  }
  servername
}