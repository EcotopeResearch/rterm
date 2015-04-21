
.onAttach <- function(libname, pkgname) {
  if(rbinom(1, 1, .1)) packageStartupMessage("https://twitter.com/kobebryant")
  
  if(!exists("noaa_key")) {
    warning("NOAA Key for Downloading Weather Data Not Found. See help(read.noaa) for details.")
  }
}
