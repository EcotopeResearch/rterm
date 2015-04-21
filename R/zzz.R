
.onAttach <- function(libname, pkgname) {
  if(rbinom(1, 1, .1)) packageStartupMessage("https://twitter.com/kobebryant")
  
  if(!length(grep("noaa_key", ls(.GlobalEnv)))) {
    warning("NOAA Key for Downloading Weather Data Not Found. See help(read.noaa) for details.")
  }
}
