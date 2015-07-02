
.onAttach <- function(libname, pkgname) {
  # if(rbinom(1, 1, .1)) packageStartupMessage("https://twitter.com/kobebryant")
  
  if(!exists("noaa_key")) {
    warning("NOAA Key for Downloading Weather Data Not Found. See help(read.noaa) for details.")
  }
  
  if(!exists("google_key")) {
    warning("Google Maps API key not found. See help(stationSearch) for more info.")
  }

  packageStartupMessage("rterm development version, beware of bugs!")
  
}
