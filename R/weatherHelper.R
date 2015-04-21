#Weather Data Functions...

key <- "mgPWFcAfmzIrHjXlacsksLXkhajnyFDp"

read.noaa <- function(table, param = NULL) {
  if(!is.null(param)) {
    urlx <- paste("http://www.ncdc.noaa.gov/cdo-web/api/v2/", table, "?", param, sep = "")  
  } else {
    urlx <- paste("http://www.ncdc.noaa.gov/cdo-web/api/v2/", table, sep = "")  
  }
  print(paste("Attempting to query:", urlx))
  tmp <- httr::GET(urlx, httr::add_headers(token = key))
  jsonlite::fromJSON(httr::content(tmp, "text"))$results
}

# Here is a stationid... for testing...
# stationid <- "GHCND:USW00024233"
# startdate <- "2012-01-01"
# enddate <- "2012-02-01"


read.ghcn <- function(stationid, startdate, enddate) {
  # Set-up the parameters for the call to NOAA
  startdate <- as.character(startdate)
  enddate <- as.character(enddate)
  param <- paste0("datasetid=GHCND&",
                  "stationid=", stationid, "&",
                  "startdate=", startdate, "&",
                  "enddate=", enddate, "&",
                  "datatypeid=TMIN,TMAX&",
                  "limit=1000")
  
  # Query the NOAA API for the data
  dset <- read.noaa("data", param)
  
  # Clean it up and return
  dset <- dset %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(value = C_to_F(value / 10)) %>%
    dplyr::select(date, value, datatype) %>%
    tidyr::spread(datatype, value) %>%
    dplyr::mutate(aveTemp = (TMIN + TMAX) / 2)
  
}

# test <- read.ghcn("GHCND:USW00024233", "2014-01-01", "2014-03-01")
