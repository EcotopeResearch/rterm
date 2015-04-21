#Weather Data Functions...
C_to_F <- function(x) x * 1.8 + 32
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


read.one.ghcn <- function(stationid, startdate, enddate) {
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
  if(is.null(dset)) {
    stop("NOAA Query did not return results")
  }
  
  # Clean it up and return
  dset <- dset %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(value = C_to_F(value / 10)) %>%
    dplyr::select(date, value, datatype) %>%
    tidyr::spread(datatype, value) %>%
    dplyr::mutate(aveTemp = (TMIN + TMAX) / 2)
  
}


# startdate <- "2010-01-01"
# enddate <- "2014-01-01"

read.ghcn <- function(stationid, startdate, enddate) {
  stDate <- try(as.Date(startdate))
  if(inherits(stDate, "try-error")) {
    stop(paste("Could not parse start date", startdate))
  }
  
  edDate <- try(as.Date(enddate))
  if(inherits(edDate, "try-error")) {
    stop(paste("Could not parse end date", enddate))
  }
  
  # We can download 1 year at a time
  interval <- as.numeric(difftime(edDate, stDate))
  nCalls <- ceiling(interval / 365)
  
  dsets <- lapply(1:nCalls, function(i) {
    read.one.ghcn(stationid, stDate + (i - 1) * 365, min(edDate, stDate + i * 365 - 1))
  })
  
  do.call('rbind', dsets)
}

# test <- read.one.ghcn("GHCND:USW00024233", "2013-12-31", "2014-12-31")
# ggplot(test) + geom_line(aes(date, aveTemp))
# test2 <- read.ghcn("GHCND:USW00024233", "2010-01-01", "2014-12-31")
# ggplot(test2) + geom_line(aes(date, aveTemp))
