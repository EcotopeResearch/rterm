#Weather Data Functions...
C_to_F <- function(x) x * 1.8 + 32
noaa_key <- "mgPWFcAfmzIrHjXlacsksLXkhajnyFDp"

#' Read data from NOAA's Climate Data Online Web Services
#' 
#' See the following for documentation on constructing queries.
#' \code{\link{http://www.ncdc.noaa.gov/cdo-web/webservices/v2}}, as
#' well as the examples at the bottom.
#' By default the queries return only 25 entries with a maximum limit of
#' 1000, so be mindful that often much of what you thought you were 
#' querying is not returned.
#' 
#' To use this function you need to get a key for NOAA's API
#' 
#' @param table the data table to query from, examples include 
#'  "locations", "stations", "data"
#' @param param extra parameters for the search
#' 
#' @examples
#' # Show available datasets
#' read.noaa("datasets")
#' 
#' # Show available data types of category "TEMP"
#' read.noaa("datatypes", "datacategoryid=TEMP&limit=100")
#' 
#' # Show locations for the location category of US State
#' read.noaa("locations", "locationcategoryid=ST&limit=51")
#' 
#' # Show Stations in Washington State recording Temperature GHCN
#' # daily datasets since 2010
#' tmp <- read.noaa("stations", paste("locationid=FIPS:53",
#'                                     "datacategoryid=TEMP",
#'                                     "datasetid=GHCND",
#'                                     "startdate=2010-01-01",
#'                                     "limit=1000", sep = "&"))
#' 
read.noaa <- function(table, param = NULL) {
  urlx <- paste("http://www.ncdc.noaa.gov/cdo-web/api/v2/", table, sep = "")  
  if(!is.null(param)) {
    urlx <- paste(urlx, "?", param, sep = "")  
  } 
  
  print(paste("Attempting to query:", urlx))
  tmp <- httr::GET(urlx, httr::add_headers(token = noaa_key))
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


#' Retrive GHCN Weather Data
#' 
#' Given a station id and start and end dates, retrieves daily
#' temperature data from NOAA's API. This wraps the function 
#' read.one.ghcn, which reads data one year at a time (that's the 
#' maximum you're allowed to get in a single query), which itself
#' gets raw data with the \code{\link{read.noaa}} function.
#' 
#' Two things to note: 1) you need a key from NOAA to access the data.
#' This is free and you can generate one here: https://www.ncdc.noaa.gov/cdo-web/token.
#' All weather functions in rterm look for an object in the global environment 
#' called "noaa_key", so you should probably
#' 
#' 2) You need to find a weather station to use! Probably the best way
#' to do this for now is to use the NOAA web search at 
#' http://www.ncdc.noaa.gov/cdo-web/search, where you search for 
#' "Daily Summaries" and "Stations" with the appropriate date range and a
#' keyword. This will bring up a map where you can click on icons for weather
#' stations and it will report an "ID" that looks something like
#' "GHCND:US1WAKG0179"
#' 
#' @param stationid the identifier for the weather station
#' @param startdate the first date of data requested
#' @param enddate the last date of data requested
#' 
#' @return a data frame with variables "date", "TMIN", "TMAX", and 
#'   "aveTemp"
#'   
#' @seealso \code{\link{read.noaa}}
#' 
#' @examples
#' weather <- read.ghcn("GHCND:USW00024233", "2010-01-01", "2014-12-31")
#' 
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
