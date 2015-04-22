#Weather Data Functions...


#' Read data from NOAA's Climate Data Online Web Services
#' 
#' Queries NOAA's Web Services according to the following documentation:
#' \url{http://www.ncdc.noaa.gov/cdo-web/webservices/v2}.
#' 
#' To use this function you need to get a key for NOAA's API. 
#' This is free and you can generate one here: \url{https://www.ncdc.noaa.gov/cdo-web/token}.
#' All weather functions in rterm look for an object in the global environment 
#' called "noaa_key". You can deal with this by taking the following line:
#' 
#' noaa_key <- "your_key_here"
#' 
#' and placing it either before the library(rterm) statement, or, preferably, in 
#' a .Rprofile that gets run when you load R. 
#' \url{http://www.statmethods.net/interface/customizing.html}
#' 
#' By default the queries return only 25 entries with a maximum limit of
#' 1000, so be mindful that often much of what you thought you were 
#' querying is not returned.
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
read.noaa <- function(table, param = NULL, quietly = TRUE) {
  if(!exists("noaa_key")) {
    stop("Must have a noaa key to read weather data. See help(read.noaa)")
  }
  
  urlx <- paste("http://www.ncdc.noaa.gov/cdo-web/api/v2/", table, sep = "")  
  if(!is.null(param)) {
    urlx <- paste(urlx, "?", param, sep = "")  
  } 
  
  if(!quietly) print(paste("Attempting to query:", urlx))
  tmp <- httr::GET(urlx, httr::add_headers(token = noaa_key))
  jsonlite::fromJSON(httr::content(tmp, "text"))$results
}

read.noaa.json <- function(table, param = NULL) {
  if(!exists("noaa_key")) {
    stop("Must have a noaa key to read weather data. See help(read.noaa)")
  }
  
  urlx <- paste("http://www.ncdc.noaa.gov/cdo-web/api/v2/", table, sep = "")  
  if(!is.null(param)) {
    urlx <- paste(urlx, "?", param, sep = "")  
  } 
  
  print(paste("Attempting to query:", urlx))
  tmp <- httr::GET(urlx, httr::add_headers(token = noaa_key))
  jsonlite::fromJSON(httr::content(tmp, "text"))  
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
    warning(paste("No data found from NOAA w/ param", param))
    return(NULL)
  }
  
  # Clean it up and return
  dset <- dset %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(value = C_to_F(value / 10)) %>%
    dplyr::select(date, value, datatype) %>%
    tidyr::spread(datatype, value) %>%
    dplyr::mutate(aveTemp = (TMIN + TMAX) / 2)
  
  names(dset)[names(dset) == "TMIN"] <- "tmin"
  names(dset)[names(dset) == "TMAX"] <- "tmax"
  
  dset
  
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
#' This is free and you can generate one here: \url{https://www.ncdc.noaa.gov/cdo-web/token}.
#' All weather functions in rterm look for an object in the global environment 
#' called "noaa_key". You can deal with this by taking the following line:
#' 
#' noaa_key <- "your_key_here"
#' 
#' and placing it either before the library(rterm) statement, or, preferably, in 
#' a .Rprofile. \url{http://www.statmethods.net/interface/customizing.html}
#' 
#' 2) You need to find a weather station to use! Probably the best way
#' to do this for now is to use the NOAA web search at 
#' \url{http://www.ncdc.noaa.gov/cdo-web/search}, where you search for 
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
#'   "aveTemp". Temperatures in Fahrenheit
#'   
#' @seealso \code{\link{read.noaa}} \code{\link{stationSearch}}
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
  
  dset <- do.call('rbind', dsets)
  
  if(is.null(dset)) return(NULL)
  
  # Fill in NA values...
  allDates <- seq(from = min(dset$date), to = max(dset$date), by = 1)
  isMissing <- setdiff(allDates, dset$date)
  if(length(isMissing)) {
    isMissing <- as.Date(isMissing, origin = "1970-01-01")
    dset <- rbind(dset,
                  data.frame("date" = isMissing, "tmin" = NA, "tmax" = NA, "aveTemp" = NA))
  }
  
  return(dset)
}



calcDistance <- function(lat1, lon1, lat2, lon2) {
  if(lat1 == lat2 & lon1 == lon2) {
    return(0)
  }
  degRad<-pi/180
  phi1 <- (90 - lat1) * degRad
  phi2 <- (90 - lat2) * degRad
  theta1 <- lon1 * degRad
  theta2 <- lon2 * degRad
  cosTotal <- sin(phi1) * sin(phi2) * cos(theta1 - theta2) + cos(phi1) * cos(phi2)
  arc <- acos(cosTotal)
  return(arc * 3960)
}


#' Search for GHCN Weather Stations
#' 
#' Search by station name or latitude/longitude for GHCN weather stations
#' that can be loaded with \code{\link{read.ghcn}}. 
#' 
#' 
#' @param name A name to search for in the list of station names. 
#'  Evaluated as a regular expression.
#' @param lat Latitude
#' @param lon Longitude
#' @param nClosest The number of stations to return when searching 
#'  on latitude & longitude. Example: nClosest = 10 will show the 
#'  10 closest stations. Optional, defaults to 5.
#' 
#' @return a data frame with information about the relevant stations found. 
#'  In the case that you specified lat/lon instead of a name this will 
#'  include station distance in miles from the searched location.
#'   
#' @seealso \code{\link{read.ghcn}} \code{\link{stationCompare}} 
#'  \code{\link{read.noaa}}
#' 
#' @examples
#' # Look for a weather station in Bend, Oregon
#' stationSearch("Bend, OR")
#' 
#' # Find the closest weather stations to a lat/lon pair
#' # specifying a location in the Columbia Basin
#' stationSearch(lat = 46.943, lon = -119.240)
#' 
#' 
stationSearch <- function(name = NULL, lat = NULL, lon = NULL, nClosest = 5) {
  if(!is.null(name)) {
    ind <- grep(name, stations$name, ignore.case = TRUE)
    if(length(ind)) {
      return(stations[ind, ])
    }
  } else if(!is.null(lat) & !is.null(lon)) {
    stations$milesDistant <- sapply(1:nrow(stations), function(i) {
      calcDistance(stations$latitude[i], stations$longitude[i], lat, lon)
    })
    stations <- plyr::arrange(stations, milesDistant)
    return(stations[1:nClosest, ])
  } else {
    stop("Must specify either a name to search on, or latitude/longitude coordinates. See help(stationSearch)")
  }

}

smoothTemps <- function(dset, days = 14, var = "aveTemp") {
  # Take a 2 week rolling mean for presentation
  fsmooth <- rep(1 / days, days)
  dset <- ddply(dset, .(id), function(x) {
    x$maTemp <- filter(x[var], fsmooth, sides = 1)
    x
  })
  
  # Scale by the mean at each date, to make the 
  # comparison easier to see
  dset <- ddply(dset, .(date), function(x) {
    x$scaledTemp <- scale(x$maTemp, center = TRUE, scale = FALSE)
    x$scaledTempRaw <- scale(x[var], center = TRUE, scale = FALSE)
    x
  })
  dset
}

stationCompare <- function(st, startdate, enddate) {
  ids <- stations$id
  dsets <- lapply(1:nrow(st), function(i) {
    dset <- read.ghcn(st$id[i], startdate, enddate)
    if(!is.null(dset)) {
      dset <- merge(dset, st[i, ])
    }
    dset
  })
  
  dset <- do.call("rbind", dsets)
  stationComp <- list("data" = dset, "stations" = st)
  class(stationComp) <- "stationComp"
  stationComp
}

summary.stationComp <- function(sc) {
  # We want to report the following...
  #  1) Observed Data Fraction
  #  2) Degrees above or below average between selected stations
  
  dset <- smoothTemps(sc$data)
  
  sumStats <- ddply(dset, .(id), function(x) {
    c("dataFrac" = sum(!is.na(x$aveTemp)) / nrow(x),
      "relativeTemp" = mean(x$scaledTempRaw, na.rm = TRUE))
  })
  
  results <- merge(sc$stations, sumStats)
  
  results <- results %>%
    dplyr::mutate(relTempScaled = 1 - abs(relativeTemp) / sum(abs(relativeTemp))) %>%
    dplyr::mutate(distInd = 1 - milesDistant / sum(milesDistant)) %>%
    dplyr::mutate(useIndex = (dataFrac + relTempScaled + distInd) / 3)

  results <- plyr::arrange(results, -useIndex)
  results <- dplyr::select(results, id, name, milesDistant, dataFrac, relativeTemp)
  results
}



plot.stationComp <- function(sc, days = 14, var = "aveTemp", type = "relative") {
  
  dset <- smoothTemps(sc$data, days, var)
  
  if(var == "aveTemp") {
    varLong <- "Average Temperature"
  } else if(var == "tmin") {
    varLong <- "Minimum Temperature"
  } else if(var == "tmax") {
    varLong <- "Maximum Temperature"
  } else {
    stop(paste("Unrecognized variable", var))
  }
  
  title1 <- paste(days, "day Rolling", varLong)
  
  if(type == "actual") {
    p <- ggplot2::ggplot(dset) + ggplot2::theme_bw() +
      ggplot2::geom_line(ggplot2::aes(x = date, y = maTemp, col = name)) +
      ggplot2::ggtitle(title1) +
      ggplot2::xlab("") + ggplot2::ylab(paste(title1, "F")) +
      ggplot2::theme(legend.text = element_text(size = 7))
    
  } else if(type == "relative") {
    p <- ggplot2::ggplot(dset) + ggplot2::theme_bw() + 
      ggplot2::geom_line(ggplot2::aes(x = date, y = scaledTemp, col = name)) + 
      ggplot2::ggtitle(paste(title1, "\nRelative to Mean Temp by Date")) +
      ggplot2::xlab("") +
      ggplot2::ylab(paste(title1, "(F)\nrelative to mean for that date")) +
      ggplot2::theme(legend.text = element_text(size = 7))    
  }

  p
}

