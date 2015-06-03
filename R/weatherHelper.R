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
#' @seealso \code{\link{read.ghcn}}
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



read.one.ghcn.monthly <- function(stationid, startdate, enddate) {
  # Set-up the parameters for the call to NOAA
  startdate <- as.character(startdate)
  enddate <- as.character(enddate)
  param <- paste0("datasetid=GHCNDMS&",
                  "stationid=", stationid, "&",
                  "startdate=", startdate, "&",
                  "enddate=", enddate, "&",
                  "datatypeid=MNTM&",
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
    tidyr::spread(datatype, value)
  
  names(dset)[names(dset) == "MNTM"] <- "aveTemp"
  
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
#' @param enddate the last date of data requested. Dates should 
#'  be POSIX date class or coercible to such with as.Date
#' 
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
  allDates <- seq(from = stDate, to = edDate, by = 1)
  isMissing <- setdiff(allDates, dset$date)
  if(length(isMissing)) {
    isMissing <- as.Date(isMissing, origin = "1970-01-01")
    dset <- rbind(dset,
                  data.frame("date" = isMissing, "tmin" = NA, "tmax" = NA, "aveTemp" = NA))
  }
  
  dset <- plyr::arrange(dset, date)
  
  return(dset)
}



read.ghcn.monthly <- function(stationid, startdate, enddate) {
  stDate <- try(as.Date(startdate))
  if(inherits(stDate, "try-error")) {
    stop(paste("Could not parse start date", startdate))
  }
  
  edDate <- try(as.Date(enddate))
  if(inherits(edDate, "try-error")) {
    stop(paste("Could not parse end date", enddate))
  }
  
  monthsToGet <- lubridate::interval(stDate, edDate) %/% months(1)
  
  # We can download 100 months at a time
  nCalls <- ceiling(monthsToGet / 100)
  dsets <- lapply(1:nCalls, function(i) {
    stTmp <- stDate + (i - 1) * months(101)
    edTmp <- min(edDate, stDate + i * months(100))
    read.one.ghcn.monthly(stationid, stTmp, edTmp)
  })  
  
  dset <- do.call('rbind', dsets)
  
  if(is.null(dset)) return(NULL)
  
  # Fill in NA values...
  allDates <- stDate + months(0:monthsToGet)
  isMissing <- setdiff(allDates, dset$date)
  if(length(isMissing)) {
    isMissing <- as.Date(isMissing, origin = "1970-01-01")
    dset <- rbind(dset,
                  data.frame("date" = isMissing, "aveTemp" = NA))
  }
  
  dset <- plyr::arrange(dset, date)
  
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
#' Search by location name, station name or latitude/longitude for GHCN weather stations
#' that can be loaded with \code{\link{read.ghcn}}. Note that you need to 
#' acquire a \href{https://developers.google.com/maps/documentation/geocoding/?hl=en_US#api_key}{Google Maps API key} for the default functionality, which is to 
#' search for stations close to a latitude/longitude as returned by a Google
#' Maps geocode.
#' 
#' @param geocode The default input. A string that will be sent to 
#'   Google Maps to get a latitude/longitude. This must be loaded into
#'   your R session as "google_key"
#' @param name A name to search for in the list of station names. 
#'  Evaluated as a regular expression.
#' @param lat Latitude
#' @param lon Longitude
#' @param nClosest The number of stations to return when searching 
#'  on latitude & longitude. Example: nClosest = 10 will show the 
#'  10 closest stations. Optional, defaults to 5.
#' @param country an optional specification of country
#' @param state an optional specification of us state
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
#' # It can help to specify state for a common name
#' stationSearch("Madison")
#' stationSearch("Madison", state = "wisconsin")
#' 
#' # Do we have weather sites in Switzerland?
#' stationSearch(country = "Switzerland")
#' 
#' 
stationSearch <- function(geocode = NULL, stationName = NULL, lat = NULL, lon = NULL, nClosest = 5, country = NULL, state = NULL) {
  # Check for a country and/or state
  if(!is.null(country)) {
    stations <- stations[grep(country, stations$country, ignore.case = TRUE), ]
  }
  if(!is.null(state)) {
    stations <- stations[grep(state, stations$state, ignore.case = TRUE), ]
  }
  
  if(!nrow(stations)) {
    print("No stations found")
    return(stations)
  }
  
  stations$milesDistant <- NA
  
  # Check if we need to geocode
  elev <- NULL
  if(!is.null(geocode)) {
    locs <- geocode(geocode)
    if(is.null(locs)) {
      return(NULL)
    }
    lat <- as.numeric(locs[1])
    lon <- as.numeric(locs[2])
    elev <- as.numeric(locs[3])
  }

  
  # Now search by either stationName or lat/lon
  if(!is.null(stationName)) {
    ind <- grep(stationName, stations$name, ignore.case = TRUE)
    if(!length(ind)) {
      return(NULL)
    }
  } else if(!is.null(lat) & !is.null(lon)) {
    stations$milesDistant <- sapply(1:nrow(stations), function(i) {
      calcDistance(stations$latitude[i], stations$longitude[i], lat, lon)
    })
    stations <- plyr::arrange(stations, milesDistant)
    ind <- seq(from = 1, to = min(nrow(stations), nClosest), by = 1)
    
    # Assign lat and lon of search center to stations
  } else {
    ind <- 1:nrow(stations)
  }

  stations <- subset(stations, select = -c(country, state))
  
  attr(stations, "lat") <- lat
  attr(stations, "lon") <- lon
  attr(stations, "elev") <- elev
  attr(stations, "gmapsSearch") <- geocode
  
  stations <- stations[ind, ]
}

smoothTemps <- function(dset, days = 14, var = "aveTemp") {
  # Make sure we have enough records to smooth
  if(days >= min(table(dset$id))) {
    days <- 1
  }
  
  # Take a rolling mean for presentation
  fsmooth <- rep(1 / days, days)
  dset <- do.call('rbind', by(dset, dset$id, function(x) {
    x$maTemp <- stats::filter(x[var], fsmooth, sides = 2)
    x
  }))
  
  # Scale by the mean at each date, to make the 
  # comparison easier to see
  dset <- do.call('rbind', by(dset, dset$date, function(x) {
    x$scaledTemp <- scale(x$maTemp, center = TRUE, scale = FALSE)
    x$scaledTempRaw <- scale(x[var], center = TRUE, scale = FALSE)
    x
  }))
  dset
}


#' Compare GHCN Weather Stations
#' 
#' Compare weather stations returned by \code{\link{stationSearch}}
#' 
#' 
#' 
#' @param st Data frame of stations returned by \code{\link{stationSearch}}
#' @param startdate the start date of the interval to compare
#' @param enddate the end date of the interval to compare
#' 
#' The start and end date arguments should either be POSIX date objects, 
#' or coercible to with as.Date. So for example "2013-01-01" is appropriate.
#' 
#' @return An object of S3 class stationComp. This is a list with two entries
#' 1) data - the weather data for the requested stations over the requested interval 
#' and 2) stations - the station info data frame.
#'   
#' @seealso \code{\link{summary.stationComp}} \code{\link{plot.stationComp}} 
#'  \code{\link{read.ghcn}}
#' 
#' @examples
#' # Compare 5 closest weather stations to Ecotope's Seattle office
#' stations <- stationSearch(lat = 47.6569326, lon = -122.3184546)
#' comp <- stationCompare(stations, "2014-01-01", "2014-12-31")
#' summary(comp)
#' plot(comp)
#' 
#' 
stationCompare <- function(st, startdate, enddate) {
  ids <- stations$id
  dsets <- lapply(1:nrow(st), function(i) {
    dset <- try(read.ghcn(st$id[i], startdate, enddate))
    if(inherits(dset, "try-error")) {
      return(NULL)
    }
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


stationCompareMonthly <- function(st, startdate, enddate) {
  ids <- stations$id
  dsets <- lapply(1:nrow(st), function(i) {
    dset <- try(read.ghcn.monthly(st$id[i], startdate, enddate))
    if(inherits(dset, "try-error")) {
      return(NULL)
    }
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


#' Summarize a comparison of GHCN weather stations
#' 
#' Summarize a comparison of weather stations returned by 
#' \code{\link{stationCompare}}
#' 
#' 
#' @param sc stationComp object as returned by \code{\link{stationCompare}}
#' 
#' 
#' @return a data frame summarizing the weather stations. This includes 
#'  the station name and id, as well as some summary stats to hopefully 
#'  help you choose. "dataFrac" refers to the fraction of non-missing data 
#'  in the observed interval ("datacoverage" from stationSearch refers to 
#'  the entire history of the weather station). "relativeTemp" refers to 
#'  the average temperature of that station, with respect to the average 
#'  temperature of all stations in the comparison. If you searched by 
#'  latitude/longitude, you also get "milesDistant", which is miles from your 
#'  search coordinates to the weather station.
#'   
#' @seealso \code{\link{stationSearch}} \code{\link{stationComp}} 
#'  \code{\link{plot.stationComp}}  \code{\link{read.ghcn}}
#' 
#' @examples
#' # Compare 5 closest weather stations to Ecotope's Seattle office
#' stations <- stationSearch(lat = 47.6569326, lon = -122.3184546)
#' comp <- stationCompare(stations, "2014-01-01", "2014-12-31")
#' summary(comp)
#' 
#' 
summary.stationComp <- function(sc, days = 14) {
  # We want to report the following...
  #  1) Observed Data Fraction
  #  2) Degrees above or below average between selected stations
  
  dset <- smoothTemps(sc$data, days)
  sumStats <- do.call('rbind', by(dset, dset$id, function(x) {
    data.frame("dataFrac" = sum(!is.na(x$aveTemp)) / nrow(x),
      "relativeTemp" = mean(x$scaledTempRaw, na.rm = TRUE),
      "aveTemp" = mean(x$aveTemp, na.rm = TRUE),
      "id" = x$id[1])
  }))
  
  results <- merge(sc$stations, sumStats)
  results <- dplyr::mutate(results, 
                           relTempScaled = 1 - abs(relativeTemp) / sum(abs(relativeTemp)))

  if(is.null(sc$stations$milesDistant)) {
    results <- results %>%
      dplyr::mutate(useIndex = (dataFrac + relTempScaled) / 2)
  } else {
    results <- results %>%
      dplyr::mutate(distInd = 1 - milesDistant / sum(milesDistant)) %>%
      dplyr::mutate(useIndex = (dataFrac + relTempScaled + distInd) / 3)
  }

  results <- plyr::arrange(results, -useIndex)
  results <- results[, names(results) %in% c("id", "name", "milesDistant", "dataFrac", "relativeTemp", "aveTemp")]
  results
}


#' Plot a comparison of GHCN weather stations
#' 
#' Plot a comparison of weather stations returned by 
#' \code{\link{stationCompare}}
#' 
#' 
#' @param sc stationComp object as returned by \code{\link{stationCompare}}
#' @param days The number of days for the rolling average. Optional, 
#'   defaults to 14.
#' @param var The variable to plot. Optional, defaults to "aveTemp". Can 
#'   also be "tmin" or "tmax". See \code{\link{read.ghcn}}
#' @param type The type of view, can be "actual" or "relative". "actual" 
#'   will plot the recorded temperatures, while "relative" will plot the 
#'   temperatures with reference to the mean temperature. "relative" tends 
#'   to be more useful in visualizing the differences between stations. 
#'   This argument is optional and defaults to "relative".
#' @param xvar What variable to plot against. Defaults to "date" but can 
#'   also be "doy" - day of year. "date" gives a long rolling line, while 
#'   "doy" gives multiple lines on top of each other per station.
#' 
#' 
#' @return a ggplot object of the graphic
#'   
#' @seealso \code{\link{stationSearch}} \code{\link{stationComp}} 
#'  \code{\link{plot.stationComp}}  \code{\link{read.ghcn}}
#' 
#' @examples
#' # Compare 5 closest weather stations to Ecotope's Seattle office
#' stations <- stationSearch("Seattle")
#' comp <- stationCompare(stations, "2014-01-01", "2014-12-31")
#' plot(comp)
#' plot(comp, "days" = 60)
#' plot(comp, var = "tmin", type = "actual")
#' 
#' 
plot.stationComp <- function(sc, days = 14, var = "aveTemp", type = "actual", xvar = "date") {
  
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
  
  if(type == "relative") {
    title2 <- paste(title1, "\nRelative to Mean Across Selected Weather Stations", sep = "")
  } else {
    title2 <- title1
  }
  
  if(xvar == "doy") {
    dset$dofm <- format(dset$date, "%d")
    dset$m <- format(dset$date, "%m")
    dset$doy <- as.Date(paste("2000", dset$m, dset$dofm, sep = "-"))
    dset$year <- as.numeric(format(dset$date, format = "%Y"))
    dset$groupvar <- interaction(dset$name, dset$year)
  }
  
  if(type == "actual") {
    yvar = "maTemp"
  } else if(type == "relative") {
    yvar = "scaledTemp"
  }
  
  p <- ggplot2::ggplot(dset) + ggplot2::theme_bw() +
    ggplot2::ggtitle(title2) +
    ggplot2::xlab("") + ggplot2::ylab(paste(title1, "(F)"))  
  
  if(xvar == "doy") {
    p <- p + ggplot2::geom_line(ggplot2::aes_string(x = xvar, y = yvar, col = "name", group = "groupvar", size = "year"), alpha = .8) +
      scale_x_date(breaks = "2 months", labels = scales::date_format("%B")) + 
      scale_size_continuous(range = c(.2, 1.5))
  } else {
    p <- p + ggplot2::geom_line(ggplot2::aes_string(x = xvar, y = yvar, col = "name"))
  }

  p
}


removeStation <- function(comp, name) {
  tmp <- grep(name, comp$stations$name, ignore.case = TRUE)
  if(length(tmp) > 0) {
    badName <- comp$stations$name[tmp]
    for(name in badName) {
      comp$stations <- comp$stations[-tmp, ]  
      comp$data <- comp$data[comp$data$name != name, ]
    }
  } else {
    stop(paste("Could not find station", name))
  }
  
  comp
}


# Bonus function to compare weather at a given site for x years...
#' Look at a time trend within a station, for example the past 5 years
#' 
#' Plot a comparison of weather stations returned by 
#' \code{\link{stationCompare}}
#' 
#' 
#' @param weather a weather dataset returned by read.ghcn
#' @param var The variable to plot. Optional, defaults to "aveTemp". Can 
#'   also be "tmin" or "tmax". See \code{\link{read.ghcn}}
#' @param days The number of days for the rolling average. Optional, 
#'   defaults to 14.
#' @param type The type of view, can be "actual" or "relative". "actual" 
#'   will plot the recorded temperatures, while "relative" will plot the 
#'   temperatures with reference to the mean temperature. "relative" tends 
#'   to be more useful in visualizing the differences between stations. 
#'   This argument is optional and defaults to "relative".
#' 
#' @return a list containing "data" the dataset w/ additional smoothed 
#'  variable(s) and "plot", the ggplot object
#'   
#' @seealso \code{\link{stationSearch}} \code{\link{stationComp}} 
#'  \code{\link{plot.stationComp}}  \code{\link{read.ghcn}}
#' 
#' @examples
#' # Look at the last 5 years from Ted Stevens Intl Airport in Alaska
#' anc <- read.ghcn("GHCND:USW00026451", "2010-01-01", "2015-04-15")
#' ancTrend <- stationTrend(anc)
#' ancTrend$plot
#' 
#' 
stationTrend <- function(weather, var = "aveTemp", days = 60, type = "actual") {
  
  weather$year <- as.numeric(format(weather$date, format = "%Y"))
  weather$day <- as.numeric(format(weather$date, format = "%j"))
  
  # Add a relative temperature variable
  weather <- do.call("rbind", by(weather, weather$day, function(x) {
    x$relTemp <- as.numeric(scale(x[, var], center = TRUE, scale = FALSE))
    x
  }))
  weather <- arrange(weather, date)

  if(days > 1) px <- "Smoothed" else px <- ""
  if(var == "aveTemp") {
    vname <- "Average Temp"
  } else if(var == "tmin") {
    vname <- "Min Temp"
  } else if(var == "tmax") {
    vname <- "Max Temp"
  }
  tx <- paste(px, type, vname)
  
  fsmooth <- rep(1 / days, days)
  weather$maTemp <- stats::filter(weather$relTemp, fsmooth, sides = 2)
  weather$actualSmoothed <- stats::filter(weather[var], fsmooth, sides = 2)
  weather$dofm <- format(weather$date, "%d")
  weather$m <- format(weather$date, "%m")
  weather$dateDummy <- as.Date(paste("2000", weather$m, weather$dofm, sep = "-"))
  # p <- ggplot(weather[!is.na(weather$maTemp), ]) + theme_bw() + 
  p <- ggplot(weather) + theme_bw() + 
    scale_x_date(breaks = "2 months", labels = scales::date_format("%B")) + 
    ggtitle(tx) +
    xlab("") + ylab(paste(tx, "(F)")) +
    scale_colour_discrete(name = "Year")
  
  if(type == "relative") {
    p <- p + geom_line(aes(x = dateDummy, y = maTemp, col = factor(year)))
  } else if(type == "actual") {
    p <- p + geom_line(aes(x = dateDummy, y = actualSmoothed, col = factor(year)))   
  }
  
  p
  # list("data" = weather, "plot" = p)
}



geocode <- function(name, quietly = TRUE) {
  if(!exists("google_key")) {
    stop("Must Register a Google Maps API Key to Search by City/Address")
  }
  name <- enc2utf8(gsub(" ", "%20", name))
  urlx <- paste("https://maps.google.com/maps/api/geocode/json?address=", 
                      name, "&key=", google_key, sep = "")

  if(!quietly) print(paste("Attempting to query:", urlx))
  tmp <- httr::GET(urlx)
  results <- jsonlite::fromJSON(httr::content(tmp, "text"))$results
  
  if(nrow(results) != 1) {
    warning(paste("Ambiguous City/Address", name))
    return(NULL)
  }
  
  code <- as.numeric(results$geometry$location)
  names(code) <- c("lat", "lon")
  
  urly <- paste("https://maps.googleapis.com/maps/api/elevation/json?locations=", 
                code['lat'], ",", code['lon'], "&key=", google_key, sep = "")
  tmp <- httr::GET(urly)
  results <- jsonlite::fromJSON(httr::content(tmp, "text"))$results
  code <- c(code, "elevation" = as.numeric(results['elevation']))
  
  Sys.sleep(.15)
  code
}


getElevation <- function(name, quietly = TRUE) {
  if(!exists("google_key")) {
    stop("Must Register a Google Maps API Key to Search by City/Address")
  }
  name <- enc2utf8(gsub(" ", "%20", name))
  # urlx <- paste("https://maps.google.com/maps/api/elevation/json?sensor=false&address=", 
  #              name, "&key=", google_key, sep = "")
  
  urlx <- paste("https://maps.googleapis.com/maps/api/elevation/json?address=", 
                name, sep = "")
  
  if(!quietly) print(paste("Attempting to query:", urlx))
  tmp <- httr::GET(urlx)
  results <- jsonlite::fromJSON(httr::content(tmp, "text"))$results
  
  if(nrow(results) != 1) {
    warning(paste("Ambiguous City/Address", name))
    return(NULL)
  }
  
  geocode <- as.numeric(results$geometry$location)
  names(geocode) <- c("lat", "lon")
  
  Sys.sleep(.15)
  geocode
}



assignStations <- function(dset, varname, quietly = TRUE) {
  locs <- unique(dset[, varname])
  stationInfo <- do.call('rbind', lapply(locs, function(x) {
    if(!quietly) {
      print(paste("Assigning Station for", x))
    }
    stations <- stationSearch(x, nClosest = 1)
    if(is.null(stations)) return(NULL)
    names(stations)[names(stations) == "id"] <- "stationid"
    stations[varname] <- x
    stations
  }))

  merge(dset, 
        stationInfo[, c(varname, "stationid", "name", "datacoverage", "elevation", "milesDistant")], 
        by = varname, all.x = TRUE)
  
}



# -------------Mapping utilities

stationMap <- function(x, ...) {
  UseMethod("stationMap")
}

# Map just the station locations
stationMap.data.frame <- function(stations, zoom = 10) {

  location = c("lon" = attributes(stations)$lon,
               "lat" = attributes(stations)$lat)
  
  df <- subset(stations, select = c(name, longitude, latitude))
  df <- rbind(df, data.frame("name" = "Search Location", 
                             "longitude" = location[1], 
                             "latitude" = location[2]))
  
  
  mapTmp <- ggmap::get_map(location, maptype = "terrain", color = "bw", zoom = zoom)
  ggmap::ggmap(mapTmp) + 
    ggplot2::geom_point(data = df[-nrow(df), ], ggplot2::aes(longitude, latitude, col = name), size = 6) +
    ggplot2::geom_point(data = df[nrow(df), ], ggplot2::aes(longitude, latitude, col = name), shape = 17, size = 4) +
    ggplot2::scale_colour_discrete(name = "Weather Station") +
    ggplot2::ggtitle(paste(" NOAA GHCN Weather Stations Near", attributes(stations)$gmapsSearch)) +
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")

}



stationMap.stationComp <- function(comp, zoom = 10, type = "relative", art = FALSE) {
  
  location = c("lon" = attributes(comp$stations)$lon,
               "lat" = attributes(comp$stations)$lat)
  
  dfl <- data.frame("lon" = attributes(comp$stations)$lon,
           "lat" = attributes(comp$stations)$lat)
  
  df <- merge(summary(comp, days = 1),
              comp$stations[, c("id", "longitude", "latitude", "elevation")])
  df$elevation <- df$elevation * 3.28
  
  df$lat2 <- df$latitude + .015
  
  if(type == "relative") {
    legLabel <- "Relative Temp(F)"
    vname <- "relativeTemp"
  } else if(type == "actual") {
    legLabel <- "Average Temp (F)"
    vname <- "aveTemp"
  } else {
    stop(paste("Unrecognized plot type", type))
  }
  
  
  mapTmp <- ggmap::get_map(location, maptype = "terrain", color = "bw", zoom = zoom)
  p <- ggmap::ggmap(mapTmp) + 
    ggplot2::geom_point(data = dfl, ggplot2::aes(lon, lat), col = "black", size = 4, shape = 2) +
    ggplot2::scale_colour_continuous(low = "blue", high = "red", name = legLabel) +
    ggplot2::ggtitle(paste("NOAA GHCN Weather Stations Near", attributes(comp$stations)$gmapsSearch, "\n",
                           min(comp$data$date), "to", max(comp$data$date))) +
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")
  
  if(art) {
    p <- p + ggplot2::geom_point(data = df, ggplot2::aes_string(x = "longitude", y = "lat2", col = vname, size = "elevation")) +
      ggplot2::scale_size_continuous(name = "Elevation", range = c(2.5, 5))
  } else {
    p <- p + ggplot2::geom_text(data = df, ggplot2::aes_string(x = "longitude", y = "lat2", col = vname, label = "name", size = "dataFrac")) +
      ggplot2::geom_point(data = df, ggplot2::aes_string(x = "longitude", y = "latitude", col = vname), size = 4, shape = 2) +
      ggplot2::scale_size_continuous(name = "Data Fraction", range = c(2, 4))
  }
  
  p  
}