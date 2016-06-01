


#' Initialize a new "term" - temperature energy regression model
#' 
#' Returns a new term object to populate with data, weather, and methods
#' 
#' @param name an optional name for the term. This will go in tables and graphs
#' @param address an optional address. If specified then the \code{\link{addWeather}} 
#'  function will automatically grab the closest weather station.
#' @param sqft an optional building size. If you specify a square feet, it will assume
#'  that you're dealing in total kWh and will convert that to EUI
#' 
#' @seealso \code{\link{addData}} \code{\link{addWeather}} \code{\link{addMethod}}
#' 
#' @return the new term object
#' 
#' @examples
#' # Show available datasets
#' newTerm("Ecotope Mothership", "4056 9th Ave NE Seattle, WA", 2600)
newTerm <- function(name = NULL, address = NULL, sqft = NULL) {
  term <- list()
  term$data <- NULL
  term$weather <- list()
  term$methods <- list()
  term$models <- list()
  class(term) <- "term"
  if(!is.null(name)) {
    attr(term, "name") <- name
  }
  if(!is.null(address)) {
    attr(term, "address") <- address
  }
  if(!is.null(sqft)) {
    attr(term, "sqft") <- sqft
  }
  return(term)
}


#' Add data to a "term" object
#' 
#' Adds a dataset & instructions to a term object, returns the updated object
#' 
#' @param term the term object to add data
#' @param data the dataset to add
#' @param formula an optional specification for energy variable and date variables,
#'  for example kwhd ~ dateStart + dateEnd.
#' @param interval an optional specification for interval data. "daily" or "monthly"
#' @param energyVar an optional manual specification of the energy variable of the
#'  dataset.
#' @param dateStartVar an optional manual specification of the start date variable.
#' @param dateEndVar an optional manual specification of the end date variable.
#' @param daysVar an optional specification of the days in cycle variable (mainly 
#'  for billing data). Note that you only need to specify any two of dateStartVar,
#'  dateEndVar, and daysVar if you do the manual specification.
#' @param daily whether or not the energy use is total across the interval or daily.
#'  Mostly this is daily by the time the analyst gets it, but if dealing with raw
#'  consumption specify daily = FALSE
#' 
#' @seealso \code{\link{newTerm}} \code{\link{addWeather}} \code{\link{addMethod}}
#' 
#' @return the term object with data added
#' 
#' @examples
#' # Ecotope Mothership data
#' data(ecotope)
#' mod <- newTerm("Ecotope Mothership")
#' mod <- addData(mod, ecotope, kwhd ~ dateStart + dateEnd)
#' mod <- addData(mod, ecotope, energyVar = "kwhd", dateStartVar = "dateStart", dateEndVar = "dateEnd")
addData <- function(term, data, formula = NULL, interval = NULL, energyVar = NULL, dateStartVar = NULL, dateEndVar = NULL, daysVar = NULL, daily = TRUE) {
  if(!inherits(term, "term")) {
    stop("Must add data to a term object. See help(addData)")
  }
  
  if(!is.null(term$data)) {
    warning("Data already found, will overwrite")
  }
  
  
  # First check for a formula
  if(!is.null(formula)) {
    # Need to parse the formula
    mf <- model.frame(formula, data)
    y <- model.response(mf)
    term$data <- data.frame(mf)
    eName <- tolower(names(term$data)[1])
    if(length(grep("gas", eName)) | length(grep("therm", eName)) | length(grep("thm", eName))) {
      isGas <- TRUE
      attr(term, "gas") <- TRUE
    } else {
      isGas <- FALSE
      attr(term, "gas") <- FALSE
    }
    names(term$data)[1] <- "energy"
    
    # We're going to coerce POSIX times to dates... note that 
    # none of this methodology works sub-daily anyway.
    term$data <- data.frame(lapply(term$data, function(x) {
      if(inherits(x, "POSIXt")) {
        as.Date(x)
      } else if(inherits(x, "factor") | inherits(x, "character")) {
        tmp <- lubridate::mdy(x)
        if(sum(!is.na(tmp)) == 0) {
          tmp <- lubridate::ymd(x)
          if(sum(!is.na(tmp)) == 0) {
            warning("Could not parse the dates. Set them as either 2015-05-20 of 5/20/2015")
          }
        }
        as.Date(tmp)
      } else {
        x
      }
    }))
    
    # Parse the classes of the variables to decide how to set up the model.
    varClasses <- lapply(term$data, class)
    
    # Case 1.. three variables, assume either start date + end date
    #  or end date + days of service
    if(ncol(term$data) == 3) {
      if(varClasses[2] == "Date" & varClasses[3] == "Date") {
        # Two dates mean a start date & an end date
        term$data$diffs <- as.numeric(difftime(term$data[, 2], term$data[, 3]))
        if(sum(term$data$diffs > 0) == 0) {
          names(term$data)[2:3] <- c("dateStart", "dateEnd")
        } else if(sum(term$data$diffs < 0) == 0) {
          names(term$data)[2:3] <- c("dateEnd", "dateStart")
        } else {
          stop("Interval end dates must all be greater than start dates")
        }
        term$data$diffs <- NULL
      } else if((varClasses[2] == "numeric" & varClasses[3] == "Date") | 
                  (varClasses[2] == "Date" & varClasses[3] == "numeric")) {
        dateVar <- which(varClasses[-1] == "Date")
        daysVar <- which(varClasses[-1] == "numeric")
        
        if(sum(term$data[, -1][, daysVar] > 0) == 0) {
          names(term$data)[-1][dateVar] <- "dateEnd"
        } else {
          names(term$data)[-1][dateVar] <- "dateEnd"
        }
        term$data[, -1][, daysVar] <- abs(term$data[, -1][, daysVar])
        names(term$data)[-1][daysVar] <- "days"
        
        
      } else {
        stop("Couldn't recognize date/time variables")
      }

    } else if(ncol(term$data) == 2) {
      if(!(inherits(term$data[, 2], "Date") & !inherits(term$data[, 2], "POSIXt"))) {
        stop("Right Hand Side must be either a Date or Time class")
      }
      if(interval == "monthly") {
        term$data$dateStart <- lubridate::floor_date(term$data[, 2], "month")
        term$data$dateEnd <- term$data$dateStart %m+% months(1)
        # term$data$days <- term$data$dateEnd - term$data$dateStart
      } else if(interval == "daily") {
        names(term$data)[2] <- "dateStart"
        term$data$dateEnd <- term$data$dateStart + lubridate::days(1)
      } else if(interval == "weekly") {
        names(term$data)[2] <- "dateStart"
        term$data$dateEnd <- term$data$dateStart + lubridate::days(7)
      } else if(varClasses[2] == "Date") {
        warning("One Date variable found, assuming this is the end date")
        names(term$data)[2] <- "dateEnd"
        term$data$days <- c(NA, as.numeric(diff(term$data[, 2])))
        term$data$days[1] <- mean(term$data$days, na.rm = TRUE)
        term$data$dateStart <- term$data$dateEnd - term$data$days
      }
    }
    
  } else if(!is.null(energyVar)) {
    # Else check for manually specified variables
    term$data <- data.frame("energy" = data[, energyVar])
    if(!is.null(dateStartVar)) {
      tmp <- as.Date(data[, dateStartVar])
      term$data$dateStart <- tmp
    }
    if(!is.null(dateEndVar)) {
      tmp <- as.Date(data[, dateEndVar])
      term$data$dateEnd <- tmp
    }
    if(!is.null(daysVar)) {
      tmp <- as.numeric(as.character(data[, daysVar]))
      term$data$days <- tmp
    }
  }
  
  
  # Can specify two of start date, end date, and days... fill in missing
  # if applicable
  ds <- is.null(term$data$dateStart)
  de <- is.null(term$data$dateEnd)
  da <- is.null(term$data$days)
  
  if(!ds & !de & da) {
    term$data$days <- as.numeric(difftime(term$data$dateEnd, term$data$dateStart, units = "days"))
  } else if(!ds & de & !da) {
    term$data$dateEnd <- term$data$dateStart + term$data$days
  } else if(ds & !de & !da) {
    term$data$dateStart <- term$data$dateEnd - term$data$days
  } else if(!ds & !de & !da) {
    # 
  } else  {
    stop("Error, must specify 2 of the following: start date, end date, days")
  }
  
  if(daily) {
    term$data$dailyEnergy <- term$data$energy
    term$data$energy <- term$data$dailyEnergy * term$data$days
  } else {
    term$data$dailyEnergy <- term$data$energy / term$data$days
    if(!is.null(attr(term, "sqft"))) {
      if(isGas) {
        term$data$dailyEnergy <- term$data$dailyEnergy * 100 / attr(term, "sqft") * 365
      } else {
        term$data$dailyEnergy <- term$data$dailyEnergy * 3.412 / attr(term, "sqft") * 365
      }
      
    }
  }
  
  
  
  term
}




#' Add weather to a "term" object
#' 
#' Adds a weather & instructions to a term object, returns the updated object. 
#' If you specified an address in the term construction with \code{\link{newTerm}} 
#' then this function will take care of everything automatically. Note that you 
#' can add multiple weather stations to the "term" object with repeated calls of
#' this function.
#' 
#' @param term the term object to add data
#' @param stationid optional GHCN ID as found from \code{\link{stationSearch}}. 
#' @param weather optional dataset if providing your own weather (rather than
#'  using the built-in integration with NOAA).
#' @param formula optional specification of time variable and temp variable if
#'  providing your own weather. Example temp ~ date. 
#' @param timeVar optional manual specification of date variable if providing your
#'  own weather data.
#' @param tempVar optional manual specification of temp variable if providing your
#'  own weather data.
#' @param name optional name for this particular weather. Example name = "Sea-Tac" 
#'  is way better than "SEATTLE-TACOMA INTERNATIONAL AIRPORT"
#' 
#' @seealso \code{\link{newTerm}} \code{\link{addData}} \code{\link{addMethod}}
#' 
#' @return the term object w/ weather added
#' 
#' @examples
#' # Add weather
#' stationSearch("Seattle, WA")
#' mod <- addWeather(mod, stationid = "GHCND:USW00024234")
addWeather <- function(term, weather = NULL, formula = NULL, stationid = NULL, timeVar = NULL, tempVar = NULL, name = NULL) {
  
  ind <- length(term$weather)
  # Check to see if this weather needs a name
  if(is.null(name)) {
    name <- paste0("weather", ind)
  }
  
  if(name %in% names(term$weather)) {
    stop(paste("Weather named", name, "already added to TERM"))
  }
  
  # If there are no inputs & an address was specified blindly take the first
  if(is.null(stationid) & is.null(weather) & !is.null(attr(term, "address"))) {
    address <- attr(term, "address")
    stations <- stationSearch(address)
    print("Using closest weather station to specified address")
    print(stations[1, ])
    stationid <- stations$id[1]
  }
  
  # Check if a stationid was entered for read.ghcn
  if(!is.null(stationid)) {
    if(!is.null(term$data)) {
      minDate <- min(term$data$dateStart)
      maxDate <- max(term$data$dateEnd)
      term$weather[[name]] <- read.ghcn(stationid, minDate, maxDate)
      attr(term$weather[[name]], "stationid") <- stationid
    } else {
      warning(paste("With no data, addWeather does not know how much weather data to query",
                    "See help(addData) or help(addWeather)"))
    }
  } else if(!is.null(weather)) {
    # Check if custom weather was provided
    if(!is.null(timeVar) & !is.null(tempVar)) {
      term$weather[[name]] <- data.frame("aveTemp" = weather[, tempVar])
      if(inherits(weather[, timeVar], "POSIXt")) {
        term$weather[[name]]$time <- weather[, timeVar]
        term$weather[[name]]$date <- as.Date(weather[, timeVar])
      } else if(inherits(weather[, timeVar], "Date")) {
        term$weather[[name]]$date <- weather[, timeVar]
      } else {
        stop("Weather data time variable should be either Date class or POSIX time class")
      }
    } else if(!is.null(formula)) {
      # Need to parse the formula
      mf <- model.frame(formula, weather)
      term$weather[[name]] <- data.frame(mf)
      if(!inherits(term$weather[[name]][, 1], "numeric")) {
        stop("Temperature variable must be numeric")
      }
      names(term$weather[[name]])[1] <- "aveTemp"
      if(inherits(term$weather[[name]][, 2], "POSIXt")) {
        names(term$weather[[name]])[2] <- "time"
        term$weather[[name]]$date <- as.Date(term$weather[[name]]$time)
      # } else if(inherits(term$weather[[name]][, 1], "Date")) {
      } else if(inherits(term$weather[[name]][, 2], "Date")) {
        names(term$weather[[name]])[2] <- "date"
      } else {
        stop("Time variable must be either POSIXt or Date")
      }
      # names(term$data)[1] <- "energy"
    } else if(!is.null(weather$date) & !is.null(weather$aveTemp)) {
      term$weather[[name]] <- weather  
    } else {
      stop(paste("When providing your own weather, must specify timeVar and tempVar",
           "See help(addWeather)"))
    }
  } else {
    stop("Must enter either a stationid, a weather dataset, or associate an address w/ the TERM")
  }
  

  # Interpolate missing values, if necessary
  if(!is.null(term$weather[[name]]$time)) {
    tvar <- "time"
  } else {
    tvar <- "date"
  }
  g <- splinefun(term$weather[[name]][, tvar], term$weather[[name]]$aveTemp, method = "natural")
  
  term$weather[[name]]$rawTemp <- term$weather[[name]]$aveTemp
  term$weather[[name]]$aveTemp <- g(term$weather[[name]][, tvar])
  term$weather[[name]]$missing <- FALSE
  missingTemps <- is.na(term$weather[[name]]$rawTemp)
  if(sum(missingTemps) > 0) {
    term$weather[[name]]$missing[missingTemps] <- TRUE
  }
  
  term
}


#' Add a method to a "term" object
#' 
#' Adds an evaluation method to a "term" object. Current options are for a 
#' change point model, Variable Base Degree Day (VBDD or PRISM) model, or a
#' Bayesian change point model. Note that you can add multiple methods to the 
#' same "term" with repeated calls of this function.
#' 
#' For the bayesian "web" method, you can modify the prior through "Mean" and 
#' "Sd" suffixes to the parameter names. For example, for the "baseLoad" 
#' parameter you can specify a "baseLoadMean" and "baseLoadSd" for prior 
#' mean and standard deviation of the base load.
#' 
#' For the frequentist "changepoint" and "degreeday" methods, you can specify
#' whether to generate bootstrap standard errors with se = TRUE/FALSE & the 
#' number of bootstrap replicates with "nreps". In addition you can tweak the 
#' L1 penalty strength by specifying "lambda"
#' 
#' @param term the term object to add the method.
#' @param method the method name "changepoint", "degreeday", or "web"
#' @param name an optional name for this method
#' @param ... optional extra parameters for the specific methodology. 
#' 
#' @seealso \code{\link{newTerm}} \code{\link{addData}} \code{\link{addMethod}}
#' 
#' @return the term object w/ the method added
#' 
#' @examples
#' stationSearch("Seattle, WA")
#' mod <- addMethod(mod, "changepoint")
#' mod <- addMethod(mod, "degreeday")
#' mod <- addMethod(mod, "web", name = "normal")
#' mod <- addMethod(mod, "web", baseLoadMean = 100, name = "higher base")
#' mod <- addMethod(mod, "cp", se = FALSE, lambda = 20)
addMethod <- function(term, method, name = NULL, ...) {
  

  controls <- eval(substitute(alist(...)))
  
  # I need default options... store them here
  if(tolower(method) %in% c("change-point", "changepoint", "cp")) {
    method <- "cp"
    defaults <- list(heating = NULL, cooling = NULL, intercept = TRUE, se = TRUE, nreps = 200, parametric = FALSE, lambda = 12, selection = "L1")
  } else if(tolower(method) %in% c("degree-day", "degreeday", "dd")) {
    method <- "dd"
    defaults <- list(heating = NULL, cooling = NULL, intercept = TRUE, se = TRUE, nreps = 200, parametric = FALSE, lambda = 8, selection = "L1") 
  } else if(tolower(method) %in% c("web", "bayes", "bayesian")) {
    method <- "web"
    defaults <- list(heating = NULL, cooling = NULL, intercept = TRUE, selection = "L1", lambda = 10, 
                     baseLoadMean = 50, baseLoadSd = 35, 
                     heatingBaseMean = 55, heatingBaseSd = 10,
                     heatingSlopeMean = 15, heatingSlopeSd = 10,
                     coolingBaseMean = 75, coolingBaseSd = 15,
                     coolingSlopeMean = 15, coolingSlopeSd = 10) 
  } else {
    stop(paste("Unrecognized Method", method))
  }

  ind <- length(term$methods[grep(method, names(term$methods))])
  # Check to see if this weather needs a name
  if(is.null(name)) {
    if(ind) {
      name <- paste0(method, ind)
    } else {
      name <- method
    }
  } else {
    name <- make.names(paste(method, name))
  }
  
  if(name %in% names(term$methods)) {
    stop(paste("Method named", name, "already added to TERM"))
  }

  
  
  # Loop through the default controls, over-riding where necessary
  toUse <- lapply(names(defaults), function(x) {
    if(!is.null(controls[[x]])) {
      tmp <- controls[[x]]
    } else {
      tmp <- defaults[[x]]
    }
  })
  names(toUse) <- names(defaults)
  
  term$methods[[name]] <- toUse
  
  
  term
}


#' Evaluate a term
#' 
#' Evaluates a "term" that has had data, weather, and method(s) added
#' 
#' @param term the term object to evaluate.
#' 
#' @seealso \code{\link{newTerm}} \code{\link{addData}} 
#' \code{\link{addWeather}} \code{\link{addMethod}}
#' 
#' @return the evaluated term object
evaluate <- function(term) {
  
  # term <- linkWeatherToData(term)
  
  methodWeather <- expand.grid(seq_along(term$methods), seq_along(term$weather))
  methodWeather$name <- make.names(paste(names(term$methods)[methodWeather[, 1]],
                              names(term$weather)[methodWeather[, 2]]))
  
  models <- Map(function(method, weather) {
    evalOne(term, method, weather)
  }, methodWeather[, 1], methodWeather[, 2])
  names(models) <- methodWeather$name
  
  term$models <- models
  term
}

evalOne <- function(term, method, weather) {
  
  # Pull out the master data and the current weather, for linking...
  dset <- term$data
  weather <- term$weather[[weather]]
  
  # Link the requested weather to this data
  tmp <- linkOneToData(dset, weather)
  dset <- tmp$dset
  weather <- tmp$weather
  
  #lowerName <- tolower(names(term$methods)[method])
  if(length(grep("cp", names(term$methods)[method]))) {
    mod <- cplm(dset, weather, term$methods[[method]])
  } else if(length(grep("dd", names(term$methods[method])))) {
    mod <- ddlm(dset, weather, term$methods[[method]])
  } else if(length(grep("web", names(term$methods[method])))) {
    mod <- web(dset, weather, term$methods[[method]])
  } else {
    warning(paste("Unrecognized method", names(method), "skipping"))
  }
  
  if(!is.null(attr(term, "sqft"))) {
    attr(mod, "sqft") <- attr(term, "sqft")
  }
  
  if(!is.null(attr(weather, "stationid"))) {
    attr(mod, "stationid") <- attr(weather, "stationid")
  }
  
  return(mod)
}

#' Extract an individual model from a fitted term
#' 
#' Pulls on individual model fit out of a term object. For example 
#' if you fitted both "degreeday" and "changepoint" you can pull out 
#' just the change point model with extractModel(mod, "cp")
#' 
#' @param term the term object to extract from.
#' @param name a regex matching the name of the model to extract
#' 
#' @seealso \code{\link{newTerm}} \code{\link{addData}} 
#' \code{\link{addWeather}} \code{\link{addMethod}} \code{\link{evaluate}}
#' 
#' @return the extracted model
extractModel <- function(term, name) {
  models <- names(term$models)
  toSelect <- grep(name, models)
  if(length(toSelect) > 1) {
    stop(paste("Ambiguous Model name", name))
  } else if(length(toSelect) == 0) {
    stop(paste("Model", name, "not found"))
  }
  toReturn <- term$models[[toSelect]]
  attr(toReturn, "gas") <- attr(term, "gas")
  if(!is.null(attr(term, "sqft"))) {
    attr(toReturn, "sqft") <- attr(term, "sqft")
  }
  toReturn
}

