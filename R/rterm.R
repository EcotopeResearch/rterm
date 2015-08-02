

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
    names(term$data)[1] <- "energy"
    
    # We're going to coerce POSIX times to dates... note that 
    # none of this methodology works sub-daily anyway.
    term$data <- data.frame(lapply(term$data, function(x) {
      if(inherits(x, "POSIXt")) {
        as.Date(x)
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
    term$data$days <- as.numeric(difftime(term$data$dateEnd, term$data$dateStart))
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
      term$data$dailyEnergy <- term$data$dailyEnergy * 3.412 / attr(term, "sqft") * 365
    }
  }
  
  
  
  term
}



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
      } else if(inherits(term$weather[[name]][, 1], "Date")) {
        names(term$weather[[name]])[2] <- "date"
      } else {
        stop("Time variable must be either POSIXt or Date")
      }
      names(term$data)[1] <- "energy"
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
    defaults <- list(heating = NULL, cooling = NULL, intercept = TRUE, selection = "L1", lambda = 10, baseLoad = 50, heatingBase = 55, heatingSlope = 15, coolingBase = 75, coolingSlope = 15) 
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
  
  return(mod)
}


extractModel <- function(term, name) {
  models <- names(term$models)
  toSelect <- grep(name, models)
  if(length(toSelect) > 1) {
    stop(paste("Ambiguous Model name", name))
  } else if(length(toSelect) == 0) {
    stop(paste("Model", name, "not found"))
  }
  term$models[[toSelect]]
}

