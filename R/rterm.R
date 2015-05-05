

newTerm <- function() {
  term <- list()
  term$data <- NULL
  term$weather <- list()
  term$methods <- list()
  term$models <- list()
  class(term) <- "term"
  return(term)
}


addData <- function(term, data, formula = NULL, energyVar = NULL, dateStartVar = NULL, dateEndVar = NULL, daysVar = NULL, daily = TRUE) {
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
    
    # We're going to coerce POSIX times to dates... note that 
    # none of this methodology works sub-daily anyway.
    term$data <- data.frame(lapply(term$data, function(x) {
      if(inherits(x, "POSIXt")) {
        as.Date(x)
      } else {
        x
      }
    }))
    
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
        
        names(term$data)[-1][dateVar] <- "dateEnd"
        names(term$data)[-1][daysVar] <- "days"
        
        
      } else {
        stop("Couldn't recognize date/time variables")
      }

    } else if(ncol(term$data) == 2) {
      print(str(term$data))
      if(varClasses[2] == "Date") {
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
  
  
  if(!daily) {
    term$data$energy <- term$data$energy / term$data$days
  }
  
  term
}



addWeather <- function(term, stationid = NULL, weather = NULL, timeVar = NULL, tempVar = NULL, name = NULL) {
  
  ind <- length(term$weather)
  # Check to see if this weather needs a name
  if(is.null(name)) {
    name <- paste0("weather", ind)
  }
  
  if(name %in% names(term$weather)) {
    stop(paste("Weather named", name, "already added to TERM"))
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
    if(!is.null(weather$date) & !is.null(weather$aveTemp)) {
      term$weather[[name]] <- weather  
    } else if(!is.null(timeVar) & !is.null(tempVar)) {
      term$weather[[name]] <- data.frame("aveTemp" = weather[, tempVar])
      if(inherits(weather[, timeVar], "POSIXt")) {
        term$weather[[name]]$time <- weather[, timeVar]
        term$weather[[name]]$date <- as.Date(weather[, timeVar])
      } else if(inherits(weather[, timeVar], "Date")) {
        term$weather[[name]]$date <- weather[, timeVar]
      } else {
        stop("Weather data time variable should be either Date class or POSIX time class")
      }
    } else {
      stop(paste("When providing your own weather, must specify timeVar and tempVar",
           "See help(addWeather)"))
    }
    
  }
  
  # Interpolate missing values, if necessary
  missingTemps <- is.na(term$weather[[name]]$aveTemp)
  if(sum(missingTemps) > 0) {
    if(sum(missingTemps) / length(missingTemps) > .25) {
      warning("More than 25% of temperatures are missing. Consider another weather source")
    }
    
    term$weather[[name]]$missing <- FALSE
    term$weather[[name]]$missing[missingTemps] <- TRUE
    
    x <- term$weather[[name]]
    x$sin1 <- sin(as.numeric(x$date) / 365 * 2 * pi)
    x$cos1 <- cos(as.numeric(x$date) / 365 * 2 * pi)
    
    mod <- lm(aveTemp ~ sin1 + cos1, data = x)
    x$fitted <- predict(mod, x)
  
    term$weather[[name]]$aveTemp[missingTemps] <- x$fitted[missingTemps]
    term$weather$sin1 <- NULL
    term$weather$sin2 <- NULL
  }

  
  term
}


addMethod <- function(term, method, ...) {
  
  if(!(method %in% c("change-point", "degree-day"))) {
    stop(paste("Unrecognized method", method))
  }
  
  controls <- eval(substitute(alist(...)))
  
  # I need default options... store them here
  if(method == "change-point") {
    defaults <- list(heating = NULL, cooling = NULL, se = TRUE, nreps = 200, parametric = NULL, lambda = 0)
  } else if(method == "degree-day") {
    defaults <- list(heating = NULL, cooling = NULL, se = TRUE, nreps = 200, parametric = NULL, lambda = 0) 
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
  
  term$methods[[method]] <- toUse
  
  
  term
}


evaluate <- function(term) {
  
  term <- linkWeatherToData(term)
  
  methodWeather <- expand.grid(names(term$methods), names(term$weather))
  
  term <- Map(function(weather, method) {
    evalOne(term$data, term$weather[[weather]], term$methods[[method]])
  }, methodWeather[, 1], methodWeather[, 2])
  
  term
}





