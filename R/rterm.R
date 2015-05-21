

newTerm <- function(name = NULL) {
  term <- list()
  term$data <- NULL
  term$weather <- list()
  term$methods <- list()
  term$models <- list()
  class(term) <- "term"
  if(!is.null(name)) {
    attr(term, "name") <- name
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
          names(term$data)[-1][dateVar] <- "dateStart"
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
        term$data$dateStart <- lubridate::floor_date(term$data[, 2])
        term$data$dateEnd <- term$data$dateStart %m+% months(1)
      } else if(interval == "daily") {
        names(term$data)[2] <- "dateStart"
        term$data$dateEnd <- term$data$dateStart + lubridate::days(1)
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
      } else if(inherits(term$weather[[name]][, 1])) {
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
  
  
#   
#   
#   if(sum(missingTemps) > 0) {
#     if(sum(missingTemps) / length(missingTemps) > .25) {
#       warning("More than 25% of temperatures are missing. Consider another weather source")
#     }
#     
#     term$weather[[name]]$missing <- FALSE
#     term$weather[[name]]$missing[missingTemps] <- TRUE
#     
#     x <- term$weather[[name]]
#     x$sin1 <- sin(as.numeric(x$date) / 365 * 2 * pi)
#     x$cos1 <- cos(as.numeric(x$date) / 365 * 2 * pi)
#     
#     mod <- lm(aveTemp ~ sin1 + cos1, data = x)
#     x$fitted <- predict(mod, x)
#   
#     term$weather[[name]]$aveTemp[missingTemps] <- x$fitted[missingTemps]
#     term$weather$sin1 <- NULL
#     term$weather$sin2 <- NULL
#   }

  
  term
}


addMethod <- function(term, method, name = NULL, ...) {
  

  controls <- eval(substitute(alist(...)))
  
  # I need default options... store them here
  if(tolower(method) %in% c("change-point", "changepoint", "cp")) {
    method <- "cp"
    defaults <- list(heating = NULL, cooling = NULL, intercept = TRUE, se = TRUE, nreps = 200, parametric = NULL, lambda = 12)
  } else if(tolower(method) %in% c("degree-day", "degreeday", "dd")) {
    method <- "dd"
    defaults <- list(heating = NULL, cooling = NULL, intercept = TRUE, se = TRUE, nreps = 200, parametric = NULL, lambda = 8) 
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
  } else {
    warning(paste("Unrecognized method", names(method), "skipping"))
  }
  
  return(mod)
}


ddlm <- function(data, weather, controls) {
  coefs <- ddlm.fit(data, weather, controls$heating, controls$cooling, controls$intercept, controls$lambda)
  
  mod <- list()
  mod$LS <- coefs$LS
  mod$L1 <- coefs$L1
  mod$data <- data
  
  # Choose the model fit to use... will have to add logic
  attr(mod, "fit") <- "LS"
  
  # Create derived variables
  if(attr(mod, "fit") == "LS") {
    coefs <- mod$LS
  } else {
    coefs <- mod$L1
  }
  
  if(!is.na(coefs['heatingBase'])) {
    mod$data$xHeating <- deriveOne(weather, coefs['heatingBase'], type = 2L, heatcool = 1L, n = nrow(data))
  }
  if(!is.na(coefs['coolingBase'])) {
    mod$data$xCooling <- deriveOne(weather, coefs['coolingBase'], type = 2L, heatcool = 2L, n = nrow(data))
  }
  mod$data$temp <- deriveOne(weather, coefs['coolingBase'], type = 3L, heatcool = 1L, n = nrow(data))

  class(mod) <- c("ddlm", "cplm", "xlm")

  mod$data$fitted <- fitted(mod)
  
  return(mod)
}

fitted.xlm <- function(mod, fit = NULL) {
  
  #Check if we're using the LS or L1 fitted coefficients
  if(is.null(fit)) {
    fit <- attr(mod, "fit")
  }
  
  coefs <- coef(mod, fit, silent = TRUE)
  
  if(!is.na(coefs['baseLoad'])) {
    toReturn <- rep(coefs['baseLoad'], nrow(mod$data))
  } else {
    toReturn <- rep(0, nrow(mod$data))
  }
  
  if(!is.null(mod$data$xHeating)) {
    toReturn <- toReturn + coefs['heatingSlope'] * mod$data$xHeating
  }
  if(!is.null(mod$data$xCooling)) {
    toReturn <- toReturn + coefs['coolingSlope'] * mod$data$xCooling
  }
  
  return(as.numeric(toReturn))
}

cplm <- function(data, weather, controls) {
  coefs <- cplm.fit(data, weather, controls$heating, controls$cooling, controls$intercept, controls$lambda)

  mod <- list()
  mod$LS <- coefs$LS
  mod$L1 <- coefs$L1
  mod$data <- data
  
  # Choose the model fit to use... will have to add logic
  attr(mod, "fit") <- "LS"
  
  # Create derived variables
  if(attr(mod, "fit") == "LS") {
    coefs <- mod$LS
  } else {
    coefs <- mod$L1
  }
  
  if(!is.na(coefs['heatingBase'])) {
    mod$data$xHeating <- deriveOne(weather, coefs['heatingBase'], type = 1L, heatcool = 1L, n = nrow(data))
  }
  if(!is.na(coefs['coolingBase'])) {
    mod$data$xCooling <- deriveOne(weather, coefs['coolingBase'], type = 1L, heatcool = 2L, n = nrow(data))
  }
  mod$data$temp <- deriveOne(weather, coefs['coolingBase'], type = 3L, heatcool = 1L, n = nrow(data))
  
  class(mod) <- c("cplm", "xlm")
  mod$data$fitted <- fitted(mod)
  
  return(mod)
  
}


xlm.fit <- function(data, weather, heating = TRUE, cooling = FALSE, intercept = TRUE, type = 1L) {
  
  if(type == 1) {
    y <- as.numeric(data$dailyEnergy)
  } else if(type == 2) {
    y <- as.numeric(data$dailyEnergy)
  }
  
  if(!heating & !cooling) {
    return(c("baseLoad" = mean(data$dailyEnergy)))
  }
  
  coefs <- .Call("findBaseTemp", 
                 as.numeric(weather$aveTemp),
                 as.integer(weather$rows), 
                 y,
                 rep(1, nrow(data)),
                 as.integer(heating), as.integer(cooling), 
                 as.integer(type), as.integer(intercept))
  
  # Now we need to name them
  if(intercept) {
    if(heating & cooling) {
      names(coefs) <- c("baseLoad", "heatingBase", "heatingSlope", "coolingBase", "coolingSlope")
    } else if(heating) {
      names(coefs) <- c("baseLoad", "heatingBase", "heatingSlope")
    } else if(cooling) {
      names(coefs) <- c("baseLoad", "coolingBase", "coolingSlope")
    } else {
      names(coefs) <- c("baseLoad")
    }
  } else {
    if(heating & cooling) {
      names(coefs) <- c("heatingBase", "heatingSlope", "coolingBase", "coolingSlope")
    } else if(heating) {
      names(coefs) <- c("heatingBase", "heatingSlope")
    } else if(cooling) {
      names(coefs) <- c("coolingBase", "coolingSlope")
    } else {
      warning("No base load, heating, or cooling results in no model")
    }
  }
  
  return(coefs)
}


cplm.fit <- function(data, weather, heating = NULL, cooling = NULL, intercept = TRUE, lambda = 0) {
  if(is.null(weather$rows)) {
    stop("Must link data to weather before model fitting")
  }
  
  l1Results <- .Call("l1", as.numeric(weather$aveTemp),
                as.integer(weather$rows),
                as.numeric(data$dailyEnergy),
                lambda, 1L, as.integer(intercept))

  if(intercept) {
    names(l1Results) <- c("baseLoad", "heatingBase", "heatingSlope", 
                          "coolingBase",  "coolingSlope")    
  } else {
    names(l1Results) <- c("heatingBase", "heatingSlope", 
                          "coolingBase",  "coolingSlope")    
  }

  
  if(is.null(heating)) {
    heating <- as.numeric(l1Results['heatingSlope']) > 0
  }
  if(is.null(cooling)) {
    cooling <- as.numeric(l1Results['coolingSlope']) > 0
  }

  lsResults <- c("baseLoad" = NA, "heatingBase" = NA,
                 "heatingSlope" = NA, "coolingBase" = NA,
                 "coolingSlope" = NA)
  if(!heating & !cooling & !intercept) {
    
  } else if(!heating & !cooling) {
    lsResults['baseLoad'] <- mean(data$energy)
  }
  
  lsCoefs <- xlm.fit(data, weather, heating, cooling, intercept, 1L)

  for(coef in names(lsResults)) {
    if(!is.null(lsCoefs[coef])) {
      lsResults[coef] <- lsCoefs[coef]
    }
  }
  
  # Add logic to check whether we conclusively found a change point
  
  return(list("LS" = lsResults, "L1" = l1Results))
}



ddlm.fit <- function(data, weather, heating = NULL, cooling = NULL, intercept = TRUE, lambda = 7) {
  if(is.null(weather$rows)) {
    stop("Must link data to weather before model fitting")
  }
  
  l1Results <- .Call("l1", as.numeric(weather$aveTemp),
                  as.integer(weather$rows),
                  as.numeric(data$dailyEnergy),
                  lambda, 2L, as.integer(intercept))

  if(intercept) {
    names(l1Results) <- c("baseLoad", "heatingBase", "heatingSlope", 
                          "coolingBase",  "coolingSlope")    
  } else {
    names(l1Results) <- c("heatingBase", "heatingSlope", 
                          "coolingBase",  "coolingSlope")    
  }
  
  if(is.null(heating)) {
    heating <- as.numeric(l1Results['heatingSlope']) > 0
  }
  if(is.null(cooling)) {
    cooling <- as.numeric(l1Results['coolingSlope']) > 0
  }
  
  
  lsCoefs <- xlm.fit(data, weather, heating, cooling, intercept, 2L)
  
  # In the case of degree day, we need to refit the model in R 
  # accounting for different time intervals in the weather data...
  if(!is.null(weather$time) & 0) {
    days <- median(diff(as.numeric(weather$time)) / 3600 / 24)
    # print(paste("Scaling by # of days =", days))
    if(heating) {
      data$xheating <- .Call("deriveVar", 
                             as.numeric(weather$aveTemp), 
                             as.integer(weather$rows), 
                             as.numeric(coefs['heatingBase']), 
                             as.integer(nrow(data)), 
                             1L, 2L) * days
    }
    
    if(cooling) {
      data$xcooling <- .Call("deriveVar", 
                             as.numeric(weather$aveTemp), 
                             as.integer(weather$rows), 
                             as.numeric(coefs['coolingBase']), 
                             as.integer(nrow(data)), 
                             2L, 2L) * days
    }
    
    # Select the appropriate formula
    if(intercept & heating & cooling) {
      form <- formula(dailyEnergy ~ xheating + xcooling)
    } else if(!intercept & heating & cooling) {
      form <- formula(dailyEnergy ~ 0 + xheating + xcooling)
    } else if(intercept & heating & !cooling) {
      form <- formula(dailyEnergy ~ xheating)
    } else if(!intercept & heating & !cooling) {
      form <- formula(dailyEnergy ~ 0 + xheating)
    } else if(intercept & !heating & cooling) {
      form <- formula(dailyEnergy ~ xcooling)
    } else if(!intercept & !heating & cooling) {
      form <- formula(dailyEnergy ~ 0 + xcooling)
    }
    
    mod <- lm(form, data)
    if(heating) {
      lsCoefs['heatingSlope'] <- as.numeric(coef(mod)['xheating'])
    }
    if(cooling) {
      lsCoefs['coolingSlope'] <- as.numeric(coef(mod)['xcooling'])
    }
  }

  lsResults <- c("baseLoad" = NA, "heatingBase" = NA,
                 "heatingSlope" = NA, "coolingBase" = NA,
                 "coolingSlope" = NA)
  if(!heating & !cooling & !intercept) {
    
  } else if(!heating & !cooling) {
    lsResults['baseLoad'] <- mean(data$energy)
  }
  
  for(coef in names(lsResults)) {
    if(!is.null(lsCoefs[coef])) {
      lsResults[coef] <- lsCoefs[coef]
    }
  }

  
  return(list("LS" = lsResults, "L1" = l1Results))
}




print.term <- function(term) {

  if(!is.null(attr(term, "name", exact = TRUE))) {
    print(attr(term, "name", exact = TRUE))
  }

  if(!is.null(term$data)) {
    print(paste("Data:", 
                nrow(term$data),
                "observations from", min(term$data$dateStart),
                "to", max(term$data$dateEnd)))
  } else {
    print("No Data Associated")
  }
  
  nweather <- length(term$weather)
  if(!nweather) {
    print("No Weather Associated")
  } else {
    print(paste(nweather, "Weather Files:",
                paste(names(term$weather), collapse = ", ")))
  }
  
  nmethods <- length(term$methods)
  if(!nmethods) {
    print("No Methods Associated")
  } else {
    print(paste(nmethods, "Methods:",
                paste(names(term$methods), collapse = ", ")))
  }
  
  nmodels <- length(term$models)
  if(!nmodels) {
    print("No Models Evaluated")
  } else {
    print("Evaluated Models:")
    coefTable <- do.call("rbind", lapply(term$models, function(x) x$LS))
    coefTable <- data.frame(coefTable)
    coefTable[] <- lapply(coefTable, function(x) if(!sum(!is.na(x))) NULL else x)
    
    R2s <- lapply(term$models, function(x) {
      ssTot <- sum((x$data$dailyEnergy - mean(x$data$dailyEnergy)) ^ 2)
      ssErr <- sum((x$data$dailyEnergy - x$data$fitted) ^ 2)
      1 - ssErr / ssTot
    })
    coefTable <- cbind(coefTable, "R2" = unlist(R2s))
    
    print(coefTable)
  }
}


plot.term <- function(term, xvar = NULL) {
  nmodels <- length(term$models)
  if(!nmodels) {
    stop("No Models Evaluated, cannot plot")
  }

  # Need to get fitted values...

  abc <- do.call('rbind', lapply(seq_along(term$models), function(i) {
    x <- term$models[[i]]
    tmp <- subset(x$data, select = c(dateStart, dailyEnergy, temp, fitted))
    tmp$type <- names(term$models)[i]
    tmp
  }))
  
  meanTemps <- ddply(abc, .(dateStart), function(x) {
    c("temp" = mean(x$temp),
      "dailyEnergy" = mean(x$dailyEnergy))
  })
  
  
  if(is.null(xvar)) {
    xvar <- "temp"
  }
  
  if(xvar == "temp") {
    ggplot2::ggplot(abc) + ggplot2::theme_bw() + 
      ggplot2::geom_point(data = meanTemps, ggplot2::aes(x = temp, y = dailyEnergy)) + 
      ggplot2::geom_line(ggplot2::aes(x = temp, y = fitted, col = type)) + 
      ggplot2::ggtitle(paste(attr(term, "name", exact = TRUE), "Energy versus Temperature")) + 
      ggplot2::xlab("Average Temperature (F)") +
      ggplot2::ylab("Daily Energy (kWh)")    
  } else if(xvar == "time") {
    ggplot2::ggplot(abc) + ggplot2::theme_bw() + 
      ggplot2::geom_point(ggplot2::aes(x = dateStart, y = dailyEnergy)) + 
      ggplot2::geom_line(ggplot2::aes(x = dateStart, y = fitted, col = type))   + 
      ggplot2::ggtitle(paste(attr(term, "name", exact = TRUE), "Energy over Time")) + 
      ggplot2::xlab("Date") +
      ggplot2::ylab("Daily Energy (kWh)")      
  }


}





