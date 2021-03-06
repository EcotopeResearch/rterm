# Variable transformations for a term object...



linkWeatherToData <- function(term) {
  if(is.null(term$data) | is.null(term$weather)) {
    stop("Cannot link weather to data, at least one has not been initialized")
  }
  
  term$data <- plyr::arrange(term$data, dateStart)
  
  weatherSites <- names(term$weather)
  
  term$weather <- lapply(weatherSites, function(x) {
    # print(paste("Linking Weather to Data for", x))
    y <- term$weather[[x]]
    y$rows <- .Call("linkWeatherToData", 
                    as.numeric(term$data$dateStart), 
                    as.numeric(term$data$dateEnd),
                    as.numeric(y$date))
    y$rows[y$rows <= 0] <- NA
    y <- y[!is.na(y$rows), ]
    y
  })
  
  names(term$weather) <- weatherSites

  term  
}


# Weather as an integer
linkOneToData <- function(dset, weather) {
  if(is.null(dset) | is.null(weather)) {
    stop("Cannot link weather to data, at least one has not been initialized")
  }
  
  # Sort dataset on start date for the weather linking
  dset <- plyr::arrange(dset, dateStart)
  weather$rows <- .Call("linkWeatherToData", 
                        as.numeric(dset$dateStart), 
                        as.numeric(dset$dateEnd),
                        as.numeric(weather$date))
  weather$rows[weather$rows <= 0] <- NA
  weather <- weather[!is.na(weather$rows), ]

  # Check for mismatches between weather and data time spans
  dset <- dset[unique(weather$rows), ]
  
  # Re-link to the possibly reduced data
  weather$rows <- .Call("linkWeatherToData", 
                        as.numeric(dset$dateStart), 
                        as.numeric(dset$dateEnd),
                        as.numeric(weather$date))  
  

  list("dset" = dset, "weather" = weather)
}




deriveVar <- function(term, type, base = 60, cooling = FALSE, weather = NULL) {
  
  if(cooling) {
    heatcool <- 2L
    suffix <- "xcool"
  } else {
    heatcool <- 1L
    suffix <- "xheat"
  }
  
  if(type %in% c("change-point", "cp", "changepoint")) {
    ctype <- 1L
    suffix2 <- "cp"
  } else if(type %in% c("degree-day", "dd", "degreeday")) {
    ctype <- 2L
    suffix2 <- "dd"
  } else if(type %in% c("oat", "average")) {
    ctype <- 3L
    suffix <- "oat"
    suffix2 <- ""
    base <- 0
  }
  
  if(!is.null(weather)) {
    if(is.numeric(weather)) {
      
    } else if(is.character(weather)) {
      
    }
  }
  
  newvars <- lapply(term$weather, function(x) {
    deriveOne(x, base, ctype, heatcool, nrow(term$data))
  })
  names(newvars) <- make.names(paste(suffix, suffix2, names(term$weather), base))
  
  alreadyGen <- which(names(term$data) %in% names(newvars))
  if(length(alreadyGen)) {
    term$data <- term$data[, -alreadyGen]  
  }
  
  term$data <- cbind(term$data, newvars)

  term
}



deriveOne <- function(weather, base, type, heatcool, n) {
  if(is.null(weather$rows)) {
    stop("Must Link Weather to Data before Starting")
  }
  weather <- weather[!is.na(weather$rows), ]
  newvar <- .Call("deriveVar", 
                  as.numeric(weather$aveTemp), 
                  as.integer(weather$rows), 
                  as.numeric(base), 
                  as.integer(n), 
                  as.integer(heatcool),
                  as.integer(type))
  
  # If degree days & we have non-daily weather measurements
  if(!is.null(weather$time) & type == 2) {
    days <- median(diff(as.numeric(weather$time)) / 3600 / 24)
    # print(paste("Scaling by # of days =", days))
    # newvar <- newvar * days
  }
  newvar
}




# lapply(seq_along(term$weather), function(x) {
#   x$dataRow <- sapply(1:nrow(x), function(i) {
#     print(i)
#     ind <- which(sapply(1:nrow(term$data), function(j) {
#       x$date[i] > term$data$dateStart[j] & x$date[i] <= term$data$dateEnd[j]
#     }))
#     if(!length(ind)) {
#       NA
#     } else if(length(ind) > 1) {
#       warning(paste("Multiple data rows match weather for", x$date[i], "using first"))
#       ind[1]
#     } else {
#       ind
#     }
#   })
#   
# })
