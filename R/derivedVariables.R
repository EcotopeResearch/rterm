# Variable transformations for a term object...



linkWeatherToData <- function(term) {
  if(is.null(term$data) | is.null(term$weather)) {
    stop("Cannot link weather to data, at least one has not been initialized")
  }
  
  term$data <- plyr::arrange(term$data, dateStart)
  
  weatherSites <- names(term$weather)
  
  term$weather <- lapply(weatherSites, function(x) {
    print(paste("Linking Weather to Data for", x))
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




deriveVar <- function(term, type, base, cooling = FALSE, weather = NULL) {
  
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
  }
  
  if(!is.null(weather)) {
    if(is.numeric(weather)) {
      
    } else if(is.character(weather)) {
      
    }
  }
  
  newvars <- lapply(term$weather, function(x) {
    x <- x[!is.na(x$rows), ]
    newvar <- .Call("deriveVar", 
          as.numeric(x$aveTemp), 
          as.integer(x$rows), 
          as.numeric(base), 
          as.integer(nrow(term$data)), 
          heatcool,
          ctype)
    
    if(!is.null(x$time) & ctype == 2) {
      days <- median(diff(as.numeric(x$time)) / 3600 / 24)
      print(paste("Scaling by # of days =", days))
      newvar <- newvar * days
    }
    
    newvar
  })
  names(newvars) <- make.names(paste(suffix, suffix2, names(term$weather), base))
  
  alreadyGen <- which(names(term$data) %in% names(newvars))
  if(length(alreadyGen)) {
    term$data <- term$data[, -alreadyGen]  
  }
  
  term$data <- cbind(term$data, newvars)

  term
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
