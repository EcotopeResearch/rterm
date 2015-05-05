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
    y
  })
  
  names(term$weather) <- weatherSites

  term  
}




augment <- function(term, type, base) {
  
  
  if(type %in% c("change-point", "cp")) {
    newvars[] <- lapply(seq_along(term$weather), function(x) {
      x
    })
    term$data
  } else if(type %in% c("degree-day", "dd")) {
    
  }
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
