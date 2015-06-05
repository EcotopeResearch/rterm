
ddlm <- function(data, weather, controls) {
  coefs <- ddlm.fit(data, weather, controls$heating, controls$cooling, controls$intercept, controls$lambda, controls$se, controls$nreps, controls$selection)
  
  mod <- list()
  mod$LS <- coefs$LS
  mod$L1 <- coefs$L1
  mod$data <- data
  mod$bootstraps <- coefs$bootstraps
  
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
  
  class(mod) <- c("ddlm", "tlm")
  
  mod$data$fitted <- fitted(mod)
  mod$data$resid <- mod$data$dailyEnergy - mod$data$fitted
  
  return(mod)
}

cplm <- function(data, weather, controls) {
  coefs <- cplm.fit(data, weather, controls$heating, controls$cooling, controls$intercept, controls$lambda, controls$se, controls$nreps, controls$selection)
  
  mod <- list()
  mod$LS <- coefs$LS
  mod$L1 <- coefs$L1
  mod$data <- data
  mod$bootstraps <- coefs$bootstraps
  
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
  
  class(mod) <- c("cplm", "tlm")
  mod$data$fitted <- fitted(mod)
  mod$data$resid <- mod$data$dailyEnergy - mod$data$fitted
  
  return(mod)
  
}


tlm.fit <- function(data, weather, heating = TRUE, cooling = FALSE, intercept = TRUE, se = TRUE, nreps = 200, type = 1L) {
  
  if(type == 1) {
    y <- as.numeric(data$dailyEnergy)
  } else if(type == 2) {
    y <- as.numeric(data$dailyEnergy)
  }
  
  if(!heating & !cooling) {
    return(list("coefs" = c("baseLoad" = mean(data$dailyEnergy))))
  }
  
  toReturn <- list()
  
  coefs <- .Call("findBaseTemp", 
                 as.numeric(weather$aveTemp),
                 as.integer(weather$rows), 
                 y,
                 rep(1, nrow(data)),
                 as.integer(heating), as.integer(cooling), 
                 as.integer(type), as.integer(intercept),
                 PACKAGE = "rterm")
  
  if(se) {
    boot <- .Call("bootstrapBaseTemp", 
                                 as.numeric(weather$aveTemp),
                                 as.integer(weather$rows), 
                                 y,
                                 rep(1, nrow(data)), as.integer(nreps),
                                 as.integer(heating), as.integer(cooling), 
                                 as.integer(type), as.integer(intercept),
                  PACKAGE = "rterm")
    boot <- as.data.frame(boot)
    if(intercept) {
      names(boot)[1] <- "baseLoad"
      if(heating) {
        names(boot)[2] <- "heatingBase"
        names(boot)[3] <- "heatingSlope"
      }
      if(cooling) {
        names(boot)[2 * heating + 2] <- "coolingBase"
        names(boot)[2 * heating + 3] <- "coolingSlope"
      }
    } else {
      if(heating) {
        names(boot)[1] <- "heatingBase"
        names(boot)[2] <- "heatingSlope"
      }
      if(cooling) {
        names(boot)[2 * heating + 1] <- "coolingBase"
        names(boot)[2 * heating + 2] <- "coolingSlope"
      }      
    }
    toReturn$bootstraps <- boot
  }
  
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
  
  toReturn$coefs <- coefs
  
  toReturn
}


cplm.fit <- function(data, weather, heating = NULL, cooling = NULL, intercept = TRUE, lambda = 0, se = TRUE, nreps = 200, selection = "vbsr") {
  if(is.null(weather$rows)) {
    stop("Must link data to weather before model fitting")
  }
  
  l1Results <- .Call("l1", as.numeric(weather$aveTemp),
                     as.integer(weather$rows),
                     as.numeric(data$dailyEnergy),
                     lambda, 1L, as.integer(intercept),
                     PACKAGE = "rterm")
  
  if(intercept) {
    names(l1Results) <- c("baseLoad", "heatingBase", "heatingSlope", 
                          "coolingBase",  "coolingSlope")    
  } else {
    names(l1Results) <- c("heatingBase", "heatingSlope", 
                          "coolingBase",  "coolingSlope")    
  }
  
  # Different default heating/cooling based on selection type
  if(selection == "vbsr") {
    heatCoolTmp <- vbsrSelect(data, weather, 1L)
    heatingDefault <- heatCoolTmp$heating
    coolingDefault <- heatCoolTmp$cooling  
  } else {
    heatingDefault <- as.numeric(l1Results['heatingSlope']) > 0
    coolingDefault <- as.numeric(l1Results['coolingSlope']) > 0
  }
  
  # If not manually specified, use the default
  if(is.null(heating)) {
    heating <- heatingDefault
  }
  if(is.null(cooling)) {
    cooling <- coolingDefault
  }

  
  lsResults <- c("baseLoad" = NA, "heatingBase" = NA,
                 "heatingSlope" = NA, "coolingBase" = NA,
                 "coolingSlope" = NA)
  if(!heating & !cooling & !intercept) {
    
  } else if(!heating & !cooling) {
    lsResults['baseLoad'] <- mean(data$energy)
  }
  
  fitTmp <- tlm.fit(data, weather, heating, cooling, intercept, se, nreps, 1L)
  lsCoefs <- fitTmp$coefs
  
  for(coef in names(lsResults)) {
    if(!is.null(lsCoefs[coef])) {
      lsResults[coef] <- lsCoefs[coef]
    }
  }
  
  # Add logic to check whether we conclusively found a change point
  
  return(list("LS" = lsResults, "L1" = l1Results, "bootstraps" = fitTmp$bootstraps))
}



ddlm.fit <- function(data, weather, heating = NULL, cooling = NULL, intercept = TRUE, lambda = 7, se = TRUE, nreps = 200, selection = "vbsr") {
  if(is.null(weather$rows)) {
    stop("Must link data to weather before model fitting")
  }
  
  l1Results <- .Call("l1", as.numeric(weather$aveTemp),
                     as.integer(weather$rows),
                     as.numeric(data$dailyEnergy),
                     lambda, 2L, as.integer(intercept),
                     PACKAGE = "rterm")
  
  if(intercept) {
    names(l1Results) <- c("baseLoad", "heatingBase", "heatingSlope", 
                          "coolingBase",  "coolingSlope")    
  } else {
    names(l1Results) <- c("heatingBase", "heatingSlope", 
                          "coolingBase",  "coolingSlope")    
  }
  
  # Different default heating/cooling based on selection type
  if(selection == "vbsr") {
    heatCoolTmp <- vbsrSelect(data, weather, 2L)
    heatingDefault <- heatCoolTmp$heating
    coolingDefault <- heatCoolTmp$cooling  
  } else {
    heatingDefault <- as.numeric(l1Results['heatingSlope']) > 0
    coolingDefault <- as.numeric(l1Results['coolingSlope']) > 0
  }
  
  # If not manually specified, use the default
  if(is.null(heating)) {
    heating <- heatingDefault
  }
  if(is.null(cooling)) {
    cooling <- coolingDefault
  }
  
  
  fitTmp <- tlm.fit(data, weather, heating, cooling, intercept, se, nreps, 2L)
  lsCoefs <- fitTmp$coefs
  
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
  
  
  return(list("LS" = lsResults, "L1" = l1Results, "bootstraps" = fitTmp$bootstraps))
}



fitted.tlm <- function(mod, fit = NULL) {
  
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


# Annual summary
annual.term <- function(term) {
  
}

plot.term <- function(term, xvar = NULL) {
  if(is.null(xvar)) {
    xvar <- "temp"
  }
  
  nmodels <- length(term$models)
  if(!nmodels) {
    stop("No Models Evaluated, cannot plot")
  } else if(nmodels == 1 & xvar != "time" & xvar != "resids") {
    return(plot(term$models[[1]]))
  }
  
  # Need to get fitted values...
  
  abc <- do.call('rbind', lapply(seq_along(term$models), function(i) {
    x <- term$models[[i]]
    tmp <- subset(x$data, select = c(dateStart, dailyEnergy, temp, fitted))
    tmp$type <- names(term$models)[i]
    tmp
  }))
  abc$resid <- abc$dailyEnergy - abc$fitted
  
  meanTemps <- do.call('rbind', by(abc, abc$dateStart, function(x) {
    data.frame("temp" = mean(x$temp),
               "dailyEnergy" = mean(x$dailyEnergy))
  }))
  
  if(xvar == "temp") {
    p <- ggplot2::ggplot(abc) + ggplot2::theme_bw() + 
      ggplot2::geom_point(data = meanTemps, ggplot2::aes(x = temp, y = dailyEnergy)) + 
      ggplot2::ggtitle(paste(attr(term, "name", exact = TRUE), "Energy versus Temperature")) + 
      ggplot2::xlab("Average Temperature (F)") +
      ggplot2::ylab("Daily Energy (kWh)")
    if(nmodels > 1) {
      p <- p + ggplot2::geom_line(ggplot2::aes(x = temp, y = fitted, col = type))
    } else {
      p <- p + ggplot2::geom_line(ggplot2::aes(x = temp, y = fitted))
    }
  } else if(xvar == "time") {
    p <- ggplot2::ggplot(abc) + ggplot2::theme_bw() + 
      ggplot2::geom_point(ggplot2::aes(x = dateStart, y = dailyEnergy)) + 
      ggplot2::ggtitle(paste(attr(term, "name", exact = TRUE), "Energy over Time")) + 
      ggplot2::xlab("Date") +
      ggplot2::ylab("Daily Energy (kWh)")
    if(nmodels > 1) {
      p <- p + ggplot2::geom_line(ggplot2::aes(x = dateStart, y = fitted, col = type))
    } else {
      p <- p + ggplot2::geom_line(ggplot2::aes(x = dateStart, y = fitted))
    }
  } else if(xvar == "resids") {
    p <- ggplot2::ggplot(abc) + ggplot2::theme_bw() + 
      ggplot2::ggtitle(paste(attr(term, "name", exact = TRUE), "Residuals over Time")) + 
      ggplot2::xlab("Date") +
      ggplot2::ylab("Daily Energy Relative to Expected, Given Outdoor Temp (kWh)")
    if(nmodels > 1) {
      p <- p + ggplot2::geom_point(ggplot2::aes(x = dateStart, y = resid, col = type)) +
        ggplot2::geom_smooth(ggplot2::aes(x = dateStart, y = resid, col = type), se = FALSE)
    } else {
      p <- p + ggplot2::geom_point(ggplot2::aes(x = dateStart, y = resid)) +
        ggplot2::geom_smooth(ggplot2::aes(x = dateStart, y = resid), se = FALSE)
    }    
  }
  
  p
}





plot.tlm <- function(x, fit = NULL) {
  
  if(is.null(fit)) {
    fit <- attr(x, "fit")
  }
  #Make the energy & temp into a data frame for ggplot
  df <- x$data
  
  #Look for heating/cooling
  heating <- !is.null(df$xHeating)
  cooling <- !is.null(df$xCooling)
  
  #If we have some bootstrap results...
  if(!is.null(x$bootstraps) & fit == "LS") {
    bootDf <- do.call('rbind', lapply(1:nrow(x$bootstraps), function(i) {
      dfTmp <- data.frame("temp" = ts)
      dfTmp$fitted <- x$bootstraps$baseLoad[i]
      if(heating) {
        dfTmp$xHeating <- makeCpVar(dfTmp$temp, x$bootstraps$heatingBase[i], heating = TRUE) 
        dfTmp$fitted <- dfTmp$fitted + dfTmp$xHeating * x$bootstraps$heatingSlope[i]
      }
      if(cooling) {
        dfTmp$xCooling <- makeCpVar(dfTmp$temp, x$bootstraps$coolingBase[i], heating = FALSE)
        dfTmp$fitted <- dfTmp$fitted + dfTmp$xCooling * x$bootstraps$coolingSlope[i]
      }    
      dfTmp
    }))
    
    bounds <- do.call('rbind', by(bootDf, bootDf$temp, function(x) {
      data.frame("temp" = x$temp[1],
                 "mean" = mean(x$fitted, na.rm = TRUE), 
                 "se" = sd(x$fitted, na.rm = TRUE))
    }))
    
    
    #       bounds <- plyr::ddply(bootDf, .(temp), function(x) {
    #         c("mean" = mean(x$fitted, na.rm = TRUE), 
    #           "se" = sd(x$fitted, na.rm = TRUE))
    #       })
    
    #ggplot(bounds) + theme_bw() + geom_line(aes(x = temp, y = mean))
    bounds$lower <- bounds$mean - 2 * bounds$se
    bounds$upper <- bounds$mean + 2 * bounds$se
    
  } else if(!heating & !cooling) {
    mu <- mean(df$dailyEnergy)
    sez <- sd(df$dailyEnergy) / sqrt(nrow(df))
    df$lower <- mu - 2 * sez
    df$upper <- mu + 2 * sez
  } 
  
  plotObject <- ggplot2::ggplot(df) + ggplot2::theme_bw() + 
    ggplot2::geom_point(ggplot2::aes(x = temp, y = dailyEnergy)) + 
    ggplot2::ggtitle(paste("dailyEnergy by Mean OAT")) +
    ggplot2::xlab("Mean OAT (F)") + ggplot2::ylab("dailyEnergy (kWh)")
  
  if(fit == "LS") {
    plotObject <- plotObject + ggplot2::geom_line(data = dfLs, ggplot2::aes(x = temp, y = fitted))
  } else if(fit == "L1") {
    plotObject <- plotObject + ggplot2::geom_line(data = dfL1, ggplot2::aes(x = temp, y = fitted))
  } else {
    plotObject <- plotObject + ggplot2::geom_line(data = df2, ggplot2::aes(x = temp, y = fitted, linetype = Fit))
  }
  
  if(!is.null(x$bootstraps) & fit == "LS") {
    plotObject <- plotObject + ggplot2::geom_ribbon(data = bounds, ggplot2::aes(x = temp, ymin = lower, ymax = upper), alpha = .4)
  } else if(tempOnly) {
    plotObject <- plotObject + ggplot2::geom_smooth(ggplot2::aes(x = temp, y = dailyEnergy), method = "lm", col = "black")
  } else if(!heating & !cooling) {
    #plotObject <- plotObject + ggplot2::geom_ribbon(ggplot2::aes(x = temp, ymin = lower, ymax = upper), alpha = .4)
  } 
  
  return(plotObject)
  
}

print.tlm <- function(x) {
  if(attr(x, "fit") == "LS") {
    toPrint <- x$LS[!is.na(x$LS)]
  } else {
    toPrint <- x$L1[!is.na(x$LS)]
  }
  toPrint
}


coef.tlm <- function(x, fit = NULL, silent = FALSE) {
  if(is.null(fit)) {
    fit <- attr(x, "fit")
  }
  
  if(fit == "LS") {
    if(!silent) print("Least Squares Coefficients:")
    return(x$LS[!is.na(x$LS)])
  } else {
    if(!silent) print("L1 Penalized Least Squares Coefficients:")
    return(x$L1[!is.na(x$LS)])
  }  
  
}



summary.tlm <- function(object, ...) {
  coefs <- print(object)
  if(is.null(object$bootstraps)) {
    return(coefs)
  } else {
    ses <- sapply(names(coefs), function(coef) {
      sd(object$bootstraps[, coef])
    })
    lower95s <- sapply(names(coefs), function(coef) {
      as.numeric(quantile(object$bootstraps[, coef], .05, na.rm = TRUE))
    })
    upper95s <- sapply(names(coefs), function(coef) {
      as.numeric(quantile(object$bootstraps[, coef], .95, na.rm = TRUE))
    })
    df <- data.frame("Estimate" = coefs, "Standard.Error" = ses, 
                     "Lower.95" = lower95s, "Upper.95" = upper95s)
    return(df)
  }
}


bootstraps <- function(mod) {
  if(inherits(mod, "term")) {
    if(length(mod$models) > 1) {
      warning("Multiple models found, showing bootstraps for the first")
    }
    mod <- mod$models[[1]]
  }
  if(!inherits(mod, "tlm")) {
    stop("You can only bootstraps a tlm object")
  }
  
  boot <- reshape2::melt(mod$bootstraps)
  ggplot2::ggplot(boot) + ggplot2::theme_bw() + 
    ggplot2::geom_histogram(ggplot2::aes(value)) + 
    ggplot2::facet_grid(. ~ variable, scales = "free") +
    ggplot2::ggtitle("Bootstrap Replicate Distributions")
}


vbsrSelect <- function(data, weather, type) {
  
  heatingRange <- seq(from = 30, to = 80, by = 5)
  coolingRange <- seq(from = 50, to = 90, by = 5)
  
  X <- matrix(0, nrow = nrow(data), ncol = length(heatingRange) + length(coolingRange))
  y <- data$dailyEnergy

  Xheating <- do.call('cbind', lapply(heatingRange, function(x) {
    deriveOne(weather, x, type, 1,nrow(data))
  }))
  Xcooling <- do.call('cbind', lapply(coolingRange, function(x) {
    deriveOne(weather, x, type, 2,nrow(data))
  }))
  X <- cbind(Xheating, Xcooling)
  colnames(X) <- c(paste0("th", heatingRange), paste0("tc", coolingRange))
  
  # Remove columns of zeros
  
  X <- X[, apply(X, 2, function(x) sum(x > 0)) > 0]
  
  # Fit the spike model and look for significance
  mod <- vbsr::vbsr(y, X, family = "normal")
  pos <- mod$beta > 0
  
  pval <- 0.05 / ncol(X)
  signif <- which(mod$pval[pos] < pval)
  toReturn <- list("heating" = FALSE, "cooling" = FALSE)
  if(length(signif)) {
    print(colnames(X)[pos][signif])
    if(length(grep("th", colnames(X)[pos][signif]))) {
      toReturn$heating <- TRUE
    }
    if(length(grep("tc", colnames(X)[pos][signif]))) {
      toReturn$cooling <- TRUE
    }
  }
  toReturn
}


