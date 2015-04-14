#change point linear model functions...
#Mike Logsdon, Spring 2015


#Wraps cplm to perform many change point linear models 
#on a dataset with many buildings
#' Fit Multiple Change Point Linear Models from a Single Dataset
#' 
#' This function (change point linear model "extended") wraps \code{\link{cplm}} 
#' for a dataset with multiple buildings.
#' 
#' @param formula a formula as would be used in a linear model
#' @param data the dataset to perform the regression with
#' @param weights an optional vector of observation weights, if non-NULL will use these for weighted least squares
#' @param heating optional to force evaluation of a heating change point 
#' @param cooling optional to force evaluation of a cooling change point
#' @param se estimate standard errors with a bootstrap re-sampling technique
#' @param nreps number of bootstrap replicates, defaults to 200
#' @param parametric specify true for a parametric bootstrap, FALSE for a 
#'   non-parametric bootstrap. Defaults to parametric for < 100 observations,
#'   non-parametric for >= 100 observations
#' @param lambda optional override for L1 penalty. Modifies the mean-squared
#'   error from a full least-squares fit. Larger values correspond to larger
#'   penalties. A value of 0 corresponds to ordinary least-squares.
#' 
#' @return The coefficients from the change point linear model w/ specified 
#'  fitting method.
#'  
#' @examples
#' data(dhp)
#' results <- cplmx(kwhd ~ avetemp, data = dhp, 
#'                    id_vars = c("id", "post"))
#' summary(results)
#' plot(results, "heatingChangePoint")
#' 
#' 
cplmx <- function(formula, data, id_vars, heating = NULL, cooling = NULL, se = FALSE, lambda = 0) {
  namesFull <- c("baseLoad", "heatingSlope", "coolingSlope", "slope", "intercept",
                 "heatingChangePoint", "coolingChangePoint")

  allCoefs <- plyr::ddply(data, id_vars, function(x) {
    modTmp <- cplm(formula, x, se = FALSE)
    coefTmp <- coef(modTmp, silent = TRUE)
    
    toReturn <- lapply(namesFull, function(name) {
      if(!is.na(coefTmp[name])) {
        tmp <- list(as.numeric(coefTmp[name]))
      } else {
        tmp <- list(NA)
      }
      names(tmp)[1] <- name
      tmp
    })
    toReturn$Fit <- attr(modTmp, "fit")
    data.frame(toReturn)
  })
  results <- list("coefs" = allCoefs, "formula" = formula, "id_vars" = id_vars, data = "data")
  class(results) <- "cplmx"
  
  return(results)
}

plot.cplmx <- function(x, var) {
  
  ggplot2::ggplot(x$coefs) + ggplot2::theme_bw() + 
    ggplot2::geom_histogram(ggplot2::aes_string(x = var), fill = "grey") + 
    ggplot2::ggtitle(paste("Distribution of", var)) + 
    ggplot2::xlab(var)
  
}

classifyCplmx <- function(x) {
  
  x$coefs$type <- NA
  
  #Base Load...
  baseOnlyDf <- x$coefs[, -which(names(x$coefs) %in% c(x$id_vars, "baseLoad", "Fit"))]
  baseOnly <- apply(baseOnlyDf, 1, function(z) sum(!is.na(z)))
  x$coefs$type[baseOnly == 0] <- "No Weather Dependence Detected"
  
  # No Change Point Found
  noCpDf <- x$coefs[, which(names(x$coefs) %in% c("slope", "intercept"))]
  noCp <- apply(noCpDf, 1, function(z) sum(!is.na(z)))
  x$coefs$type[noCp == 2] <- "No Change Point Detected"
  
  # Found heating only...
  heatingOnly <- apply(x$coefs, 1, function(z) {
    if(!is.na(z['baseLoad']) & !is.na(z['heatingSlope']) & !is.na(z['heatingChangePoint']) &
         is.na(z['coolingSlope']) & is.na(z['coolingChangePoint'])) {
      TRUE
    } else {
      FALSE
    }
  })
  x$coefs$type[heatingOnly] <- "Heating Only"
  
  # Found cooling only...
  coolingOnly <- apply(x$coefs, 1, function(z) {
    if(!is.na(z['baseLoad']) & !is.na(z['coolingSlope']) & !is.na(z['coolingChangePoint']) &
         is.na(z['heatingSlope']) & is.na(z['heatingChangePoint'])) {
      TRUE
    } else {
      FALSE
    }
  })
  x$coefs$type[coolingOnly] <- "Cooling Only"
  
  
  # Found heating + cooling
  heatingCooling <- apply(x$coefs, 1, function(z) {
    if(!is.na(z['baseLoad']) & !is.na(z['coolingSlope']) & !is.na(z['coolingChangePoint']) &
         !is.na(z['heatingSlope']) & !is.na(z['heatingChangePoint'])) {
      TRUE
    } else {
      FALSE
    }
  })
  x$coefs$type[heatingCooling] <- "Heating and Cooling"
  
  x
}

summary.cplmx <- function(x) {
  n <- nrow(x$coefs)
  #print(paste("Out of", n, "combinations of id variables..."))
  #Number of L1 Fits...
  l1 <- sum(x$coefs$Fit == "L1")
  print(paste("Defaulted to L1 fit in", l1, "of", n, "occasions"))

  x <- classifyCplmx(x)
  
  print("")
  
  print(table(x$coefs$type))
  
}


#The basic change point linear model function.
#' Fit a change point linear model
#' 
#' Given a formula and data frame, this function will by default 
#' look for heating and cooling with an L1 penalized least-squares
#' regression, then based on those results fit an ordinary least-
#' squares regression of the selected model.
#' 
#' 
#' @param formula a formula as would be used in a linear model
#' @param data the dataset to perform the regression with
#' @param weights an optional vector of observation weights, if non-NULL will use these for weighted least squares
#' @param heating optional to force evaluation of a heating change point 
#' @param cooling optional to force evaluation of a cooling change point
#' @param se estimate standard errors with a bootstrap re-sampling technique
#' @param nreps number of bootstrap replicates, defaults to 200
#' @param parametric specify TRUE for a parametric bootstrap, FALSE for a 
#'   non-parametric bootstrap. Defaults to parametric for < 100 observations,
#'   non-parametric for >= 100 observations
#' @param lambda optional override for L1 penalty. Modifies the mean-squared
#'   error from a full least-squares fit. Larger values correspond to larger
#'   penalties. A value of 0 corresponds to ordinary least-squares.
#'   
#' @return
#' An object of class 'cplm'. Contains the original data.frame as 'dataOrig', 
#' the model formula, the regression data.frame w/ truncated basis vars as 'data',
#' Least-Squares coefficients as 'LS', L1 penalized coefficients as 'L1', and 
#' optionally a data.frame of 'bootstraps' if se = TRUE.
#' 
#' The following methods have been implemented for the 'cplm' class:
#' print, coef, predict, plot, resids
#' 
#' @examples
#' data(rfm)
#' mod <- cplm(eui ~ oat, data = rfm)
#' summary(mod)
#' coef(mod, "LS")
#' coef(mod, "L1")
#' plot(mod)
#' 
#' @seealso \code{\link{plot.cplm}} to plot model output,
#'   \code{\link{residsPlot}} to plot residuals energy use (net of weather),
#'   \code{\link{summary.cplm}} to report coefficients + standard errors
#'     if calculated.
#' 
cplm <- function(formula, data, weights, heating = NULL, cooling = NULL, se = TRUE, nreps = 200, parametric = NULL, lambda = 0) {
  
  #Bunch of stuff pasted from the lm code... parses the formula, data, subset, weights
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  energy <- model.response(mf, "numeric")
  weights <- as.vector(model.weights(mf))
  temp <- model.matrix(mt, mf)
  temp <- temp[, -1]

  # Artifact...
  doubleMode = FALSE
  
  #Check if we have weights
  if(is.null(weights)) {
    weights <- rep(1, nrow(data))
  }

  #Remove rows w/ NAs...
  data <- na.omit(data)
  if(!nrow(data)) {
    print("Error: No Rows with non-NA values")
    return(NULL)
  }
  
  #Set the parametric/non-parametric...
  if(is.null(parametric)) {
    if(nrow(data) < 100) {
      parametric <- TRUE
    } else {
      parametric <- FALSE
    }
  }
  #Check for a subset
  #   if(is.null(subset)) {
  #     subset <- TRUE
  #   }

  #Calculate the penalized coefficients
  l1Results <- .Call("cpl1", temp, energy, lambda)
  names(l1Results) <- c("baseLoad", "heatingSlope", "heatingChangePoint",
                        "coolingSlope", "coolingChangePoint")
  l1Results <- c(l1Results, "slope" = NA, "intercept" = NA)
  
  
  #Do we have heating/cooling?
  if(is.null(heating) & is.null(cooling)) {
    heating <- l1Results[2] > 0  
    cooling <- l1Results[4] > 0 
  }
  
  # Check to see if a changepoint is out of bounds...
  cpExists <- isChangePoint(l1Results, temp)
  if(!cpExists) {
    heating <- FALSE
    cooling <- FALSE
#     if(l1Results['heatingSlope'] > 0) {
#       l1Results['slope'] <- l1Results['heatingSlope']
#       l1Results['heatingSlope'] <- NA
#       l1Results['heatingChangePoint'] <- NA
#     } else if(l1Results['coolingSlope'] > 0) {
#       l1Results['slope'] <- l1Results['coolingSlope']
#       l1Results['coolingSlope'] <- NA
#       l1Results['coolingChangePoint'] <- NA      
#     }
  }
  
  #If we found a significant heating/cooling change-point, else just do a constant
  if(heating | cooling) {
    results <- cplm.one(temp, energy, weights, heating, cooling, se, doubleMode, nreps, parametric) 
  } else {
    results <- list()
    results$data <- data.frame("temp" = temp, "energy" = energy)
    cpExists <- isChangePoint(l1Results, results$data$temp)
    
    lsResults <- c("baseLoad" = NA, "heatingSlope" = NA, "heatingChangePoint" = NA,
                   "coolingSlope" = NA, "coolingChangePoint" = NA)
    if(!cpExists) {
      modTmp <- lm(energy ~ temp, data = results$data)
      lsResults <- c(lsResults, "slope" = as.numeric(coef(modTmp)[2]), 
                     "intercept" = as.numeric(coef(modTmp)[1]))
    } else {
      lsResults['baseLoad'] <- mean(energy)
      lsResults <- c(lsResults, "slope" = NA, "intercept" = NA)
    }
    results$LS <- lsResults
 
    results$formula <- formula
    class(results) <- "cplm"

  } 
  
  #Add in the formula...
  results$formula <- formula
  results$dataOrig <- data
  results$L1 <- l1Results
  
  # Specify whether to use the LS or L1 coefficients. Default to LS
  attr(results, "fit") <- "LS"
  # Check 1 - outlying bills
  if(max(abs(scale(energy))) > 3.5) {
    attr(results, "fit") <- "L1"
  } else if(!is.na(results$LS['heatingSlope'])) {
    if(!is.na(results$L1['heatingSlope'])) {
      sratio <- results$LS['heatingSlope'] / results$L1['heatingSlope']
    } else if(!is.na(results$L1['slope'])) {
      sratio <- results$LS['heatingSlope'] / results$L1['slope']
    } else {
      sratio <- 0
    }
    if(sratio > 10) attr(results, "fit") <- "L1"
  } else if(!is.na(results$LS['coolingSlope'])) {
    if(!is.na(results$L1['coolingSlope'])) {
      sratio <- results$LS['coolingSlope'] / results$L1['coolingSlope']
    } else if(!is.na(results$L1['slope'])) {
      sratio <- results$LS['coolingSlope'] / results$L1['slope']
    } else {
      sratio <- 0
    }
    if(sratio > 10) attr(results, "fit") <- "L1"    
  }
  
  return(results)
  
}


#One cplm model.
cplm.one <- function(temp, energy, weights, heating, cooling, se, doubleMode, nreps, parametric) {
  
  #Fit the regression parameters
  results <- cplm.fit(temp, energy, weights, heating, cooling)
  class(results) <- "cplm"
  
  #Check if we have a no-changepoint situation...
  if(is.na(results$LS['heatingChangePoint'] & is.na(results$LS['coolingChangePoint']))) {
    se <- FALSE
  } 
  
  #If it looks like we may have a double mode scenario, perform the double
  #operating mode fit
  if(doubleMode) {
    groups <- cplm.fit.double(temp, energy, weights, heating, cooling)
    rows1 <- groups$group == 1
    rows2 <- groups$group == 2
    results1 <- cplm.fit(temp[rows1], energy[rows1], weights[rows1], heating, cooling)
    results2 <- cplm.fit(temp[rows2], energy[rows2], weights[rows2], heating, cooling)
    results1$formula <- results2$formula <- formula
    class(results1) <- class(results2) <- "cplm"
    results <- list(results1, results2)
    class(results) <- "cplm"
    attr(results, "type") <- "double"
  }
  
  #If se = TRUE (standard error = TRUE), we need to bootstrap
  if(se) {
    bootstraps <- .Call("bootstrapChangePoint", temp, energy, weights, as.integer(nreps), heating, cooling, parametric);
    bootstraps <- as.data.frame(bootstraps) 
    if(heating & !cooling) {
      names(bootstraps) <- c("baseLoad", "heatingSlope", "heatingChangePoint")      
    } else if(cooling & !heating) {
      names(bootstraps) <- c("baseLoad", "coolingSlope", "coolingChangePoint")
    } else {
      names(bootstraps) <- c("baseLoad", "heatingSlope", "coolingSlope",
                             "heatingChangePoint", "coolingChangePoint")
    }
    
    #Stupid hack for now, remove where we lost the change point. Should be incorporated...
    if(heating) bootstraps <- bootstraps[bootstraps$heatingSlope > 0, ]
    if(cooling) bootstraps <- bootstraps[bootstraps$coolingSlope > 0, ]
    results$bootstraps <- bootstraps
  } else {
    bootstraps <- NULL
  }
  
  results
  
}


cplm.fit <- function(temp, energy, weights = NULL, heating = TRUE, cooling = FALSE) {
  cp <- .Call("findChangePoint", temp, energy, weights, heating, cooling)
  df <- data.frame(temp, energy)
  
  # Will return a vector of coefficients & the data frame
  lsResults <- rep(NA, 7)
  names(lsResults) <- c("baseLoad", "heatingSlope", "heatingChangePoint",
                        "coolingSlope", "coolingChangePoint",
                        "slope", "intercept")
  
  # 1) Just baseload, 2) No Change Point Found, 3) Normal
  if(!heating & !cooling) {
    lsResults['baseLoad'] <- mean(energy)
  } else if(sum(cp) == 0) {
    modTmp <- lm(energy ~ temp)
    lsResults['intercept'] <- as.numeric(coef(modTmp)[1])
    lsResults['slope'] <- as.numeric(coef(modTmp)[2])
  } else {
    if(heating) {
      df$xHeating <- makeCpVar(temp, cp[1], heating = TRUE)
      if(sum(df$temp > cp[1]) == 0 | (sum(df$temp < cp[1]) == 0)) {
        cp[1] <- NA
        df$xHeating <- NULL
      }
    }
    if(cooling) {
      cp <- round(cp, 1)
      names(cp)[heating + cooling] <- "cooling"
      df$xCooling <- makeCpVar(temp, cp[heating + cooling], heating = FALSE)
      if(sum(df$temp < cp[heating + cooling] | (sum(df$temp > cp[1]) == 0)) == 0) {
        cp[heating + cooling] <- NA
        df$xCooling <- NULL
      }
    }
    
    # Check if we lost our heating/cooling because the change point was 
    # out of bounds
    if(is.null(df$xHeating) & is.null(df$xCooling)) {
      lsResults['baseLoad'] <- mean(energy)
    } else {
      mod <- lm(energy ~ ., data = df[, names(df) %in% c("energy", "xHeating", "xCooling")])  
      lsResults['baseLoad'] <- as.numeric(coef(mod)[1])
      if(heating) {
        lsResults['heatingSlope'] <- as.numeric(coef(mod)['xHeating']);
        lsResults['heatingChangePoint'] <- cp[1];
      }
      if(cooling) {
        lsResults['coolingSlope'] <- as.numeric(coef(mod)['xCooling'])
        lsResults['coolingChangePoint'] <- cp[heating + cooling];
      }      
    }
    
  }
  
  return(list("data" = df, "LS" = lsResults))
}


isChangePoint <- function(coefs, temp) {
  # 'Heating' found
  n <- length(temp)
  if(coefs[2] > 0) {
    # Pts in 'heating'
    x <- sum(temp <= coefs[3])
    if(x == n | x == 0) {
      return(FALSE)
    }
  }
  # 'Cooling' found
  if(coefs[4] > 0) {
    # Pts in 'cooling'
    x <- sum(temp >= coefs[5])
    if(x == n | x == 0) {
      return(FALSE)
    }    
  }
  return(TRUE)
}


print.cplm <- function(x) {
  if(attr(mod, "fit") == "LS") {
    return(x$LS[!is.na(x$LS)])
  } else {
    return(x$L1[!is.na(x$LS)])
  }
#   toPrint <- c("baseLoad" = as.numeric(coef(x$mod)[1]))
#   if(length(coef(x$mod)) == 1) return(toPrint)
#   if("xHeating" %in% names(x$data)) {
#     toPrint <- c(toPrint, "changePointHeating" = as.numeric(x$changePoint[1]))
#     toPrint <- c(toPrint, "heatingSlope" = as.numeric(coef(x$mod)[2]))
#     if("xCooling" %in% names(x$data)) {
#       toPrint <- c(toPrint, "changePointCooling" = as.numeric(x$changePoint[2]))
#       toPrint <- c(toPrint, "coolingSlope" = as.numeric(coef(x$mod)[3]))
#     }
#   } else {
#     toPrint <- c(toPrint, "changePointCooling" = as.numeric(x$changePoint[1]))
#     toPrint <- c(toPrint, "coolingSlope" = as.numeric(coef(x$mod)[2]))
#   }
#   toPrint
}


#' Change Point Linear Model Coefficients
#' 
#' @param x a 'cplm' object
#' @param fit an optional specification of coefficient type "LS" or "L1". If 
#'   not specified returns the best-guess as to which is more appropriate. 
#'   (The presence of outlying bills trips the standard output to the penalized 
#'   "L1" fit)
#' 
#' @return The coefficients from the change point linear model w/ specified 
#'  fitting method.
#'  
#' @examples
#' data(rfm)
#' mod <- cplm(eui ~ oat, data = rfm)
#' coef(mod)
#' coef(mod, "L1")
#' coef(mod, "LS")
#' 
coef.cplm <- function(x, fit = NULL, silent = FALSE) {
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

#Have an 'annual' method that can be used by the various objects
annual <- function(x) UseMethod("annual")

annual.cplm <- function(x, type = "observed") {
  coefs <- coef(x, silent = TRUE)
  if(is.na(coefs['baseLoad'])) {
    stop("Cannot Annualize A Fit W/ No Base Load")
  } else {
    energy <- x$data$energy
    temp <- x$data$temp
    if(type == "observed") {
      toRet <- c("Total" = sum(energy))
      if(!is.na(coefs['heatingSlope'])) {
        tmp <- sum((energy - coefs['baseLoad'])[temp < coefs['heatingChangePoint']])
        toRet <- c(toRet, "Heating" = tmp)
      }
      if(!is.na(coefs['coolingSlope'])) {
        tmp <- sum((energy - coefs['baseLoad'])[temp > coefs['coolingChangePoint']])
        toRet <- c(toRet, "Cooling" = tmp)
      }
      
      tmp <- toRet['Total']
      if(!is.na(toRet['Heating'])) {
        tmp <- tmp - toRet['Heating']
      }
      if(!is.na(toRet['Cooling'])) {
        tmp <- tmp - toRet['Cooling']
      }
      toRet <- c(toRet, "Base Load" = as.numeric(tmp))
      return(toRet)
    } else if(type == "TMY") {
      stop("TMY Annualization Not Yet Implemented")
    }
  }
}


summary.cplm <- function(object, ...) {
  coefs <- print(object)
  if(is.null(object$bootstraps)) {
    return(coefs)
  } else {
    ses <- sapply(names(coefs), function(coef) {
      sd(object$bootstraps[, coef])
    })
    lower95s <- sapply(names(coefs), function(coef) {
      as.numeric(quantile(object$bootstraps[, coef], .05))
    })
    upper95s <- sapply(names(coefs), function(coef) {
      as.numeric(quantile(object$bootstraps[, coef], .95))
    })
    df <- data.frame("Estimate" = coefs, "Standard.Error" = ses, 
                     "Lower.95" = lower95s, "Upper.95" = upper95s)
    return(df)
  }
}


predict.cplm <- function(object, newdata, fit = NULL) {

  
  #Pull out the formula from the original object, should fit new data
  mf <- model.frame(object$formula, newdata)
  newdata$energy <- model.response(mf, "numeric")
  temp <- model.matrix(object$formula, mf)
  newdata$temp <- temp[, -1]
  
  #Check if we're using the LS or L1 fitted coefficients
  if(is.null(fit)) {
    fit <- attr(object, "fit")
  }
  
  coefs <- coef(object, fit, silent = TRUE)
  
  
  if(!is.na(coefs['slope'])) {
    return(coefs['intercept'] + coefs['slope'] * newdata$temp)
  } else {
    toReturn <- rep(coefs['baseLoad'], nrow(newdata))
    #Look for heating/cooling, make change point vars as appropriate
    heating <- !is.null(object$data$xHeating)
    cooling <- !is.null(object$data$xCooling)
    if(heating) {
      newdata$xHeating <- makeCpVar(newdata$temp, coefs['heatingChangePoint'])
      toReturn <- toReturn + coefs['heatingSlope'] * newdata$xHeating
    }
    if(cooling) {
      newdata$xCooling <- makeCpVar(newdata$temp, coefs['coolingChangePoint'], heating = FALSE)
      toReturn <- toReturn + coefs['coolingSlope'] * newdata$xCooling
    }    
    return(as.numeric(toReturn))
  }

  return(toReturn)

}




#' Plot Change Point Linear Model
#' 
#' @param x a 'cplm' object
#' @param fit an optional specification of coefficient type "LS" or "L1". If 
#'   not specified returns the best-guess as to which is more appropriate. 
#'   (The presence of outlying bills trips the standard output to the penalized 
#'   "L1" fit.) Additionally specifying "both" here will overlay both fits.
#' 
#' @return A ggplot object
#'  
#' @examples
#' data(rfm)
#' mod <- cplm(eui ~ oat, data = rfm)
#' plot(mod)
#' plot(mod, "both")
#' 
#' @seealso \code{\link{residsPlot}} to plot residuals (this is the fun part
#'   anyway - looking at trends net of weather)
#' 
plot.cplm <- function(x, fit = NULL) {
  
  #Check if this is a single or a double fit
  #if(attr(x, "type") == "single") {
  if(1) {
    if(is.null(fit)) {
      fit <- attr(x, "fit")
    }
    #Make the energy & temp into a data frame for ggplot
    df <- x$data
        
    #Look for heating/cooling
    heating <- !is.null(df$xHeating)
    cooling <- !is.null(df$xCooling)
    tempOnly <- "slope" %in% names(coef(x, silent = TRUE))
    
    x$formula <- formula(energy ~ temp)
    #Make another data frame of fitted values for ggplot
    tmin <- min(df$temp)
    tmax <- max(df$temp)
    ts <- seq(from = tmin, to = tmax, length.out = 100)
    dfLs <- data.frame("temp" = ts, "energy" = 1, "Fit" = "Least-Squares")
    dfLs$fitted <- predict(x, dfLs, "LS")
    dfL1 <- data.frame("temp" = ts, "energy" = 1, "Fit" = "L1 Penalized")
    dfL1$fitted <- predict(x, dfLs, "L1")
    df2 <- rbind(dfLs, dfL1)
    
    
    #If we have some bootstrap results...
    if(!is.null(x$bootstraps) & fit == "LS") {
      bootDf <- do.call('rbind', lapply(1:nrow(x$bootstraps), function(i) {
        dfTmp <- data.frame("temp" = ts)
        dfTmp$fitted <- x$bootstraps$baseLoad[i]
        if(heating) {
          dfTmp$xHeating <- makeCpVar(dfTmp$temp, x$bootstraps$heatingChangePoint[i], heating = TRUE)  
          dfTmp$fitted <- dfTmp$fitted + dfTmp$xHeating * x$bootstraps$heatingSlope[i]
        }
        if(cooling) {
          dfTmp$xCooling <- makeCpVar(dfTmp$temp, x$bootstraps$coolingChangePoint[i], heating = FALSE)
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
      mu <- mean(df$energy)
      sez <- sd(df$energy) / sqrt(nrow(df))
      df$lower <- mu - 2 * sez
      df$upper <- mu + 2 * sez
    } 
    
    plotObject <- ggplot2::ggplot(df) + ggplot2::theme_bw() + 
      ggplot2::geom_point(ggplot2::aes(x = temp, y = energy)) + 
      ggplot2::ggtitle(paste("Energy by Mean OAT")) +
      ggplot2::xlab("Mean OAT (F)") + ggplot2::ylab("Energy (kWh)")
    
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
      plotObject <- plotObject + ggplot2::geom_smooth(ggplot2::aes(x = temp, y = energy), method = "lm", col = "black")
    } else if(!heating & !cooling) {
      plotObject <- plotObject + ggplot2::geom_ribbon(ggplot2::aes(x = temp, ymin = lower, ymax = upper), alpha = .4)
    } 
    
    return(plotObject)
  } else if(attr(x, "type") == "double") {
    
    #Or else we have a double fit...
    x1 <- x[[1]]
    x2 <- x[[2]]
    
    #Make the energy & temp into a data frame for ggplot
    df1 <- x1$data
    df1$group <- 1
    df2 <- x2$data
    df2$group <- 2
    dfboth <- rbind(df1, df2)
    
    #Look for heating/cooling
    heating <- !is.null(df1$xHeating)
    cooling <- !is.null(df1$xCooling)
    
    #Make another data frame of fitted values for ggplot
    tmin <- min(c(df1$temp, df2$temp))
    tmax <- max(c(df1$temp, df2$temp))
    ts <- seq(from = tmin, to = tmax, length.out = 100)
    df1x <- data.frame("temp" = ts, "energy" = 1)
    x1$formula <- formula(energy ~ temp)
    df1x$fitted <- predict(x1, df1x)
    df1x$group <- 1
    
    df2x <- data.frame("temp" = ts, "energy" = 1)
    x2$formula <- formula(energy ~ temp)
    df2x$fitted <- predict(x2, df2x)
    df2x$group <- 2
    
    plotObject <- ggplot2::ggplot(dfboth) + ggplot2::theme_bw() + 
      ggplot2::geom_point(ggplot2::aes(x = temp, y = energy, col = factor(group))) + 
      ggplot2::geom_line(data = df1x, ggplot2::aes(x = temp, y = fitted, col = factor(group))) +
      ggplot2::geom_line(data = df2x, ggplot2::aes(x = temp, y = fitted, col = factor(group))) +
      ggplot2::ggtitle(paste("Energy by Mean OAT, Change Point\nDouble Change Point Model")) +
      ggplot2::xlab("Mean OAT (F)") + ggplot2::ylab("Energy (kWh)") +
      ggplot2::scale_colour_discrete(name = "Assigned Mode")
    plotObject
    return(plotObject)
    
  }
  
}



#' Plot Residuals from a Change Point Linear Model
#' 
#' Note that this is a generic, currently with a method for 
#' S3 class 'cplm'. The vision is that once variable base degree day
#' & Bayesian methodologies are developed there will also be methods
#' to plot the residuals from those models.
#' 
#' @param x a 'cplm' object
#' @param var variable name to plot against in the dataset used to 
#'   fit the model.
#' 
#' @return A ggplot object
#'  
#' @examples
#' data(ecotope)
#' mod <- cplm(kwhd ~ oat, data = ecotope)
#' residsPlot(mod, "dateEnd")
#' 
#' 
#' @seealso \code{\link{plot.cplm}} 
#' 
residsPlot <- function(x, ...) UseMethod("residsPlot")

residsPlot.cplm <- function(x, var) {
  #Pull out the formula from the original object, should fit new data
  mf <- model.frame(x$formula, x$dataOrig)
  energy <- model.response(mf, "numeric")
  
  df <- x$dataOrig
  df$resids <- energy - predict(x, x$dataOrig)
  
  ggplot2::ggplot(df, ggplot2::aes_string(x = var, y = "resids")) + ggplot2::theme_bw() + 
    ggplot2::geom_point() + ggplot2::geom_smooth(se = FALSE) + 
    ggplot2::ggtitle(paste("Residuals from Mean Weather-Based Usage")) + 
    ggplot2::xlab(var) + ggplot2::ylab("Observed Energy - Expected from Weather")
}


