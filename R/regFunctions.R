#Billing analysis functions
#system("R CMD SHLIB changePoint.c")
#dyn.load("changePoint.so")


#-----Change Point Functions

findChangePt <- function(temp, energy, tmin = 40, tmax = 70, step = .1) {
  logLikBest <- -Inf
  tBest <- tmin
  for(T in seq(from = tmin, to = tmax, by = step)) {
    xTmp <- makeCpVarC(temp, T)
    modTmp <- lm(energy ~ xTmp)
    logLikTmp <- logLik(modTmp)
    if(logLikTmp > logLikBest) {
      logLikBest <- logLikTmp
      tBest <- T
    }
  }
  tBest
}

makeCpVar <- function(x, T, heating = TRUE) {
  xTmp <- rep(0, length(x))
  if(heating) {
    rowsTmp <- which(x < T)
    if(length(rowsTmp)) xTmp[rowsTmp] <- T - x[rowsTmp] 
  } else {
    rowsTmp <- which(x > T)
    if(length(rowsTmp)) xTmp[rowsTmp] <- x[rowsTmp]  - T
  }
  xTmp
}


#Fake change point data...
makeRandomData <- function(n = 100, alpha, beta1, beta2, cp1, cp2, sd = 3, tmin = 40, tmax = 90, temp = NULL) {
  if(is.null(temp)) temp <- runif(n, min = tmin, max = tmax)
  actual <- sapply(temp, function(t) {
    if(t >= cp1 & t <= cp2) {
      return(alpha)
    } else if(t < cp1) {
      return(alpha + beta1 * (cp1 - t))
    } else {
      return(alpha + beta2 * (t - cp2))
    }
  })
  energy <- actual + rnorm(length(actual), mean = 0, sd = sd)
  df <- data.frame(temp, energy, actual)
}





cplm.fit.double <- function(temp, energy, weights, heating, cooling) {
  
  x <- data.frame(temp, energy, weights)
  x$group = sample(rep(c(1, 2), floor(nrow(x) / 2) + 1), nrow(x), replace = FALSE)
  
  i <- 0
  sumChanged <- 1
  nMax <- 50
  while(sumChanged > 0 & i <= nMax) {
    x2 <- iterate_one(x, heating, cooling)
    sumChanged <- sum(x$group != x2$group)
    x <- x2
    i <- i + 1
  }
  if(i == nMax) stop("Double Change Point Algorithm Did Not Converge")
  
  #Return dataframe with groupings?
  x
}



#Take a dataframe of temp, energy and "groups", return new groups
iterate_one <- function(x, heating, cooling) {
  #If we only have one group, exit
  if(sum(x$group == 1) == 0 | sum(x$group == 2) == 0) {
    return(x)
  }
  
  #Define "upper" cluster
  if(mean(x$energy[x$group == 1]) > mean(x$energy[x$group == 2])) {
    upper = 1
  } else {
    upper = 2
  }
  
  rows1 <- x$group == 1
  rows2 <- x$group == 2
  mod1 <- cplm.fit(x$temp[rows1], x$energy[rows1], x$weights[rows1], heating, cooling)
  mod2 <- cplm.fit(x$temp[rows2], x$energy[rows2], x$weights[rows2], heating, cooling)

  base1 <- coef(mod1$mod)[1]
  base2 <- coef(mod2$mod)[1]

  if(heating & length(coef(mod1$mod) > 1)) {
    slope1 <- coef(mod1$mod)[2]  
    slope2 <- coef(mod2$mod)[2]
    if(cooling & length(coef(mod1$mod) > 1)) {
      slope1c <- coef(mod1$mod)[3]
      slope2c <- coef(mod2$mod)[3]
      if(is.na(slope1c)) slope1c <- 0
      if(is.na(slope2c)) slope2c <- 0
    }
  } else if(cooling & length(coef(mod1$mod) > 1)) {
    slope1c <- coef(mod1$mod)[2]
    slope2c <- coef(mod2$mod)[2]
    if(is.na(slope1c)) slope1c <- 0
    if(is.na(slope2c)) slope2c <- 0
  }
  if(is.na(slope1)) slope1 <- 0
  if(is.na(slope2)) slope2 <- 0


  #The 'Lower' level can't have a higher slope than the upper level, w/ a x1.5?
  sc <- 1.5
  if(upper == 1) {
    if(slope2 > (sc * slope1)) slope2 <- sc * slope1
  } else {
    if(slope1 > (sc * slope2)) slope1 <- sc * slope2
  }
  
  #Neither slope can be negative... replace with constant
  if(slope1 < 0) {
    slope1 <- 0
    base1 <- mean(x$energy[x$group == 1])
  }
  if(slope2 < 0) {
    slope2 <- 0
    base2 <- mean(x$energy[x$group == 2])
  }
  
  x$xHeating1 <- makeCpVar(x$temp, T=mod1$changePoint)
  x$fitted1 <- base1 + slope1 * x$xHeating1
  
  x$xHeating2 <- makeCpVar(x$temp, T=mod2$changePoint)
  x$fitted2 <- base2 + slope2 * x$xHeating2
  
  x$newGroup <- sapply(1:nrow(x), function(i) {
    if(x$energy[i] > x$fitted1[i] & x$energy[i] > x$fitted2[i]) {
      upper
    } else if(x$energy[i] < x$fitted1[i] & x$energy[i] < x$fitted2[i]) { 
      3 - upper
    } else if(x$energy[i] == x$fitted1[i] | x$energy[i] == x$fitted2[i]) {
      3 - x$group[i]
    } else if((x$energy[i] - x$fitted1[i]) ^ 2 < (x$energy[i] - x$fitted2[i]) ^ 2) {
      1
    } else {
      2
    }
  })
  
  #If the "vacation" slope is greater than the "full" slope, assign the highest
  #'Vacation' point  to the full group
  
  x$oldGroup <- x$group
  x$group <- x$newGroup
  x
}

