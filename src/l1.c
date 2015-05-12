/*
C Functions for fitting L1 penalized regression models with 
either change point or variable-base-degree-day
*/


#include <R.h>
#include <Rinternals.h>
#include <stdio.h>

#include "classicModels.h"
#include "derivedVariables.h"
#include "lmFast.h"
#include "l1.h"
#include "utility.h"



//Do an l1 penalized changepoint model. Return heating/cooling
SEXP l1(SEXP temps, SEXP rows, SEXP energy, SEXP lambdax, SEXP type, SEXP intercept) {
  double mse_cur, mse_best, lambda, ss_tmp, fitted;
  double xmax1, xmax2;
  double mean_energy;
  
  // Define how much data we're dealing with
  int nweather = length(temps);
  int ndata = length(energy);

  //Read some inputs
  double *cTemp = REAL(temps);
  double *cEnergy = REAL(energy);
  double *cLambda = REAL(lambdax);
  int *cRows = INTEGER(rows);
  int *pType = INTEGER(type);
  int *pIntercept = INTEGER(intercept);
  int cIntercept = *pIntercept;
  int cType = *pType;
  
  //Set up the change point search parameters
  double tmin = 40;
  double tmax = 80;
  double tstep = 1;
  double th_best = 0;
  double tc_best = 0;
  
  //Allocate space for xHeating and xCooling variables
  double *xHeating = malloc(ndata * sizeof(double));
  double *xCooling = malloc(ndata * sizeof(double));
  double *betahat = malloc((cIntercept + 2) * sizeof(double));
  double *w = malloc(ndata * sizeof(double));
  double *cp = malloc((4 + cIntercept) * sizeof(double));
  double *X = malloc(ndata * (cIntercept + 2) * sizeof(double));
  
  //Just put the weights as 1
  for(int a = 0; a < ndata; a++) {
    w[a] = 1;
  }
  
  //Prep the output
  SEXP results = PROTECT(allocVector(REALSXP, 5));
  double *coefs = REAL(results);
  

  //Do a least-squares fit... then base lambda off of residual variation
  findBaseTempC(cTemp, cRows, cEnergy, w, ndata, nweather, cp, 1, 1, cType, cIntercept);

  // First case, if we never found a valid solution we get zeros. Suppose model
  // then is just the mean
  if(cp[1] == 0 && cp[3] == 0) {
    mean_energy = vecsum(cEnergy, ndata) / ndata;
    ss_tmp = 0;
    for(int i = 0; i < ndata; i++) {
      ss_tmp += (cEnergy[i] - mean_energy) * (cEnergy[i] - mean_energy);
    }
  } else {
    // Otherwise let's proceed.
    deriveVarC(cTemp, cp[1], xHeating, cRows, 1, ndata, nweather, cType);
    deriveVarC(cTemp, cp[3], xCooling, cRows, 2, ndata, nweather, cType);
    
    // Check if we actually have non-zero xHeating/xCooling to avoid errors
    int heating = 0;
    int cooling = 0;
    xmax1 = vecmax(xHeating, ndata);
    if(xmax1 > 0.0) heating = 1;
    xmax2 = vecmax(xCooling, ndata);
    if(xmax2 > 0.0) cooling = 1;;
    
    // Build the appropriate model-matrix & fit to get residual error from 
    // least-squares (remember, this is to select a level of penalty)
    for(int i = 0; i < ndata; i++) {
      if(cIntercept) {
        X[i] = 1;  
      }
      if(xmax1 > 0.0) {
        X[ndata * cIntercept + i] = xHeating[i];
      }
      if(xmax2 > 0.0) {
        X[(cIntercept + heating) * ndata + i] = xCooling[i];
      }
    }   
    
    lmFastCW(X, cEnergy, w, ndata, heating + cooling + cIntercept, betahat);
    ss_tmp = ssError(X, cEnergy, ndata, heating + cooling + cIntercept, betahat);
    
  }
  

  //Have a default value that can be overriden
  //printf("MSE = %f\n", ss_tmp / n);
  if(*cLambda == 0) {
    lambda = sqrt(ss_tmp) * 12;  
    //lambda = ss_tmp / n * 2.5;
  } else {
    lambda = *cLambda * sqrt(ss_tmp);
    //lambda = *cLambda * ss_tmp / n;
  }

  
  mse_best = 0;
  
  //Loop over all heating/cooling change point combinations, coarse scale
  for(double t = tmin; t < tmax; t = t + tstep) {
    for(double t2 = t; t2 < tmax; t2 = t2 + tstep) {
      //Create the xHeating and xCooling
      deriveVarC(cTemp, t, xHeating, cRows, 1, ndata, nweather, cType);
      deriveVarC(cTemp, t2, xCooling, cRows, 2, ndata, nweather, cType);
      
      //mse_cur = l1_best(xHeating, xCooling, n, cEnergy, betahat); 
      //mse_cur = cv_mse(xHeating, xCooling, n, cEnergy, lambda);
      //Was doing Cross Validation... now just doing the objective function
      l1_fit(xHeating, xCooling, ndata, cEnergy, lambda, betahat, cIntercept);
      mse_cur = 0;
      for(int z = 0; z < ndata; z++) {
        if(cIntercept) {
          fitted = betahat[0] + betahat[1] * xHeating[z] + betahat[2] * xCooling[z];  
        } else {
          fitted = betahat[0] * xHeating[z] + betahat[1] * xCooling[z];
        }
        
        mse_cur += ((cEnergy[z] - fitted) * (cEnergy[z] - fitted));
      }
      mse_cur += (lambda * (fabs(betahat[cIntercept]) + fabs(betahat[cIntercept + 1])));
        
      //printf("t = %f, t2 = %f, mse = %f, hs = %f, cs = %f\n", t, t2, mse_cur, betahat[1], betahat[2]);
      if(betahat[cIntercept] >= 0 && betahat[cIntercept + 1] >= 0) {
        if(mse_best == 0) {
          mse_best = mse_cur;
          th_best = t;
          tc_best = t2;          
        } else {
          if(mse_cur < mse_best) {
            mse_best = mse_cur;
            th_best = t;
            tc_best = t2;
          }          
        }
      }
    }
  }

  //Case: we never found anything w/ the correct coefficient signs
  //printf("mse_best = %f, th = %f, tc = %f\n", mse_best, th_best, tc_best);
  if(mse_best < 0.00001) {
    //heatcool[0] = 0;
    //heatcool[1] = 0;
    for(int q = 0; q < (4 + cIntercept); q++) {
      coefs[q] = 0;
    }
  } else {
    //Fit w/ the best, return coefficients...
    deriveVarC(cTemp, th_best, xHeating, cRows, 1, ndata, nweather, cType);
    deriveVarC(cTemp, tc_best, xCooling, cRows, 2, ndata, nweather, cType);
    l1_fit(xHeating, xCooling, ndata, cEnergy, lambda, betahat, cIntercept);
  
    if(cIntercept) {
      coefs[0] = betahat[0];  
    }
    coefs[cIntercept] = th_best;
    coefs[cIntercept + 1] = betahat[cIntercept];
    coefs[cIntercept + 2] = tc_best;
    coefs[cIntercept + 3] = betahat[cIntercept + 1];
    
  }

  
  UNPROTECT(1);
  free(xHeating);
  free(xCooling);
  free(betahat);
  free(w);
  free(cp);
  free(X);
  return results;
}



//Fits the coordinate descent algorithm for a single value of lambda
int l1_fit(double *xHeating, double *xCooling, int n, double *y, double lambda, double *betahat, int intercept) {
  double max_error = 1;
  int nx = 0;
  int ncoefs = 2 + intercept;
  
  //Initialize betahat as 20, 2, 2
  if(intercept) {
    betahat[0] = 20;
  }
  betahat[intercept] = 2;
  betahat[intercept + 1] = 2;
  
  //Malloc a betahat_last
  double *betahat_last = malloc((2 + intercept) * sizeof(double));
  for(int q = 0; q < ncoefs; q++) {
    betahat_last[q] = betahat[q];
  }
  
  while(max_error > .001 && nx++ < 25) {
    one_wheel(xHeating, xCooling, n, y, lambda, betahat, intercept);
    max_error = fabs(betahat[intercept] - betahat_last[intercept]) +
                  fabs(betahat[intercept + 1] - betahat_last[intercept + 1]);
    if(intercept) {
      max_error += fabs(betahat[0] - betahat_last[0]);
    }

    for(int q = 0; q < ncoefs; q++) {
      betahat_last[q] = betahat[q];
    }
  }
  
  free(betahat_last);
  return 0;
}

//Do one round of updating the betahat coefficients for l1_fit
int one_wheel(double *xHeating, double *xCooling, int n, double *y, double lambda, double *betahat, int intercept) {
  double tmp1, tmp2;
  
  // Malloc a vector to hold residuals
  double *r = malloc(n * sizeof(double));
  
  // Update betahat[0], if applicable
  if(intercept) {
    betahat[0] = 0;
    for(int i = 0; i < n; i++) {
      betahat[0] += (y[i] - betahat[1] * xHeating[i] - betahat[2] * xCooling[i]);
    }
    betahat[0] = betahat[0] / n;    
  }

  //Calculate the residuals
  for(int i = 0; i < n; i++) {
    r[i] = y[i]- betahat[intercept] * xHeating[i] - betahat[intercept + 1] * xCooling[i];
    if(intercept) {
      r[i] = r[i] - betahat[0];
    }
  }

  //Calculate a couple of helper sums
  double dx = 0;
  double dx2 = 0;
  double sumxHeating = 0;
  double sumxCooling = 0;
  for(int i = 0; i < n; i++) {
    dx += -(r[i] * xHeating[i]);
    dx2 += -(r[i] * xCooling[i]);
    sumxHeating += (xHeating[i] * xHeating[i]);
    sumxCooling += (xCooling[i] * xCooling[i]);
  }

  //Update betahat[1]
  tmp1 = min(0, betahat[intercept] - (dx - lambda) / sumxHeating);
  tmp2 = max(0, betahat[intercept] - (dx + lambda) / sumxHeating);
  if(fabs(tmp1) > fabs(tmp2)) {
    betahat[1] = tmp1;
  } else {
    betahat[1] = tmp2;
  }
  
  //Update betahat[2]
  tmp1 = min(0, betahat[intercept + 1] - (dx2 - lambda) / sumxCooling);
  tmp2 = max(0, betahat[intercept + 1] - (dx2 + lambda) / sumxCooling);
  if(fabs(tmp1) > fabs(tmp2)) {
    betahat[intercept + 1] = tmp1;
  } else {
    betahat[intercept + 1] = tmp2;
  }  

  free(r);

  return 0;  
}

