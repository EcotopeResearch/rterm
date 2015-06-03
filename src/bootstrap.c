/*
Functions to bootstrap changepoint/degree day models
Mike Logsdon Spring/Summer 2015
*/

#include <R.h>
#include <Rinternals.h>
#include <R_ext/BLAS.h>
#include <R_ext/Linpack.h>
#include <R_ext/Lapack.h>
#include <stdio.h>

#include "classicModels.h"
#include "derivedVariables.h"
#include "lmFast.h"
#include "utility.h"
#include "bootstrap.h"


//C Function callable from R bootstrap a change point model
SEXP bootstrapBaseTemp(SEXP temps, SEXP rows, SEXP energy, SEXP weights, SEXP nreps, SEXP heating, SEXP cooling, SEXP type, SEXP interceptx) {
  int i, j;
  double sigma;
  
  // Process the S Expression arguments
  double *cTemp = REAL(temps);
  int *cRows = INTEGER(rows);
  double *cEnergy = REAL(energy);
  double *w = REAL(weights);
  int *cHeating = INTEGER(heating);
  int *cCooling = INTEGER(cooling);
  int *cNreps = INTEGER(nreps);
  int *cType = INTEGER(type);
  int *cIntercept = INTEGER(interceptx);
  
  // Figure out how much data we're dealing with
  int ndata = length(energy);
  int nweather = length(temps);
  int p = *cIntercept + *cHeating + *cCooling;
  int nCoefs = *cIntercept + 2 * (*cHeating) + 2 * (*cCooling);
  
  //Allocate space to store the coefficients from one regression
  double *coefs = malloc(nCoefs * sizeof(double));
  
  //Allocate a vector to store resampled values
  double *fitted = malloc(ndata * sizeof(double));
  double *cEnergy2 = malloc(ndata * sizeof(double));
  
  // Allocate temp variables xheating and xcooling based on model fit
  double *xheating = malloc(ndata * sizeof(double));
  double *xcooling = malloc(ndata * sizeof(double));
  
  //Initialize a matrix of estimated coefficients, n x nCoefs
  SEXP results = PROTECT(allocMatrix(REALSXP, *cNreps, nCoefs));
  double *cResults = REAL(results);
  
  //Parametric bootstrap, need to fit the model, then simulate from it...
  findBaseTempC(cTemp, cRows, cEnergy, w, ndata, nweather, coefs, *cHeating, *cCooling, *cType, *cIntercept);
  
  // vecprint(coefs, nCoefs);
  
  // Create xcooling & xheating as needed
  if(*cHeating) {
    deriveVarC(cTemp, coefs[*cIntercept], xheating, cRows, 1, ndata, nweather, *cType);
  }
  if(*cCooling) {
    deriveVarC(cTemp, coefs[*cIntercept + (*cHeating * 2)], xcooling, cRows, 2, ndata, nweather, *cType);
  }

  
  //Grab the fitted values
  fittedVals(*cIntercept, *cHeating, *cCooling, xheating, xcooling, ndata, fitted, coefs);
  
  //Estimate the residual standard error -- Sum squared errors / (n - p)
  sigma = 0;
  for(j = 0; j < ndata; j++) {
    sigma = sigma + (fitted[j] - cEnergy[j]) * (fitted[j] - cEnergy[j]);
  }
  sigma = sqrt(sigma / (ndata - nCoefs));
  
  //printf("sigma = %f\n", sigma);
  
  //Loop over reps, resample, get new betahat coefficients, copy to output results
  for(i = 0; i < *cNreps; i++) {
    simOne(fitted, cEnergy2, ndata, sigma);
    //Perform a regression with the bootstrap vectors, copy results
    findBaseTempC(cTemp, cRows, cEnergy2, w, ndata, nweather, coefs, *cHeating, *cCooling, *cType, *cIntercept);
    for(j = 0; j < nCoefs; j++) {
      //printf("coefs[%d] = %f\n", j, coefs[j]);
      cResults[i + (*cNreps) * j] = coefs[j];
    }
  }
  
  //Clean up and exit.
  UNPROTECT(1);
  free(coefs);
  free(fitted);
  free(cEnergy2);
  free(xheating);
  free(xcooling);
  return results;
}



/*Simulate data from the model with coefs held in double *coefs
and temperatures in double *temp. Write the results into double *energyNew
*/

int simOne(double *fitted, double *newVec, int n, double sigma) {
  for(int i = 0; i < n; i++) {
    newVec[i] = rnorm1(fitted[i], sigma);
  }
  return 0;
}

int fittedVals(int intercept, int heating, int cooling, double *xheating, double *xcooling, int n, double *fitted, double *coefs) {
  for(int i = 0; i < n; i++) {
    if(intercept) {
      fitted[i] = coefs[0];
    } else {
      fitted[i] = 0;
    }
    
    if(heating) {
      fitted[i] += coefs[intercept + 1] * xheating[i];
    }
    
    if(cooling) {
      fitted[i] += coefs[intercept + 2 * heating + 1] * xcooling[i];
    }
  }
  return 0;
}

/*
int simulateData(double *xheating, double *xcooling, int n, double *energyNew, int intercept, int heating, int cooling, double *coefs, double sigma) {
  
  for(int i = 0; i < n; i++) {
    if(intercept) {
      
    }
  }
  
  for(int i = 0; i < n; i++) {
    //Reconstruct the mean model
    // base load
    if(intercept) {
      energyNew[i] = coefs[0];
    } else {
      energyNew[i] = 0;
    }
    
    //Cases for different heating/cooling combinations to add slope term(s)
    if(heating && !cooling) {
      energyNew[i] += (coefs[2] * xheating[i];
    } else if(cooling && !heating) {
      if(temp[i] > coefs[2]) {
        energyNew[i] += (coefs[1] * (temp[i] -coefs[2]));
      }
    } else if(heating && cooling) {
      if(temp[i] < coefs[3]) {
        energyNew[i] += (coefs[1] * (coefs[3] - temp[i]));
      } else if(temp[i] > coefs[4]) {
        energyNew[i] += (coefs[2] * (temp[i] - coefs[4]));
      }
    }
    
    
    //Add Gaussian error. Use the Box-Muller transform
    energyNew[i] += rnorm1(0.0, sigma);
  }
  
  return 0;
}
*/



/*
//Writes regression parameters + optimal change point(s) into coefs
int changePointRegression(double *temps, int *rows, double *energy, double *w, int ndata, int nweather, int heating, int cooling, double *coefs, int type, int intercept) {
  int nCps = heating + cooling;
  int p = intercept + nCps;
  int ncoefs = intercept + 2 * nCps;
  int i;
  
  //Allocate space for change points, regression coefs, model matrix, & tmp var
  double *betahat = malloc(p * sizeof(double));
  double *X = malloc(ndata * p * sizeof(double));
  double *cpVarTmp = malloc(ndata * sizeof(double));
  
  //printf("heating = %d, cooling = %d\n", heating, cooling);
  //Find optimal changepoint, writes into cp
  findBaseTempC(temps, rows, energy, w, ndata, nweather, coefs, heating, cooling, type, intercept);
  
  //Check if we lost the change point, just do an OLS
    //Build Model Matrix w/ change point vars based on optimum change point(s)
    for(i = 0; i < n; i++) {
      //First column of ones
      X[i] = 1;
    }
    int k = 1;
    if(heating) {
      //printf("hcp = %f, mintemp = %f, maxtemp = %f\n", cp[0], vecmin(temp, n), vecmax(temp, n));
      if(cp[0] > vecmin(temp, n) && cp[0] < vecmax(temp, n)) {
        makeChangePointVar(temp, n, cp[0], cpVarTmp, 1);
        for(i = 0; i < n; i++) {
          X[k * n + i] = cpVarTmp[i];
        }
        k++;      
      } else {
        heating = 0;
      }
    }
    if(cooling) {
      if(cp[heating + cooling - 1] > vecmin(temp, n) && cp[heating+cooling-1] < vecmax(temp, n)) {
        makeChangePointVar(temp, n, cp[heating + cooling - 1], cpVarTmp, 2);
        for(i = 0; i < n; i++) {
          X[k * n + i] = cpVarTmp[i];
        }      
      } else {
        cooling = 0;
      }
    }
  
    int p2 = heating + cooling + 1;
    //Fit the model
    lmFastC(X, energy, w, n, p2, betahat);
    
    //Copy the results into the coef pointer
    for(i = 0; i < p; i++) {
      if(i < p2) {
        coefs[i] = betahat[i];  
      } else{
        coefs[i] = 0; 
      }
    }
    for(i = 0; i < nCps; i++) {
      if(i < heating + cooling) {
        coefs[p + i] = cp[i];  
      } else {
        coefs[p + i] = 0;
      }
    }
  
  
  //Clean up and exit
  free(cp);
  free(betahat);
  free(X);
  free(cpVarTmp);
  
  return 0;
}
*/


//Populate an integer vector length n by sampling with replacement
//from integers 1:n
int resampleIndexes(int *ind, int n) {
  double randMax = RAND_MAX;
  int indTmp;
  
  for(int i = 0; i < n; i++) {
    indTmp = floor(rand() / randMax * n);
    ind[i] = indTmp;
  }
  return 0;
}

