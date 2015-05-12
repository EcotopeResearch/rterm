#include <R.h>
#include <Rinternals.h>
#include <R_ext/BLAS.h>
#include <R_ext/Linpack.h>
#include <R_ext/Lapack.h>
#include <stdio.h>

#include "classicModels.h"
#include "derivedVariables.h"
#include "lmFast.h"




//Wrapper for findBaseTempC that talks to R
SEXP findBaseTemp(SEXP temps, SEXP rows, SEXP energy, SEXP weights, SEXP heating, SEXP cooling, SEXP type, SEXP intercept) {
  // Set up the controls..
  int nweather = length(temps);
  int ndata = length(energy);
  int *cHeating = INTEGER(heating);
  int *cCooling = INTEGER(cooling);
  int *cType = INTEGER(type);
  int *cIntercept = INTEGER(intercept);
  int *cRows = INTEGER(rows);
  int nCps = *cHeating + *cCooling;

  // Get C versions of what should be double precision numeric arguments
  double *cTemp = REAL(temps);
  double *cEnergy = REAL(energy);
  double *w = REAL(weights);
  
  
  // Allocate space to hold the coefficients
  // Order = Intercept, heatingBase, heatingSlope, coolingBase, coolingSlope
  int ncoefs = *cIntercept + 2 * nCps;
  double *cp = malloc(ncoefs * sizeof(double));
  findBaseTempC(cTemp, cRows, cEnergy, w, ndata, nweather, cp, *cHeating, *cCooling, *cType, *cIntercept);
  
  
  // Create an R object to return and populate it with the coefficients
  SEXP ans = PROTECT(allocVector(REALSXP, ncoefs));
  for(int i = 0; i < ncoefs; i++) {
    REAL(ans)[i] = cp[i];
  }

  // Free the C vector we created, return the R object
  free(cp);
  
  UNPROTECT(1);
  return ans;
  
}


/*Function to find the optimal change point(s)
Takes vectors of temperature and energy, controls on the loop and controls on
heating/cooling, and a pointer to double for the change point that will be written
with the optimum.*/
int findBaseTempC(double *temps, int *rows, double *energy, double *w, int ndata, int nweather, double *cp, int heating, int cooling, int type, int intercept) {
  double ssBest, ssTmp, tmin1, tmin2, tmax1, tmax2;
  int i, heatcool;
  double tstepLarge = 1;
  double tstepSmall = 0.1;
  double tstep2 = 1;
  
  //Control for how many change points we're dealing with...
  //also set the the tmin and tmax ranges for change point search
  tmin1 = 30;
  tmax1 = 80;
  tmin2 = 55;
  tmax2 = 90;
  int nCps = heating + cooling;
  if(nCps == 1) {
    if(heating) {
      heatcool = 1; 
    } else {
      heatcool = 2;
      tmin1 = 55;
      tmax1 = 90;
    }
  } else {
    heatcool = 0;
  }
  int ncoefs = intercept + nCps * 2;

  //Set up an array to hold truncated basis function vars for heating/cooling
  //for the regression.
  double *cpVarTmp = malloc(ndata * sizeof(double));


  //Set up the model Matrix X for the regressions
  int p = intercept + nCps;
  double *X = malloc(ndata * p * sizeof(double)); 

  //Set up a vector betahat for regression coefficients
  double *betahat = malloc(p * sizeof(double));

  //Fill the first column of the model matrix X with ones
  if(intercept) {
    for(i = 0; i < ndata; i++) {
      X[i] = 1;
    }
  }
  
  // Do the first, coarse search.
  ssBest = findBestBaseTemp(X, temps, rows, energy, w, ndata, nweather, cp, nCps, heatcool, cpVarTmp, betahat, tmin1, tmax1, tmin2, tmax2, tstepLarge, type, intercept);

  // Check if ssBest == 0, that means we never found a valid solution
  if(ssBest == 0) {
     for(int i = 0; i < ncoefs; i++) {
       cp[i] = 0;
     }
  } else {

    //Set the parameters for the second, fine search
    tmin1 = cp[intercept] - tstep2;
    tmax1 = cp[intercept] + tstep2;
    if(nCps == 2) {
      tmin2 = cp[intercept + 2] - tstep2;
      tmax2 = cp[intercept + 2] + tstep2;
    }
  
    // Do the second, fine search.
    ssBest = findBestBaseTemp(X, temps, rows, energy, w, ndata, nweather, cp, nCps, heatcool, cpVarTmp, betahat, tmin1, tmax1, tmin2, tmax2, tstepSmall, type, intercept);

    // Check if ssBest == 0, that means we never found a valid solution
    if(ssBest == 0) {
      for(int i = 0; i < ncoefs; i++) {
        cp[i] = 0;
      }
    }
  }

  free(X);
  free(cpVarTmp);
  free(betahat);
  return 0;
}


/*Do a change point search. We'll use this twice, once for the 
coarse search and once for the fine search*/

double findBestBaseTemp(double *X, double *temps, int *rows, double *energy, double *w, int ndata, int nweather, double *cp, int nCps, int heatcool, double *cpVarTmp, double *betahat, double tmin1, double tmax1, double tmin2, double tmax2, double tstep, int type, int intercept) {
  double t, t2;
  double ssTmp, ssBest;
  int j, p;
  p = nCps + intercept;
  
  ssBest = 0;
  //Loop over the possible change points. If we have two change
  //point variables then we have to do a time-consuming nested loop
  for(t = tmin1; t <= tmax1; t = t + tstep) {
    //If we have just heating, or just cooling
    if(nCps == 1) {
      deriveVarC(temps, t, cpVarTmp, rows, heatcool, ndata, nweather, type);
      // makeChangePointVar(temp, n, t, cpVarTmp, heatcool);
      if(vecsum(cpVarTmp, ndata) == 0) {
        continue;
      }
      for(j = 0; j < ndata; j++) {
        X[intercept * ndata + j] = cpVarTmp[j];
      }

      //Fit a model, calc sum of squared errors
      lmFastCW(X, energy, w, ndata, p, betahat);
      ssTmp = ssError(X, energy, ndata, p, betahat);

      //Check if the sum of squared errors is better
      if(ssBest == 0 && betahat[intercept] >= 0) {
        ssBest = ssTmp;
        if(intercept) {
          cp[0] = betahat[0];
          cp[1] = t;
          cp[2] = betahat[1];
        } else {
          cp[0] = t;
          cp[1] = betahat[0];
        }
      } else if(betahat[intercept] >= 0) {
        if(ssTmp < ssBest) {
          ssBest = ssTmp;
          if(intercept) {
            cp[0] = betahat[0];
            cp[1] = t;
            cp[2] = betahat[1];
          } else {
            cp[0] = t;
            cp[1] = betahat[0];
          }
        }
      }
      
    } else {
      //Else we have heating and cooling, so we need a nested loop
      for(t2 = max(tmin2, t); t2 <= tmax2; t2 = t2 + tstep) {
        deriveVarC(temps, t, cpVarTmp, rows, 1, ndata, nweather, type);
        for(j = 0; j < ndata; j++) {
          X[intercept * ndata + j] = cpVarTmp[j];
        }
  
        deriveVarC(temps, t, cpVarTmp, rows, 2, ndata, nweather, type);
        for(j = 0; j < ndata; j++) {
          X[(1 + intercept) * ndata + j] = cpVarTmp[j];
        }
        
        //Fit a model, calc sum of squared errors
        lmFastCW(X, energy, w, ndata, p, betahat);
        //ssTmp = ssError(X, energy, n, p, betahat);
        ssTmp = ssError(X, energy, ndata, p, betahat);
        //printf("t = %f, t2 = %f, bl = %f, hs = %f, cs = %f, ss = %f\n", t, t2, betahat[0], betahat[1], betahat[2], ssTmp);

        //Check if the sum of squared errors is better
        if(ssBest == 0 && betahat[intercept] >= 0 && betahat[intercept + 1] >= 0) {
          ssBest = ssTmp;
          if(intercept) {
            cp[0] = betahat[0];
            cp[1] = t;
            cp[2] = betahat[1];
            cp[3] = t2;
            cp[4] = betahat[2];
          } else {
            cp[0] = t;
            cp[1] = betahat[0];
            cp[2] = t2;
            cp[3] = betahat[1];
          }
        } else if(betahat[intercept] >= 0 && betahat[intercept + 1] >= 0) {
          if(ssTmp < ssBest) {
            ssBest = ssTmp;
            if(intercept) {
              cp[0] = betahat[0];
              cp[1] = t;
              cp[2] = betahat[1];
              cp[3] = t2;
              cp[4] = betahat[2];
            } else {
              cp[0] = t;
              cp[1] = betahat[0];
              cp[2] = t2;
              cp[3] = betahat[1];
            }
          }
        }
        
      } //Close of nested loop 
      
    }  //Close of if/else on number of change point variables
    
  } //Close of top loop      
  
  return ssBest;
}


//Function to calculate the sum of squared errors for a regression fit
double ssError(double *X, double *y, int n, int p, double *betahat) {
  double ss = 0;
  
  //Start by calculating residuals...
  double *resids = malloc(n * sizeof(double));
  multXTy("N", X, n, p, betahat, resids);
  //int multXTy(char *trans, double *X, int nrowX, int ncolX, double *y, double *z)
  
  //Now sum them
  for(int i = 0; i < n; i++) {
    ss += (resids[i] - y[i]) * (resids[i] - y[i]);
  }
  
  //Free the resids vector
  free(resids);
  
  return ss;
}



double vecsum(double *x, int n) {
  double total = 0.0;
  
  for(int i = 0; i < n; i++) {
    total += x[i];
  }
  
  return total;
}













