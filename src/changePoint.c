#include <R.h>
#include <Rinternals.h>
#include <R_ext/BLAS.h>
#include <R_ext/Linpack.h>
#include <R_ext/Lapack.h>
#include <stdio.h>

#include "changePoint.h"



SEXP add(SEXP a, SEXP b) {
  SEXP result = PROTECT(allocVector(REALSXP, 1));
  REAL(result)[0] = asReal(a) + asReal(b);
  UNPROTECT(1);

  return result;
}


//Function to take a temperature vector and a changepoint and return
//the truncated basis function for a regression.
//heatcool: 1 = heating, 2 = cooling
int makeChangePointVar(double *x, int n, double T, double *cpVar, int heatcool) {
  for(int i = 0; i < n; i++) {
    if(heatcool == 1) {
      if(x[i] < T) {
        cpVar[i] = T - x[i];
      } else {
        cpVar[i] = 0;
      }      
    } else {
      if(x[i] > T) {
        cpVar[i] = x[i] - T;
      } else {
        cpVar[i] = 0;
      }
    }

  }
  return 0;
}

/*
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
*/


/*
//lmFast Function to return coefs of a linear regression w/ weights
int lmFastC(double *X, double *y, double *w, int n, int p, double *betahat) {
  
  //Copy x & y.. put the weights into the copies
  double *X2 = malloc(n * p * sizeof(double));
  double *y2 = malloc(n * sizeof(double));
  
  //Put the weights into X2 and y2
  for(int j = 0; j < p; j++) {
    for(int i = 0; i < n; i++) {
      X2[j * n + i] = X[j * n + i] * sqrt(w[i]);
    }
  }
  for(int i = 0; i < n; i++) y2[i] = y[i] * sqrt(w[i]);
  
  //Multiply X' X
  double *XTX = malloc(p * p * sizeof(double));
  multXTX(X2, n, p, XTX);
  
  //Multiply X' y
  double *XTy = malloc(p * sizeof(double));
  multXTy("T", X2, n, p, y2, XTy);
    
  //Solve for the coefs... as solution of X'X * beta = X'y
  solveLinearSystem(XTX, XTy, p);
  //int solveLinearSystem(double *A, double *b, int n, double *x);

  for(int i = 0; i < p; i++) {
    betahat[i] = XTy[i];
  }

  //Free the memory we allocated
  free(XTX);
  free(XTy);
  free(X2);
  free(y2);

  return 0;
}
*/

/*
//lmFast Function that plugs into R... I might not need this to plug into R
SEXP lmFast(SEXP X, SEXP y, SEXP weights) {
  //Grab the dimensions
  int ntotX = length(X);
  int ny = length(y);
  int nrowX = ny;
  int ncolX = ntotX / nrowX;

  //Get C versions of what should be double precision numeric arguments
  double *xX = REAL(X);
  double *xy = REAL(y);
  double *w = REAL(weights);
  
  //Allocate space to hold the coefficients, do a linear fit
  double *betahat = malloc(ncolX * sizeof(double));
  lmFastC(xX, xy, w, nrowX, ncolX, betahat);
  
  //Create an R object to return and populate it with the coefficients
  SEXP ans = PROTECT(allocVector(REALSXP, ncolX));
  for(int i = 0; i < ncolX; i++) {
    REAL(ans)[i] = betahat[i];
  }

  //Free the C vector we created, return the R object
  free(betahat);
  UNPROTECT(1);
  return ans;
}
*/

/*
//Multiply a matrix X' %*% y
int multXTy(char *trans, double *X, int nrowX, int ncolX, double *y, double *z) {
  int incx = 1;
  
  double alpha = 1.0;
  double beta = 0.0;
  
  F77_NAME(dgemv)(trans, &nrowX, &ncolX, &alpha, X, &nrowX, y, &incx, &beta, z, &incx);
  
  return 0;
}

//Multiply a matrix X' %*% X
int multXTX(double *X, int nrowX, int ncolX, double *Y) {
  double alpha = 1.0;
  double beta = 0.0;
  
  F77_NAME(dgemm)("T", "N", &ncolX, &ncolX, &nrowX, &alpha, X, &nrowX, X, &nrowX, &beta, Y, &ncolX);
  
  return 0;
}




//Solve linear system Ax=b, where A is n x n. 'Returns' the answer in 
//the pointer to b
int solveLinearSystem(double *A, double *b, int n) {
  int one = 1;
  int info;
  
  int *IPIV = malloc(n * sizeof(int));
  
  F77_NAME(dgesv)(&n, &one, A, &n, IPIV, b, &n, &info);
  free(IPIV);
  return 0;
}
*/




//Wrapper for findChangePointC that talks to R
SEXP findChangePoint(SEXP temp, SEXP energy, SEXP weights, SEXP heating, SEXP cooling) {
  //Set up the controls..
  int n = length(temp);
  int *cHeating = INTEGER(heating);
  int *cCooling = INTEGER(cooling);
  int nCps = *cHeating + *cCooling;

  //Get C versions of what should be double precision numeric arguments
  double *cTemp = REAL(temp);
  double *cEnergy = REAL(energy);
  double *w = REAL(weights);
  
  
//Allocate space to hold the change points
  double *cp = malloc(nCps * sizeof(double));
  findChangePointC(cTemp, n, cEnergy, w, cp, *cHeating, *cCooling);
  
  
  //Create an R object to return and populate it with the coefficients
  SEXP ans = PROTECT(allocVector(REALSXP, nCps));
  for(int i = 0; i < nCps; i++) {
    REAL(ans)[i] = cp[i];
  }

  //Free the C vector we created, return the R object
  free(cp);
  
  UNPROTECT(1);
  return ans;
  
}


/*Function to find the optimal change point(s)
Takes vectors of temperature and energy, controls on the loop and controls on
heating/cooling, and a pointer to double for the change point that will be written
with the optimum.*/
int findChangePointC(double *temp, int n, double *energy, double *w, double *cp, int heating, int cooling) {
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

  //Set up an array to hold truncated basis function vars for heating/cooling
  //for the regression.
  double *cpVarTmp = malloc(n * sizeof(double));

  //Set up the model Matrix X for the regressions
  int p = 1 + nCps;
  double *X = malloc(n * p * sizeof(double));
  double *X2 = malloc(n * 2 * sizeof(double));

  //Set up a vector betahat for regression coefficients
  double *betahat = malloc(p * sizeof(double));

  //Fill the first column of the model matrix X with ones
  for(i = 0; i < n; i++) {
    X[i] = 1;
    X2[i] = 1;
    X2[n + i] = temp[i];
  }
  
  //Do the first, coarse search.
  ssBest = findBestChangePoint(X, n, temp, energy, w, cp, nCps, heatcool, cpVarTmp, betahat, tmin1, tmax1, tmin2, tmax2, tstepLarge);
  
  //Also fit a model with no change point...
  lmFastC(X2, energy, w, n, 2, betahat);
  ssTmp = ssError(X2, energy, n, 2, betahat);
  if(ssTmp < ssBest) {
    cp[0] = 0;
    if(nCps == 2) {
      cp[1] = 0;
    }
  } else {
       
    //Set the parameters for the second, fine search
    tmin1 = cp[0] - tstep2;
    tmax1 = cp[0] + tstep2;
    if(nCps == 2) {
      tmin2 = cp[1] - tstep2;
      tmax2 = cp[1] + tstep2;
    }
    
  
    //Do the second, fine search.
    findBestChangePoint(X, n, temp, energy, w, cp, nCps, heatcool, cpVarTmp, betahat, tmin1, tmax1, tmin2, tmax2, tstepSmall);
  
    //Check to see if we put in an elbow for a single point
    int nx1 = 0;
    int nx2 = 0;
    if(heating) {
      for(int q = 0; q < n; q++) {
        if(temp[q] < cp[0]) nx1++;
      }
    }
    if(cooling) {
      for(int q = 0; q < n; q++) {
        if(temp[q] > cp[heating + cooling - 1]) nx2++;
      }         
    }
    
    //Heating put in an elbow just for one point
    if(nx1 == 1) {
      cp[0] = vecmin(temp, n) - 1;
    }
    
    //Cooling put in an elbow just for one point
    if(nx2 == 1) {
      cp[heating + cooling - 1] = vecmax(temp, n) + 1;
    } 
  }

  free(X);
  free(X2);
  free(cpVarTmp);
  free(betahat);
  return 0;
}


/*Do a change point search. We'll use this twice, once for the 
coarse search and once for the fine search*/

double findBestChangePoint(double *X, int n, double *temp, double *energy, double *w, double *cp, int nCps, int heatcool, double *cpVarTmp, double *betahat, double tmin1, double tmax1, double tmin2, double tmax2, double tstep) {
  double t, t2;
  double ssTmp, ssBest;
  int j, p;
  p = nCps + 1;
  
  ssBest = 0;
  //Loop over the possible change points. If we have two change
  //point variables then we have to do a time-consuming nested loop
  for(t = tmin1; t <= tmax1; t = t + tstep) {
    //If we have just heating, or just cooling
    if(nCps == 1) {
      makeChangePointVar(temp, n, t, cpVarTmp, heatcool);
      for(j = 0; j < n; j++) {
        X[n + j] = cpVarTmp[j];
      }

      //Fit a model, calc sum of squared errors
      lmFastC(X, energy, w, n, p, betahat);
      ssTmp = ssError(X, energy, n, p, betahat);

      //Check if the sum of squared errors is better
      if(ssBest == 0 && betahat[1] >= 0) {
        ssBest = ssTmp;
        cp[0] = tmin1;
      } else if(betahat[1] >= 0) {
        if(ssTmp < ssBest) {
          ssBest = ssTmp;
          cp[0] = t;
        }
      }
      
    } else {
      //Else we have heating and cooling, so we need a nested loop
      for(t2 = max(tmin2, t); t2 <= tmax2; t2 = t2 + tstep) {
        makeChangePointVar(temp, n, t, cpVarTmp, 1);
        for(j = 0; j < n; j++) {
          X[n + j] = cpVarTmp[j];
        }
  
        makeChangePointVar(temp, n, t2, cpVarTmp, 2);
        for(j = 0; j < n; j++) {
          X[2 * n + j] = cpVarTmp[j];
        }
        
        //Fit a model, calc sum of squared errors
        lmFastC(X, energy, w, n, p, betahat);
        //ssTmp = ssError(X, energy, n, p, betahat);
        ssTmp = ssError(X, energy, n, p, betahat);
        //printf("t = %f, t2 = %f, bl = %f, hs = %f, cs = %f, ss = %f\n", t, t2, betahat[0], betahat[1], betahat[2], ssTmp);

        //Check if the sum of squared errors is better
        if(ssBest == 0 && betahat[1] >= 0 && betahat[2] >= 0) {
          ssBest = ssTmp;
          cp[0] = tmin1;
          cp[1] = tmin2;
        } else if(betahat[1] >= 0 && betahat[2] >= 0) {
          if(ssTmp < ssBest) {
            ssBest = ssTmp;
            cp[0] = t;
            cp[1] = t2;
          }
        }
        
      } //Close of nested loop 
      
    }  //Close of if/else on number of change point variables
    
  } //Close of top loop      
  
  return ssBest;
}




//Application #2) A self-contained regression w/ a lambda for L1 penalization
SEXP l1_path(SEXP temp, SEXP energy, SEXP cps, SEXP lambdas) {
  double *cTemp = REAL(temp);
  double *cEnergy = REAL(energy);
  double *cCps = REAL(cps);
  double *cLambdas = REAL(lambdas);
  int n = length(temp);
  int nl = length(lambdas);
  
  //Allocate space for xHeating and xCooling change point vars
  double *xHeating = malloc(n * sizeof(double));
  double *xCooling = malloc(n * sizeof(double));
  
  //Allocate space for a vector of beta hats
  double *betahat = malloc(3 * sizeof(double));
  
  //Set up the results matrix...
  SEXP results = PROTECT(allocMatrix(REALSXP, nl, 4));
  double *cResults = REAL(results);
  
  //Populate the xHeating and xCooling variables
  makeChangePointVar(cTemp, n, cCps[0], xHeating, 1);
  makeChangePointVar(cTemp, n, cCps[1], xCooling, 2);
  
  //Iterate over values of lambda
  for(int j = 0; j < nl; j++) {
    // 1) Perform the fit & write out the coefficients
    l1_fit(xHeating, xCooling, n, cEnergy, cLambdas[j], betahat);
    for(int q = 0; q < 3; q++) {
      cResults[q * nl + j] = betahat[q];
    }
    // 2) Calculate the mean squared error from leave-one-out CV
    cResults[3 * nl + j] = cv_mse(xHeating, xCooling, n, cEnergy, cLambdas[j]);
  }
  
  //Clean up and return
  UNPROTECT(1);
  free(xHeating);
  free(xCooling);
  free(betahat);
  return results;
}



//Do an l1 penalized changepoint model. Return heating/cooling
SEXP cpl1(SEXP temp, SEXP energy, SEXP lambdax) {
  double mse_cur, mse_best, lambda, ss_tmp, fitted;
  double xmax1, xmax2;

  //Read some inputs
  double *cTemp = REAL(temp);
  double *cEnergy = REAL(energy);
  double *cLambda = REAL(lambdax);
  int n = length(temp);
  
  //Set up the change point search parameters
  double tmin = 40;
  double tmax = 80;
  double tstep = 1;
  double th_best = 0;
  double tc_best = 0;
  
  //Allocate space for xHeating and xCooling variables
  double *xHeating = malloc(n * sizeof(double));
  double *xCooling = malloc(n * sizeof(double));
  double *betahat = malloc(3 * sizeof(double));
  double *w = malloc(n * sizeof(double));
  double *cp = malloc(2 * sizeof(double));
  double *X = malloc(n * 3 * sizeof(double));
  
  //Just put the weights as 1
  for(int a = 0; a < n; a++) {
    w[a] = 1;
  }
  
  //Prep the output
  SEXP results = PROTECT(allocVector(REALSXP, 5));
  double *coefs = REAL(results);
  

  //Do a least-squares fit... then base lambda off of residual variation
  findChangePointC(cTemp, n, cEnergy, w, cp, 1, 1);

  if(cp[0] == 0 && cp[1] == 0) {
    for(int i = 0; i < n; i++) {
      X[i] = 1;
      X[n + i] = cTemp[i];
    }
    lmFastC(X, cEnergy, w, n, 2, betahat);
    ss_tmp = ssError(X, cEnergy, n, 2, betahat);    
  } else {
    makeChangePointVar(cTemp, n, cp[0], xHeating, 1);
    makeChangePointVar(cTemp, n, cp[1], xCooling, 2);  
    
    int heating = 0;
    int cooling = 0;
    xmax1 = vecmax(xHeating, n);
    if(xmax1 > 0.0) heating = 1;
    xmax2 = vecmax(xCooling, n);
    if(xmax2 > 0.0) cooling = 1;;
      
    for(int i = 0; i < n; i++) {
      X[i] = 1;
      if(xmax1 > 0.0) {
        X[n + i] = xHeating[i];
      }
      if(xmax2 > 0.0) {
        X[(heating + cooling) * n + i] = xCooling[i];
      }
    }   
    
    lmFastC(X, cEnergy, w, n, heating + cooling + 1, betahat);
    ss_tmp = ssError(X, cEnergy, n, heating + cooling + 1, betahat);
    
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

  
  //printf("hcp = %f, ccp = %f, bl = %f, hs = %f, cs = %f", cp[0], cp[1], betahat[0], betahat[1], betahat[2]);
  mse_best = 0;
  //Loop over all heating/cooling change point combinations, coarse scale
  for(double t = tmin; t < tmax; t = t + tstep) {
    for(double t2 = t; t2 < tmax; t2 = t2 + tstep) {
      //Create the xHeating and xCooling
      makeChangePointVar(cTemp, n, t, xHeating, 1);
      makeChangePointVar(cTemp, n, t2, xCooling, 2);
      
      //mse_cur = l1_best(xHeating, xCooling, n, cEnergy, betahat); 
      //mse_cur = cv_mse(xHeating, xCooling, n, cEnergy, lambda);
      //Was doing Cross Validation... now just doing the objective function
      l1_fit(xHeating, xCooling, n, cEnergy, lambda, betahat);
      mse_cur = 0;
      for(int z = 0; z < n; z++) {
        fitted = betahat[0] + betahat[1] * xHeating[z] + betahat[2] * xCooling[z];
        mse_cur += ((cEnergy[z] - fitted) * (cEnergy[z] - fitted));
      }
      mse_cur += (lambda * (fabs(betahat[1]) + fabs(betahat[2])));
        
      //printf("t = %f, t2 = %f, mse = %f, hs = %f, cs = %f\n", t, t2, mse_cur, betahat[1], betahat[2]);
      if(betahat[1] >= 0 && betahat[2] >= 0) {
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
    coefs[0] = betahat[0]; //Baseload
    coefs[0] = 0;
    coefs[1] = 0;  //Heating Slope
    coefs[2] = 0;  //Heating Change Point
    coefs[3] = 0;  //Cooling Slope
    coefs[4] = 0;  //Cooling Change Point
  } else {
    //Fit w/ the best, return coefficients...
    makeChangePointVar(cTemp, n, th_best, xHeating, 1);
    makeChangePointVar(cTemp, n, tc_best, xCooling, 2);
    l1_fit(xHeating, xCooling, n, cEnergy, lambda, betahat);
  
    //heatcool[0] = (betahat[1] != 0);
    //heatcool[1] = (betahat[2] != 0);    
    coefs[0] = betahat[0];
    coefs[1] = betahat[1];
    coefs[2] = th_best;
    coefs[3] = betahat[2];
    coefs[4] = tc_best;
    
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




/*
//Function to return the optimal coefficients from an L1 penalized regression
//Starts at least squares and ratchets up the penalty until there is no more 
//improvement in the leave-one-out Cross Validation error
double l1_best(double *xHeating, double *xCooling, int n, double *y, double *betahat) {
  double mse_cur, mse_last;
  int nx = 0;
  int NMAX = 1000;
  
  double lambda = 0;
  double lambda_step = 1.0;
  
  //Fit the non-penalized dealy first.
  mse_cur = cv_mse(xHeating, xCooling, n, y, lambda);
  mse_last = mse_cur + 1;
  
  while(mse_cur < mse_last && nx++ < NMAX) {
    mse_last = mse_cur;
    lambda = lambda + lambda_step;
    mse_cur = cv_mse(xHeating, xCooling, n, y, lambda);
  }
  if(nx == NMAX) printf("ERROR DID NOT CONVERGE\n");
  
  lambda = lambda - lambda_step;
  l1_fit(xHeating, xCooling, n, y, lambda, betahat);
  
  return mse_last;
}

*/



//calculate a cross-validation mean-squared error
double cv_mse(double *xHeating, double *xCooling, int n, double *y, double lambda) {
  double mse = 0;
  double fitted;
  
  double *xh2 = malloc((n - 1) * sizeof(double));
  double *xc2 = malloc((n - 1) * sizeof(double));
  double *betahat = malloc(3 * sizeof(double));
  
  //Leave one out...
  for(int i = 0; i < n; i++) {
    int k = 0;
    for(int j = 0; j < n; j++) {
      if(j != i) {
        xh2[k] = xHeating[k];
        xc2[k] = xCooling[k];
        k++;
      }
    }
    l1_fit(xh2, xc2, n - 1, y, lambda, betahat);
    
    fitted = betahat[0] + betahat[1] * xHeating[i] + betahat[2] * xCooling[i];
    mse += ((y[i] - fitted) * (y[i] - fitted));
  }
  
  mse = mse / n;
  
  free(xh2);
  free(xc2);
  free(betahat);
  
  return mse;
}



/*
//Fits the coordinate descent algorithm for a single value of lambda
int l1_fit(double *xHeating, double *xCooling, int n, double *y, double lambda, double *betahat) {
  double max_error = 1;
  int nx = 0;
  
  //Initialize betahat as 20, 2, 2
  betahat[0] = 20;
  betahat[1] = 2;
  betahat[2] = 2;
  
  //Malloc a betahat_last
  double *betahat_last = malloc(3 * sizeof(double));
  for(int q = 0; q < 3; q++) {
    betahat_last[q] = betahat[q];
  }
  
  while(max_error > .001 && nx++ < 25) {
    one_wheel(xHeating, xCooling, n, y, lambda, betahat);
    max_error = fabs(betahat[0] - betahat_last[0]) + 
                  fabs(betahat[1] - betahat_last[1]) +
                  fabs(betahat[2] - betahat_last[2]);
    for(int q = 0; q < 3; q++) {
      betahat_last[q] = betahat[q];
    }
  }
  
  free(betahat_last);
  return 0;
}
*/



/*
//Do one round of updating the betahat coefficients for l1_fit
int one_wheel(double *xHeating, double *xCooling, int n, double *y, double lambda, double *betahat) {
  double tmp1, tmp2;
  
  //Malloc a vector to hold residuals
  double *r = malloc(n * sizeof(double));
  
  //Update betahat[0]
  betahat[0] = 0;
  for(int i = 0; i < n; i++) {
    betahat[0] += (y[i] - betahat[1] * xHeating[i] - betahat[2] * xCooling[i]);
  }
  betahat[0] = betahat[0] / n;
  
  //Calculate the residuals
    for(int i = 0; i < n; i++) {
    r[i] = y[i]- betahat[0] - betahat[1] * xHeating[i] - betahat[2] * xCooling[i];
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
  tmp1 = min(0, betahat[1] - (dx - lambda) / sumxHeating);
  tmp2 = max(0, betahat[1] - (dx + lambda) / sumxHeating);
  if(fabs(tmp1) > fabs(tmp2)) {
    betahat[1] = tmp1;
  } else {
    betahat[1] = tmp2;
  }
  
  //Update betahat[2]
  tmp1 = min(0, betahat[2] - (dx2 - lambda) / sumxCooling);
  tmp2 = max(0, betahat[2] - (dx2 + lambda) / sumxCooling);
  if(fabs(tmp1) > fabs(tmp2)) {
    betahat[2] = tmp1;
  } else {
    betahat[2] = tmp2;
  }  

  free(r);

  return 0;  
}

*/


//C Function callable from R bootstrap a change point model
SEXP bootstrapChangePoint(SEXP temp, SEXP energy, SEXP weights, SEXP nreps, SEXP heating, SEXP cooling, SEXP parametric) {
  int i, j;
  double sigma;
  
  //Process the S Expression arguments
  double *cTemp = REAL(temp);
  double *cEnergy = REAL(energy);
  double *w = REAL(weights);
  int *cHeating = INTEGER(heating);
  int *cCooling = INTEGER(cooling);
  int *cNreps = INTEGER(nreps);
  int *cParam = INTEGER(parametric);
  int n = length(temp);
  int p = 1 + *cHeating + *cCooling;
  int nCoefs = p + p - 1;
  
  //Allocate space to store the coefficients from one regression
  double *coefs = malloc(nCoefs * sizeof(double));
  
  //Allocate a vector to store resampled indexes, temp & energy vectors for 
  //resampled values
  int *ind = malloc(n * sizeof(int));
  double *cTemp2 = malloc(n * sizeof(double));
  double *cEnergy2 = malloc(n * sizeof(double));
  
  //Initialize a matrix of estimated coefficients, n x nCoefs
  SEXP results = PROTECT(allocMatrix(REALSXP, *cNreps, nCoefs));
  double *cResults = REAL(results);
  
  //Loop over reps, resample, get new betahat coefficients, copy to output results
  for(i = 0; i < *cNreps; i++) {
    
    //Perform the bootstrap... We have two ways of doing this
    //1) Non-parametric and 2) Parametric. Parametric probably makes more sense
    //for small datasets.
    
    if(*cParam == 0) {
      //Non-Parametric, resample the indexes
      resampleIndexes(ind, n);
  
      //Populate the temp & energy dummy vectors according to resampled indexes
      for(j = 0; j < n; j++) {
        cTemp2[j] = cTemp[ind[j]];
        cEnergy2[j] = cEnergy[ind[j]];
      }      
    } else if(*cParam == 1) {
      //Parametric bootstrap, need to fit the model, then simulate from it...
      changePointRegression(cTemp, n, cEnergy, w, *cHeating, *cCooling, coefs);
      //for(int abc = 0; abc < nCoefs; abc++) printf("coefs[%d] = %f\n", abc, coefs[abc]);
      
      //We need the fitted values from the regression -- sim data w/ sd 0
      simulateData(cTemp, n, cEnergy2, *cHeating, *cCooling, coefs, 0.0);
      
      //Estimate the residual standard error -- Sum squared errors / (n - p)
      sigma = 0;
      for(j = 0; j < n; j++) {
        sigma = sigma + (cEnergy2[j] - cEnergy[j]) * (cEnergy2[j] - cEnergy[j]);
      }
      sigma = sqrt(sigma / (n - nCoefs));

      //Simulate new data
      simulateData(cTemp, n, cEnergy2, *cHeating, *cCooling, coefs, sigma);
      
      //In this case the temp data is unchanged, so we copy cTemp into cTemp2
      for(j = 0; j < n; j++) {
        cTemp2[j] = cTemp[j];
      }

    }

    
    //Perform a regression with the bootstrap vectors, copy results
    changePointRegression(cTemp2, n, cEnergy2, w, *cHeating, *cCooling, coefs);
    for(j = 0; j < nCoefs; j++) {
      //printf("coefs[%d] = %f\n", j, coefs[j]);
      cResults[i + (*cNreps) * j] = coefs[j];
    }
  }
  
  //Clean up and exit.
  UNPROTECT(1);
  free(coefs);
  free(ind);
  free(cTemp2);
  free(cEnergy2);
  return results;
}


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


//Writes regression parameters + optimal change point(s) into coefs
int changePointRegression(double *temp, int n, double *energy, double *w, int heating, int cooling, double *coefs) {
  int nCps = heating + cooling;
  int p = 1 + nCps;
  int i;
  
  //Allocate space for change points, regression coefs, model matrix, & tmp var
  double *cp = malloc(nCps * sizeof(double));
  double *betahat = malloc(p * sizeof(double));
  double *X = malloc(n * p * sizeof(double));
  double *cpVarTmp = malloc(n * sizeof(double));
  
  //printf("heating = %d, cooling = %d\n", heating, cooling);
  //Find optimal changepoint, writes into cp
  findChangePointC(temp, n, energy, w, cp, heating, cooling);
  
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


/*Simulate data from the model with coefs held in double *coefs
and temperatures in double *temp. Write the results into double *energyNew
*/

int simulateData(double *temp, int n, double *energyNew, int heating, int cooling, double *coefs, double sigma) {
  
  for(int i = 0; i < n; i++) {
    //Reconstruct the mean model
    energyNew[i] = coefs[0]; //Base Load
    
    //Cases for different heating/cooling combinations to add slope term(s)
    if(heating && !cooling) {
      if(temp[i] < coefs[2]) {
        energyNew[i] += (coefs[1] * (coefs[2] - temp[i]));
      }
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

//Box Muller transform for a normal random variable
double rnorm1(double mu, double sigma) {
  const double pi = 3.141592653589793;
  double u1 = rand() * (1.0 / RAND_MAX);
  double u2 = rand() * (1.0 / RAND_MAX);
  double z0 = sqrt(-2.0 * log(u1)) * cos(2 * pi * u2);
  
  return z0 * sigma + mu;
}



double max(double a, double b) {
  double tmp;
  if(a >= b) {
    tmp = a;
  } else {
    tmp = b;
  }
  return tmp;
}

double min(double a, double b) {
  double tmp;
  if(a <= b) {
    tmp = a;
  } else {
    tmp = b;
  }
  return tmp;
}


double vecmin(double *x, int n) {
  double min;
  min = x[0];
  for(int i = 1; i < n; i++) {
    if(x[i] < min) min = x[i];
  }
  return min;
}


double vecmax(double *x, int n) {
  double max;
  max = x[0];
  for(int i = 1; i < n; i++) {
    if(x[i] > max) max = x[i];
  }  
  return max;
}


int vecprint(double *x, int n) {
  for(int i = 0; i < n; i++) {
    printf("x[%d] = %f\n", i, x[i]);
  }
  return 0;
}


/*

SEXP cplm_fit(SEXP temp, SEXP energy, SEXP weights, SEXP heating, SEXP cooling, SEXP lambda) {
  
  //Set up the controls..
  int n = length(temp);
  int *cHeating = INTEGER(heating);
  int *cCooling = INTEGER(cooling);
  int nCps = *cHeating + *cCooling;
  int nCoefs = nCps * 2 + 1;

  //Get C versions of what should be double precision numeric arguments
  double *cTemp = REAL(temp);
  double *cEnergy = REAL(energy);
  double *w = REAL(weights);
  double *cLambda = REAL(lambda);  
  
  
  //Allocate space to hold the coefficients
  double *coefs = malloc(nCoefs * sizeof(double));
  findChangePointC(cTemp, n, cEnergy, w, cp, *cHeating, *cCooling, *cLambda);
  
  
  //Given the optimal change points, fit the model again...
  makeChangePointVar(cTemp, n, t, cpVarTmp, 1);
  for(j = 0; j < n; j++) {
    X[n + j] = cpVarTmp[j];
  }
  
  makeChangePointVar(temp, n, t2, cpVarTmp, 2);
  for(j = 0; j < n; j++) {
    X[2 * n + j] = cpVarTmp[j];
  }
        
  //Fit a model, calc sum of squared errors
  lmFastC(X, energy, w, n, p, betahat);
  
  
  //Create an R object to return and populate it with the coefficients
  SEXP ans = PROTECT(allocVector(REALSXP, nCoefs));
  for(int i = 0; i < nCps; i++) {
    REAL(ans)[i] = cp[i];
  }

  //Free the C vector we created, return the R object
  free(cp);
  
  UNPROTECT(1);
  return ans;
  
  
}
*/
