/*
Fast linear model functions
Weighted and non-weighted least squares
Using lapack/linpack compiled fortran

Mike Logsdon - Spring 2015
*/

#include <R.h>
#include <Rinternals.h>
#include <R_ext/BLAS.h>
#include <R_ext/Linpack.h>
#include <R_ext/Lapack.h>
#include <stdio.h>

#include "lmFast.h"


// lmFast Function that plugs into R... I might not need this to plug into R
// but it seems useful for testing my custom lm code
SEXP lmFast(SEXP X, SEXP y) {
  //Grab the dimensions
  int ntotX = length(X);
  int ny = length(y);
  int nrowX = ny;
  int ncolX = ntotX / nrowX;

  //Get C versions of what should be double precision numeric arguments
  double *xX = REAL(X);
  double *xy = REAL(y);
  
  //Allocate space to hold the coefficients, do a linear fit
  double *betahat = malloc(ncolX * sizeof(double));
  lmFastC(xX, xy, nrowX, ncolX, betahat);
  
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


// lmFast Function that plugs into R... I might not need this to plug into R
// but it seems useful for testing my custom lm code
SEXP lmFastW(SEXP X, SEXP y, SEXP weights) {
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
  lmFastCW(xX, xy, w, nrowX, ncolX, betahat);
  
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



//lmFast Function to return coefs of a linear regression w/ weights
int lmFastC(double *X, double *y, int n, int p, double *betahat) {
  
  
  //Multiply X' X
  double *XTX = malloc(p * p * sizeof(double));
  multXTX(X, n, p, XTX);
  
  //Multiply X' y
  double *XTy = malloc(p * sizeof(double));
  multXTy("T", X, n, p, y, XTy);
    
  //Solve for the coefs... as solution of X'X * beta = X'y
  solveLinearSystem(XTX, XTy, p);
  //int solveLinearSystem(double *A, double *b, int n, double *x);

  for(int i = 0; i < p; i++) {
    betahat[i] = XTy[i];
  }

  //Free the memory we allocated
  free(XTX);
  free(XTy);

return 0;
}



//lmFast Function to return coefs of a linear regression w/ weights
int lmFastCW(double *X, double *y, double *w, int n, int p, double *betahat) {
  
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


