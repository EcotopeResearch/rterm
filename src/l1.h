/*
Header File for Functions to fit Changepoint models in C
Mike Logsdon
Spring 2015
*/
#include <R.h>
#include <Rinternals.h>

// Here's the summary paper on fitting L1
// http://projecteuclid.org/download/pdfview_1/euclid.aoas/1206367819

//Application #2) A self-contained regression w/ a lambda for L1 penalization
// SEXP l1_path(SEXP temp, SEXP energy, SEXP cps, SEXP lambdas);
SEXP l1(SEXP temps, SEXP rows, SEXP energy, SEXP lambdax, SEXP type, SEXP intercept);
int l1_fit(double *xHeating, double *xCooling, int n, double *y, double lambda, double *betahat, int intercept);
int one_wheel(double *xHeating, double *xCooling, int n, double *y, double lambda, double *betahat, int intercept);
