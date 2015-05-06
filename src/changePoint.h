/*
Header File for Functions to fit Changepoint models in C
Mike Logsdon
Spring 2015
*/
#include <R.h>
#include <Rinternals.h>

//At a basic level we need to fit a LM in C (+ an R interface to make sure it works)
//SEXP lmFast(SEXP X, SEXP y, SEXP w);
//int lmFastC(double *X, double *y, double *w, int n, int p, double *betahat);

//Functions to help with that
//int multXTy(char *trans, double *X, int nrowX, int ncolX, double *y, double *z);
//int multXTX(double *X, int nrowX, int ncolX, double *Y);
//int solveLinearSystem(double *A, double *b, int n);

//Application #1) Find the optimal change point by sum of squared resids
SEXP findChangePoint(SEXP X, SEXP y, SEXP weights, SEXP heating, SEXP cooling);
int findChangePointC(double *temp, int n, double *energy, double *w, double *cp, int heating, int cooling);
double findBestChangePoint(double *X, int n, double *temp, double *energy, double *w, double *cp, int nCps, int heatcool, double *cpVarTmp, double *betahat, double tmin1, double tmax1, double tmin2, double tmax2, double tstep);
int makeChangePointVar(double *x, int n, double T, double *cpVar, int heatcool);
double ssError(double *X, double *y, int n, int p, double *betahat);

//Application #2) A self-contained regression w/ a lambda for L1 penalization
SEXP l1_path(SEXP temp, SEXP energy, SEXP cps, SEXP lambdas);
SEXP cpl1(SEXP temp, SEXP energy, SEXP lambdax);
double l1_best(double *xHeating, double *xCooling, int n, double *y, double *betahat);
int l1_fit(double *xHeating, double *xCooling, int n, double *y, double lambda, double *betahat);
int one_wheel(double *xHeating, double *xCooling, int n, double *y, double lambda, double *betahat);
double cv_mse(double *xHeating, double *xCooling, int n, double *y, double lambda);

//Application #3) Bootstrap, the SEXP function loops over the second two
SEXP bootstrapChangePoint(SEXP temp, SEXP energy, SEXP weights, SEXP nreps, SEXP heating, SEXP cooling, SEXP parametric);
int resampleIndexes(int *ind, int n);
int changePointRegression(double *temp, int n, double *energy, double *w, int heating, int cooling, double *coefs);
int simulateData(double *temp, int n, double *energyNew, int heating, int cooling, double *coefs, double sigma);
double rnorm1(double mu, double sigma);


//Utility functions
double max(double a, double b);
double min(double a, double b);
double vecmin(double *x, int n);
double vecmax(double *x, int n);
int vecprint(double *x, int n);

