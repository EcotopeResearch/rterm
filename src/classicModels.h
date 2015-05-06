/*
Header File for Functions to fit Changepoint models in C
Mike Logsdon
Spring 2015
*/
#include <R.h>
#include <Rinternals.h>

//At a basic level we need to fit a LM in C (+ an R interface to make sure it works)
SEXP lmFast(SEXP X, SEXP y, SEXP w);
int lmFastC(double *X, double *y, int n, int p, double *betahat);
int lmFastCW(double *X, double *y, double *w, int n, int p, double *betahat);

//Functions to help with that
int multXTy(char *trans, double *X, int nrowX, int ncolX, double *y, double *z);
int multXTX(double *X, int nrowX, int ncolX, double *Y);
int solveLinearSystem(double *A, double *b, int n);

//Application #1) Find the optimal change point by sum of squared resids
SEXP findBaseTemp(SEXP temps, SEXP rows, SEXP energy, SEXP weights, SEXP heating, SEXP cooling, SEXP type, SEXP intercept);
int findBaseTempC(double *temps, int *rows, double *energy, double *w, int ndata, int nweather, double *cp, int heating, int cooling, int type, int intercept);
double findBestBaseTemp(double *X, double *temps, int *rows, double *energy, double *w, int ndata, int nweather, double *cp, int nCps, int heatcool, double *cpVarTmp, double *betahat, double tmin1, double tmax1, double tmin2, double tmax2, double tstep, int type, int intercept);
double ssError(double *X, double *y, int n, int p, double *betahat);



/*
//Application #2) A self-contained regression w/ a lambda for L1 penalization
// SEXP l1_path(SEXP temp, SEXP energy, SEXP cps, SEXP lambdas);
SEXP l1(SEXP temp, SEXP energy, SEXP lambdax, SEXP type);
double l1_best(double *xHeating, double *xCooling, int n, double *y, double *betahat, int type);
int l1_fit(double *xHeating, double *xCooling, int n, double *y, double lambda, double *betahat);
int one_wheel(double *xHeating, double *xCooling, int n, double *y, double lambda, double *betahat);


//Application #3) Bootstrap, the SEXP function loops over the second two
SEXP bootstrap(SEXP temp, SEXP energy, SEXP weights, SEXP nreps, SEXP heating, SEXP cooling, SEXP parametric, SEXP type);
int resampleIndexes(int *ind, int n);
int baseTempRegression(double *temp, int n, double *energy, double *w, int heating, int cooling, double *coefs, int type);
int simulateData(double *temp, int n, double *energyNew, int heating, int cooling, double *coefs, double sigma, int type);
*/

//Utility functions
double max(double a, double b);
double min(double a, double b);
double vecmin(double *x, int n);
double vecmax(double *x, int n);
double vecsum(double *x, int n);
int vecprint(double *x, int n);
double rnorm1(double mu, double sigma);
