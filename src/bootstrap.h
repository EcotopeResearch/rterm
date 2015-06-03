/*
Header File for Functions to bootstrap changepoint/degree day models
Mike Logsdon
Spring 2015
*/

SEXP bootstrapBaseTemp(SEXP temps, SEXP rows, SEXP energy, SEXP weights, SEXP nreps, SEXP heating, SEXP cooling, SEXP type, SEXP intercept);
int simOne(double *fitted, double *newVec, int n, double sigma);
int fittedVals(int intercept, int heating, int cooling, double *xheating, double *xcooling, int n, double *fitted, double *coefs);


int resampleIndexes(int *ind, int n);
// int changePointRegression(double *temp, int n, double *energy, double *w, int heating, int cooling, double *coefs);
// int simulateData(double *temp, int n, double *energyNew, int heating, int cooling, double *coefs, double sigma);
