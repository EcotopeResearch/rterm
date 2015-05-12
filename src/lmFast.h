/*
Fast linear model functions
Weighted and non-weighted least squares
Using lapack/linpack compiled fortran

Mike Logsdon - Spring 2015
*/

// C lm function and a wrapper for R arguments
SEXP lmFast(SEXP X, SEXP y);
SEXP lmFastW(SEXP X, SEXP y, SEXP w);
int lmFastC(double *X, double *y, int n, int p, double *betahat);
int lmFastCW(double *X, double *y, double *w, int n, int p, double *betahat);

// Matrix algebra
int multXTy(char *trans, double *X, int nrowX, int ncolX, double *y, double *z);
int multXTX(double *X, int nrowX, int ncolX, double *Y);
int solveLinearSystem(double *A, double *b, int n);
