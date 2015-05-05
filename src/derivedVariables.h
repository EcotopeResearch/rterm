/*
Header File for Functions to derive change-point or degree day variables
Mike Logsdon
Spring 2015
*/
#include <R.h>
#include <Rinternals.h>

// Map weather file dates to energy use dates
SEXP linkWeatherToData(SEXP dateStart, SEXP dateEnd, SEXP date);

// Make change point variables for regression
SEXP changePointVar(SEXP temp, SEXP rows, SEXP base);
int changePointVarC(double *temp, double base, double *newvar, int *rows, int heatcool, int ndata, int nweather);

// Make degree day variables for regression
SEXP degreeDayVar(SEXP temp, SEXP rows, SEXP base);
int degreeDayVarC(double *temp, double base, double *newvar, int *rows, int heatcool, int ndata, int nweather);

