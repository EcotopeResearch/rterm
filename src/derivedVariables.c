#include <R.h>
#include <Rinternals.h>
#include <stdio.h>

#include "derivedVariables.h"


SEXP linkWeatherToData(SEXP dateStart, SEXP dateEnd, SEXP date) {
  
  int n1 = length(dateStart);
  int n2 = length(date);
  
  int k;
  
  double *dateStartC = REAL(dateStart);
  double *dateEndC = REAL(dateEnd);
  double *dateC = REAL(date);
  
  long dateTmp;
  
  SEXP ans = PROTECT(allocVector(REALSXP, n2));
  double *ansC = REAL(ans);
  
  // For each entry of weather "date"
  for(int i = 0; i < n2; i++) {
    dateTmp = dateC[i];
    k = 0;
    
    // For each entry of dateStart/dateEnd
    for(int j = 0; j < n1; j++) {
      if(dateTmp > dateStartC[j] && dateTmp <= dateEndC[j]) {
        ansC[i] = j + 1;
        k++;
      }
    }
    
    if(k == 0) {
      ansC[i] = -1;
    } else if(k > 1) {
      fprintf(stderr, "Multiple data rows match weather row %d\n", i);
    }
  }
  
  
  UNPROTECT(1);
  
  return ans;
}

// type == 1 is change point, else degree day
SEXP deriveVar(SEXP temp, SEXP rows, SEXP base, SEXP ndata, SEXP heatcool, SEXP type) {
  
  int nt = length(temp);
  int *nd = INTEGER(ndata);
  
  SEXP results = PROTECT(allocVector(REALSXP, *nd));
  double *newvar = REAL(results);
  double *tempC = REAL(temp);
  double *baseC = REAL(base);
  int *rowsC = INTEGER(rows);
  int *heatcoolC = INTEGER(heatcool);
  int *typeC = INTEGER(type);

  // Make sure that our newvar is populated by 0
  for(int i = 0; i < *nd; i++) {
    newvar[i] = 0;
  }

  deriveVarC(tempC, *baseC, newvar, rowsC, *heatcoolC, *nd, nt, *typeC);

  UNPROTECT(1);
  return results;
}


// heatcool 1 for heating, 2 for cooling
int deriveVarC(double *temp, double base, double *newvar, int *rows, int heatcool, int ndata, int nweather, int type) {
  
  int tmpRow, tmpInd;
  double tmpVal;
  
  // For each row of the data, count how many weather observations correspond
  double *ns = malloc(ndata * sizeof(double));
  for(int i = 0; i < ndata; i++) {
    ns[i] = 0;
  }
  for(int j = 0; j < nweather; j++) {
    int tmpInd = rows[j] - 1;
    if(tmpInd < 0 || tmpInd > (ndata - 1)) {
      fprintf(stderr, "Row Association of Weather to Data does not Match\n");
      fprintf(stderr, "Weather File Row %d, data file row %d\n", j, tmpInd);
      fprintf(stderr, "ndata = %d\n", ndata);
      return 1;
    }
    ns[rows[j] - 1]++;
  }
  
  // Zero out the new variable, to be safe
  for(int i = 0; i < ndata; i++) {
    newvar[i] = 0.0;
  }
  
  // With change point, average first, then do transformation
  if(type == 1) {
    for(int i = 0; i < nweather; i++) {
      tmpRow = rows[i] - 1;
      // fprintf(stderr, "datarow = %d\n", tmpRow);
      newvar[tmpRow] += temp[i] / ns[tmpRow];
    }    
      // Transformation
    for(int i = 0; i < ndata; i++) {
      if(heatcool == 1) {
        newvar[i] = base - newvar[i];
      } else {
        newvar[i] = newvar[i] - base;
      }
      if(newvar[i] < 0) newvar[i] = 0;
    }
  } else {
    // With degree day we transform & then sum
    for(int i = 0; i < nweather; i++) {
      tmpRow = rows[i] - 1;
      if(heatcool == 1) {
        tmpVal = base - temp[i];
      } else {
        tmpVal = temp[i] - base;
      }
      if(tmpVal < 0) tmpVal = 0;
      
      newvar[tmpRow] += tmpVal / ns[tmpRow];
    }
    
  }
  
  
  free(ns);
  return 0;
}


SEXP degreeDayVar(SEXP temp, SEXP rows, SEXP base, SEXP ndata, SEXP heatcool) {
  
  SEXP results = PROTECT(allocVector(REALSXP, 10));
  
  return results;
}

int degreeDayVarC(double *temp, double base, double *newvar, int *rows, int heatcool, int ndata, int nweather) {
  
  
  return 0;
}




