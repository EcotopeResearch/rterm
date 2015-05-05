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





