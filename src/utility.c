#include <R.h>
#include <Rinternals.h>
#include <stdio.h>

#include "utility.h"


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



double vecsum(double *x, int n) {
  double total = 0.0;
  
  for(int i = 0; i < n; i++) {
    total += x[i];
  }
  
  return total;
}


int vecprint(double *x, int n) {
  for(int i = 0; i < n; i++) {
    printf("x[%d] = %f\n", i, x[i]);
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


