data {
  int<lower=1> N;             // number of choices (and choice tasks)
  int<lower=1> M;             // number of rows in X (M > N)
  int<lower=1> K;             // number of cols in X (parameters)
  matrix[M, K] X;             // design matrix
  int<lower=1> y[N];          // choices (row number as indicator)
  int<lower=1> start_n[N];    // row number in X where nth choice task starts
  int<lower=1> end_n[N];      // row number in X where nth choice task ends
  real<lower=0> sig_beta;     // prior sd for beta parameters
}

parameters {
  matrix[K, 1] beta;    
}

transformed parameters {
  matrix[M, 1] u;
  u = X * beta;
}

model {
  to_vector(beta) ~ normal(0, sig_beta); 
  for (n in 1:N) {
    target += u[y[n]] - log_sum_exp(u[start_n[n]:end_n[n]]);
  }
}

