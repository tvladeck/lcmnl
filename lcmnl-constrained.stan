data {
  int<lower=1> N;             // number of choices (and choice tasks)
  int<lower=1> M;             // number of rows in X (M > N)
  int<lower=1> K;             // number of cols in X (beta parameters)
  int<lower=1> S;             // number of segments/classes 
  int<lower=1> I;             // number of individuals
  int<lower=1> L;             // number of cols in Z (gamma parameters)
  matrix[M, K] X;             // design matrix in utility function
  matrix[I*S, L] Z;           // design matrix in class function
  int<lower=1> y[N];          // row number in X that belongs to nth choice
  int<lower=1> start_n[N];    // row number in X where nth choice task starts
  int<lower=1> end_n[N];      // row number in X where nth choice task ends
  int<lower=1> start_i[I];    // which observation belongs to which individual
  int<lower=1> end_i[I];      // which observation belongs to which individual
  real<lower=0> sig_beta;     // prior sd for beta parameters
  real<lower=0> sig_gamma;    // prior sd for gamma parameters
}

parameters {
  // instead of just including a "beta" parameter, we split this up into 
  // a matrix (beta_head) and a vector (the last row of beta)
  // which we constrain to be ordered across the latent classes
  // this will ensure that the class assignments are the same across chains
  // we join these two pieces together in the "transfored parameters" block
  
  // we have to do this here because it's the only place you can place an 
  // "ordered" constraint on values in stan
  // we can also only do it on one row since the ordering of utilities for 
  // one level may not be the same for any others (and one row is enough)
  matrix[K-1, S] beta_head;
  ordered[S] beta_tail; 
  
  
  vector[L] gamma;
}

transformed parameters {
  vector[S] log_theta[I];
  matrix[M, S] u;
  matrix[N, S] log_p;
  matrix[K, S] beta; 
  
  // hee is where we join the head and tail of beta
  beta = append_row(beta_head, to_row_vector(beta_tail)); 
  
  // class prob
  for (i in 1:I) {
    log_theta[i] = log_softmax(Z[(1+(i-1)*S):(i*S),] * gamma);
  }
  
  // choice prob
  u = X * beta;
  for (n in 1:N) {
    for (s in 1:S) {
      log_p[n, s] = u[y[n], s] - log_sum_exp(u[start_n[n]:end_n[n], s]);
    }
  }
}

model {
  to_vector(beta) ~ normal(0, sig_beta); 
  gamma ~ normal(0, sig_gamma);
  
  for (i in 1:I) {
    vector[S] lps = log_theta[i];
      for (s in 1:S) {
        lps[s] += sum(log_p[start_i[i]:end_i[i], s]);
      }
    target += log_sum_exp(lps);
  }
}



