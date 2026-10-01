// 14_state_space_cov.stan
// Same latent talent model as 07_state_space.stan, with covariates on the mean.
//   y_jt = curve(e_jt) + X_jt * beta + alpha_j + z_jt + noise_jt
// X holds each player's PREVIOUS style scores (lagged PCs) and a no-lag indicator.
// The covariance structure is unchanged, so only the mean differs from 07.

data {
  int<lower=1> N;
  int<lower=1> J;
  array[J] int<lower=1> start;
  array[J] int<lower=1> len;
  vector[N] y;
  vector[N] se;
  vector[N] ec;
  array[N] int<lower=0> e;
  int<lower=1> K;                       // number of covariates
  matrix[N, K] X;
}

parameters {
  real b0;
  real b1;
  real b2;
  vector[K] beta;
  real<lower=0> tau_a;
  real<lower=0> tau_z;
  real<lower=0, upper=1> phi;
  real<lower=0> sigma;
}

model {
  b0    ~ normal(0, 0.05);
  b1    ~ normal(0, 0.02);
  b2    ~ normal(0, 0.01);
  beta  ~ normal(0, 0.02);              // PC scores are roughly unit scale, effects are small in TS%
  tau_a ~ exponential(30);
  tau_z ~ exponential(30);
  phi   ~ beta(2, 2);
  sigma ~ exponential(50);

  for (j in 1:J) {
    int n = len[j];
    int s = start[j];
    matrix[n, n] S;
    vector[n] mu = b0 + b1 * segment(ec, s, n) + b2 * square(segment(ec, s, n))
                   + block(X, s, 1, n, K) * beta;

    for (a in 1:n) {
      for (b in a:n) {
        real k = square(tau_a)
                 + square(tau_z) * pow(phi, abs(e[s + a - 1] - e[s + b - 1]));
        S[a, b] = k;
        S[b, a] = k;
      }
      S[a, a] += square(se[s + a - 1]) + square(sigma);
    }
    segment(y, s, n) ~ multi_normal(mu, S);
  }
}
