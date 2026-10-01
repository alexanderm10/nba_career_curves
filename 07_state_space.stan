// 07_state_space.stan
// Latent talent model, written in marginal form.
//
// For player j in season t (experience e_jt), the observed league-adjusted TS% is
//   y_jt = curve(e_jt) + alpha_j + z_jt + noise_jt
// where
//   curve(e)   population development curve (quadratic in centred experience)
//   alpha_j    permanent player level, sd tau_a
//   z_jt       transient form, stationary AR(1) in experience, sd tau_z, persistence phi
//              (a gap of g seasons decays as phi^g, so missing seasons are handled)
//   noise_jt   known binomial sampling error se_jt plus extra unexplained noise sigma
//
// Integrating out alpha and z gives, for each player, a multivariate normal:
//   Cov(y_s, y_t) = tau_a^2 + tau_z^2 * phi^|e_s - e_t|     (plus se^2 + sigma^2 on the diagonal)
// This is the same distribution a Kalman filter would evaluate. Latent states and forecasts
// are recovered afterwards in R with the conditional normal formulas (see ss_helpers.R).

data {
  int<lower=1> N;                       // player-season rows
  int<lower=1> J;                       // players
  array[J] int<lower=1> start;          // first row of each player (rows sorted by player, then exp)
  array[J] int<lower=1> len;            // rows per player
  vector[N] y;                          // league-adjusted TS%
  vector[N] se;                         // sampling error of y
  vector[N] ec;                         // experience, centred
  array[N] int<lower=0> e;              // experience, raw (for lags)
}

parameters {
  real b0;
  real b1;
  real b2;
  real<lower=0> tau_a;
  real<lower=0> tau_z;
  real<lower=0, upper=1> phi;
  real<lower=0> sigma;
}

model {
  // Priors are weak. y is already league-adjusted, so the curve sits near a small positive
  // number (filtered players are regulars). Scales are in TS% units.
  b0    ~ normal(0, 0.05);
  b1    ~ normal(0, 0.02);
  b2    ~ normal(0, 0.01);
  tau_a ~ exponential(30);              // mean 0.033
  tau_z ~ exponential(30);
  phi   ~ beta(2, 2);
  sigma ~ exponential(50);              // mean 0.02

  for (j in 1:J) {
    int n = len[j];
    int s = start[j];
    matrix[n, n] S;
    vector[n] mu = b0 + b1 * segment(ec, s, n) + b2 * square(segment(ec, s, n));

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
