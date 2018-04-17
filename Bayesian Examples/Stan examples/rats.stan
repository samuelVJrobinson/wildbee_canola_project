// http://www.mrc-bsu.cam.ac.uk/bugs/winbugs/Vol1.pdf
// Page 3: Rats
data {
  int<lower=0> N; // number of rats (rows of data matrix)
  int<lower=0> T; // number of time steps (columns of data matrix)
  real x[T]; // days (main covariate)
  real y[N,T]; // weight (main response)
  real xbar;
}
parameters {
  real alpha[N]; //intercept 
  real beta[N]; //slope

  real mu_alpha; //prior for intercept - alpha.c in original bugs model
  real mu_beta;  //prior for slope - beta.c in original bugs model

  real<lower=0> sigmasq_y; // sigma in original bugs model (var for response)
  real<lower=0> sigmasq_alpha; //prior for sigma_alpha (var for intercept)
  real<lower=0> sigmasq_beta; //prior for sigma_beta (var for slope)
}
transformed parameters {
  real<lower=0> sigma_y;       
  real<lower=0> sigma_alpha; 
  real<lower=0> sigma_beta;

  sigma_y = sqrt(sigmasq_y); //SD for response (sqrt of var)
  sigma_alpha = sqrt(sigmasq_alpha); //SD for intercept
  sigma_beta = sqrt(sigmasq_beta); //SD for slope
}
model {
  mu_alpha ~ normal(0, 100); //Hyperprior for intercept mean - similar to "global intercept" in LMM
  mu_beta ~ normal(0, 100); //Hyperprior for slope mean - similar to "global slope" in LMM
  sigmasq_y ~ inv_gamma(0.001, 0.001); //Prior for response SD
  sigmasq_alpha ~ inv_gamma(0.001, 0.001); //Hyperprior for intercept SD
  sigmasq_beta ~ inv_gamma(0.001, 0.001); //Hyperprior for slope SD
  alpha ~ normal(mu_alpha, sigma_alpha); // Prior for intercept - vectorized
  beta ~ normal(mu_beta, sigma_beta);  // Prior for slope - vectorized
  for (n in 1:N) //For each rat
    for (t in 1:T) //For each time step
      y[n,t] ~ normal(alpha[n] + beta[n] * (x[t] - xbar), sigma_y);
	  //Weight ~ normal(intercept+slope*(centered weight),SD)
	  //Generates alpha & beta for every n (random effect)
}
generated quantities { //Stuff we're interested in interpreting post-hoc
  real alpha0; //Intercept at time 0 (birth weight)
  alpha0 = mu_alpha - xbar * mu_beta; //Prior for intercept (mean) - avg weight * Prior for Slope (mean)
  //Difference between alpha0 and mu_alpha is due to the fact that mu_alpha uses centered weight
}

 