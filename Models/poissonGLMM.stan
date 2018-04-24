data { //Vectorized form
	int N; // number of samples
	int Nsite; // number of sites  
	  
	int<lower=0> count[N]; // number of bees
	int<lower=1,upper=Nsite> site[N]; //site index
	real traplength[N]; //trapping period (offset)
	real centDate[N]; // centered date	
}
parameters {
	real b0site[Nsite]; //site intercept
	real b0; //global intercept
	real<lower=0> b0sd; //SD for global intercept		
	real b1; //global slope
	//real<lower=0> phi; // neg. bin. dispersion
}
transformed parameters{
	real mu[N]; //Predictions on the link scale
	real<lower=0> lambda[N]; //Predictions on the data scale (fed through link function)
	
	for(n in 1:N){ //For every data point
		mu[n] = log(traplength[n]) + b0site[site[n]] + b1 * centDate[n];
		lambda[n]=exp(mu[n]);
	}
}
model {	
	//priors
	b0 ~ normal(0,10); //Hyperprior for the site mean ("global intercept")
	b0sd ~ uniform(0,10); //Hyperprior for the site SD
	
	b0site ~ normal(b0,b0sd);	//Prior for site means			
	b1 ~ normal(-1,10); //Prior for global slope
	//phi ~ cauchy(0,10); //Prior for NB dispersion parameter (half-cauchy)
		
	count ~ poisson(lambda); //NB regression
	//count ~ neg_binomial_2_log(lambda, phi); //NB regression
}
	
