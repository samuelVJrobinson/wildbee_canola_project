data { //Vectorized form
	int N; // number of samples
	int Nsite; // number of sites  
	  
	int<lower=0> count[N]; // number of bees per sample
	int<lower=1,upper=Nsite> site[N]; //site index
	//vector[N] centDate; // date
	//vector[N] traplength; //trapping period (offset)	
}
parameters {
	//vector[Nsite] b0site; //site intercept
	//real b0; //global intercept
	//real<lower=0,upper=100> b0sd; //SD for global intercept		
	//real b1; //global slope
	//real<lower=0> phi; // neg. bin. dispersion
	real<lower=0> lambda; //population parameter
	real<lower=0,upper=1> p; //detection prob
	
}
model {	
	//Priors
	lambda ~ cauchy(0, 10);
	p ~ uniform(0,1)
	//Likelihood
	for(s in 1:Nsite){ //For each site
		count[n] ~ 
	
	}
		
}
