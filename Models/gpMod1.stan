functions { 
  /* compute a latent Gaussian process - function generated by brms  
   *	This *should* be faster than inverting a 300x300 matrix each iteration
   *   x: array of continuous predictor values
   *   sdgp: marginal SD parameter
   *   lscale: length-scale parameter
   *   zgp: vector of independent standard normal variables 
   *   delta0: small number to make matrix positive-definite
   * Returns:  
   *   a vector to be added to the linear predictor
   */ 
  vector gp(vector x, real sdgp, real lscale, vector zgp) {
    matrix[rows(x), rows(x)] cov; //Declare covariance matrix
	real x_act[rows(x)];
	for(i in 1:rows(x)){ //This is a weird work-around that allows x to be defined as a row vector.
		x_act[i]=x[i];
	}		
    cov = cov_exp_quad(x_act, sdgp, lscale) + diag_matrix(rep_vector(1e-9, rows(x))); // deal with numerical non-positive-definiteness    	
    return cholesky_decompose(cov) * zgp; //Decomposed covariance matrix * unit normals
  }  
} 
data {
	int N; //Number of data points for both years
	int Nsite; //Number of sites used in each year
	int Ncanola; //Number of observed canola measurements
	int<lower=1,upper=Nsite> site[N]; //Site index	
	int year[N]; //Year
	int NperSite[Nsite,2]; //Number of data points per site per year
	int<lower=0> count[N]; //Number of bees	
	vector[N] traplength; //Traplength offset (in weeks)
	vector[N] centDate; //Centered date (in weeks, centered per-year)	
	vector[Ncanola] centEndDate; //Centered end date (in weeks, centered per-year using mean of middate)
	vector[Ncanola] canolaBloom; //Canola bloom	measurements	
	int bloomIndex[Ncanola]; //Index for bloom measurements
	int<lower=0,upper=1> nearCanola[Ncanola]; //Was trap near canola?
}

parameters {
	//Params for GP - 1 for each year
	real<lower=0> rho[2]; //Length-scale: how fast does correlation decay with distance
	real<lower=0> alpha[2]; //Max covariance b/w points aka marginal SD parameter		
	vector[N] eta; //Unit normals
	
	//Bee count parameters - 1 for each year
	real b0[2]; //"Global intercept" - mean for poisson process	
	real <lower=0> b0sd[2]; //SD for global intercept
	matrix[Nsite,2] b0site; //site intercept	
	real<lower=0> phi; //Dispersion parameter for neg. bin.
	
	//Canola bloom parameters - 1 for each year
	real muCanola[2]; //Peak bloom time
	real<lower=0> sigmaCanola[2]; //SD of bloom time
	real<lower=0> residCanola; //"Residual" for canola bloom - diff b/w predicted and actual	
}

model {  		
	//Setup
	vector[N] mu = rep_vector(0, N); //Expected value for neg. bin. process
	vector[Ncanola] predCanola = rep_vector(0, Ncanola); //Predicted canola bloom for observed values				
		
	/*Model for per-pass bee counts:
	counts ~ negbin(mu,phi)
	mu= overall intercept + site intercept + gaussian process 
	*/
	int pos = 1; //First position in vector			
	for(i in 1:Nsite){ //Adds mu to gaussian process for year 1
		mu[pos:(NperSite[i,1]+pos-1)] =  b0[1] + b0site[i,1] + gp(centDate[pos:(NperSite[i,1]+pos-1)], alpha[1], rho[1], eta[pos:(NperSite[i,1]+pos-1)]); 
		pos=pos+NperSite[i,1]; //Increment position
	}
	for(i in 1:Nsite){ //Adds mu to gaussian process for year 2 
		mu[pos:NperSite[i,2]+pos-1] =  b0[2] + b0site[i,2] + gp(centDate[pos:NperSite[i,2]+pos-1], alpha[2], rho[2], eta[pos:NperSite[i,2]+pos-1]); 
		pos=pos+NperSite[i,2]; //Increment position
	}
	 
	/* Model for canola bloom
	*/
	for(i in 1:Ncanola){ //Clunky, but no vectorized form of pow(), so this is as good as it gets
		if(nearCanola[i]==1){
			if(year[bloomIndex[i]]==2015){ 
				predCanola[i]=100*exp(-0.5*pow((centEndDate[i]-muCanola[1])/sigmaCanola[1],2));
			} else {
				predCanola[i]=100*exp(-0.5*pow((centEndDate[i]-muCanola[2])/sigmaCanola[2],2));
			}			
		} 
	} 
	
	//Priors	
	//Gaussian process - 1 for each year
	rho[1] ~ gamma(1, 1); //Length-scale
	rho[2] ~ gamma(1, 1); 
	alpha[1] ~ gamma(2, 0.2); //Max covariance			
	alpha[2] ~ gamma(2, 0.2); 
  	eta ~ normal(0,1); //Unit normals
	
	//Bee counts - 1 for each year
	b0[1] ~ normal(0,5); //Hyperprior for the site mean ("global intercept")
	b0[2] ~ normal(0,5); 
	b0sd[1] ~ gamma(1,0.5); //Hyperprior for the site SD 
	b0sd[2] ~ gamma(1,0.5); 
	b0site[,1] ~ normal(0,b0sd[1]); //Prior for site (difference from global intercept)
	b0site[,2] ~ normal(0,b0sd[2]); 	
	phi ~ gamma(1.5,1); //Prior for NB dispersion parameter 
	
	//Canola bloom
	muCanola[1] ~ normal(0,5); //Mean bloom date
	muCanola[2] ~ normal(0,5);
	sigmaCanola[1] ~ gamma(3,0.1); //Width of bloom
	sigmaCanola[2] ~ gamma(3,0.1); 
	residCanola ~ gamma(1,1); //"Residual"
		
	//Likelihood
	//count ~ poisson_log(mu+log(traplength));	
	count ~ neg_binomial_2_log(mu+log(traplength),phi);	
	canolaBloom ~ normal(predCanola,residCanola);
}
