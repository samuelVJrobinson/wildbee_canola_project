functions { 
  /* compute a latent Gaussian process - function generated by brms  
   *	This *should* be faster than inverting a 300x300 matrix each iteration
   *   x: array of continuous predictor values
   *   sdgp: marginal SD parameter
   *   lscale: length-scale parameter
   *   zgp: vector of independent standard normal variables 
   *   sigma: variance if distance = 0
   * Returns:  
   *   a vector to be added to the linear predictor
   */ 
	vector gp(vector x, real sdgp, real lscale, real sigma, vector zgp) {
		matrix[rows(x), rows(x)] cov; //Declare covariance matrix
		real x_act[rows(x)];
		for(i in 1:rows(x)){ //This is a weird work-around that allows x to be defined as a row vector.
			x_act[i]=x[i];
		}		
		cov = cov_exp_quad(x_act, sdgp, lscale) + diag_matrix(rep_vector(sigma + 1e-9, rows(x))); // deal with numerical non-positive-definiteness    	
		return cholesky_decompose(cov) * zgp; //Decomposed covariance matrix * unit normals
	}

	/* Function to calculate inverse (y=x) of Gaussian curve, given:
	yval: y-value to find x values at
	mu: mean
	sigma: standard deviation
	amp: amplitude (height of curve)
	*/
	vector invGaus(real yval, real mu, real sigma, real amp){
		vector[2] xval; //Declare x-values		
		xval[1]=mu+(sqrt(2)*sigma*sqrt(log(amp/yval)));
		xval[2]=mu-(sqrt(2)*sigma*sqrt(log(amp/yval)));
		return xval;
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
	vector[N] centEndDate; //Centered end date 
	vector[Ncanola] centBloomDate; //Centered end date (in weeks, centered per-year using mean of middate) - used for canola bloom model
	vector[Ncanola] canolaBloom; //Canola bloom	measurements	
	int bloomIndex[Ncanola]; //Index for bloom measurements
	int<lower=0,upper=1> nearCanola[Ncanola]; //Was trap near canola?
	vector[Nsite] percSNL; //Amount of SNL within 500m of each site
	real percCanola[Nsite,2]; //Amount of canola within 250m of each site in each year
}

parameters {
	//Params for GP - 1 for each year
	real<lower=0,upper=8> rho[2]; //Length-scale: how fast does correlation decay with distance
	real<lower=0> alpha[2]; //Max covariance b/w points aka marginal SD parameter
	real<lower=0> sigma; //Variance between obs at same time
	// matrix<lower=0,upper=8>[Nsite,2] rhoSite; // Rho for each site
	// matrix<lower=0,upper=8>[Nsite,2] alphaSite; //Alpha for each site	
	vector[N] eta; //Unit normals
	
	//Bee count parameters - 1 for each year
	real b0[2]; //"Global intercept" - mean for neg.bin. process		
	// vector[Nsite] b0err; //"Error" for site intercept (random effect of site)
	// real<lower=0> b0sd; //SD for generating site error
	vector[Nsite] b0err; //"Error" for site intercept (random effect of site)
	real<lower=0> b0sd; //SD for generating site error
	real SNLslope[2]; //SNL effect on site intercept
	real slopeLastYear; //Effect of last year on site intercept	
	//real intLastYear; //Interaction between SNL and last year's intercept
	real slopeCanolaOverlap; //Effect of last year's canola overlap on this year's abundance	
	real canolaEffect[2]; //Effect of canola abundance on per-pass bee count
	real<lower=0> phi; //Dispersion parameter for neg. bin.
	
	//Canola bloom parameters - 1 for each year
	real muCanola[2]; //Peak bloom time
	real<lower=0> sigmaCanola[2]; //SD of bloom time
	real<lower=0> residCanola; //"Residual" for canola bloom - diff b/w predicted and actual	
}

transformed parameters {
	//Params for canola bloom overlap
	vector<lower=0, upper=1>[N] predCanolaPass; //Predicted canola overlap for each pass
	vector<lower=0>[Nsite] siteCanolaOverlap = rep_vector(0, Nsite); //Summed overlap of canola bloom for each site for year 1
	matrix[2,2] siteCanolaLims; //stop and start of canola bloom (columns) for year 1 and 2 (rows)
	
	/* 	Calculate "overlap" metric for each pass (for how much of each pass was canola bloom "on" or "off") and
		multiply by proportion of canola surrounding each site - spatiotemporal availability
		Sum overlap for each site	*/
	
	siteCanolaLims[1,]=invGaus(10,muCanola[1],sigmaCanola[1],100)'; //Start and stop dates of canola (over 10%) for year 1
	siteCanolaLims[2,]=invGaus(10,muCanola[2],sigmaCanola[2],100)'; // Year 2
		
	for(i in 1:N){
			if(year[i]==2015){  					
			//Calculate canola overlap for pass = %canola2015 x overlap		
			predCanolaPass[i]= max([min([siteCanolaLims[1,1],centEndDate[i]])-max([siteCanolaLims[1,2],centEndDate[i]-traplength[i]]),0])/traplength[i]*
				percCanola[site[i],1]; 				
			//Add overlap to siteCanolaOverlap if year == 2015
			siteCanolaOverlap[site[i]]=siteCanolaOverlap[site[i]]+predCanolaPass[i];
		} else {
			//Calculate canola overlap for pass = %canola2016 x overlap
			predCanolaPass[i]= max([min([siteCanolaLims[2,1],centEndDate[i]])-max([siteCanolaLims[2,2],centEndDate[i]-traplength[i]]),0])/traplength[i]*
				percCanola[site[i],2]; 
		}
	}	
}

model {  		
	//Setup
	vector[N] mu = rep_vector(0, N); //Expected value for neg. bin. process
	vector[Ncanola] predCanola = rep_vector(0, Ncanola); //Predicted canola bloom for observed values				
	matrix[Nsite,2] b0site; // Site intercept - influenced by other things (see below)	
	int pos = 1; //First position in vector
	
	/* Model for per-site bee counts (intercepts)
		intercept2015 = SNLslope2015*SNL : SNL controls "long-term" abundance 
		intercept2016 = SNLslope2016*SNL + slope2015*count2015 : year-to-year transition rate + last year's abundance
	*/

	//Site intercept 2015	
	b0site[,1]= SNLslope[1]*percSNL + //Effect of SNL ("Long-term" effect)			 
				b0err;  //Random effect of site
	// Site intercept 2016 = Effect of SNL + Effect of 2015 + Interaction
	b0site[,2]= SNLslope[2]*percSNL + //Effect of SNL ("year-to-year" effect)
				slopeLastYear*(b0site[,1]) + //Effect of last year's intercept (population)
				slopeCanolaOverlap*siteCanolaOverlap; //Effect of canola overlap from last year
				// b0err[,2]; //Random effect of site	
				
	// b0site[,1]= b0err[,1]; //Test version estimating only random intercepts, but without any 
	// b0site[,2]= b0err[,2]; // Site intercept 2016 = Effect of SNL + Effect of 2015 + "Error"
	
	//Gives a "deep copy" warning if using vector directly
		
	/*Model for per-pass bee counts:
	counts ~ negbin(mu,phi)
	mu= global intercept + site intercept + gaussian process + SNL effect + canola effect + overlap effect
	*/			
	for(i in 1:Nsite){ //Model for counts in year 1
		mu[pos:(NperSite[i,1]+pos-1)] = b0[1] + b0site[i,1] + //Global intercept + site intercept
			gp(centDate[pos:(NperSite[i,1]+pos-1)], alpha[1], rho[1], sigma, eta[pos:(NperSite[i,1]+pos-1)]) + //Gaussian process model			
			canolaEffect[1]*predCanolaPass[pos:(NperSite[i,1]+pos-1)]; //Effect of canola on count
		pos=pos+NperSite[i,1]; //Increment position
	}
	for(i in 1:Nsite){ //Model for counts in year 2	
		mu[pos:NperSite[i,2]+pos-1] =  b0[2] + b0site[i,2] + //Global intercept + site intercept
			gp(centDate[pos:NperSite[i,2]+pos-1], alpha[2], rho[2], sigma, eta[pos:NperSite[i,2]+pos-1]) + //Gaussian process model			
			canolaEffect[2]*predCanolaPass[pos:(NperSite[i,2]+pos-1)]; //Effect of canola on count
		pos=pos+NperSite[i,2]; //Increment position
	}
	 
	/* Model for canola bloom	 
		Clunky, but no vectorized form of pow(), so this is as good as it gets
	*/
	for(i in 1:Ncanola){ 
		if(nearCanola[i]==1){
			if(year[bloomIndex[i]]==2015){ 
				predCanola[i]=100*exp(-0.5*pow((centBloomDate[i]-muCanola[1])/sigmaCanola[1],2));
			} else {
				predCanola[i]=100*exp(-0.5*pow((centBloomDate[i]-muCanola[2])/sigmaCanola[2],2));
			}			
		} 
	}	
	
	//Priors	
	//Gaussian process - 1 for each year
	rho ~ gamma(2, 1); //Prior for length-scale	
	// rhoSite[,1] ~ gamma(2,rho[1]); //Length-scale
	// rhoSite[,2] ~ gamma(2,rho[2]);
	alpha ~  cauchy(0, 1); //Prior for covariance				
	// alphaSite[,1] ~ cauchy(0,alpha[1]); //Covariance
	// alphaSite[,2] ~ cauchy(0,alpha[2]);	
	sigma ~ cauchy(0,1); //Variance between points at same site at same time	
  	eta ~ normal(0,1); //Unit normals
	
	//Bee counts for each site - 1 for each year
	b0 ~ normal(0,5); //Hyperprior for the site mean ("global intercept")	
	b0sd ~ inv_gamma(2,1);//Hyperprior for site SD	
	// b0err ~ normal(0,b0sd); //Site intercept ~ predicted site intercept 
	b0err ~ normal(0,b0sd); //Other version using 2 separate intercepts - not enough info to estimate
	// b0err[,2] ~ normal(0,b0sd[2]);
	SNLslope ~ normal(0,3); //Prior for effect of SNL	
	slopeLastYear ~ normal(0,3); //Effect of last year's intercept
	// intLastYear ~ normal(0,3); //Interaction b/w SNL and last year's intercept
	canolaEffect ~ normal(0,5);	
	phi ~ gamma(1.5,1); //Prior for NB dispersion parameter 		
	
	//Canola bloom
	muCanola ~ normal(0,5); //Mean bloom date	
	sigmaCanola ~ gamma(3,0.1); //Width of bloom	
	residCanola ~ gamma(1,1); //"Residual"
		
	//Likelihood	
	count ~ neg_binomial_2_log(mu+log(traplength),phi);	//Count data along with trapping offset
	canolaBloom ~ normal(predCanola,residCanola);	
}

generated quantities {
	matrix[N,N] predCov; //Covariance matrix
	matrix[N,N] predCov_L; //Cholesky decomposition
	vector[N] predGP; //Generated value from Gaussian process
	
	vector[N] predMu; //Generated mu value for negative binomial process
	int pos = 1; //First position in vector
	
	
	//Construct covariance matrix
	for(i in 1:Nsite){ //Matrix components for year 1
		predCov[pos:(NperSite[i,1]+pos-1),pos:(NperSite[i,1]+pos-1)] = cov_exp_quad(centDate[pos:(NperSite[i,1]+pos-1)], alpha[1], rho[1]);
		pos=pos+NperSite[i,1]; //Increment position
	}
	for(i in 1:Nsite){ //Model for counts in year 2	
		predCov[pos:(NperSite[i,2]+pos-1),pos:(NperSite[i,2]+pos-1)] = cov_exp_quad(centDate[pos:(NperSite[i,2]+pos-1)], alpha[2], rho[2]);	
		pos=pos+NperSite[i,2]; //Increment position
	}
	predCov=predCov+diag_matrix(rep_vector(sigma,N));
	predCov_L = cholesky_decompose(predCov);
	predGP = multi_normal_cholesky_rng(rep_vector(0, N), predCov_L); //Generated predicted value for Gaussian process
	
	for(i in 1:N){
		if(year[i]==2015){
			predMu[i]=b0[1]
		} else {
			predMu[i]=b0[2]
		}		
	}
	
	//GET THIS TO GENERATE COUNTS, AND SEE IF THEY MATCH UP WITH ACTUAL COUNTS	
		
	neg_binomial_2_log_rng(predMu+log(traplength),phi)

}
