/* Gaussian process model of bee presence, using bernoulli process */

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
		int N = rows(x);
		matrix[N, N] cov; //Declare covariance matrix
		real x_act[N];
		for(i in 1:N){ //This is a weird work-around that allows x to be defined as a row vector.
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
	int NperYear[2]; //Number of data points per year
	int Nsite; //Number of sites used in each year
	int Ncanola; //Number of observed canola measurements
	int<lower=1,upper=Nsite> site[N]; //Site index	
	int year[N]; //Year
	int NperSite[Nsite,2]; //Number of data points per site per year	
	int<lower=0,upper=1> present[N]; //Bees present/absent
	vector[N] traplength; //Traplength offset (in weeks)	
	//Unique dates for GP technique
	int Ndates2015; //Number of unique dates for 2015
	int Ndates2016; //Number of unique dates for 2016
	vector[Ndates2015] centDates2015; //Unique dates for 2015
	vector[Ndates2016] centDates2016; //Unique dates for 2016	
	int dateIndex2015[NperYear[1]]; //Index for 2015 samples (which date?)
    int dateIndex2016[NperYear[2]]; //Index for 2016 samples	
	// vector[N] centDate; //Centered date (in weeks, centered per-year)	
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
	real<lower=0.1,upper=7.35> rho[2]; //(log) Length-scale: how fast does correlation decay with distance. Largest possible difference is 7.35
	real<lower=0> alpha[2]; //Max (log) covariance b/w points aka marginal SD parameter
	// real<lower=0> sigma; //Variance between obs at same time	
	vector[Ndates2015] eta2015; //Unit normals
	vector[Ndates2016] eta2016; //Unit normals
	
	
	//Bee count parameters - 1 for each year
	// matrix<lower=0,upper=1>[Nsite,2] occupied; //Is each site occupied during each year?
	real b0[2]; //"Global intercept" - mean for neg.bin. process				
	matrix[Nsite,2] b0_site; //Random intercepts for site in each year - lognormal
	real<lower=0> sigma_site[2]; //SD for site 
	// real<lower=0> lambda_site[2]; //Lambda for site
	real SNLslope[2]; //SNL effect on site intercept
	real slopeLastYear; //Effect of last year on site intercept	
	//real intLastYear; //Interaction between SNL and last year's intercept
	real slopeCanolaOverlap; //Effect of last year's canola overlap on this year's abundance	
	real canolaEffect[2]; //Effect of canola abundance on per-pass bee count
	// real<lower=0> phi[2]; //Dispersion parameter for neg. bin.
	
	//Canola bloom parameters - 1 for each year
	real muDateCanola[2]; //Peak bloom time
	real<lower=0> sigmaDateCanola[2]; //SD of bloom time
	real<lower=0> sigmaCanola; //SD for actual bloom - diff b/w predicted and actual	
	real<lower=0,upper=120> ampCanola[2]; //Amplitude of canola bloom (supposed to be 100, but may differ b/w years)
}

transformed parameters {
	//Params for canola bloom overlap
	vector<lower=0, upper=1>[N] predCanolaPass; //Predicted canola overlap for each pass
	vector<lower=0>[Nsite] siteCanolaOverlap = rep_vector(0, Nsite); //Summed overlap of canola bloom for each site for year 1
	matrix[2,2] siteCanolaLims; //stop and start of canola bloom (columns) for year 1 and 2 (rows)
	//Setup	
	vector[Ncanola] muCanola = rep_vector(0, Ncanola); //Predicted canola bloom for observed values				
	matrix[Nsite,2] mu_site; // Site intercept - influenced by other things (see below)	
	vector[N] mu; //Expected value for neg. bin. process
	vector[Ndates2015] gpTrend2015; //Effect of gaussian process (temporal trend) for 2015
	vector[Ndates2016] gpTrend2016; //Effect of gaussian process (temporal trend) for 2016
	
	/* 	Calculate "overlap" metric for each pass (for how much of each pass was canola bloom "on" or "off") and
		multiply by proportion of canola surrounding each site - spatiotemporal availability
		Sum overlap for each site	*/
	
	//Start and stop dates of canola (over 10%). Assumes amplitude of 100%
	siteCanolaLims[1,]=invGaus(10,muDateCanola[1],sigmaDateCanola[1],100)'; //Year 1
	siteCanolaLims[2,]=invGaus(10,muDateCanola[2],sigmaDateCanola[2],100)'; // Year 2
	
	for(i in 1:N){			  					
		//Calculate canola overlap for pass = %canola2015 x overlap		
		predCanolaPass[i]= max([min([siteCanolaLims[year[i],1],centEndDate[i]])-max([siteCanolaLims[year[i],2],centEndDate[i]-traplength[i]]),0])/
			traplength[i]*percCanola[site[i],1]; 				
		//Add overlap to siteCanolaOverlap if year == 2015
		if(year[i]==1){
			siteCanolaOverlap[site[i]]=siteCanolaOverlap[site[i]]+predCanolaPass[i];
		}		
	}
	
	/* Model for per-site bee counts (intercepts)
		intercept2015 = SNLslope2015*SNL : SNL controls "long-term" abundance 
		intercept2016 = SNLslope2016*SNL + slope2015*count2015 : year-to-year transition rate + last year's abundance
	*/

	//Site-level intercept 2015	
	mu_site[,1]= SNLslope[1]*percSNL + //Effect of SNL ("Long-term" effect) 
		// (b0_site[,1]-exp(pow(sigma_site[1],2)/2)); //site random intercept (centered lognormal)		
		// (b0_site[,1]-(1/lambda_site[1])); //Centered Exp-Normal
		b0_site[,1]; //Normal
	// Site intercept 2016 = Effect of SNL + Effect of 2015 + Interaction
	mu_site[,2]= slopeLastYear*mu_site[,1] + //Effect of last year's intercept (population)
				SNLslope[2]*percSNL + //Effect of SNL ("year-to-year" effect)
				slopeCanolaOverlap*siteCanolaOverlap + //Effect of canola overlap from last year
				// (b0_site[,2]-exp(pow(sigma_site[2],2)/2)); //Site random intercept - centered lognormal				
				// (b0_site[,2]-(1/lambda_site[2])); //Exp-Normal
				b0_site[,2]; //Normal
	//Gives a "deep copy" warning if using vector directly
		
	/*Model for per-pass bee counts:
	presence ~ bernoulli_logit(mu)
	mu= global intercept + site intercept + gaussian process + SNL effect + canola effect + overlap effect
	*/					
	
	gpTrend2015 = gp(centDates2015,alpha[1],rho[1],0,eta2015); //Trend for 2015
	gpTrend2016 = gp(centDates2016,alpha[2],rho[2],0,eta2016); //Trend for 2016	
	
	{
		int startPos = 1; //First position in vector, must be declared locally		
		int endPos = 0; //Last position in vector
		for(i in 1:Nsite){ //Model for counts in year 1
			endPos = (NperSite[i,1]+startPos-1); //End position in vector			
			
			//Expected value for trap counts
			mu[startPos:endPos] = b0[1] + //Intercept for year
				mu_site[i,1] + //Site effects
				traplength[startPos:endPos] + //Offset for trapping length
				gpTrend2015[dateIndex2015[startPos:endPos]] + //Gaussian process model			
				canolaEffect[1]*predCanolaPass[startPos:endPos]; //Effect of canola on count
			startPos=startPos+NperSite[i,1]; //Increment position
		}
		for(i in 1:Nsite){ //Model for counts in year 2	
			endPos = NperSite[i,2]+startPos-1; //End position			
			
			//Expected value for trap counts					
			mu[startPos:endPos] =  b0[2] + //Global intercept for year
				mu_site[i,2] + // Site effects
				traplength[startPos:endPos] + //Offset for trapping length
				gpTrend2016[dateIndex2016[(startPos-NperYear[1]):(endPos-NperYear[1])]] + //Gaussian process model			
				canolaEffect[2]*predCanolaPass[startPos:endPos]; //Effect of canola on count
			startPos=startPos+NperSite[i,2]; //Increment position
		}
	}
	 
	/* Model for canola bloom	 
		Clunky, but no vectorized form of pow(), so this is as good as it gets
	*/
	for(i in 1:Ncanola){ 
		if(nearCanola[i]==1){			 
			muCanola[i]=ampCanola[year[bloomIndex[i]]]*
				exp(-0.5*pow((centBloomDate[i]-muDateCanola[year[bloomIndex[i]]])/sigmaDateCanola[year[bloomIndex[i]]],2));			
		} 
	}	
}

model {  			
	// vector[N] phiVector = rep_vector(phi[1],N); //Assign phi[1] and phi[2] to a vector
	// for(i in 1:N){
		// if(year[i]==2){
			// phiVector[i]=phi[2];
		// }
	// }	
	
	//Priors	
	//Gaussian process - 1 for each year
	rho ~ inv_gamma(10,18); //Prior for length-scale	
	alpha ~  normal(0,1); //Prior for covariance				
	// sigma ~ gamma(1,1); // Variance between points at same site at same time	
  	eta2015 ~ normal(0,1); //Unit normals	
	eta2016 ~ normal(0,1); 
	
	//Bee counts for each site - 1 for each year
	b0 ~ normal(0,5); //Intercept for both years
	sigma_site[1] ~ normal(0,1); //Sigma for sites in 2015
	sigma_site[2] ~ normal(0,1); //Sigma for sites in 2016 - appears much smaller
	// lambda_site ~ gamma(2,2); //Lambda for site skew
	b0_site[,1] ~ normal(0,sigma_site[1]); //Random intercept for site (2015)
	b0_site[,2] ~ normal(0,sigma_site[2]); //Random intercept for site (2016)	
	// b0_site[,1] ~ lognormal(0,sigma_site[1]); //Random intercept for site (2015)
	// b0_site[,2] ~ lognormal(0,sigma_site[2]); //Random intercept for site (2016)	
	// b0_site[,1] ~ exp_mod_normal(0,sigma_site[1],lambda_site[1]); //Random intercept for site (2015)
	// b0_site[,2] ~ exp_mod_normal(0,sigma_site[2],lambda_site[2]); //Random intercept for site (2016)
	
	// b0err ~ normal(0,b0sd);	
	// b0sd ~ gamma(1.5,1); //Hyperprior for gamma term - non-normal random effect
	// b0err ~ gamma(1,b0sd);	
	
	SNLslope ~ normal(0,3); //Prior for effect of SNL		
	slopeLastYear ~ normal(0,3); //Effect of last year's intercept
	// intLastYear ~ normal(0,3); //Interaction b/w SNL and last year's intercept
	canolaEffect ~ normal(0,3);	
	// phi ~ gamma(2,2); //Prior for NB dispersion parameter 		
	
	//Canola bloom - informative priors
	muDateCanola ~ normal(-1,1); //Mean bloom date	
	sigmaDateCanola ~ gamma(2,2); //Width of bloom	
	ampCanola ~ normal(100,20); //Amplitude of bloom
	sigmaCanola ~ gamma(1,1); //SD for bloom
		
	//Likelihood		
	canolaBloom ~ normal(muCanola,sigmaCanola);		
	count ~ bernoulli_logit(mu);	//Count data along with trapping offset	
}

generated quantities {	
	real predCanola[Ncanola]; //Simulated canola proportion
	real canola_resid[Ncanola]; //Residual for canola
	real predCanola_resid[Ncanola]; //Residual for generated canola
	
	int predCount[N]; //Simulated bee counts
	real count_resid[N]; //Residual
	real predCount_resid[N]; //Residual for generated
	
	
	for(i in 1:Ncanola){
		if(nearCanola[i]==0){ //If no canola nearby
			canola_resid[i] = 0;
			predCanola[i] = 0;
			predCanola_resid[i] = 0;			
		} else { //If canola nearby
			canola_resid[i] = muCanola[i] - canolaBloom[i];
			predCanola[i] = normal_rng(muCanola[i],sigmaCanola);
			predCanola_resid[i] = muCanola[i] - predCanola[i]; 
		}	
	}	
		
	for(i in 1:N){
		count_resid[i] = inv_logit(mu[i]) - presence[i]; //Residual for actual value
		predCount[i] = bernoulli_logit_rng(mu[i]); //Simulate bee counts
		predCount_resid[i] = inv_logit(mu[i]) - predCount[i]; //Residual for simulated bee counts
	}
		
}
