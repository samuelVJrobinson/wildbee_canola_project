#Model of count of A. terminalis using poisson GLMM framework - doesn't fit very well
model {
  
  #Hyperpriors
  alpha.mean ~ dunif(-10,10)
  alpha.sd ~ dunif(0,10)
  alpha.prec <- 1 / (alpha.sd*alpha.sd)
  
  #Priors
  # alpha ~ dnorm(0,0.001) #Prior for intercept
  beta ~ dnorm(0,0.001) #Prior for slope
  theta.zero ~ dunif(0,1) #Prior for zero-inflation term
  
  #Generate random intercept for each site
  for (i in 1:Nsite){
    alpha[i] ~ dnorm(alpha.mean,alpha.prec) #Random intercept for count~centDate model
	siteZero[i] ~ dbern(theta.zero) #Zero-inflation term for each site
  }
  
  # Likelihood
  #Note double indexing of population
  for(i in 1:N){ #For each sample...
	#Model lambda as linear function of centDate, using traplength as an offset, where ZI term multiplied by main effects
	log(lambda[i])<-log(traplength[i])+siteZero[site[i]]*(alpha[site[i]]+beta*centDate[i])+0.00001
	#Model actual count
	count[i]~dpois(lambda[i])
	
	#Absolute residual
    res[i] <- abs(count[i] - lambda[i])
    #Generate new dataset
    count.new[i] ~ dpois(lambda[i])
    res.new[i] <- abs(count.new[i] - lambda[i])
  }
  
  #Derived parameters for posterior predictive check
  fit <- sum(res[])
  fit.new <- sum(res.new[])
  
}