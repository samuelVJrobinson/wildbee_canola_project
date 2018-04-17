model {
  
  #Priors
  
  #Hyperparameters for random effect
  block.mean ~ dunif(-300,300)
  block.sd ~ dunif(0,500)
  block.tau <- pow(block.sd, -2)
  
  #Covariate effects
  beta.golden ~ dnorm(0,0.01)
  beta.marv ~ dnorm(0,0.01)
  beta.n2 ~ dnorm(0,0.01)
  beta.n4 ~ dnorm(0,0.01)
  beta.n6 ~ dnorm(0,0.01)
  
  #Residual error
  ind.sd ~ dunif(0,500)
  ind.tau <- pow(ind.sd,-2)
   
  #Likelihood
  
  #Generate random intercepts
  for (i in 1:ngrp){
    block.intercept[i] ~ dnorm(block.mean,block.tau)
  }
  
  for (i in 1:N){
    
    lin.pred[i] <- block.intercept[block[i]] + beta.golden*var.golden[i] + beta.marv*var.marv[i]
                + beta.n2*n2[i] + beta.n4*n4[i] + beta.n6*n6[i]
    
    yield[i] ~ dnorm(lin.pred[i],ind.tau)
    
    #New dataset for predictive check
    yield.new[i] ~ dnorm(lin.pred[i],ind.tau)
    
    #absolute residual
    res[i] <- abs(yield[i] - lin.pred[i])
    res.new[i] <- abs(yield.new[i] - lin.pred[i])
    
  }
  
  #derived parameters
  
  fit <- sum(res[])
  fit.new <- sum(res.new[])
  
}