#Stan examples - from https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started  

#Preamble -----------------------
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd("~/Projects/UofC/Wild bee time project/Bayesian Examples/Stan examples")

#Example 1: 8 schools --------------

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = '8schools.stan', data = schools_dat, 
            iter = 1000, chains = 4)

print(fit)
plot(fit)
pairs(fit, pars = c("mu", "tau", "lp__")) #Looks "fuzzy" because it uses smoothScatter() rather than points() - see docs

la <- extract(fit, permuted = TRUE) # return a list of arrays 
mu <- la$mu 

### return an array of three dimensions: iterations, chains, parameters 
a <- extract(fit, permuted = FALSE) 

#Example 2: rats ---------------

y <- as.matrix(read.table('rats.txt', header = TRUE))
x <- c(8, 15, 22, 29, 36)
xbar <- mean(x)
N <- nrow(y)
T <- ncol(y)
rats_fit <- stan(file = 'rats.stan')

#Parameters of interest (no random effects, no )
params=c('mu_alpha','mu_beta','sigmasq_y','sigmasq_alpha','sigmasq_beta','sigma_y','sigma_alpha',
       'sigma_beta','alpha0')
print(rats_fit)

#Parameter plots:

#Parameter values
stan_plot(rats_fit,pars=params) #Large CIs for sigma_alpha (SD for global intercept)
#Trace plot
stan_trace(rats_fit,pars=params)
#Scatter plot (2 params only)
stan_scat(rats_fit,pars=c('mu_alpha','mu_beta'))
#Other version, using whatever parameters you want
pairs(rats_fit, pars=c('mu_alpha','mu_beta','sigmasq_alpha','sigmasq_beta')) 
#Histogram/density plots of parameter values - unscaled version of stan_plot
stan_hist(rats_fit,pars=params)
stan_dens(rats_fit,pars=params)


#Diagnostic plots:

#Autocorrelation plots for all params
stan_ac(rats_fit,pars=params)
#Diagnostics for sampler HMC and NUTS sampler
stan_diag(rats_fit)
#R-hat (convergence) statistic
stan_rhat(rats_fit)
#Sample size ratio
stan_ess(rats_fit)
#Monte Carlo Standard Error: Posterior SD ratio
stan_mcse(rats_fit)

