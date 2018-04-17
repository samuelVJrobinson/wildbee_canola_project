#Data
y <-structure(c(0L, 3L, 1L, 1L, 1L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 
              0L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 0L, 0L, 1L, 1L, 0L, 1L, 0L, 1L, 
              1L, 0L, 1L, 3L, 2L, 0L, 2L, 0L, 1L, 0L, 0L, 1L, 1L, 3L, 0L, 3L, 
              0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 4L, 0L, 1L, 2L, 1L, 1L, 
              0L, 5L, 2L, 0L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 4L, 1L, 
              2L, 1L, 0L, 1L, 2L, 0L, 1L, 2L, 0L, 0L, 2L, 1L, 0L, 0L, 1L, 0L, 
              0L, 1L, 2L, 1L, 1L, 0L, 0L, 2L, 1L, 2L, 0L, 2L, 2L, 2L, 0L, 1L, 
              0L, 3L, 1L, 2L, 0L, 0L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 3L, 2L, 
              1L, 0L, 0L, 1L, 1L, 2L, 1L, 4L, 0L, 0L, 2L, 3L, 0L, 0L, 1L, 1L, 
              2L, 1L, 1L, 0L, 2L, 0L, 2L, 2L, 0L, 0L, 2L, 0L, 0L, 1L, 2L, 1L, 
              0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 2L, 3L, 4L, 2L, 0L, 1L, 0L, 1L, 
              0L, 4L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 
              0L, 0L, 3L, 1L, 0L, 0L, 1L, 1L, 5L, 1L, 1L, 0L, 0L, 2L, 0L, 1L, 
              0L, 1L, 1L, 0L, 0L, 0L, 2L, 1L, 1L, 0L, 2L, 0L, 1L, 1L, 0L, 0L, 
              1L, 0L, 1L, 1L, 0L, 1L, 0L, 1L, 2L, 0L, 1L, 0L, 1L, 1L, 4L, 3L, 
              2L, 1L, 0L, 1L, 2L, 3L, 1L, 2L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 2L, 
              0L, 2L, 3L, 2L, 2L, 0L, 1L, 2L, 2L, 3L, 1L, 0L, 0L, 1L, 2L, 0L, 
              1L, 0L, 2L, 0L, 0L, 1L, 3L, 2L, 1L, 0L, 1L, 1L, 2L, 0L, 0L, 3L, 
              0L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 0L, 
              1L, 1L, 3L, 2L, 1L, 2L, 1L, 0L, 1L, 2L, 0L, 1L, 0L, 0L, 1L, 1L, 
              1L, 1L, 2L, 1L, 0L, 2L, 3L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 
              0L, 0L, 2L, 1L, 1L, 3L, 1L, 0L, 1L, 1L, 2L, 0L, 1L, 2L, 2L, 1L, 
              0L, 1L, 0L, 0L, 0L, 1L, 2L, 2L, 0L, 2L, 0L, 1L, 2L, 0L, 0L, 1L, 
              1L, 2L, 3L, 0L, 1L, 2L, 0L, 0L, 3L, 2L, 0L, 1L, 0L, 0L, 2L, 1L, 
              1L, 0L, 0L, 1L, 1L, 2L, 0L, 3L, 1L, 0L, 1L, 2L, 0L, 1L, 0L, 1L, 
              4L, 0L, 0L, 0L, 2L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 2L, 0L, 
              0L, 1L, 0L, 2L, 0L, 0L, 0L, 0L, 2L, 0L, 2L, 1L, 0L, 0L, 0L, 1L, 
              2L, 1L, 3L, 0L, 1L, 1L, 2L, 1L, 2L, 1L, 0L, 0L, 1L, 3L, 1L, 0L, 
              0L, 0L, 0L, 0L, 2L, 0L, 0L, 1L, 0L, 2L, 4L, 2L, 1L, 1L, 2L, 2L, 
              0L, 5L, 1L, 1L, 0L, 1L, 0L, 1L, 3L, 1L, 2L, 2L, 0L, 0L, 3L, 1L, 
              2L, 0L, 2L, 1L, 2L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 0L, 2L, 0L, 0L, 
              0L, 1L, 1L, 2L, 0L, 1L, 1L, 2L, 2L, 1L, 3L, 2L, 2L, 4L, 1L, 0L, 
              2L, 2L, 0L, 3L, 0L, 0L, 0L, 0L, 1L, 2L, 2L, 1L, 0L, 2L, 2L, 1L, 
              1L, 2L, 1L, 1L, 1L, 1L, 0L, 2L, 0L, 0L, 1L, 2L, 1L, 2L, 2L, 1L, 
              3L, 1L, 2L, 0L, 0L, 1L, 2L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 
              0L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 3L, 3L, 0L, 0L, 1L, 1L, 0L, 
              0L, 3L, 1L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 1L, 2L, 0L, 0L, 1L, 1L, 
              1L, 1L, 2L, 1L, 0L, 1L, 1L, 1L, 3L, 0L, 1L), .Dim = c(200L, 3L
              ))
R <-  200L
T <-  3L

## 12. Metapopulation modeling of abundance using
##     hierarchical Poisson regression: binomial mixture models
## 12.2. Generation and analysis of simulated data
## 12.2.1. The simplest case with constant parameters

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
set.seed(123)

## The data generation code is in bpa-code.txt, available at
## http://www.vogelwarte.ch/de/projekte/publikationen/bpa/complete-code-and-data-files-of-the-book.html

## Parameters monitored
params <- c("lambda", "p")

## MCMC settings
ni <- 1000
nt <- 1
nb <- 500
nc <- 4

## Initial values
inits <- lapply(1:nc, function(i)
  list(p = runif(1, 0, 1),
       lambda = runif(1, 0, 1)))

## Call Stan from R - takes about 10 mins to run, where K=100, about 4 where K=50...
out <- stan("binmix.stan",
            data = list(y = y, R = R, T = T, K = 50),
            init = inits, pars = params,
            chains = nc, iter = ni, warmup = nb, thin = nt,
            seed = 1,
            open_progress = FALSE)
print(out, digits = 3)

stan_plot(out)
stan_trace(out)
