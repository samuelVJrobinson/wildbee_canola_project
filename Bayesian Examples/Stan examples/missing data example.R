library(rstan)
library(MASS)

# Simulate some data
mu <- c(10, 5, -5)
Sig <- matrix(c(1, 0.7, 0.8,
                0.7, 2, 0.2,
                0.8, 0.2, 1.5), 
              3, 3)
N <- 100
dat <- mvrnorm(N, mu, Sig)

# Randomly remove some data
nmiss <- 115
miss <- sample.int(prod(dim(dat)), size = nmiss)
dat[miss] <- NA

# Extract the missing values into a VECTOR
dat_complete <- dat[!is.na(dat)]

# Extract the missing and present values as MATRICES
ind_pres <- which(!is.na(dat), arr.ind = TRUE)
ind_miss <- which(is.na(dat), arr.ind = TRUE)

# STAN model
mod.nomiss <- '
data {
int<lower=0> Nrow;
int<lower=0> Ncol;
int<lower=0> Ncomp; // Number of non-missing values
int<lower=0> Nmiss; // Number of missing values
real dat_complete[Ncomp];   // Vector of non-missing values
int ind_pres[Ncomp, 2];     // Matrix (row, col) of non-missing value indices
int ind_miss[Nmiss, 2];     // Matrix (row, col) of missing value indices
}
parameters {
// Multivariate normal distribution parameters
vector[Ncol] mu;
cov_matrix[Ncol] Sigma;
// Vector containing "stochastic" nodes (for filling missing values
real ymiss[Nmiss];      
}
transformed parameters {
vector[Ncol] y[Nrow];   // The "data" with interpolated missing values
// Fill y with non-missing values 
for(n in 1:Ncomp) {
y[ind_pres[n,1]][ind_pres[n,2]] <- dat_complete[n];
}
// Fill the rest of y with missing value "parameters"
for(n in 1:Nmiss){
y[ind_miss[n,1]][ind_miss[n,2]] <- ymiss[n];
}
}
model {
for(i in 1:Nrow){
y[i] ~ multi_normal(mu, Sigma);
}
}
'

mod.data <- list(Nrow = nrow(dat), #100 rows
                 Ncol = ncol(dat), #3 columns
                 Ncomp = length(dat_complete), #Number of complete entries (185)
                 Nmiss = sum(is.na(dat)), #Number of incomplete entries (115)
                 dat_complete = dat_complete, #Vector of complete entries
                 ind_pres = ind_pres, 
                 ind_miss = ind_miss)

fit <- stan(model_code = mod.nomiss, data = mod.data,
            iter = 1000, chains = 3, verbose=TRUE,
            pars = c("mu", "Sigma"))

print(fit)
