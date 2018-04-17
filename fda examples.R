library(fda)


# 4.2 Methods for functional data objects ---------------------------------

daytime <- (1:365)-0.5 #Days of the year (minus half a day)

JJindex <- c(182:365,1:181) #DOY running from July 1-June30 (similar to a water year)
tempmat <- daily$tempav[JJindex,] #Matrix of daily temperatures (rows) for each station (columns)

#Makes fourier basis for every day (0-365) using 65 basis functions (sine/cosine combinations) - no roughness penalty
tempbasis <- create.fourier.basis(c(0,365),65) 

#Fit fd object using days of the year, daily temperatures for each day, and fourier basis
tempfd <- smooth.basis(daytime,tempmat,tempbasis)

#Provides names for rows (x-axis), columns, and predictions (y-axis)
tempfd$names <- list("Day (July 2 to June 30)","Weather station","Mean temperature (deg. C)")

#Plots predictions for each station
plot(tempfd,col=1,lty=1)

#4.2.1 - another way to fit sine curve using a spline - close but not perfect

basis13 <- create.bspline.basis(c(0,10),13)
tvec <- seq(0,1,len=13)
sinecoef <- sin(2*pi*tvec)
sinefd <- fd(sinecoef,basis13,list("t","","f(t)"))
op <- par(cex=1.2)
plot(sinefd,lwd=2)
points(tvec*10,sinecoef,lwd=2)
par(op)


# 9.2 Scalar response model for log annual precipitation ------------------

annualprec <- log10(apply(daily$precav,2,sum)) #Summed annual precipitation from each station

tempbasis65  = create.fourier.basis(c(0,365),65)
tempSmooth65 = smooth.basis(day.5, daily$tempav, tempbasis65)
tempfd65     = tempSmooth65$fd

plot(tempfd65,xlab='Day of year',ylab='Log10 Precip')

## Section 9.4 Three Estimates of the Regression Coefficient
##             Predicting Annual Precipitation

templist      = vector("list",2) #Creates list with 35x1 vector of ones, and fd object
templist[[1]] = rep(1,35)
templist[[2]] = tempfd65

# 9.4.1 Low Dimensional Regression Coefficient Function beta

conbasis   = create.constant.basis(c(0,365)) #Constant basis
betabasis5 = create.fourier.basis(c(0,365),5) #Fourier basis with 5 functions
betalist1  = vector("list",2)
betalist1[[1]] = conbasis #Puts the 2 basis functions together in a list
betalist1[[2]] = betabasis5

fRegressList1 = fRegress(annualprec,templist,betalist1) #Fits ann.prec as a function of temperature defined by betalist operators

betaestlist1  = fRegressList1$betaestlist
tempbetafd1   = betaestlist1[[2]]$fd
plot(tempbetafd1, xlab="Day", ylab="Beta for temperature") #Beta coefficents for fourier effect of temp on ann.prec.

tempbetafd2   = betaestlist1[[1]]$fd
plot(tempbetafd2, xlab="Day", ylab="Beta for temperature") #Beta coefficent for constant effect of temp on ann.prec.


coef(betaestlist1[[1]]) #Very small coefficent for constant basis
coef(betaestlist1[[2]]) #Set of coefficients for fourier basis

#Assess quality of fit - extract residuals (fitted-actual)
annualprechat1 = fRegressList1$yhatfdobj #Predicted ann. prec. for each station
annualprecres1 = annualprec - annualprechat1 #Residual
SSE1.1  = sum(annualprecres1^2) #SSQ Residual
SSE0    = sum((annualprec - mean(annualprec))^2) #SSQ Mean
(RSQ1   = (SSE0-SSE1.1)/SSE0) #RMSE
# 0.80 as in the book
(Fratio1 = ((SSE0-SSE1.1)/5)/(SSE1.1/29))
# 22.6 as in the book

Lcoef = c(0,(2*pi/365)^2,0) #Harmonic acceleration operator
harmaccelLfd = vec2Lfd(Lcoef, c(0,365))

# refit with 35 terms rather than 5 in the fourier basis

betabasis35 = create.fourier.basis(c(0, 365), 35)
lambda      = 10^12.5 #Amount of smoothing (penalizing) to be applied
#Functional parameter object: 35-function fourier basis, linear differential operator for fourier, and penalty term 
betafdPar.  = fdPar(betabasis35, harmaccelLfd, lambda) 

betalist2      = betalist1 #Takes the first betalist and adds in the new fourier terms
betalist2[[2]] = betafdPar.

#Fits ann.prec as a function of temperature defined by betalist operators
annPrecTemp    = fRegress(annualprec, templist, betalist2) 
betaestlist2   = annPrecTemp$betaestlist
annualprechat2 = annPrecTemp$yhatfdobj #Predicted ann. precip.

print(annPrecTemp$df) #Degrees of freedom for model

SSE1.2 = sum((annualprec-annualprechat2)^2)
(RSQ2 = (SSE0 - SSE1.2)/SSE0)
# 0.75 as in the book - drop in R^2 value, due to penalizing

(Fratio2 = ((SSE0-SSE1.2)/3.7)/(SSE1.2/30.3))

# Figure 9.2
plot(annualprechat2, annualprec, lwd=2,ylab='Annual Precip.',xlab='Predicted Annual Precip.') 
abline(lm(annualprec~annualprechat2), lty='dashed', lwd=2)

plot(betaestlist2[[2]]$fd, lwd=2, xlab="Day", ylab="Beta for temperature") #Beta for temperature - stronger effect of fall temperatures on ann. precip.

betalist      = betalist1
betalist[[2]] = fdPar(conbasis)
fRegressList  = fRegress(annualprec, templist, betalist)
betaestlist   = fRegressList$betaestlist

annualprechat = fRegressList$yhatfdobj
SSE1 = sum((annualprec-annualprechat)^2)

(RSQ = (SSE0 - SSE1)/SSE0)
# 0.49 as in the book

(Fratio = ((SSE0-SSE1)/1)/(SSE1/33))


# Chapter 10: -------------------------------------------------------------

#  select only the data for sites Uyak and Uganik, which have data
#  from 1986 to 2005, except for 1998
sites = c('Uganik', 'Uyak')
sel   = seabird$Bay %in% sites
UU    = seabird[sel,] 

# Drop 2 species with many NAs
NAs       = sapply(UU, function(x)sum(is.na(x)))
NAs.      = which(NAs > 2)
birdindex = (1:15)[-NAs.]
birds     = names(UU)[birdindex]

#  Compute mean counts taken over both sites and transects
meanCounts = matrix(NA, 20, 13) #Empty 20x13 matrix
dimnames(meanCounts) = list(1986:2005, birds) #Years and bird names for row/col names

for(i in 1:20){ #For each year
  sel = (UU$Year == rownames(meanCounts)[i]) #Rows in year i
  meanCounts[i, ] = sapply(UU[sel, birds], mean, na.rm=TRUE) #Mean number of birds, averaged over transect/site
} #The Bayesian in me dies a little bit...

selYear   = !is.na(meanCounts[,1]) #Years that don't have NAs or NaNs (1998)
logCounts = log10(meanCounts[selYear,]) #log10 of means, stripping out 1998

#  time vectors in years and in indices in 1:20
yearObs  = as.numeric(rownames(logCounts)) #Years in use (not 1998)
yearCode = (1:20)[selYear] #Year indices (1:20, but no 13)

shellfishindex = c(1,2,5,6,12,13) #Which bird spp eat shellfish?
fishindex      = (1:13)[-shellfishindex] #All others eat fish

# Figure 10.2 - population trends through time for each spp.
ylim = range(logCounts) #Y-range
op = par(mfrow=c(2,1), mar=c(2, 4, 4, 1)+.1) #Plotting parameters

matplot(yearObs, logCounts[, shellfishindex], xlab='', ylab='',
        ylim=ylim, main='Shellfish Diet', type='b', col=1)
meanShellfish = apply(meanCounts[, shellfishindex], 1, mean)
lines(yearObs, log10(meanShellfish[!is.na(meanShellfish)]), lwd=3)
abline(h=0, lty='dotted')

matplot(yearObs, logCounts[, fishindex], xlab='', ylab='',
        ylim=ylim, main='Fish Diet', type='b', col=1)
meanFish = apply(meanCounts[, shellfishindex], 1, mean)
lines(yearObs, log10(meanFish[!is.na(meanFish)]), lwd=3)
abline(h=0, lty='dotted')

par(op)

#  Compute mean counts taken over transects only within sites
#  so we have 2 observations for each bird species each year.
#  Two of these counts are zero, and are replaced by 1/(2*n)

meanCounts2 = matrix(NA, 20, 26) #Empty 20x26 matrix

for(i in 1:20) for (j in 1:2) { #Same as above, but has a separate column for each site
  sel = (UU$Year == rownames(meanCounts)[i] & as.character(UU$Bay) == sites[j])
  meanCountsij = sapply(UU[sel, birds], mean, na.rm=TRUE)
  n = sum(sel)
  if (n > 0) {
    meanCountsij[meanCountsij == 0] = 1/(2*n)
  }
  meanCounts2[i,(j-1)*13+(1:13)] = meanCountsij
}

selYear2   = !is.na(meanCounts2[, 1])
yearCode  = (1:20)[selYear2]
all.equal(yearCode, c(1:12, 14:20))

logCounts2 = log10(meanCounts2[selYear2,])

#  Represent log mean counts exactly with a polygonal basis

birdbasis = create.polygonal.basis(yearCode) #Polygonal basis (linear changes b/w years)
#birdbasis = create.bspline.basis(range(yearCode),11) #Bspline basis with 11 basis functions
birdlist2 = smooth.basis(yearCode, logCounts2, birdbasis) #Smooths logCounts2 across yearCode, using polygonal basis
birdfd2 = birdlist2$fd 
plot(birdfd2) #Shows "smoothed" (not really, since it's a linear smoother) data - the next question is how this smoothing pattern relates to other stuff:

#  The design matrix contains an intercept dummy variable, a
#  feed dummy variable, and dummy variables for birds, excluding
#  the second bird in each group, which turns out to be the each
#  group's most abundant species, and which is designated as the
#  baseline bird for that group.

# 15 columns for the intercept + diet + 13 bird species
# 26 rows for the 26 (species - bay) combinations
Zmat0 = matrix(0,26,15)

#  Intercept or baseline effect
Intercept = rep(1,26)

#  Crustacean/Mollusc feeding effect:  a contrast between the two groups - difference rather than intercepts for each
foodindex = c(1,2,5,6,12,13)
fooddummy = c(2*rep(1:13 %in% foodindex, 2)-1)

#  Bird effect, one for each species
birddummy = diag(rep(1,13))
birdvarbl = rbind(birddummy,birddummy)

#  fill the columns of the design matrix
Zmat0[,1]    = Intercept #First column 
Zmat0[,2]    = fooddummy #Contrast b/w 2 feeding types
Zmat0[,3:15] = birdvarbl #Bird spp

#  Two extra dummy observations are added to the functional data
#  object for log counts, and two additional rows are added to
#  the design matrix to force the bird effects within each diet
#  group to equal 0.

birdfd3 = birdfd2 #Copy of smoothed data
birdfd3$coefs = cbind(birdfd3$coefs, matrix(0,19,2))

Zmat = rbind(Zmat0, matrix(0,2,15))
Zmat[27,shellfishindex+2] = 1
Zmat[28,     fishindex+2] = 1

p = 15
xfdlist = vector("list",p) #List of length p
names(xfdlist) = c("const", "diet", birds) #Names of list elements
betalist = xfdlist
for (j in 1:p) xfdlist[[j]] = Zmat[,j] #Columns of design matrix into list elements

#  set up the functional parameter object for (the regression fns.
#  use cubic b-spline basis for intercept and food coefficients
betabasis1 = create.bspline.basis(c(1,20),21,4,yearCode) #Make bspline basis from 1:20, using 21 basis functions. 4 indicates order of splines (4=cubic). breaks are specified as yearCode.
Lfdobj1    = int2Lfd(2) #Converts integer (2) to lin. diff. operator (penalty for squared acceleration)
Rmat1      = eval.penalty(betabasis1, Lfdobj1) #Create basis penalty matrix
lambda1    = 10 #Penalty term
betafdPar1 = fdPar(betabasis1,Lfdobj1,lambda1,TRUE,Rmat1) #Define fd parameter object (bspline basis,LDF,penalty,basis penalty matrix)
betalist[[1]] = betafdPar1 #Bspline basis function into 1st slot of betalist ("const" or intercept)
betalist[[2]] = betafdPar1 #Bspline basis function into 2nd slot of betalist ("diet")
betabasis2 = create.constant.basis(c(1,20)) #Constant basis
betafdPar2 = fdPar(betabasis2) #Define fd parameter object (constant basis)
for (j in 3:15) betalist[[j]] = betafdPar2 #Constant basis function for every other slot (constant for each spp)

#Fits birdfd3 (smoothed bird counts) as a function of xfdlist(intercept,diet difference,spp) defined by betalist operators (bspline,bspline,constant)
birdRegress = fRegress(birdfd3, xfdlist, betalist) #Regress 
betaestlist = birdRegress$betaestlist

# Figure 10.3 is produced in Section 10.2.2 below
# after estimating the smoothing parameter in Section 10.1.3
#
# Here we plot the regression parameters
# without the confidence intervals.

op = par(mfrow=c(2,1))
plot(betaestlist$const$fd)
plot(betaestlist$diet$fd)
par(op)

