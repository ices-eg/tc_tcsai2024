# Fitting Models to Data in R

# Colin Millar, modified from Iago Mosqueira and Ernesto Jardim

# This script shows the steps followed to fit a stock-recruitment model to
# in the file 'north_sea_herring_SR.csv'

# Load and explore data ----

# load data from comma-separated file to data.frame
herring <- read.csv(file = "data/north_sea_herring_SR.csv", header = TRUE)

# take a look at what we have
head(herring) # this looks at the first 6 rows
str(herring) # this lets us inspect what the columns contain

# lets rename some columns because I am lazy and want the code to be
# readable and easier to type
names(herring)
names(herring) <- c("yc", "ssb", "rec")
head(herring)

# to access the diffrent columns use '$' i.e to see the SSB values:
herring$ssb
# and to see observed recruitment
herring$rec

# initial plot of SSB vs. recruits
plot(
  rec ~ ssb,
  data = herring, # look in herring for x and y values
  xlab = "SSB (thousand tonnes)",
  ylab = "Recruits at age 0 (millions)"
)


# probably better to set x and y limits to start at zero
plot(
  rec ~ ssb,
  data = herring, # look in herring for x and y values
  xlim = c(0, max(ssb)), # set x limits
  ylim = c(0, max(rec)), # set y limits
  xlab = "SSB (thousand tonnes)",
  ylab = "Recruits at age 0 (millions)"
)


# We are now going to demonstrate the same techniques employed in the spreadsheet
# solution to the assignment

# Beverton and holt recruitmet model R=b1*S/(b2+S) ----


# (1) Calculate predicted R for each year ----
# Rpred = b1 * S / (b2 + S)

# starting values for b1 and b2
a <- 1
b <- 1

# set up the other variables (i.e. S)
S <- herring$ssb

Rpred <- a * S / (b + S)

# (2) Calculate log residuals, Ln(obs/pred) ----

# assign observed recruitment
Robs <- herring$rec

resids <- log(Robs / Rpred) # = log(Robs) - log(Rpred)

# note that in R, log is the natural log:
?log # see the help file for the log function
log(exp(1))
log(exp(10))

# (3) Calculate sum of squared residuals ----

?sum # see the help file for sum
ssq_resids <- sum(resids^2)

# (4) Minimize sum-of-squares with solver by adjusting a and b ----

# to do this, we need to set up a function that takes
# a and b as input, and returns the sums of squared residuals

# in R a function is a collection of steps: i.e.
add <- function(b1, b2) {
  b1 + b2
}
add(1, 2)
# 3

# the sums of squares function is collection of the previous 3 steps:
ssq <- function(a, b) {
  # 1. Calculate predicted R for each year
  Rpred <- a * S / (b + S)
  # 2. Calculate log residuals, Ln(obs/pred)
  resids <- log(Robs / Rpred)
  # 3. Calculate sum of squared residuals
  ssq_resids <- sum(resids^2)

  # return
  ssq_resids
}

# lets test this out:
ssq(a = 1, b = 1)
# you don't have to 'name' the inputs, but then the order has to be correct
ssq(1, 1)
# variables can be use also, not just numbers
ssq(a, b)
# try some different values, you can imagine trying out differnet values and finding the one that gives the lowest ssq
ssq(2, 1)


# now we need to search over lots of values for b1 and b2 to
# find the minimum.
# There are lots of ways to do this, we will first look at the optim function.
# the help file for optim is:
?optim

ssq_optim <- function(par) {
  a <- par[1]
  b <- par[2]

  ssq(a, b)
}

ssq_optim(c(1, 1))
ssq_optim(c(2, 1))

# use c to combine the starting values into a vector
?c
par0 <- c(1, 1)

# lets test the new ssq funciton at the starting values
ssq_optim(par0)

# lets run it..
opt <- optim(par0, ssq_optim)

opt

# it didn't do so well....  lets try with different starting values:
opt <- optim(c(32000000, 300), ssq_optim)

opt

# better now :)

# (5) Plot observed and predicted R ----

# get the parameter estimates from the optimisation
a <- opt$par[1]
b <- opt$par[2]

# predict recruitment
Rpred <- a * S / (b + S)

# plot
plot(
  Robs ~ S,
  xlim = c(0, max(S)), # set x limits
  ylim = c(0, max(Robs)), # set y limits
  xlab = "Spawning Stock Biomass (tonnes)",
  ylab = "Age-0 Recruitment"
)

# add predictions to the plot
points(Rpred ~ S, col = "red", pch = 2)


# (6) Plot residuals ----

# calculate residuals
resids <- log(Robs / Rpred)

# plot them
plot(resids ~ S)
# add in a reference line
abline(h = 0, lty = 2)





















###############################################################################
# We are now going to demonstrate the same solution, but taking advantage of
# the tools provided by a programming / scripting language
###############################################################################

#==============================================================================
# Beverton and holt recruitmet model R=b1*S/(b2+S)
#==============================================================================

#------------------------------------------------------------------------------
# (1) Calculate predicted R for each year
# Rpred = b1 * S / (b2 + S)
#------------------------------------------------------------------------------

# this time we will write a function to do this called bevholt
#  to be safe we will also pass in S
#  this way we know for sure wha values of S are being used

bevholt <- function(b, S) {
  b[1] * S / (b[2] + S)
}

# compute R at the starting values for b1 and b2
Rpred <- bevholt(c(1, 1), S = herring$ssb)

# lets jump to step 4 ...

#------------------------------------------------------------------------------
# (4) Minimize sum-of-squares with solver by adjusting b1 and b2
#------------------------------------------------------------------------------

# now lets modify the ssq function to accept S and Robs,
# and use the function bevholt

# the sums of squares function is collection of the previous 3 steps:
ssq <- function(b, S, Robs) {
  # 1. Calculate predicted R for each year
  Rpred <- bevholt(b, S)
  # 2. Calculate log residuals, Ln(obs/pred)
  resids <- log(Robs / Rpred)
  # 3. Calculate sum of squared residuals
  ssq_resids <- sum(resids^2)

  # return
  ssq_resids
}

# lets test this out:
ssq(c(a, b), herring$ssb, herring$rec) # what to you notice this time?
ssq(c(1, 1), herring$ssb, herring$rec)
ssq(c(2, 2), herring$ssb, herring$rec)

# now we need to search over lots of values for b1 and b2 to
# find the minimum.

ssq_optim <- function(par, S, Robs) {
  b <- exp(par)

  ssq(b, S, Robs)
}

# use c to combine the starting values into a vector
par0 <- log(c(1, 1))

# lets test the new ssq funciton
ssq_optim(par0, S = herring$ssb, Robs = herring$rec)

# lets run it..
opt <- optim(par0, ssq_optim, S = herring$ssb, Robs = herring$rec)

opt

# the fit is not quite there yet, so lets try better starting values.
# this highlights the presence of multiple 'local' minima
par0 <- c(20, 5)
opt <- optim(par0, ssq_optim, S = herring$ssb, Robs = herring$rec)

opt


#------------------------------------------------------------------------------
# (5) Plot observed and predicted R
#------------------------------------------------------------------------------

# predict recruitment over the full S range
Spred <- seq(0, max(herring$ssb), length.out = 100)
Rpred <- bevholt(exp(opt$par), S = Spred)

# plot
plot(
  rec ~ ssb,
  data = herring, # pass in data this time
  xlim = c(0, max(S)), # set x limits
  ylim = c(0, max(Robs)), # set y limits
  xlab = "Spawning Stock Biomass (tonnes)",
  ylab = "Age-0 Recruitment"
)

# add predictions to the plot as a line
lines(Rpred ~ Spred, col = "red", pch = 2)















###############################################################################
# The following is to demonstrate a techniques for calculating confidence
# intervals - this is not part of the course and purely for demonstation
# purposes
###############################################################################

# Bootstrapping is so called because it is like you are acheieving something
# from nothing.
#
# but in fact it is taking advantage of the fact that your samle of data
# contains information about how it varies...
#
# this can be seen from the residuals:

# lets run the fit again
fit <- optim(par0, ssq_optim, S = herring$ssb, Robs = herring$rec)

# and calculate the residuals
Rpred <- bevholt(exp(fit$par), herring$ssb)
resids <- log( herring$rec / Rpred)

# and plot a histogram
hist(resids, nclass = 20)

# the mean of the residuals is:
mean(resids)

# but is there not error in this?

# resample from this as if the resuduals are random and reclaculate the mean
r_star <- sample(resids,  replace = TRUE)
mean(r_star)

# do it again
r_star <- sample(resids, replace = TRUE)
mean(r_star)

# do it lots of times!
rmean_star <-
  replicate(10000, {
    r_star <- sample(resids, replace = TRUE)
    mean(r_star)
  })

hist(rmean_star)

#------------------------------------------------------------------------------
# so we are able to access the error inherent in the model fit?
#
# And we can propagate this through to the parameter estimates?
#------------------------------------------------------------------------------

# resample from the residuals as if the resuduals are random and reestimate the
# parameters
r_star <- sample(resids, replace = TRUE)
opt <- optim(par0, ssq_optim, S = herring$ssb, Robs = Rpred + r_star)
opt$par

# do it again
r_star <- sample(resids, replace = TRUE)
opt <- optim(par0, ssq_optim, S = herring$ssb, Robs = Rpred + r_star)
opt$par


# do it lots of times!
par_star <-
  replicate(10000, {
    r_star <- sample(resids, replace = TRUE)
    opt <- optim(par0, ssq_optim, S = herring$ssb, Robs = Rpred * exp(r_star),
                 method = "BFGS")
    opt$par
  })

# separate b1 and b2 bootstrap simulations for ease of inspection
b1_star <- exp(par_star[1,])
b2_star <- exp(par_star[2,])

# plot histograms of simulations
hist(log(b1_star), nclass = 50)
# add confidence intervals
abline(v = quantile(log(b1_star), c(0.025, 0.975)), col = "red")
quantile(b1_star, c(0.025, 0.975))

# what does the 2D bootstrap simulation look like?
plot(log(b1_star), log(b2_star), pch = ".", col = grey(.5, alpha = 0.5))

# a colourful 2d densty plot
image(MASS::kde2d(log(b1_star), log(b2_star), n = 400),
      xlab = "log b1", ylab = "log b2",
      xlim = quantile(log(b1_star), c(0.025, 0.975)),
      ylim = quantile(log(b2_star), c(0.025, 0.975)),
      main = "bootstraped uncertainty in parameter estimates")
# plot the least squares estimate
points(fit$par[1], fit$par[2], pch = 16, col = "blue")



# plot
# predict recruitment over the full S range
Spred <- seq(0, max(herring$ssb), length.out = 100)
Rpred <- apply(par_star, 2, function(x) bevholt(exp(x), S = Spred))

# plot a few curves to see the uncertainty in the relationship
matplot(Spred, Rpred[, sample(1:ncol(Rpred), 100)], type = "l", lty = 1, col = grey(0.5, alpha = 0.5),
        xlim = c(0, max(S)), # set x limits
        ylim = c(0, max(Robs)), # set y limits
        xlab = 'Spawning Stock Biomass (tonnes)',
        ylab = 'Age-0 Recruitment',
        main = "boostraped error in stock recruitment relationship")
# add the data
points(herring$ssb, herring$rec, type = "b", pch = 16, col = "red")











# ==============================================================================
# A note on GLMs for SR modeling
# ==============================================================================

# for plotting lets order the SR data
herring_ord <- herring[order(herring$ssb),]

# mean
mean_fit <- glm(rec ~ 1, data = herring_ord)

plot(herring_ord$ssb, herring_ord$rec)
lines(herring_ord$ssb, fitted(mean_fit), col = "red", lwd = 2)

# linear 1
linear1_fit <- glm(rec ~ ssb - 1, data = herring_ord)

plot(herring_ord$ssb, herring_ord$rec)
lines(herring_ord$ssb, fitted(linear1_fit), col = "red", lwd = 2)

# linear 2
linear2_fit <- glm(rec ~ ssb, data = herring_ord)

plot(herring_ord$ssb, herring_ord$rec)
lines(herring_ord$ssb, fitted(linear2_fit), col = "red", lwd = 2)


# log transformed ricker
# R = a S exp(-bS)
# =>  log R = log(a) -bS + log(S)

ricker_fit <- glm(rec ~ ssb, offset = log(ssb), data = herring_ord, family = Gamma(log))

plot(herring_ord$ssb, herring_ord$rec)
lines(herring_ord$ssb, fitted(ricker_fit), col = "red", lwd = 2)

# inverse transformed beverton holt
# R = a S / (b + S)
# =>  1 / R = (b + S) / (aS)
# =>        = b / (aS) + S / (aS)
# =>        = b/a * 1/S + 1 / a

bh_fit <- glm(rec ~ I(1 / ssb), data = herring_ord, family = Gamma(inverse))

plot(herring_ord$ssb, herring_ord$rec)
lines(herring_ord$ssb, fitted(bh_fit), col = "red", lwd = 2)



# all of 'em
plot(herring_ord$ssb, herring_ord$rec)
lines(herring_ord$ssb, fitted(mean_fit), col = "red", lwd = 2)
lines(herring_ord$ssb, fitted(linear1_fit), col = "blue", lwd = 2)
lines(herring_ord$ssb, fitted(linear2_fit), col = "lightblue", lwd = 2)
lines(herring_ord$ssb, fitted(ricker_fit), col = "seagreen", lwd = 2)
lines(herring_ord$ssb, fitted(bh_fit), col = "orange", lwd = 2)


# I can add code for confidence intervals if that would be helpful?
# you get to the you can extract the standard errors of the fit from the
# predict.glm function.

# this can be useful if you want to quickly check for covariates etc.
# but if you are going to use this in a stock assessmet model you will
# have to go down the route of using optim or a similar optimiser anyway
