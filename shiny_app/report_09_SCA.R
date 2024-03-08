## Read data
C <- as.matrix(read.table("data/nscod_catage.dat",
  header = TRUE,
  check.names = FALSE, row.names = 1
))
I <- as.matrix(read.table("data/nscod_survey.dat",
  header = TRUE,
  check.names = FALSE, row.names = 1
))
M <- as.matrix(read.table("data/nscod_natmort.dat",
  header = TRUE,
  check.names = FALSE, row.names = 1
))

minYear <- min(as.integer(rownames(C)))
maxYear <- max(as.integer(rownames(C)))
minAge <- min(as.integer(colnames(C)))
maxAge <- max(as.integer(colnames(C)))
nYears <- maxYear - minYear + 1
nAges <- maxAge - minAge + 1

## Prepare containers
N <- matrix(NA_real_,
  nrow = nYears + 1, ncol = nAges,
  dimnames = list(minYear:(maxYear + 1), minAge:maxAge)
)
F <- matrix(NA_real_, nrow = nYears, ncol = nAges, dimnames = dimnames(C))

## Set parameter initial values
logNa <- c(6.8, 5.7, 3.5, 3.0, 1.8, 2.8)
logNt <- c(
  7.6, 6.0, 7.6, 6.5, 6.2, 6.8, 5.9, 6.0, 6.7, 6.1, 6.8, 6.3, 5.9, 7.0,
  5.2, 5.6, 6.1, 5.1, 5.4, 4.7, 5.4, 5.2, 5.9, 5.2, 5.3, 5.3, 5.7, 5.0,
  5.4, 5.6, 6.0, 5.1, 4.9
)
logFa <- c(0.2, 1.6, 1.8, 1.6, 1.4)
logFt <- c(
  -1.7, -1.7, -1.7, -1.6, -1.8, -1.8, -1.6, -1.8, -1.7, -1.8, -1.8,
  -1.7, -1.8, -1.8, -1.8, -1.7, -1.6, -1.6, -2.1, -1.7, -2.3, -2.0,
  -2.1, -2.1, -2.2, -2.3, -2.4, -2.6, -2.8, -2.8, -2.9, -2.9, -2.8
)
logQ <- c(-5.1, -3.6, -2.9, -2.9, -2.7)

## Evaluate F, Z, and N
Fa <- exp(c(logFa, 0))
Ft <- exp(logFt)
F[] <- Ft %o% Fa
Z <- F + M
N[1, ] <- exp(logNa)
N[-1, 1] <- exp(logNt)

A <- ncol(N)
T <- nrow(N)
for (t in 1:(T - 1))
{
  for (a in 1:(A - 2))
  {
    N[t + 1, a + 1] <- N[t, a] * exp(-Z[t, a])
  }
  N[t + 1, A] <- N[t, A - 1] * exp(-Z[t, A - 1]) + N[t, A] * exp(-Z[t, A])
}

## Predict C and I
Nc <- N[-nrow(N), ]
Chat <- F / Z * Nc * (1 - exp(-Z))
Chat <- Chat[rownames(Nc) %in% rownames(C), colnames(Nc) %in% colnames(C)]
Cres <- log(C) - log(Chat)

Ni <- N[rownames(N) %in% rownames(I), colnames(N) %in% colnames(I)]
Ihat <- sweep(Ni, 2, exp(logQ), "*")
Ires <- log(I) - log(Ihat)

## Evaluate SSQ
ssq <- function(res) {
  # if you want to switch to max likelihood
  #-sum(dnorm(res, sd=sqrt(mean(res^2)), log=TRUE))
  sum(res^2)
}

c(catch = ssq(Cres), survey = ssq(Ires))

# now lets wrap all this in a function and apply

source("utilities_sca.R")

## Read data
C <-
  as.matrix(read.table("data/nscod_catage.dat",
    header = TRUE,
    check.names = FALSE, row.names = 1
  ))
I <- as.matrix(read.table("data/nscod_survey.dat",
  header = TRUE,
  check.names = FALSE, row.names = 1
))
M <- as.matrix(read.table("data/nscod_natmort.dat",
  header = TRUE,
  check.names = FALSE, row.names = 1
))
data <- list(C = C, I = I, M = M)

## Set initial parameter values

parlist <- list(
  logNa = rep(8, ncol(C)),
  logNt = rep(8, nrow(C)),
  logFa = rep(0, ncol(C) - 1),
  logFt = rep(0, nrow(C)),
  logQ = rep(-5, ncol(I))
)


par <- unlist(parlist)

################################################################################

## Fit model

sca(par, data, full = TRUE)
sca(par, data)


opt1 <- optim(par, sca, data = data)
opt1

optim(par, sca,
  data = data,
  control = list(maxit = 1000)
)


opt2 <- optim(par, sca, data = data, method = "BFGS")
opt2

# or we can use nlminb
# ?nlminb
opt4 <- nlminb(par, sca, data = data)
opt4

opt5 <- nlminb(par, sca,
  data = data,
  control = list(eval.max = 1000, iter.max = 1000)
)
opt5

## OR! we can use TMB
if (FALSE) {
  # need to fix
  library(RTMB)
  tmbsca <- function(params) {
    sca2(params, data)
  }
  tbmobj <- MakeADFun(tmbsca, parlist)

  obj$hessian <- TRUE
  opt <- do.call("optim", obj)
  opt
  opt$hessian ## <-- FD hessian from optim
  obj$he() ## <-- Analytical hessian
  rep <- sdreport(obj)
  summary(rep)
}

# summarise fits to check objective value and convergence
opt4$value <- opt4$objective
opt5$value <- opt5$objective
opts <- list(opt1 = opt1, opt2 = opt2, opt4 = opt4, opt5 = opt5)
sapply(
  opts,
  function(x) {
    c(
      value = x$value,
      convergence = x$convergence
    )
  }
)

# lets go with BFGS and maximum iterations of 1000

## final run (do it with maximum likelihood)

run <- optim(par = par, fn = sca, data = data, ssq = FALSE, method = "BFGS", hessian = TRUE)

run

# evaluate the model at the optimised parameter values
predictions <- sca(run$par, data, full = TRUE)

## View results

par(mfrow = c(2, 2))

## 1 Population
round(predictions$N)
Year <- as.integer(rownames(predictions$N))
plot(apply(predictions$N, 2, median),
  ylim = c(0, 400),
  yaxs = "i", type = "l", lty = 3,
  main = "Population in 2016 (bars) vs.\n median population (line)",
  xlab = "Age", ylab = "Individuals"
)
points(c(tail(predictions$N, 1)), type = "h", lwd = 6)

## 2 Recruitment
barplot(predictions$N[, 1], ylab = "Individuals at age 1", main = "Recruitment")

## 3 Selectivity
round(predictions$F, 2)
plot(colMeans(predictions$F) / max(colMeans(predictions$F)),
  ylim = c(0, 1.05), yaxs = "i", type = "l",
  main = "Selectivity", xlab = "Age", ylab = "Average F at age"
)

## 4 Fbar
Fbar2.4 <- rowMeans(predictions$F[, 2:4])
plot(Year[-length(Year)], Fbar2.4,
  ylim = c(0, 1.2), yaxs = "i", type = "l",
  main = "Fbar (2-4)", ylab = "Average F at ages 2-4"
)


# get some errors out, sometimes called a parametric bootstrap
library(MASS)
Sigma <- solve(run$hessian)
par_sim <- mvrnorm(1000, run$par, Sigma)

sims <- lapply(1:1000, function(i) sca(par_sim[i, ], data, full = TRUE))

Fbar2.4_sim <-
  sapply(
    sims,
    function(x) {
      rowMeans(x$F[, 2:4])
    }
  )

# check out: ?sapply, ?lapply, ?sweep, ?apply...

# plot a few curves to see the uncertainty in the estimate
matplot(Year[-length(Year)], Fbar2.4_sim[, sample(1:ncol(Fbar2.4_sim), 100)],
  type = "l", lty = 1, col = grey(0.5, alpha = 0.5),
  ylim = c(0, max(Fbar2.4_sim)), # set y limits
  main = "Fbar (2-4)", ylab = "Average F at ages 2-4", xlab = "Year"
)

# overlay fit
lines(Year[-length(Year)], Fbar2.4, lty = 1, lwd = 2)

# add confidence intervals
lines(Year[-length(Year)], apply(Fbar2.4_sim, 1, quantile, 0.025), col = "red", lty = 2)
lines(Year[-length(Year)], apply(Fbar2.4_sim, 1, quantile, 0.975), col = "red", lty = 2)
