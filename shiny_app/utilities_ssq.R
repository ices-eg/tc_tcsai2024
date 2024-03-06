library(RTMB)

# calculate nll surface
ssq <- function(data_, model_) {
  nll <- function(params) {
    loga <- params$loga
    logb <- params$logb
    logSigma <- params$logSigma

    log_pred <- model_(exp(loga), exp(logb), data_$S)

    -sum(
      dnorm(
        x = log(data_$R),
        mean = log_pred,
        sd = exp(logSigma),
        log = TRUE
      )
    )
  }

  ssq <- function(loga, logb) {
    sum((log(data_$R) - model_(exp(loga), exp(logb), data_$S))^2)
  }

  # get good starting values
  parameters <- list(loga = log(mean(data_$R)), logb = 0, logSigma = log(sd(log(data_$R))))
  obj <- MakeADFun(nll, parameters, silent = TRUE)

  opt <- nlminb(obj$par, obj$fn, obj$gr)
  rep <- sdreport(obj)
  est <- summary(rep)
  print(est)

  # find a good range of loga and logb
  sda <- est["loga", "Std. Error"]
  sdb <- est["logb", "Std. Error"]

  # hacky saftey check for plotting
  if (!is.finite(sda) || sda > abs(est["loga", "Estimate"])) sda <- est["loga", "Estimate"]
  if (!is.finite(sdb) || sdb > abs(est["logb", "Estimate"])) sdb <- abs(est["logb", "Estimate"])

  logas <- seq(-1, 1, length = 200) * 3 * sda + est["loga", "Estimate"]
  logbs <- seq(-1, 1, length = 200) * 3 * sdb + est["logb", "Estimate"]

  # fill out ssq
  ssqs <- t(outer(logas, logbs, Vectorize(ssq)))
  ssqs[ssqs >= (min(ssqs) + 20)] <- NA

  # trim based on NAs
  atrim <- apply(ssqs, 2, function(x) any(!is.na(x)))
  btrim <- apply(ssqs, 1, function(x) any(!is.na(x)))

  logas <- logas[atrim]
  logbs <- logbs[btrim]
  ssqs <- ssqs[btrim, atrim]

  list(ssqs = ssqs, logas = logas, logbs = logbs, est = est)
}
