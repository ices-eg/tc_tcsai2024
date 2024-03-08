#' Statistical Catch at Age
#'
#' Run simple statistical catch-at-age model: one fleet, one survey, and a plus
#' group.
#'
#' @param par a vector of parameters named \code{logNa}, \code{logNt},
#'        \code{logFa}, \code{logFt}, and \code{logQ}.
#' @param data a list of matrices named \code{C}, \code{I}, and \code{M}.
#'        Alternatively, \code{M} can be a scalar.
#' @param full whether to return a full list of objects.
#' @param ssq whether to use sums of squares or maximum likelhood
#'
#' @details
#' The \code{par} vector consists of five blocks:
#' \tabular{lll}{
#'   Name         \tab Length           \tab Notes\cr
#'   \code{logNa} \tab \code{ncol(N}    \tab Age 1 to \eqn{A} in catch\cr
#'   \code{logNt} \tab \code{nrow(N)-1} \tab Year 2 to \eqn{T+1} in catch\cr
#'   \code{logFa} \tab \code{ncol(F)}   \tab Age 1 to \eqn{A-1} in catch\cr
#'   \code{logFt} \tab \code{nrow(F)}   \tab Year 1 to \eqn{T} in catch\cr
#'   \code{logQ}  \tab \code{ncol(I)}   \tab Age 1 to \eqn{A} in survey
#' }
#'
#' Selectivity of the younger ages is relative to the oldest age. The
#' selectivity of the oldest age is always 1, and is not included in the
#' parameter vector.
#'
#' @return
#' Objective function value as a scalar, or a full list of objects if
#' \code{full = TRUE}.
#'
#' @examples
#' data <- list(C = nscod_catage, I = nscod_survey, M = nscod_natmort)
#'
#' logNa <- rep(8, ncol(data$C))
#' logNt <- rep(8, nrow(data$C))
#' logFa <- rep(0, ncol(data$C) - 1)
#' logFt <- rep(0, nrow(data$C))
#' logQ <- rep(-5, ncol(data$I))
#' par <- c(logNa = logNa, logNt = logNt, logFa = logFa, logFt = logFt, logQ = logQ)
#'
#' optim(f = sca, par = par, data = data, method = "BFGS", control = list(maxit = 1000))
#'
#' @export

sca2 <- function(par, full = FALSE, ssq = TRUE) {
  ## Get parameters and data
  logNa <- par$logNa
  logNt <- par$logNt
  logFa <- par$logFa
  logFt <- par$logFt
  logQ <- par$logQ
  logSigmaCatch <- par$logSigmaCatch
  logSigmaSurvey <- par$logSigmaSurvey

  C <- data$C
  I <- data$I
  M <- data$M

  minYear <- min(as.integer(rownames(C)))
  maxYear <- max(as.integer(rownames(C)))
  minAge <- min(as.integer(colnames(C)))
  maxAge <- max(as.integer(colnames(C)))
  nYears <- maxYear - minYear + 1
  nAges <- maxAge - minAge + 1

  ## Prepare containers
  N <- advector(matrix(
    nrow = nYears + 1, ncol = nAges,
    dimnames = list(minYear:(maxYear + 1), minAge:maxAge)
  ))

  ## Evaluate F, Z, and N
  Fa <- exp(c(logFa, 0))
  Ft <- exp(logFt)
  F <- outer(Ft, Fa)
  Z <- F + M
  N[1, ] <- exp(logNa)
  N[-1, 1] <- exp(logNt)

  A <- ncol(N)
  T <- nrow(N)
  for (t in 1:(T - 1))
  {
    N[t + 1, 2:(A - 1)] <- N[t, 1:(A - 2)] * exp(-Z[t, 1:(A - 2)])
    # plus group
    N[t + 1, A] <- N[t, A - 1] * exp(-Z[t, A - 1]) + N[t, A] * exp(-Z[t, A])
  }

  ## Predict C and I
  Nc <- N[-nrow(N), ]
  Chat <- F / Z * Nc * (1 - exp(-Z))
  Chat <- Chat[rownames(Nc) %in% rownames(C), colnames(Nc) %in% colnames(C)]

  Ni <- N[rownames(N) %in% rownames(I), colnames(N) %in% colnames(I)]
  Ihat <- t(t(Ni) * exp(logQ))

  ## Evaluate nll
  nll <- -sum(dnorm(log(c(C)), mean = log(c(Chat)), sd = exp(logSigmaCatch), log = TRUE))

  nll <- nll -sum(dnorm(log(c(I)), mean = log(c(Ihat)), sd = exp(logSigmaSurvey), log = TRUE))

  nll
}
