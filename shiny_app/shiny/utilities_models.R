# define models
ricker <- function(a, b, S) {
  log(a) + log(S) - b * S
}

bevholt <- function(a, b, S) {
  log(a) + log(S) - log(b + S)
}

bevholt2 <- function(a, b, S) {
  log(a) + log(S) - log(1 + b * S)
}

AgeModel <- function(Ninit, M, Fmort, mat, w, Amax, Tmax, alpha, beta, v) {
  N <- matrix(nrow = Amax, ncol = Tmax)
  SSB <- numeric(Tmax)

  ## Year 1
  N[1, 1] <- Ninit
  for (a in 1:(Amax - 1)) {
    N[a + 1, 1] <- N[a, 1] * exp(-M[a] - Fmort[a])
  }
  SSB[1] <- sum(N[, 1] * mat * w)

  ## Later years
  for (t in 1:(Tmax - 1))
  {
    N[1, t + 1] <- (alpha * SSB[t]) / (beta + SSB[t]) + v * runif(1, -0.5, 0.5)
    for (a in 1:(Amax - 1)) {
      N[a + 1, t + 1] <- N[a, t] * exp(-M[a] - Fmort[a])
    }
    SSB[t + 1] <- sum(N[, t + 1] * mat * w)
  }

  list(alpha = alpha, beta = beta, N = N, SSB = SSB, Rec = N[1, ])
}
