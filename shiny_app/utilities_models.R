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
