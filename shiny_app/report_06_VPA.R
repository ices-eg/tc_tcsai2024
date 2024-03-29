source("utilities_vpa.R")

## Read catch at age
catch <- read.csv("data/cod_catch.csv", header = TRUE, check.names = FALSE, row.names = 1)
Year <- as.numeric(row.names(catch))

## Run model
model <- vpa(catch, Mvec = c(rep(0.1, 2), rep(0.3, 8)), Fterm = 0.1, Fages = 3)

## View results
par(mfrow = c(2, 2))

## Fishing mortality
round(model$F, 3)

## Recruitment
barplot(model$N[, 1], ylab = "Recruitment at age 1", main = "Recruitment (N age 1)")

## Selectivity
round(model$F, 3)
plot(colMeans(model$F) / max(colMeans(model$F)),
  ylim = c(0, 1.05),
  type = "l", main = "Selectivity", xlab = "Age", ylab = "Mean F at age"
)

## Fbar
Fbar <- rowMeans(model$F)
plot(Year, Fbar,
  ylim = c(0, max(Fbar)), main = "Mean F ages 1-10",
  ylab = "Mean F (ages 1-10)", type = "l"
)

## SSB

## Read weigths and maturity at age
wt <- read.csv("data/cod_weights.csv", header = TRUE, check.names = FALSE, row.names = 1)
mat <- read.csv("data/cod_maturity.csv", header = TRUE, check.names = FALSE, row.names = 1)

ssb <- rowSums(model$N * wt * mat) / 1000
plot(Year, ssb,
  ylim = c(0, max(ssb)), main = "Spawning stock biomass (SSB)",
  ylab = "SSB (kt)", type = "l"
)


# Now for some haddock
## Read catch at age
catch <- read.csv("data/haddock_catch.csv", header = TRUE, check.names = FALSE, row.names = 1)
Year <- as.numeric(row.names(catch))

## Run model
model <- vpa(catch, Mvec = 0.2, Fterm = 0.1, Fages = 3)

## View results
par(mfrow = c(2, 2))

## Fishing mortality
round(model$F, 3)

## Recruitment
barplot(model$N[, 1], ylab = "Recruitment at age 0", main = "Recruitment (N age 0)")

## Selectivity
round(model$F, 3)
plot(colMeans(model$F) / max(colMeans(model$F)),
  ylim = c(0, 1.05),
  type = "l", main = "Selectivity", xlab = "Age", ylab = "Mean F at age"
)

## Fbar
Fbar <- rowMeans(model$F[, paste(2:4)])
plot(Year, Fbar,
  ylim = c(0, max(Fbar)), main = "Mean F ages 2-4",
  ylab = "Mean F (ages 1-10)", type = "l"
)

## SSB

## Read weigths and maturity at age
wt <- read.csv("data/haddock_weights.csv", header = TRUE, check.names = FALSE, row.names = 1)
mat <- read.csv("data/haddock_maturity.csv", header = TRUE, check.names = FALSE, row.names = 1)

ssb <- rowSums(model$N * wt * mat) / 1000
plot(Year, ssb,
  ylim = c(0, max(ssb)), main = "Spawning stock biomass (SSB)",
  ylab = "SSB (kt)", type = "l"
)
