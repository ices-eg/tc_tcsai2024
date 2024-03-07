## Before:
## After:

library(icesTAF)
mkdir("data")

source("utilities_models.R")
source("utilities_ssq.R")

# load data from comma-separated file to data.frame
haddock <- read.csv(file = taf.data.path("northern_shelf_haddock_SR.csv"), header = TRUE)
cod <- read.csv(file = taf.data.path("north_sea_cod_SR.csv"), header = TRUE)
herring <- read.csv(file = taf.data.path("north_sea_herring_SR.csv"), header = TRUE)
names(herring) <- names(cod) <- names(haddock) <- c("yc", "S", "R")

# some model fits
ssqs <-
  sapply(
    c("haddock", "cod", "herring"),
    function(y) {
      sapply(
        c("ricker", "bevholt", "bevholt2"),
        function(x) ssq(get(y), get(x)),
        simplify = FALSE
      )
    },
    simplify = FALSE
  )
