

## Read catch at age
cod_catch <- read.csv(taf.data.path("cod_catch.csv"), header = TRUE, check.names = FALSE, row.names = 1)
cod_Year <- as.numeric(row.names(cod_catch))

## Read weigths and maturity at age
cod_wt <- read.csv(taf.data.path("cod_weights.csv"), header = TRUE, check.names = FALSE, row.names = 1)
cod_mat <- read.csv(taf.data.path("cod_maturity.csv"), header = TRUE, check.names = FALSE, row.names = 1)

## Read catch at age
haddock_catch <- read.csv(taf.data.path("haddock_catch.csv"), header = TRUE, check.names = FALSE, row.names = 1)
haddock_Year <- as.numeric(row.names(haddock_catch))

## Read weigths and maturity at age
haddock_wt <- read.csv(taf.data.path("haddock_weights.csv"), header = TRUE, check.names = FALSE, row.names = 1)
haddock_mat <- read.csv(taf.data.path("haddock_maturity.csv"), header = TRUE, check.names = FALSE, row.names = 1)
