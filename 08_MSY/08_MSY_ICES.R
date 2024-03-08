install.packages("msy", repos = c("https://ices-tools-prod.r-universe.dev", "https://cloud.r-project.org"))

library(msy)

FIT <- eqsr_fit(icesStocks$saiNS,
  nsamp = 1000,
  models = c("Ricker", "Segreg")
)

SIM <- eqsim_run(FIT,
  bio.years = c(2004, 2013),
  sel.years = c(2004, 2013),
  Fcv = 0.24,
  Fphi = 0.42,
  Blim = 106000,
  Bpa = 200000,
  Fscan = seq(0, 1.2, len = 40),
  verbose = FALSE
)

eqsr_plot(FIT, n = 2e4)

SIM$Refs

eqsim_plot(SIM, catch = TRUE)

eqsim_plot_range(SIM, type = "mean")

eqsim_plot_range(SIM, type = "median")

eqsim_plot_range(SIM, type = "ssb")
