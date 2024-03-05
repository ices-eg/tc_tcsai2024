# Fitting Models to Data in R

# Colin Millar

# This script shows the steps followed to fit a stock-recruitment model to
# in the file 'northern_shelf_haddock_SR.csv'

library(shiny)
library(ggplot2)
library(htmltools)
library(RTMB)

# load data from comma-separated file to data.frame
haddock <- read.csv(file = "02_Model_fitting/northern_shelf_haddock_SR.csv", header = TRUE)
cod <- read.csv(file = "02_Model_fitting/north_sea_cod_SR.csv", header = TRUE)
herring <- read.csv(file = "02_Model_fitting/north_sea_herring_SR.csv", header = TRUE)
names(herring) <- names(cod) <- names(haddock) <- c("yc", "S", "R")

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

# shiny user interface
ui <- fluidPage(
  h1("Stock Recruitment fits for ICES TCISA2024"),
  fluidRow(
    column(3, selectInput("data", "Select dataset", c("haddock", "cod", "herring"))),
    column(3, selectInput("model", "Select model", c("ricker", "bevholt", "bevholt2"))),
    column(3, textOutput("equation")),
    column(3, textOutput("ab"))
  ),
  fluidRow(
    column(6, plotOutput("sr_plot")),
    column(6, plotOutput("ssq", click = "plot_click"))
  )
)

# the app logic
server <- function(input, output, session) {
  # assign model
  model <- reactive({
    switch(input$model,
      ricker = ricker,
      bevholt = bevholt,
      bevholt2 = bevholt2
    )
  })

  output$equation <- renderText({
    as.character(body(model())[2])
  })

  # assign dataset
  data <- reactive({
    switch(input$data,
      haddock = haddock,
      cod = cod,
      herring = herring
    )
  })

  # calculate nll surface
  ssq <- reactive({
    data_ <- data()
    model_ <- model()

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
  })

  best_fit <- reactive({
    # S > 0
    S <- seq(1e-9, max(data()$S), length = 100)
    a <- exp(ssq()$est["loga", "Estimate"])
    b <- exp(ssq()$est["logb", "Estimate"])
    data.frame(S = S, R = exp(model()(a, b, S)))
  })

  # process inputs
  fit <- reactive({
    # S > 0
    S <- seq(1e-9, max(data()$S), length = 100)
    a <- exp(input$plot_click$y)
    b <- exp(input$plot_click$x)
    out <- data.frame(S = S, R = exp(model()(a, b, S)))
    out[out$R < max(data()$R), ]
  })

  # make a plot
  output$sr_plot <- renderPlot({
    data_ <- data()
    p <- ggplot(data_, aes(S, R)) +
      geom_point() +
      geom_line(data = best_fit(), col = "blue") +
      scale_x_continuous("Spawners (kt)", limits = c(0, max(data_$S)), expand = expansion(mult = c(0, .04))) +
      scale_y_continuous("Recruits (millions age 0)", limits = c(0, max(data_$R)), expand = expansion(mult = c(0, .04))) +
      ggtitle("stock and recruitment") +
      theme_minimal()

    if (!is.null(input$plot_click) && nrow(fit()) > 4) {
      p +
        geom_line(data = fit(), col = "red", lwd = 1.5)
    } else {
      p
    }
  })

  output$ssq <- renderPlot({
    image(
      ssq()$logbs, ssq()$logas, ssq()$ssqs,
      xlab = "log b", ylab = "log a", main = "SSQ surface",
      col = c(heat.colors(20, rev = TRUE))
    )
    points(ssq()$est["logb", "Estimate"], ssq()$est["loga", "Estimate"], pch = 16, col = "blue")
  })

  output$ab <- renderText({
    if (!is.null(input$plot_click)) {
      paste0("Selected\n a:", round(exp(input$plot_click$y), 3), "  b:", round(exp(input$plot_click$x), 3))
    } else {
      "Click SSQ plot to select a value for a and b"
    }
  })
}

# combine into one object and launch
app <- shinyApp(ui, server)
runApp(app, launch.browser = FALSE)
