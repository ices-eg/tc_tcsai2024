# Fitting Models to Data in R

# Colin Millar

# This script shows the steps followed to fit a stock-recruitment model to
# in the file 'northern_shelf_haddock_SR.csv'

library(shiny)
library(ggplot2)
library(htmltools)
library(RTMB)

# ==============================================================================
# Load data
# ==============================================================================

# load data from comma-separated file to data.frame
haddock <- read.csv(file = "02_Model_fitting/northern_shelf_haddock_SR.csv", header = TRUE)
names(haddock) <- c("yc", "ssb", "rec")
#haddock$rec <- haddock$rec / 1000
#haddock$ssb <- haddock$ssb / 100

# useful functions
bevholt <- function(a, b, S) {
  a * S / (b + S)
}

ricker <- function(a, b, S) {
  a * S * exp(-b * S)
}

get_ssq <- function(loga, logb, srmodel) {
  nll <- function(loga, logb, logSigma) {
    a <- exp(loga)
    b <- exp(logb)

    -sum(dnorm(log(haddock$rec), log(srmodel(a, b, haddock$ssb)), exp(logSigma), TRUE))
  }

  ssq_func <- function(params) {
    loga <- params$loga
    logb <- params$logb
    logSigma <- params$logSigma

    nll(loga, logb, logSigma)
  }

  parameters <- list(loga = loga, logb = logb, logSigma = 0)
  obj <- MakeADFun(ssq_func, parameters)

  obj$hessian <- TRUE
  opt <- do.call("optim", obj)
  rep <- sdreport(obj)
  est <- summary(rep)

  # find a good range of loga and logb
  sda <- est["loga", "Std. Error"]
  sdb <- est["logb", "Std. Error"]

  if (!is.finite(sda)) sda <- 0.1
  if (!is.finite(sdb)) sdb <- 50
  logas <- seq(-1, 1, by = 0.01) * 2 * sda + est["loga", "Estimate"]
  logbs <- seq(-1, 1, by = 0.01) * 2 * sdb + est["logb", "Estimate"]

  # fill out ssq
  ssqs <- t(outer(logas, logbs, Vectorize(nll), logSigma = est["logSigma", "Estimate"]))
  ssqs[ssqs > min(ssqs) + 5] <- NA

  list(ssqs = ssqs, logas = logas, logbs = logbs, est = est)
}

model <- bevholt
ssq <- get_ssq(0, 10, model)
ssq$est

# shiny user interface
ui <- fluidPage(
  h1("Stock Recruitment fit to Northern Shelf Haddock"),
  textOutput("ab"),
  fluidRow(
    column(6, plotOutput("sr_plot")),
    column(6, plotOutput("ssq", click = "plot_click"))
  )
)

# the app logic
server <- function(input, output, session) {





  # process inputs
  fit <- reactive({
    S <- seq(0, max(haddock$ssb))
    a <- exp(input$plot_click$y)
    b <- exp(input$plot_click$x)
    data.frame(ssb = S, rec = model(a, b, S))
  })

  # make a plot
  output$sr_plot <- renderPlot({
    p <- ggplot(haddock, aes(ssb, rec)) +
      geom_point() +
      scale_x_continuous("Spawners (kt)", limits = c(0, max(haddock$ssb)), expand = expansion(mult = c(0, .04))) +
      scale_y_continuous("Recruits (millions age 0)", limits = c(0, max(haddock$rec)), expand = expansion(mult = c(0, .04))) +
      ggtitle("stock and recruitment") +
      theme_minimal()

    if (!is.null(input$plot_click)) {
      p + geom_line(data = fit(), col = "red", lwd = 1.5)
    } else {
      p
    }
  })

  output$ssq <- renderPlot({
    image(
      ssq$logbs, ssq$logas, ssq$ssqs,
      xlab = "log b", ylab = "log a", main = "SSQ surface",
      col = c(heat.colors(20, rev = TRUE))
    )
    points(ssq$est["logb", "Estimate"], ssq$est["loga", "Estimate"], pch = 16, col = "red")
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
