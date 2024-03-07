library(shiny)
library(ggplot2)

## Functions

vpa <- function(C, Mvec, Fterm, Fages) {
  ## Prepare matrices
  C <- as.matrix(C)
  T <- nrow(C)
  A <- ncol(C)
  N <- F <- Z <- matrix(NA_real_, nrow = T, ncol = A, dimnames = dimnames(C))

  ## create M matrix
  M <- matrix(Mvec, nrow = T, ncol = A, byrow = TRUE, dimnames = dimnames(C))

  ## Set F in terminal year
  F[T, ] <- Fterm
  Z <- F + M

  ## Calculate N in terminal year
  N <- C * Z / (F * (1 - exp(-Z)))

  ## Calculate N and F up to terminal year,
  ## assuming F[oldest] = avg(preceding ages)
  for (t in (T - 1):1)
  {
    for (a in 1:(A - 1))
    {
      N[t, a] <- N[t + 1, a + 1] * exp(M[t, a]) + C[t, a] * exp(M[t, a] / 2)
      F[t, a] <- log(N[t, a] / N[t + 1, a + 1]) - M[t, a]
    }
    F[t, A] <- mean(F[t, A - (1:Fages)])
    Z[t, ] <- F[t, ] + M[t, ]
    N[t, A] <- C[t, A] * Z[t, A] / (F[t, A] * (1 - exp(-Z[t, A])))
  }

  list(N = N, F = F, Z = Z, C = C, M = M, Fterm = Fterm, Fages = Fages)
}

## source data (cod)

## Read catch at age
catch <- read.csv("06_VPA/cod_catch.csv", header = TRUE, check.names = FALSE, row.names = 1)
Year <- as.numeric(row.names(catch))

## Read weigths and maturity at age
wt <- read.csv("06_VPA/cod_weights.csv", header = TRUE, check.names = FALSE, row.names = 1)
mat <- read.csv("06_VPA/cod_maturity.csv", header = TRUE, check.names = FALSE, row.names = 1)

## empty plot data
df_old <- data.frame(year = numeric(0), age = numeric(0), val = numeric(0), what = character(0), keep = integer(0))

ui <- fluidPage(

  # Application title
  titlePanel("Simple VPA"),
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "Fages",
        "Number of ages to average over for oldest age F", value = 3
      ),
      sliderInput("Fterm", "F in final year", value = 0.1, min = 0, max = 2, step = 0.05),
      numericInput(
        inputId = "Xmin",
        "Year to start plotting from", value = min(Year), min = min(Year), max = max(Year) - 5
      ),
      checkboxInput(inputId = "keep", "show previous runs", value = TRUE),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = "LinePlot", height = "800px")
    )
  )
)

server <- function(input, output) {
  output$LinePlot <- renderPlot({

    # input variables
    Mvec <- c(rep(0.1, 2), rep(0.3, 8))

    # fit VPA
    model <- vpa(catch, Mvec = Mvec, Fterm = input$Fterm, Fages = input$Fages)

  # summarise output
    ssb <- rowSums(model$N * wt * mat) / 1000

    Fbar <- rowMeans(model$F)

    Sel <- colMeans(model$F) / max(colMeans(model$F))

    Rec <- model$N[, 1]

    Ns <- model$N

    df <- rbind(
      data.frame(year = Year, age = 0, val = ssb, what = "SSB"),
      data.frame(year = Year, age = 0, val = Fbar, what = "Fbar"),
      data.frame(year = Year, age = 0, val = Rec, what = "Recruitment"),
      data.frame(year = Year, age = rep(1:ncol(Ns), each = nrow(Ns)), val = log(c(Ns)), what = "log Numbers")
    )

  df$FinalF <- input$Fterm

  if (input$keep) {
    df <-
      rbind(
        df,
        df_old
      )
    if (!input$Fterm %in% df_old$FinalF) {
      df_old <<- rbind(df_old, df)
    }
  } else {
    df_old <<- df_old[numeric(0),]
  }

  df <- df[df$year >= input$Xmin,]

    ggplot(data = df, aes(year, val, colour = FinalF, linetype = factor(age) , group = interaction(FinalF, age))) +
      geom_line() +
      labs(x = "Year") +
      facet_wrap(~what, scales = "free", ncol = 2) +
      expand_limits(y = 0)
  })
}

shinyApp(ui = ui, server = server)
