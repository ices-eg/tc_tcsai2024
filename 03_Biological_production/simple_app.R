library(shiny)
library(ggplot2)

## Functions

AgeModel <- function(Ninit, M, Fmort, mat, w, Amax, Tmax, alpha, beta, v)
{
  N <- matrix(nrow=Amax, ncol=Tmax)
  SSB <- numeric(Tmax)

  ## Year 1
  N[1,1] <- Ninit
  for (a in 1:(Amax - 1)) {
    N[a + 1, 1] <- N[a, 1] * exp(-M[a] - Fmort[a])
  }
  SSB[1] <- sum(N[,1] * mat * w)

  ## Later years
  for(t in 1:(Tmax-1))
  {
    N[1,t+1] <- (alpha*SSB[t]) / (beta+SSB[t]) + v*runif(1,-0.5,0.5)
    for (a in 1:(Amax - 1)) {
      N[a + 1, t + 1] <- N[a, t] * exp(-M[a] - Fmort[a])
    }
    SSB[t+1] <- sum(N[,t+1] * mat * w)
  }

  list(alpha=alpha, beta=beta, N=N, SSB=SSB, Rec=N[1,])
}


ui <- fluidPage(

  # Application title
  titlePanel("Biological production"),
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "Amax",
        "Maximum age", value = 15
      ),
      numericInput(
        inputId = "Ninit",
        "Initial population size", value = 1000
      ),
      sliderInput("Mage", "M at age", value = 0.2, min = 0, max = 2, step = 0.05),
      sliderInput("Fage", "F at age", value = 0.3, min = 0, max = 2, step = 0.05),
      sliderInput("alpha", "SR alpha", value = 1000, min = 0, max = 2000),
      sliderInput("beta", "SR beta", value = 1000, min = 0, max = 2000),
      sliderInput("v", "Recruitment error", value = 0, min = 0, max = 1000, step = 1),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = "LinePlot")
    )
  )
)

server <- function(input, output) {
  output$LinePlot <- renderPlot({
    ageAtF <- 3
    Tmax <- 25
    M <- rep(input$Mage, input$Amax)
    Fmort <- c(rep(0, ageAtF - 1), rep(input$Fage, input$Amax - ageAtF + 1))
    mat <- c(0, 0, rep(1, input$Amax - 2))

    w <- 10 * (1 - exp(-0.5 * ((1:input$Amax) + 0.1)))^3

    pop <- AgeModel(input$Ninit, M, Fmort, mat, w, input$Amax, Tmax, input$alpha, input$beta, v = input$v)

    df <- rbind(
      data.frame(year = 1:length(pop$SSB), val = pop$SSB, what = "SSB"),
      data.frame(year = 1:length(pop$SSB), val = pop$Rec, what = "Recruitment")
    )

    ggplot(data = df, aes(year, val)) +
      geom_line() +
      labs(x = "Year") +
      facet_wrap(~what,scales="free") +
      expand_limits(y = 0)
  })
}

shinyApp(ui = ui, server = server)
