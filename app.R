## app.R ##
library(shinydashboard)

INTEGER_MAX <- 0x7fffffff

roundS <- function(f, d) {
  signif(f, digits=d)
}

ui <- dashboardPage(
  dashboardHeader(title = "Bayesian Models"),
  dashboardSidebar(
    hr(),
    sidebarMenu(
      menuItem("Poisson-Gamma", tabName = "poisson"),
      menuItem("Binomial-Beta", tabName = "binomial"),
      menuItem("Normal", tabName = "normal")
    ),
    hr(),
    sidebarMenu(
      menuItem("About", tabName = "about",icon = icon("question"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              includeMarkdown("About.Rmd")
      ),
      tabItem(tabName = "poisson",
              fluidRow(
                box(
                  collapsible = TRUE,
                  width=12,
                  title = "Poisson-Gamma Model",
                  box(width=6, 
                      status="info",
                      title="Prior Belief",
                      solidHeader = TRUE,
                      br(),
                      fluidRow(
                        box(status = "primary",numericInput("poisson_prior_a", "a", min=1, max=INTEGER_MAX, value=2)),
                        box(status = "primary",numericInput("poisson_prior_b", "b", min=1, max=INTEGER_MAX, value=1)),
                        valueBoxOutput("poisson_prior_dist")
                      )),   
                  box(width=6, 
                      status = "warning", 
                      solidHeader = TRUE,
                      title = "Data",
                      fluidRow(
                        hr(),
                        column(
                          width=12, 
                          box(
                            width=8,
                            status = "info",
                            checkboxInput("poissonSource", "Use Data From File", F, width="100%"),
                            conditionalPanel(
                              "input.poissonSource == true",
                              fileInput("poissonFile", "File"))
                            )
                          ),
                        hr(),
                        conditionalPanel(
                          "input.poissonSource == false",
                          box(status = "primary",numericInput("poissonN", "N", min=0, max=INTEGER_MAX, value=44)),
                          box(status = "primary",numericInput("poissonYSum", "Y sum", min=0, max=INTEGER_MAX, value=66))
                        ),
                        conditionalPanel(
                          "input.poissonSource == true",
                          column(width=12, fluidRow(
                            valueBoxOutput("poisson_data_N"),
                            valueBoxOutput("poisson_data_YSUM")
                          ))
                        ),
                        valueBoxOutput("poisson_post_dist")
                      ))              
                )
              ),
              fluidRow(
                box(plotOutput("poisson_prior"), width=12),
                box(plotOutput("poisson_posterior"), width=12)
              )
      ),
      tabItem(tabName = "binomial",
              fluidRow(
                box(
                  collapsible = TRUE,
                  width=12,
                  title = "Binomial-Beta Model",
                  box(width=6, 
                      status="info",
                      title="Prior Belief",
                      solidHeader = TRUE,
                      br(),
                      fluidRow(
                        box(status = "primary",numericInput("binomial_prior_a", "a", min=1, max=INTEGER_MAX, value=3)),
                        box(status = "primary",numericInput("binomial_prior_b", "b", min=1, max=INTEGER_MAX, value=2)),
                        valueBoxOutput("beta_prior_dist")
                      )),                  
                  box(width=6, 
                      status = "warning", 
                      solidHeader = TRUE,
                      title = "Data", 
                      fluidRow(
                        hr(),
                        column(
                          width=12, 
                          box(
                            width=8,
                            status = "info",
                            checkboxInput("binomialSource", "Use Data From File", F, width="100%"),
                            conditionalPanel(
                              "input.binomialSource == true",
                              fileInput("binomialFile", "File"))
                          )
                        ),
                        hr(),
                        conditionalPanel(
                          "input.binomialSource == false",
                          box(status = "primary",numericInput("binomialN", "N", min=0, max=INTEGER_MAX, value=100)),
                          box(status = "primary",numericInput("binomialY", "Success", min=0, max=INTEGER_MAX, value=20))
                        ),
                        conditionalPanel(
                          "input.binomialSource == true",
                          column(width=12, fluidRow(
                            valueBoxOutput("binomial_data_N"),
                            valueBoxOutput("binomial_data_Y")
                          ))
                        ),
                        valueBoxOutput("binomial_post_dist")
                      ))                  
                ),
                fluidRow(
                  box(plotOutput("binomial_plot"), width=12)
                )
              )
      ),
      tabItem(tabName = "normal",
              fluidRow(
                box(
                  collapsible = TRUE,
                  width=12,
                  title = "Normal Model",
                  box(width=6, 
                      status="info",
                      title="Prior Belief",
                      solidHeader = TRUE,
                      br(),
                      fluidRow(
                        box(status = "primary",numericInput("normal_prior_mu", "mu0", min=1, max=INTEGER_MAX, value=1.9)),
                        box(status = "primary",numericInput("normal_prior_tau", "tau20", min=1, max=INTEGER_MAX, value=0.9025)),
                        valueBoxOutput("normal_prior_dist")
                      )),                  
                  box(width=6, 
                      status = "warning", 
                      solidHeader = TRUE,
                      title = "Data", 
                      fluidRow(
                        hr(),
                        column(
                          width=12, 
                          box(
                            width=8,
                            status = "info",
                            checkboxInput("normalSource", "Use Data From File", F, width="100%"),
                            conditionalPanel(
                              "input.normalSource == true",
                              fileInput("normalFile", "File"))
                          )
                        ),
                        hr(),
                        conditionalPanel(
                          "input.normalSource == false",
                          box(status = "primary",numericInput("normalN", "N", min=0, max=INTEGER_MAX, value=9)),
                          box(status = "primary",numericInput("normalMu", "mu", min=0, max=INTEGER_MAX, value=1.804)),
                          box(status = "primary",numericInput("normalTau", "sigma2", min=0, max=INTEGER_MAX, value=0.017))
                        ),
                        conditionalPanel(
                          "input.normalSource == true",
                          column(width=12, fluidRow(
                            valueBoxOutput("normal_data_N"),
                            valueBoxOutput("normal_data_Mu"),
                            valueBoxOutput("normal_data_Tau")
                          ))
                        ),
                        valueBoxOutput("normal_post_dist")
                      ))
                  )),
              fluidRow(
                box(plotOutput("normal_plot"), width=12)
              )
      )
    )
  )
)

server <- function(input, output) {
  output$poisson_prior <- renderPlot({
    thetas <- seq(0.1, 15, .1)
    a <- input$poisson_prior_a
    b <- input$poisson_prior_b
    pTheta <- dgamma(thetas, a, b)
    plot(thetas, pTheta, 
         ylab=expression(paste("p(", theta, ")")),
         xlab=expression(theta),
         main=paste(
           "Prior distribution with a =", a,
           ", b =", b
         ),
         type="l"
    )
  })
  
  output$poisson_posterior <- renderPlot({
    thetas <- seq(0.1, 15, .1)
    a <- input$poisson_prior_a
    b <- input$poisson_prior_b
    flag <- input$poissonSource
    if (isTRUE(flag)) {
      inFile <- input$poissonFile
      N <- 0
      ySum <- 0
      if (!is.null(inFile)) {
        data <- read.csv(inFile$datapath)
        print(1)
        N <- length(data[[1]])
        ySum <- sum(data[[1]])
      }
    } else {
      N <- input$poissonN
      ySum <- input$poissonYSum
    }
    
    pTheta <- dgamma(thetas, a + ySum, b + N)
    plot(thetas, pTheta, 
         ylab=expression(paste("p(", theta, "|", list(y[1],...,y[N]), ")")),
         xlab=expression(theta),
         main=expression(paste("Posterior distribution of ", theta)),
         col="blue",
         type="l"
    )
  })
  
  output$poisson_prior_dist <- renderValueBox({
    valueBox(
      paste0("~"), paste("dgamma(", input$poisson_prior_a, ", ", input$poisson_prior_b, ")"), color="olive"
    )
  })
  
  output$poisson_post_dist <- renderValueBox({
    a <- input$poisson_prior_a
    b <- input$poisson_prior_b
    flag <- input$poissonSource
    if (isTRUE(flag)) {
      inFile <- input$poissonFile
      N <- 0
      ySum <- 0
      if (!is.null(inFile)) {
        data <- read.csv(inFile$datapath)
        N <- length(data[[1]])
        ySum <- sum(data[[1]])
      }
    } else {
      N <- input$poissonN
      ySum <- input$poissonYSum
    }
    valueBox(
      paste0("~"), paste("dgamma(", a, " + ", ySum, ", ", b, " + ", N, ")"), color="light-blue"
    )
  })
  
  output$poisson_data_N <- renderValueBox({
    inFile <- input$poissonFile
    N <- 0
    if (!is.null(inFile)) {
      data <- read.csv(inFile$datapath)
      N <- length(data[[1]])
    }
    valueBox(
      paste("N = "), paste(N), color="teal"
    )
  })
  
  output$poisson_data_YSUM <- renderValueBox({
    inFile <- input$poissonFile
    ySum <- 0
    if (!is.null(inFile)) {
      data <- read.csv(inFile$datapath)
      ySum <- sum(data[[1]])
    }
    valueBox(
      paste("Y Sum = "), paste(ySum), color="teal"
    )
  })
  
  output$beta_prior_dist <- renderValueBox({
    valueBox(
      paste0("~"), paste("dbeta(", input$binomial_prior_a, ", ", input$binomial_prior_b, ")"), color="olive"
    )
  })
  
  output$binomial_post_dist <- renderValueBox({
    a <- input$binomial_prior_a
    b <- input$binomial_prior_b
    flag <- input$binomialSource
    if (isTRUE(flag)) {
      inFile <- input$binomialFile
      N <- 0
      ySum <- 0
      if (!is.null(inFile)) {
        data <- read.csv(inFile$datapath)
        N <- length(data[[1]])
        ySum <- sum(data[[1]])
      }
    } else {
      N <- input$binomialN
      ySum <- input$binomialY
    }
    valueBox(
      paste0("~"), paste("dbeta(", a, " + ", ySum, ", ", b, " + ", N, "-", ySum, ")"), color="light-blue"
    )
  })
  
  output$binomial_data_N <- renderValueBox({
    inFile <- input$binomialFile
    N <- 0
    if (!is.null(inFile)) {
      data <- read.csv(inFile$datapath)
      N <- length(data[[1]])
    }
    valueBox(
      paste("N = "), paste(N), color="teal"
    )
  })
  
  output$binomial_data_Y <- renderValueBox({
    inFile <- input$binomialFile
    ySum <- 0
    if (!is.null(inFile)) {
      data <- read.csv(inFile$datapath)
      ySum <- sum(data[[1]])
    }
    valueBox(
      paste("Y = "), paste(ySum), color="teal"
    )
  })
  
  output$binomial_plot <- renderPlot({
    thetas <- seq(0, 1, .01)
    a <- input$binomial_prior_a
    b <- input$binomial_prior_b
    
    flag <- input$binomialSource
    if (isTRUE(flag)) {
      inFile <- input$binomialFile
      N <- 0
      ySum <- 0
      if (!is.null(inFile)) {
        data <- read.csv(inFile$datapath)
        print(1)
        N <- length(data[[1]])
        ySum <- sum(data[[1]])
      }
    } else {
      N <- input$binomialN
      ySum <- input$binomialY
    }
    
    pTheta <- dbeta(thetas, a, b)
    postTheta <- dbeta(thetas, a + ySum, b + N - ySum)
    ylim <- c(0, max(postTheta, pTheta))
    plot(thetas, pTheta, 
         ylab=expression(paste("p(", theta, "|", list(y[1],...,y[N]), ")")),
         xlab=expression(theta),
         ylim=ylim,
         main=paste(
           "beta(", a,
           ",", b, "), N =", N, ", Succ No. =", ySum
         ),
         type="l"
    )
    lines(thetas, postTheta, col="blue")
    legend(
      "topright",
      legend=c("Prior", "Posterior"),
      col=c("black", "blue"),
      lty=1:1
    )
  })
  
  output$normal_prior_dist <- renderValueBox({
    valueBox(
      paste0("~"), paste("dnorm(", input$normal_prior_mu, ", ", signif(sqrt(input$normal_prior_tau), digits=5), ")"), color="olive"
    )
  })
  
  output$normal_data_N <- renderValueBox({
    inFile <- input$normalFile
    N <- 0
    if (!is.null(inFile)) {
      data <- read.csv(inFile$datapath)
      N <- length(data[[1]])
    }
    valueBox(
      paste("N = "), paste(N), color="teal"
    )
  })
  
  output$normal_data_Mu <- renderValueBox({
    inFile <- input$normalFile
    ySum <- 0
    if (!is.null(inFile)) {
      data <- read.csv(inFile$datapath)
      ySum <- mean(data[[1]])
    }
    valueBox(
      paste("Y_Bar = "), paste(roundS(ySum,6)), color="teal"
    )
  })
  
  output$normal_data_Tau <- renderValueBox({
    inFile <- input$normalFile
    ySum <- 1
    if (!is.null(inFile)) {
      data <- read.csv(inFile$datapath)
      ySum <- var(data[[1]])
    }
    valueBox(
      paste("Sigama2 = "), paste(signif(ySum, digits=5)), color="teal"
    )
  })
  
  output$normal_post_dist <- renderValueBox({
    mu0 <- input$normal_prior_mu
    tau0 <- input$normal_prior_tau
    flag <- input$normalSource
    if (isTRUE(flag)) {
      inFile <- input$normalFile
      N <- 0
      mu <- 0
      tau <- 1
      if (!is.null(inFile)) {
        data <- read.csv(inFile$datapath)
        N <- length(data[[1]])
        mu <- mean(data[[1]])
        tau <- var(data[[1]])
      }
    } else {
      N <- input$normalN
      mu <- input$normalMu
      tau <- input$normalTau
    }
    mun <- signif(((1/tau0) * mu0 + (N/tau) * mu) / (1/tau0 + N/tau), digits=6)
    taun <- signif(1 / (1/tau0 + N/tau), digits=6)
    valueBox(
      paste0("~"), paste("dnorm(", mun, ",", taun, ")"), color="light-blue"
    )
  })
  
  output$normal_plot <- renderPlot({
    mu0 <- input$normal_prior_mu
    tau0 <- input$normal_prior_tau
    flag <- input$normalSource
    if (isTRUE(flag)) {
      inFile <- input$normalFile
      N <- 0
      mu <- 0
      tau <- 1
      if (!is.null(inFile)) {
        data <- read.csv(inFile$datapath)
        N <- length(data[[1]])
        mu <- mean(data[[1]])
        tau <- var(data[[1]])
      }
    } else {
      N <- input$normalN
      mu <- input$normalMu
      tau <- input$normalTau
    }
    mun <- signif(((1/tau0) * mu0 + (N/tau) * mu) / (1/tau0 + N/tau), digits=6)
    taun <- signif(1 / (1/tau0 + N/tau), digits=6)
    
    thetas <- seq(0, 5, .02)
    pTheta <- dnorm(thetas, mu0, sqrt(tau0))
    postTheta <- dnorm(thetas, mun, sqrt(taun))
    ylim <- c(0, max(postTheta, pTheta))
    plot(thetas, pTheta, 
         ylab=expression(paste("p(", theta, "|", list(y[1],...,y[N]), ",", sigma^2, ")")),
         xlab=expression(theta),
         ylim=ylim,
         main=paste("Normal Distribution"),
         type="l"
    )
    lines(thetas, postTheta, col="blue")
    legend(
      "topright",
      legend=c("Prior", "Posterior"),
      col=c("black", "blue"),
      lty=1:1
    )
  })
}

shinyApp(ui, server)