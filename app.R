## app.R ##
library(shinydashboard)

INTEGER_MAX <- 0x7fffffff

# helper function
roundS <- function(f, d) {
  signif(f, digits=d)
}

# models uis
poisson_tab <- function() {
  tabItem(tabName = "poisson",
          fluidRow(
            box(
              collapsible = TRUE,
              width=12,
              solidHeader = TRUE,
              status="primary",
              title = "Poisson-Gamma Model",
              box(width=6, 
                  status="info",
                  collapsible = TRUE, 
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
                  collapsible = TRUE, 
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
                          column(
                            width=12,
                            fileInput("poissonFile", "File"),
                            checkboxInput("poissonHeader", "Header", T),
                            uiOutput("poisson_data_field_ui")
                          )
                        )
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
            box(
              status = "info",
              collapsible = TRUE, 
              solidHeader = TRUE,
              title = "Prior Plot",
              plotOutput("poisson_plot"), width=12
              )
          )
  )
}

binomial_tab <- function() {
  tabItem(tabName = "binomial",
          fluidRow(
            box(
              collapsible = TRUE,
              width=12,
              status="primary",
              solidHeader = TRUE,
              title = "Binomial-Beta Model",
              box(width=6, 
                  status="info",
                  collapsible = TRUE, 
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
                  collapsible = TRUE, 
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
                          column(
                            width=12,
                            fileInput("binomialFile", "File"),
                            checkboxInput("binomialHeader", "Header", F),
                            textInput("binomial_standard", "TRUE Value", "1"),
                            uiOutput("binomial_data_field_ui")
                          )
                          )
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
            )),
            fluidRow(
              box(status = "info",
                  collapsible = TRUE, 
                  solidHeader = TRUE,
                  title = "Prior and Posterior Plot",
                  plotOutput("binomial_plot"), width=12)
            )
  )
}

normal_tab <- function() {
  tabItem(tabName = "normal",
          fluidRow(
            box(
              collapsible = TRUE,
              width=12,
              solidHeader = TRUE,
              status="primary",
              title = "Normal Model",
              box(width=6, 
                  status="info",
                  collapsible = TRUE, 
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
                  collapsible = TRUE, 
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
                          column(
                            width=12,
                            fileInput("normalFile", "File"),
                            checkboxInput("normalHeader", "Header", T),
                            uiOutput("normal_data_field_ui")
                          ))
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
            box(status = "info",
                collapsible = TRUE, 
                solidHeader = TRUE,
                title = "Prior and Posterior Plot",
                plotOutput("normal_plot"), width=12)
          )
  )
}

# app ui
ui <- function() {
  dashboardPage(
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
        menuItem("About", tabName = "about",icon = icon("question")),
        menuItem("Source code", icon = icon("file-code-o"), 
                 href = "https://github.com/GavinQ1/R-Shiny-Bayesian-Models")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "about",
                includeMarkdown("About.Rmd")
        ),
        poisson_tab(),
        binomial_tab(),
        normal_tab()
      )
    )
  )
}

# server 
server <- function(input, output) {
  
  output$poisson_data_field_ui <- renderUI({
    inFile <- input$poissonFile
    choices <- NULL
    if (!is.null(inFile)) {
      data <- read.csv(inFile$datapath, header=input$poissonHeader)
      choices <- colnames(data)
    }
    selectInput("poisson_data_field", "Field", choices)
  })
  
  output$poisson_plot <- renderPlot({
    thetas <- seq(0.1, 15, .1)
    a <- input$poisson_prior_a
    b <- input$poisson_prior_b
    flag <- input$poissonSource
    if (isTRUE(flag)) {
      inFile <- input$poissonFile
      N <- 0
      ySum <- 0
      if (!is.null(inFile)) {
        data <- read.csv(inFile$datapath, header=input$poissonHeader)
        field <- input$poisson_data_field
        N <- length(data[[field]])
        ySum <- sum(data[[field]])
      }
    } else {
      N <- input$poissonN
      ySum <- input$poissonYSum
    }
    
    pTheta <- dgamma(thetas, a + ySum, b + N)
    priorTheta <- dgamma(thetas, a, b)
    ylim <- c(0, max(priorTheta, pTheta))
    plot(thetas, pTheta, 
         ylab=expression(paste("p(", theta, "|", list(y[1],...,y[N]), ")")),
         xlab=expression(theta),
         main=paste(
           "Prior distribution with a =", a,
           ", b =", b, ", and Posterior distribution"
         ),
         col="blue",
         type="l"
    )
    lines(thetas, priorTheta, col="black")
    legend(
      "topright",
      legend=c("Prior", "Posterior"),
      col=c("black", "blue"),
      lty=1:1
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
        data <- read.csv(inFile$datapath, header=input$poissonHeader)
        field <- input$poisson_data_field
        N <- length(data[[field]])
        ySum <- sum(data[[field]])
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
      data <- read.csv(inFile$datapath, header=input$poissonHeader)
      field <- input$poisson_data_field
      N <- length(data[[field]])
    }
    valueBox(
      paste("N = "), paste(N), color="teal"
    )
  })
  
  output$poisson_data_YSUM <- renderValueBox({
    inFile <- input$poissonFile
    ySum <- 0
    if (!is.null(inFile)) {
      data <- read.csv(inFile$datapath, header=input$poissonHeader)
      field <- input$poisson_data_field
      ySum <- sum(data[[field]])
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
  
  output$binomial_data_field_ui <- renderUI({
    inFile <- input$binomialFile
    choices <- NULL
    if (!is.null(inFile)) {
      data <- read.csv(inFile$datapath, header=input$binomialHeader)
      choices <- colnames(data)
    }
    selectInput("binomial_data_field", "Field", choices)
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
        data <- read.csv(inFile$datapath, header=input$binomialHeader)
        field <- input$binomial_data_field
        N <- length(data[[field]])
        ySum <- sum(data[[field]] == input$binomial_standard)
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
      data <- read.csv(inFile$datapath, header=input$binomialHeader)
      field <- input$binomial_data_field
      N <- length(data[[field]])
    }
    valueBox(
      paste("N = "), paste(N), color="teal"
    )
  })
  
  output$binomial_data_Y <- renderValueBox({
    inFile <- input$binomialFile
    ySum <- 0
    if (!is.null(inFile)) {
      data <- read.csv(inFile$datapath, header=input$binomialHeader)
      field <- input$binomial_data_field
      ySum <- sum(data[[field]] == input$binomial_standard)
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
        data <- read.csv(inFile$datapath, header=input$binomialHeader)
        field <- input$binomial_data_field
        N <- length(data[[field]])
        ySum <- sum(data[[field]] == input$binomial_standard)
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
  
  output$normal_data_field_ui <- renderUI({
    inFile <- input$normalFile
    choices <- NULL
    if (!is.null(inFile)) {
      data <- read.csv(inFile$datapath, header=input$normalHeader)
      choices <- colnames(data)
    }
    selectInput("normal_data_field", "Field", choices)
  })
  
  output$normal_data_N <- renderValueBox({
    inFile <- input$normalFile
    N <- 0
    if (!is.null(inFile)) {
      data <- read.csv(inFile$datapath, header=input$normalHeader)
      field <- input$normal_data_field
      N <- length(data[[field]])
    }
    valueBox(
      paste("N = "), paste(N), color="teal"
    )
  })
  
  output$normal_data_Mu <- renderValueBox({
    inFile <- input$normalFile
    ySum <- 0
    if (!is.null(inFile)) {
      data <- read.csv(inFile$datapath, header=input$normalHeader)
      field <- input$normal_data_field
      ySum <- mean(data[[field]])
    }
    valueBox(
      paste("Y_Bar = "), paste(roundS(ySum,6)), color="teal"
    )
  })
  
  output$normal_data_Tau <- renderValueBox({
    inFile <- input$normalFile
    ySum <- 1
    if (!is.null(inFile)) {
      data <- read.csv(inFile$datapath, header=input$normalHeader)
      field <- input$normal_data_field
      ySum <- var(data[[field]])
    }
    valueBox(
      paste("Sigma2 = "), paste(signif(ySum, digits=5)), color="teal"
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
        data <- read.csv(inFile$datapath, header=input$normalHeader)
        field <- input$normal_data_field
        N <- length(data[[field]])
        mu <- mean(data[[field]])
        tau <- var(data[[field]])
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
        data <- read.csv(inFile$datapath, header=input$normalHeader)
        field <- input$normal_data_field
        N <- length(data[[field]])
        mu <- mean(data[[field]])
        tau <- var(data[[field]])
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


shinyAppDir(".")
shinyApp(ui(), server)