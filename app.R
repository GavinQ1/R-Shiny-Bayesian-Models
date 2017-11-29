## app.R ##
library(shinydashboard)
require(rjags)
require(coda)

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

relops <- c("==", "<", ">", "<=", ">=", "!=")

judge <- function(op, op1, op2) {
  if (op == "==") {
    op1 == op2
  } else if (op == "<") {
    op1 < op2
  } else if (op == ">") {
    op1 > op2
  } else if (op == "<=") {
    op1 <= op2
  } else if (op == ">=") {
    op1 >= op2
  } else {
    op1 != op2
  }
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
                            fluidRow(
                              column(width=6, selectInput("binomial_relop", "Relational Operation", relops)),
                              column(width=6, textInput("binomial_standard", "TRUE Value", "1"))
                            ),
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

hierachical_tab <- function() {
  tabItem(tabName = "hierachical",
          fluidRow(
            box(
              collapsible = TRUE,
              width=12,
              status="primary",
              solidHeader = TRUE,
              title = "Hierarchical Model",
              box(width=6, 
                  status="info",
                  collapsible = TRUE, 
                  title="JAGS Model Text",
                  solidHeader = TRUE,
                  column(
                    width=12,
                    textAreaInput("hierachicaL_text", "Model", height=450)
                  )
              ),
              box(width=6,
                  status="warning",
                  collapsible = TRUE, 
                  title="JAGS Controls",
                  solidHeader = TRUE,
                  box(width=12,
                      status="primary",
                      collapsible = TRUE,
                      title="Input Variables",
                      solidHeader = TRUE,
                      tags$div(id = 'input_placeholder'),
                      column(width=12, actionButton("hier_input_add", "Add", icon=icon("plus-square")))
                  ),
                  hr(),
                  box(width=12,
                      status="primary",
                      collapsible = TRUE,
                      title="Output Variables",
                      solidHeader = TRUE,
                      fluidRow(
                        column(width=6,
                               numericInput("hier_sim_n", "No. Simulations", min=0, value=10000)),
                        column(width=6,
                               numericInput("hier_sim_thin", "thin", min=0, value=1))
                      ),
                      tags$div(id = 'output_placeholder'),
                      actionButton("hier_output_add", "Add", icon=icon("plus-square"))
                  ),
                  hr(),
                  box(width=12,
                      div(actionButton("hier_clear", "Clear", icon=icon("close")),
                          style="float:left"),
                      div(actionButton("hier_generate", "Generate", icon=icon("check")),
                          style="float:right")
                  )
              )
            )
          ),
          fluidRow(
            box(status = "info",
                collapsible = TRUE, 
                solidHeader = TRUE,
                title = "Results",
                width=12,
                uiOutput("hier_plot_1"),
                uiOutput("hier_plots")
          ))
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
        menuItem("Normal", tabName = "normal"),
        menuItem("Hierachical", tabName="hierachical")
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
        normal_tab(),
        hierachical_tab()
      )
    )
  )
}


getInputUIName <- function(i) {
  paste0("input_ui_", i)
}

getInputVarName <- function(i) {
  paste0("input_var_", i)
}

getInputValName <- function(i) {
  paste0("input_val_", i)
}

getInputUseFileName <- function(i) {
  paste0("input_use_file_", i)
}

getInputFileName <- function(i) {
  paste0("input_file_", i)
}

getInputHeaderName <- function(i) {
  paste0("input_header_", i)
}

getInputDataFieldUI <- function(i) {
  paste0("input_data_field_ui", i)
}

getInputDataFieldOutUI <- function(i) {
  paste0("input_data_field_out_ui", i)
}

getOutputVarName <- function(i) {
  paste0("output_var_", i)
}

getOutputRemoveName <- function(i) {
  paste0("output_remove_", i)
}

getInputRemoveName <- function(i) {
  paste0("input_remove_", i)
}

getOutputUseIndexRange <- function(i) {
  paste0("output_use_range", i)
}

getOutputIndexRangeLower <- function(i) {
  paste0("output_range_lower", i)
}

getOutputIndexRangeUpper <- function(i) {
  paste0("output_range_upper", i)
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
        op <- input$binomial_relop
        ySum <- sum(judge(op, data[[field]], input$binomial_standard))
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
      op <- input$binomial_relop
      ySum <- sum(judge(op, data[[field]], input$binomial_standard))
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
        op <- input$binomial_relop
        ySum <- sum(judge(op, data[[field]], input$binomial_standard))
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
  
  obslist <- list()
  
  inputUIs <- list()
  outputUIs <- list()
  
  observeEvent(input$hier_input_add, {
    i <- inputUI_id
    inputUI_id <<- inputUI_id + 1
    id <- paste0("input_UI_", i)
    insertUI(
      selector = '#input_placeholder',
      ui = div(
        id=id,
        box(
          width=12,
          collapsible = TRUE,
          title=paste0("# ", i),
          textInput(getInputVarName(i), paste0("Input Name ", i), value=paste0("input_", i)),
          fluidRow(
            column(width=12,
                   fluidRow(
                     conditionalPanel(
                       paste0("input.", getInputUseFileName(i), " == false"),
                       column(width=6, textInput(getInputValName(i), "Value", value=0))
                       ),
                     column(width=6, checkboxInput(getInputUseFileName(i), "Use File"))
                   ),
                   conditionalPanel(
                     paste0("input.", getInputUseFileName(i), " == true"),
                     fluidRow(
                       column(width=12, checkboxInput(getInputHeaderName(i), "Header", value=T)),
                       column(width=6, fileInput(getInputFileName(i), "File")),
                       column(width=6, uiOutput(getInputDataFieldOutUI(i)))
                     )
                   ),
                   div(actionButton(getInputRemoveName(i), "Delete", icon=icon("close")),
                       style="float:right")
                   )
          )
        )
      )
    )
    
    renderDataField(i)
    
    obslist[[getInputRemoveName(i)]] <<- observeEvent(input[[getInputRemoveName(i)]], {
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', id)
      )
      inputUIs[[i]] <<- F
      obslist[[getInputRemoveName(i)]] <<- NULL
    }) 
    
    inputUIs[[i]] <<- T
  })
  
  renderDataField <- function(i) {
    output[[getInputDataFieldOutUI(i)]] <- renderUI({
      inFile <- input[[getInputFileName(i)]]
      choices <- NULL
      if (!is.null(inFile)) {
        data <- read.csv(inFile$datapath, header=input[[getInputHeaderName(i)]])
        choices <- colnames(data)
      }
      selectInput(getInputDataFieldUI(i), "Field", choices)
    })
  }
  
  outputUI_id <- 1
  inputUI_id <- 1
  
  observeEvent(input$hier_output_add, {
    i <- outputUI_id
    outputUI_id <<- outputUI_id + 1
    
    id <- paste0("output_UI_", i)
    insertUI(
      selector = '#output_placeholder',
      ui = div(
        id=id,
        box(
          width=12,
          collapsible = TRUE,
          title=paste0("# ", i),
          fluidRow(
            column(width=8, 
                   textInput(getOutputVarName(i), paste0("Output Name ", i), value=paste0("output_", i))),
            column(width=4,
                   checkboxInput(getOutputUseIndexRange(i), "Use Index Range", value=F))
          ),
          conditionalPanel(
            paste0("input.", getOutputUseIndexRange(i), " == true"),
            fluidRow(
              column(width=6,
                     numericInput(getOutputIndexRangeLower(i), "Lower Bound", min = 1, value = 2)),
              column(width=6,
                     numericInput(getOutputIndexRangeUpper(i), "Upper Bound",min = 1, value = 100))
            )
          ),
          div(actionButton(getOutputRemoveName(i), "Delete", icon=icon("close")),
              style="float:right")
        )
      )
    )
    
    obslist[[getOutputRemoveName(i)]] <<- observeEvent(input[[getOutputRemoveName(i)]], {
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', id)
      )
      outputUIs[[i]] <<- F
      obslist[[getOutputRemoveName(i)]] <<- NULL
    }) 
    
    outputUIs[[i]] <<- T
  })
  
  observeEvent(input$hier_clear, {
    input_num <- length(inputUIs)
    output_num <- length(outputUIs)
    for (i in 1:input_num) {
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', inputUIs[i])
      )
    }
    for (i in 1:output_num) {
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', outputUIs[i])
      )
    }
    
    inputUIs <<- c()
    outputUIs <<- c()
  })
  
  observeEvent(input$hier_generate, {
    inputNum <- length(inputUIs)
    outputNum <- length(outputUIs)
    
    inputNames <- c()
    inputValues <- c()
    input_value_pair <- list()
    for (i in 1:inputNum) {
      if (i > inputNum) break
      if (isTRUE(inputUIs[[i]])) {
        name <- input[[getInputVarName(i)]]
        inputNames <- c(inputNames, name)
        flag <- input[[getInputUseFileName(i)]]
        value <- 0
        if (isTRUE(flag)) {
          inFile <- input[[getInputFileName(i)]]
          if (!is.null(inFile)) {
            data <- read.csv(inFile$datapath, header=input[[getInputHeaderName(i)]])
            field <- input[[getInputDataFieldUI(i)]]
            value <- data[[field]]
          }
        } else {
          value <- input[[getInputValName(i)]]
        }
        inputValues <- c(inputValues, value)
        input_value_pair[[name]] <- value
      }
    }
    outputNames <- c()
    for (i in 1:outputNum) {
      if (i > outputNum) break
      if (isTRUE(outputUIs[[i]])) {
        outputNames <- c(outputNames, input[[getOutputVarName(i)]])
      }
    }
    
     model.text <- input$hierachicaL_text
    
     model.spec <- textConnection(model.text)
    
    # interactiveRender(outputNames)
    tryCatch({
      model.jags <- jags.model(model.spec, data=input_value_pair)
      
      size <- input$hier_sim_n
      thin <- input$hier_sim_thin
      # burn <- input$burn * size
      # thinEvery <- input$thin
      
      
      set.seed(1)
      samps.coda <- coda.samples(model.jags, outputNames, n.iter=size, thin=thin)
    }, error=function(e) {
      print(e)
    })
    
    interactiveRenderer(samps.coda)
  })
  
  interactiveRenderer <- function(coda) {
    names <- c()
    outputNum <- length(outputUIs)
    
    for (i in 1:outputNum) {
      if (i > outputNum) break
      if (isTRUE(outputUIs[[i]])) {
        if (!isTRUE(input[[getOutputUseIndexRange(i)]])) {
          names <- c(names, input[[getOutputVarName(i)]])
        }
      }
    }
    
    if (length(names) > 0) {
      output$hier_plot_1 <- renderUI({
        box(
          collapsible = TRUE,
          width=12,
          renderPlot({plot(coda[[1]][, names])})
        )
      })
    }
    
    
    output$hier_plots <- renderUI({
      plot_output_list <- lapply(1:outputNum, function(i) {
        plotname <- paste0("hier_plot_sub_", i)
        uiOutput(plotname)
      })
      do.call(tagList, plot_output_list)
    })
    
    for (i in 1:outputNum) {
      local({
        thisI <- i
        if (isTRUE(outputUIs[[thisI]])) {
          plotname <- paste0("hier_plot_sub_", thisI)
          if (isTRUE(input[[getOutputUseIndexRange(i)]])) {
            lower <- input[[getOutputIndexRangeLower(i)]]
            upper <- input[[getOutputIndexRangeUpper(i)]]
            output[[plotname]] <- renderUI({
              box(
                collapsible = TRUE,
                width=12,
                renderPlot({plot(density(coda[[1]][, lower : upper]),main=names[thisI])})
              )
            })
          } else {
            output[[plotname]] <- renderUI({
              box(
                collapsible = TRUE,
                width=12,
                renderPlot({plot(density(coda[[1]][, input[[getOutputVarName(i)]]]),main=names[thisI])})
              )
            })
          }
          
          
        }
      })
    }
  }
}

if (F) {
  box(
    status="warning",
    collapsible = TRUE, 
    solidHeader = TRUE,
    title = "Burning and Thining",
    width=12,
    fluidRow(
      column(
        width=6,
        numericInput("burn", "Burning Percentage", min=0, max=1, value=0.75)
      ),
      column(
        width=6,
        numericInput("thin", "Thinning Every", min=0, value=5)
      )
    )
  )
}

shinyAppDir(".")
shinyApp(ui(), server)




