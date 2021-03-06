library(shiny)
library(MASS)

shinyUI(navbarPage("Gaussian Process Playground", theme = "spacelab.css",id='nav',
  h4("1. define distribution"),                 
  fluidRow(
    column(6,
           #hr(),
           #h4("mean function"),
           selectInput("mean", HTML("mean function: <br/>"),c("centered"=0,"constant"=1,"linear"=2,"quadratic"=3),selectize=FALSE)
     ),
    column(6,offset=6.1,withMathJax(),
           #hr(),
           #h4("covariance function (kernel)"),
           selectInput("kernel", "covariance function:",selected="kMat52",selectize=FALSE,
                       c("Exponential" = "kExp",
                         "Matern 3/2" = "kMat32",
                         "Matern 5/2" = "kMat52",
                         "Squared Exp" = "kGauss",
                         "Brownian" = "kBrown"))
    )
  ),
  
  
  fluidRow(
    column(6,withMathJax(),
           uiOutput('priormean')
    ),
    column(6,offset=6.1,withMathJax(),
           uiOutput('priorkernel')
    )
  ),
  
  fluidRow(
    column(6,withMathJax(),
           tags$head(
             #tags$style(type="text/css", "label, .selectize-control.single{ display: table-cell; text-align: left; vertical-align: center; } .form-group { display: table-row;}")
             tags$style(type="text/css", "label, .selectize.single{ display: table-cell; vertical-align: middle; } .form-group { display: table-row; }")
           ),
           column(4, conditionalPanel(condition = "input.mean > 0",numericInput("b0", "\\(\\beta_0\\,=\\,\\)",value = 0, step = 0.1))),
           column(4, conditionalPanel(condition = "input.mean > 1",numericInput("b1", "\\(\\beta_1\\,=\\,\\)",value = 0, step = 0.1))),
           column(4, conditionalPanel(condition = "input.mean > 2",numericInput("b2", "\\(\\beta_2\\,=\\,\\)",value = 0, step = 0.1)))
    ),
    column(6,offset=6.1,withMathJax(),
          column(4,offset=2, numericInput("sig2", "\\(\\sigma^2\\,=\\,\\)", min = 0, max = 10, value = 1, step = 0.1, width='100%')),
           column(4, conditionalPanel("input.kernel != 'kBrown'",
                                      numericInput("theta", "\\(\\theta\\,=\\,\\)", min = 0.01, max = 3, value = .2, step = 0.1, width='100%'))
           )
    )
  ),
  
  hr(),
  h4("2. plottings"),
  fluidRow(
    tabsetPanel(id="plots",
      tabPanel("moments", 
               fluidRow(
                 column(6,plotOutput("mean")),
                 column(6,plotOutput("kernel"))
               )),
      tabPanel("mean and confidence intervals",
               column(8,offset=1,plotOutput("model", click="plotModel_click")),
               column(2,br(),br(),br(),
                      p("Click on plot to"),
                      radioButtons("addPts2", label = "",selected = TRUE,
                                   choices = list("add points" = TRUE, "remove points" = FALSE)),
                      br(),p("nb grid points:"),
                      numericInput("n_grid2", "", min = 10, max = 500, value = 100, step = 100)
               )
      ),
      tabPanel("samples",
               column(8,offset=1,plotOutput("samples", click="plotSample_click")),
               column(2,br(),br(),br(),
                      p("Click on plot to"),
                      radioButtons("addPts", label = "",selected = TRUE,
                                     choices = list("add points" = TRUE, "remove points" = FALSE)
                                     ),
                      br(),p("nb grid points:"),
                      numericInput("n_grid", "", min = 10, max = 500, value = 100, step = 100),
                      br(),p("nb samples:"),
                      numericInput("n_sample", "", min = 1, max = 1000, value = 100, step = 10)
               )
      )
    )
  ),
  

  hr(),
  
#   fluidRow(
#     column(4,p("plotting parameters")),
#     column(4,numericInput("n_grid", "nb grid points", min = 10, max = 500, value = 101, step = 1)),
#     column(4,conditionalPanel(condition = "input.plots == 'samples'",numericInput("n_sample", "nb samples", min = 1, max = 1000, value = 100, step = 1)))
#   ),
  br(),br(),br()
  # verbatimTextOutput("info")

))