library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Catapult simulator"),
  
  sidebarPanel(
    sliderInput("Fact1", 
                "x1: rotation axis", 
                min = 0,
                max = 1, 
                value = 0.5,
                step = 0.01),
    sliderInput("Fact2", 
                "x2: arm stop", 
                min = 0,
                max = 1, 
                value = .5,
                step = 0.01),
    sliderInput("Fact3", 
                  "x3: spring binding 1", 
                  min = 0,
                  max = 1, 
                  value = .5,
                  step = 0.01),
      sliderInput("Fact4", 
                  "x4: spring binding 2", 
                  min = 0,
                  max = 1, 
                  value = .5,
                  step = 0.01)
    ),
  
  mainPanel(
    tabsetPanel(type = "tabs",  
                tabPanel("Settings", plotOutput("catapultPlot")),
                tabPanel("Trajectory", plotOutput("trajectoryPlot"))
    )#,textOutput("text1")
  )
))
