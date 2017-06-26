library(shiny)
source('catapultSettings.R')
source('catapultFunctions.R')

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  # plot catapult settings
  output$catapultPlot <- renderPlot({
    # source('catapultSettings.R')
    # source('catapultFunctions.R')
    Fact <- c(input$Fact1,input$Fact2,input$Fact3,input$Fact4)
    cat <- computeCatapult(Fact,0)
    par(mar=c(0,0,0,0),mfrow=c(1,2))
    plot(c(-25,5),rep(-widthBall,2),ylim=c(-5,20),type='l',asp=1,ylab='',xlab="",axes=FALSE)
    plotCatapult(cat,lightBlue,darkBlue)
    thetaMax <- maxAngle(Fact)
    cat <- computeCatapult(Fact,thetaMax)
    plot(c(-25,5),rep(-widthBall,2),ylim=c(-5,20),type='l',asp=1,ylab='',xlab="",axes=FALSE)
    plotCatapult(cat,lightBlue,darkBlue)
    par(mfrow=c(1,1))
  })
  
  # Action
  observeEvent(input$action, {
    output$trajectoryPlot <- renderPlot({
      Fact <- c(input$Fact1,input$Fact2,input$Fact3,input$Fact4)
      runExperiment(Fact,input$wind,plot='new')
    })
  })
  
  output$trajectoryPlot <- renderPlot({
    Fact <- c(input$Fact1,input$Fact2,input$Fact3,input$Fact4)
    runExperiment(Fact,input$wind,plot='new')
  })
  
  output$text1 <- renderText({ 
    c(input$Fact1,input$Fact2,input$Fact3,input$Fact4,input$wind)
  })
})