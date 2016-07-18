#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2] 
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    # # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
      df_all2 <- read.csv("apr_weekend.csv") 
      
      library(ggplot2)
      g <- ggplot(data=df_all2,aes(x=hour,y=x,color=factor(year)))
      g <- g + geom_line() + geom_point()
      g <- g + labs(x="hour",y="count")
      g <- g + labs(title="April weekday hourly Uber taxi usage in NYC")
      g <- g + theme(legend.title=element_blank())
      plot(g)
    
  })
  
})
