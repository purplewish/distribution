#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Distributions"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("dist",label=h4("Distribution"),
                    choices=c("Binomial" = "binom", 
                              "Geometric" = "geom",
                              "Negative Binomial" = "nbinom"),
                    selected="binom"),
        
        conditionalPanel(condition = "input.dist=='binom'",
                         numericInput("p1.binom","Number of trials: n",1,min=1)),
        conditionalPanel(condition = "input.dist=='binom'",
                         numericInput("p2.binom","Prob of success: p",0.5,min=0,max=1)),
        
        conditionalPanel(condition = "input.dist=='geom'",
                         numericInput("p1.geom","Prob of success: p",0.5,min=0,max=1)),
        conditionalPanel(condition = "input.dist=='geom'",
                         numericInput("p2.geom","maximum shown",10,min=10)),
        
        conditionalPanel(condition = "input.dist=='nbinom'",
                         numericInput("p1.nbinom","number of successes: r",1,min=1)),
        conditionalPanel(condition = "input.dist=='nbinom'",
                         numericInput("p2.nbinom","Prob of success: p",0.5,min=0,max=1)),
        conditionalPanel(condition = "input.dist=='nbinom'",
                         numericInput("p3.nbinom","maximum shown", 10,min=10))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  library(ggplot2)
  output$distPlot <- renderPlot({

    if(input$dist == "binom")
    {
      g1 = qplot(as.factor(0:(input$p1.binom)), dbinom(0:(input$p1.binom),input$p1.binom,input$p2.binom), geom = "col",xlab = "x",ylab="p(x)")
    }
    
    if(input$dist == "geom")
    {
      g1 = qplot(as.factor(1:(input$p2.geom)), dgeom(0:(input$p2.geom-1),prob = input$p1.geom), geom = "col",xlab = "x",ylab="p(x)")
    }
    
    if(input$dist == "nbinom")
    {
      g1 = qplot(as.factor((input$p1.nbinom):(input$p3.nbinom)), dnbinom(0:(input$p3.nbinom - input$p1.nbinom) ,size = input$p1.nbinom ,prob = input$p2.nbinom), geom = "col",xlab = "x",ylab="p(x)")
    }
    
    print(g1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

