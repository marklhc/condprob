#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Conditional Probability"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("x",
                     "x-value",
                     min = 1,
                     max = 61,
                     value = 1), 
         sliderInput("y",
                     "y-value",
                     min = 1,
                     max = 87,
                     value = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("surfPlot"),
         plotlyOutput("condPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$surfPlot <- renderPlotly({
     plot_ly(z = ~volcano) %>% add_surface()
   })
   output$condPlot <- renderPlotly({
     p1 <- plot_ly() %>% add_lines(x = seq_along(volcano[ , input$x]), 
                                   y = volcano[ , input$x]) %>%
       layout(xaxis = list(title = paste0("P(Y | X = ", input$x, ")")))
     p2 <- plot_ly() %>% add_lines(x = seq_along(volcano[input$y , ]), 
                                   y = volcano[input$y , ]) %>%
       layout(xaxis = list(title = paste0("P(X | Y = ", input$y, ")")))
     subplot(p1, p2, titleX = TRUE)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

