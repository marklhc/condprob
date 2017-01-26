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
library(MASS)
post <- readRDS("schools_post.RDS")
mu <- post[ , "mu"]; tau <- post[ , "tau"]
kd <- kde2d(mu, tau, n = 50, h = c(width.SJ(mu), width.SJ(tau)))
p <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()

kd2 <- kde2d(mu, tau, n = c(77, 73), 
             h = c(width.SJ(mu), width.SJ(tau)), 
             lims = c(round(range(mu), 0), round(range(tau), 0)))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Conditional Probability"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("x",
                     "x-value",
                     min = round(min(mu), 0),
                     max = round(max(mu), 0),
                     value = 0), 
         sliderInput("y",
                     "y-value",
                     min = round(min(tau), 0),
                     max = round(max(tau), 0),
                     value = 0)
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
     p
   })
   output$condPlot <- renderPlotly({
     p1 <- plot_ly() %>% add_lines(x = kd2$y,
                                   y = kd2$z[which(kd2$x == input$x), ]) %>%
       layout(xaxis = list(title = paste0("P(Y | X = ", input$x, ")")))
     p2 <- plot_ly() %>% add_lines(x = kd2$x,
                                   y = kd2$z[ , which(kd2$y == input$y)]) %>%
       layout(xaxis = list(title = paste0("P(X | Y = ", input$y, ")")))
     subplot(p1, p2, titleX = TRUE)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

