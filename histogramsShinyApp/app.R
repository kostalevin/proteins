#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source('./plot_density_3D_library.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Density plot"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("point_size",
                     "Size of points:",
                     min = 1,
                     max = 30,
                     value = 8),
         sliderInput("opacity",
                     "Opacity of points:",
                     min = 0,
                     max = 1,
                     value = 0.2), 
         width = "2"
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("density_plot", height = "800px")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   data <- load_data(file_path = '../AA03_nearest39random.pdb')
   
   output$density_plot <- renderPlotly({
      # plot the data
      # plot_density(input$point_size, input$opacity, input$filepath)
     plot_density(point_size = input$point_size, 
                  opacity = input$opacity, data = data)

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

