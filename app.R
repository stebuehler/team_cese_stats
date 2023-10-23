#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source('util.R')

df <- get_source_data()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Team Cese Statistik"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("years",
                        "Jahre:",
                        min = min(df$jahr),
                        max = max(df$jahr),
                        value = c(min(df$jahr), max(df$jahr))
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("bar_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$bar_plot <- renderPlot({
    ggplot(
      df[df$jahr >= input$years[1] & df$jahr <= input$years[2],],
      mapping=aes(x = `jahr`, fill=`x3_satzer`)
      ) +
      geom_bar(position='stack', color='black')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)