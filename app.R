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
                        value = c(min(df$jahr), max(df$jahr)),
                        sep = ""
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
          tabsetPanel(type = "tabs",
                      tabPanel("Matches", plotOutput("bar_plot")),
                      tabPanel("Plotly", plotlyOutput("bar_plot_plotly")),
                      tabPanel("Plotly with ggplot", plotlyOutput("bar_plot_plotly_v2"))
          )
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
  #
  output$bar_plot_plotly <- renderPlotly({
    plot <- plot_ly(
      x = unique(df$jahr),
      type = 'bar'
    )
  })
  #
  output$bar_plot_plotly_v2 <- renderPlotly({
    gg <- ggplot(
      df[df$jahr >= input$years[1] & df$jahr <= input$years[2],],
      mapping=aes(x = `jahr`, fill=`x3_satzer`)
    )
    plot <- gg + geom_bar(position='stack')
    ggplotly(plot)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
