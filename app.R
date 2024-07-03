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

source_df <- get_source_data()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Team Cese Statistik"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # sliderInput("years",
            #             "Jahre:",
            #             min = min(source_df$jahr),
            #             max = max(source_df$jahr),
            #             value = c(min(source_df$jahr), max(source_df$jahr)),
            #             sep = ""
            #             )
            selectInput("years",
                        "Jahr(e)",
                        choices = sort(unique(source_df$jahr), decreasing=TRUE),
                        multiple = TRUE,
                        selected = max(source_df$jahr)
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
          tabsetPanel(type = "tabs",
                      tabPanel("Total Matches", plotOutput("bar_plot_matches"))
                      #,tabPanel("Plotly", plotlyOutput("bar_plot_plotly"))
                      #,tabPanel("Plotly with ggplot", plotlyOutput("bar_plot_plotly_v2"))
          )
        )
    )
)

################################################################################
### OUTPUT FUNCTIONS and other reactive expressions ############################
################################################################################
server <- function(input, output) {
  
  filtered_data <- reactive({
    df <-source_df[source_df$jahr >= min(input$years) & source_df$jahr <= max(input$years),]
    return(df)
  })
  #
  output$bar_plot_matches <- renderPlot({
    df <- filtered_data()
    chart_data <- get_df_for_matches_and_3satz_bar_and_line_chart(df, jahr)
    plot <- ggplot(chart_data, mapping=aes(x=jahr))
    plot <- plot + geom_bar(aes(y = count, group=1), stat='identity', position = "dodge")
    plot <- plot + ylab("Anzahl Matches")
    plot <- plot + geom_line(
      aes(y = x3satz_percentage * max(chart_data$count), group=1),
      color = "red"
    )
    plot <- plot + scale_y_continuous(
      name = "Anzahl Matches",
      sec.axis = sec_axis(~./max(chart_data$count), name = "Prozent 3-Sätzer")
    )
    plot <- plot + theme_minimal()
    print(plot)
    #ggplotly(plot)
    # TODO move this code and the corresponding df prep into its own .R file and do so for each subsequent plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
