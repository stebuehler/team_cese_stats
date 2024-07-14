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
library(DT)
library(dplyr)
library(shinyWidgets)
source('util.R')

source_df <- get_source_data()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Team Cese Statistik"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "years",
          "Jahr(e)",
          choices = sort(unique(source_df$jahr), decreasing=TRUE),
          multiple = TRUE,
          selected = max(source_df$jahr)
        ),
        radioGroupButtons(
          "scope",
          "Spieler",
          choices = c("Alle", "Team Cese", "Team Cese classic")
        ),
        width = 3
      ),
      #
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("Rangliste", dataTableOutput("table_by_player_short")),
                    tabPanel("Spieler Statistik", dataTableOutput("table_by_player", width = "100%")),
                    tabPanel("Team Statistik", dataTableOutput("table_by_team", width = "100%")),
                    tabPanel("Total Matches", plotOutput("bar_plot_matches"))
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
    df <- source_df %>%
      filter(jahr >= min(input$years) & jahr <= max(input$years))
    if(input$scope == "Team Cese"){
      df <- df %>% filter(team_cese_match == TRUE)
    }
    if(input$scope == "Team Cese classic"){
      df <- df %>% filter(original_match == TRUE)
    }
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
  #
  output$table_by_player_short <- renderDataTable(
    datatable(
      get_player_stats_short(
        filtered_data()
        ),
      options = list(dom = "t", pageLength = 99),
      rownames = FALSE
    ) %>% 
      formatPercentage(c(
        "Spiele gewonnen (%)"
      ), 1)
  )
  #
  output$table_by_player <- renderDataTable(
    datatable(
      get_standard_stats(
        filtered_data(), "Spieler"),
      options = list(dom = "t", pageLength = 99),
      rownames = FALSE
    ) %>%
      formatPercentage(c(
        "Spiele gewonnen (%)",
        "Saetze gewonnen (%)",
        "Punkte gewonnen (%)"
      ), 1)
  )
  #
  output$table_by_team <- renderDataTable(
    datatable(
      get_standard_stats(
        filtered_data(), "Team"),
      options = list(dom = "t", pageLength = 99),
      rownames = FALSE
    ) %>% 
      formatPercentage(c(
        "Spiele gewonnen (%)",
        "Saetze gewonnen (%)",
        "Punkte gewonnen (%)"
      ), 1)
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
