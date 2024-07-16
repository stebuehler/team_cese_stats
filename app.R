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
library(gsubfn)

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
                    tabPanel("Spieler Stats",
                      fluidRow(
                        dataTableOutput("table_by_player")
                      ),
                      fluidRow(
                        plotlyOutput("cum_stats_per_player")
                      )
                    ),
                    tabPanel("Team Stats", dataTableOutput("table_by_team")),
                    tabPanel("Total Stats", plotlyOutput("bar_plot_matches"))
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
      filter(jahr %in% input$years)
    if(input$scope == "Team Cese"){
      df <- df %>% filter(team_cese_match == TRUE)
    }
    if(input$scope == "Team Cese classic"){
      df <- df %>% filter(original_match == TRUE)
    }
    return(df)
  })
  #
  output$bar_plot_matches <- renderPlotly({
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
    ggplotly(plot)
  })
  #
  output$cum_stats_per_player <- renderPlotly({
    cumulative_stats <- get_df_for_cumulative_match_percentage(filtered_data())
    # prerequisites
    x_axis <- 1:length(unique(cumulative_stats$reihenfolge_alltime))
    list[ticktext, tickvals] <- give_x_ticks_for_cumulative_stats_plot(cumulative_stats)
    # plot
    plot <- plot_ly(x = x_axis)
    for(player in unique(cumulative_stats$Spieler)){
      df_temp <- cumulative_stats %>%
        filter(Spieler == player) %>%
        arrange(reihenfolge_alltime) %>%
        fill(siegprozent)
      plot <- plot %>% add_trace(
        y = df_temp$siegprozent,
        name = player,
        type = 'scatter',
        mode = 'lines')  
    }
    # layout
    plot <- plot %>%
      layout(
        yaxis = list(tickformat = ".1%"),
        xaxis = list(
          ticktext = ticktext, 
          tickvals = tickvals,
          tickmode = "array"
        )
      )
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
