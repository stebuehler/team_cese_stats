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
library(shinyjs)

source('util.R')
source_df <- get_source_data()
all_years <- unique(source_df$jahr)
all_players <- get_all_players(source_df)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Team Cese Statistik"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          "years",
          "Jahr(e)",
          choices = sort(all_years, decreasing=TRUE),
          multiple = TRUE,
          selected = max(source_df$jahr)
        ),
        actionButton("select_all_years", "Alle Jahre"),
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
                    tabPanel("Team Stats",
                             fluidRow(
                               dataTableOutput("table_by_team")
                               ),
                             fluidRow(
                               plotlyOutput("matchup_heatmap")
                             )
                    ),
                    tabPanel("Total Stats",
                             fluidRow(
                               dataTableOutput("table_by_year")
                             ),
                             fluidRow(
                               plotlyOutput("bar_plot_sets")
                               )
                    ),
                    tabPanel("Spieler Steckbrief",
                             fluidRow(
                               selectizeInput(
                                 "player",
                                 "Spieler",
                                 choices = sort(all_players),
                                 multiple = FALSE,
                                 selected = source_df$spieler_1_team_a[1]
                               )
                             ),
                             fluidRow(
                               dataTableOutput("table_single_player")
                             ),
                             fluidRow(
                               column(12, align="center",
                                      imageOutput("player_image")
                               )
                             )
                    ),
                    tabPanel("Satz Histo", plotlyOutput("histogram_sets")),
                    tabPanel("Rohdaten", div(
                      dataTableOutput("raw_data_short")),
                      style = "font-size:70%"
                      )
        )
      )
    ),
    tags$head(
      HTML(
        "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
      )
    ),
    span(textOutput("keepAlive"), style="color: white")
)

################################################################################
### OUTPUT FUNCTIONS and other reactive expressions ############################
################################################################################
server <- function(input, output, session) {
  #
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })
  #
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
  observeEvent(input$select_all_years, {
    updateSelectizeInput(
      session,
      "years",
      choices=all_years,
      selected=all_years)
  })
  #
  output$player_image <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('./images',
                                        paste(input$player, '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste(input$player))
    
  }, deleteFile = FALSE)
  #
  output$bar_plot_matches <- renderPlotly({
    df <- filtered_data()
    chart_data <- get_df_for_matches_and_3satz_bar_and_line_chart(df, jahr)
    plot <- ggplot(chart_data, mapping=aes(x=jahr))
    plot <- plot + geom_bar(aes(y = count, group=1), stat='identity', position = "dodge")
    plot <- plot + ylab("Anzahl Matches")
    plot <- plot + scale_y_continuous(name = "Anzahl Matches")
    plot <- plot + theme_minimal()
  })
  #
  output$bar_plot_sets <- renderPlotly({
    df_sets <- get_df_for_set_stats(
      get_df_for_team_stats(
        filtered_data()
        )
      )
    groupby <- ifelse(length(unique(df_sets$jahr)) > 1, "jahr", "tag")
    groupby_sym <- sym(groupby)
    chart_data <- get_df_for_satz_chart(df_sets, groupby)
    plot <- ggplot(chart_data, mapping=aes(x={{ groupby_sym }}))
    plot <- plot + geom_bar(aes(y = count, group=1), stat='identity', position = "dodge")
    plot <- plot + ylab("Anzahl Sätze")
    plot <- plot + scale_y_continuous(name = "Anzahl Sätze")
    plot <- plot + theme_minimal()
  })
  #
  output$histogram_sets <- renderPlotly({
    df_set_bar_chart <- get_df_for_satz_stacked_bar_chart(
      get_df_for_set_stats(
        get_df_for_team_stats(
          filtered_data()
        )
      )
    )
    plot <- plot_ly(
      df_set_bar_chart,
      x = ~punktedifferenz_satz,
      y = ~satz1,
      type = 'bar',
      name = 'Satz 1'
    )
    plot <- plot %>% add_trace(y = ~satz2, name = 'Satz 2')
    plot <- plot %>% add_trace(y = ~satz3, name = 'Satz 3')
    plot <- plot %>% layout(
      xaxis = list(title = 'Punktedifferenz'),
      yaxis = list(title = 'Anzahl Sätze'),
      barmode = 'stack'
    )
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
  output$table_single_player <- renderDataTable(
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
  #
  output$table_by_year <- renderDataTable(
    datatable(
      get_year_stats(
        filtered_data()
      ) %>%
        arrange(desc(Jahr)),
      options = list(dom = "t", pageLength = 99),
      rownames = FALSE
    )
  )
  #
  output$raw_data_short <- renderDataTable(
    datatable(
      get_orig_df_with_less_columns(
        filtered_data()
      ) %>%
        arrange(desc(jahr)),
      options = list(dom = "lftp"),
      rownames = TRUE,
      filter = list(position = 'top', clear = FALSE)
    )
  )
  #
  output$matchup_heatmap <- renderPlotly({
    total_order <- get_standard_stats(filtered_data(), "Team")$Team
    matrix_matchup <- get_matrix_for_matchup_chart(filtered_data(), total_order)
    #
    plot <- plot_ly(
      x = total_order,
      y = total_order,
      z = matrix_matchup,
      colors = colorRamp(c("red", "yellow", "green")),
      type = "heatmap",
      texttemplate = "%{z:.3p}"
    ) %>%
      layout(yaxis = list(
        autorange="reversed"
      )
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
