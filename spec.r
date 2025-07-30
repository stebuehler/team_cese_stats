library(ggplot2)
library(plotly)
library(tidyr)
library(formattable)
library(gsubfn)

source("util.R")

df <- get_source_data()
# df_teams <- get_df_for_team_stats(df)
# df_sets <- df_sets <- get_df_for_set_stats(df_teams)
# df prep for satz stats
test <- get_df_for_plot_year_per_player(df, "Hu")
# matrix_matchup <- get_matrix_for_matchup_chart(df, total_order)
fig1 <- plot_ly(
  x = test$jahr
)
fig2 <- plot_ly(
  x = test$jahr
)
fig1 <- fig1 %>% add_trace(
  y = test$`Spiele gewonnen (%)`,
  type = 'scatter',
  mode = 'lines+markers',
  name = "Siegprozent")
fig2 <- fig2 %>%
  add_trace(
  y = test$Rang,
  type = 'scatter',
  mode = 'lines+markers',
  name = "Rang") %>%
  layout(
    yaxis = list(
      autorange = "reversed"  # Invert the axis
    )
  )
subplot(fig1, fig2, nrows = 2, shareX = TRUE)
