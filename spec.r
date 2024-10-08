library(ggplot2)
library(plotly)
library(tidyr)
library(formattable)
library(gsubfn)

source("util.R")

df <- get_source_data()
df_teams <- get_df_for_team_stats(df)
df_sets <- df_sets <- get_df_for_set_stats(df_teams)
# df prep for satz stats
total_order <- get_standard_stats(df, "Team")$Team
matrix_matchup <- get_matrix_for_matchup_chart(df, total_order)
fig <- plot_ly(
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
fig
