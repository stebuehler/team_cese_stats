library(ggplot2)
library(plotly)
library(tidyr)
library(formattable)
library(gsubfn)

source("util.R")

df <- get_source_data()
df_teams <- get_df_for_team_stats(df)
# df prep for satz stats
df_sets <- get_df_for_set_stats(df_teams)
df_set_bar_chart <- df_sets %>%
  filter(punktedifferenz_satz > 0) %>%
  group_by(punktedifferenz_satz, satz_nr) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  pivot_wider(
    names_from=satz_nr,
    values_from = count,
    names_prefix = "satz",
    )
fig <- plot_ly(df_set_bar_chart, x = ~punktedifferenz_satz, y = ~satz1, type = 'bar', name = 'Satz 1')
fig <- fig %>% add_trace(y = ~satz2, name = 'Satz 2')
fig <- fig %>% add_trace(y = ~satz3, name = 'Satz 3')
fig <- fig %>% layout(
  xaxis = list(title = 'Punktedifferenz'),
  yaxis = list(title = 'Anzahl Sätze'),
  barmode = 'stack'
  )
fig