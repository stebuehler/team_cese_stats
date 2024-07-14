library(ggplot2)
library(tidyverse)
library(formattable)

source("util.R")

df <- get_source_data()
xx <- get_standard_stats(df, "spieler")
df_player <- get_df_for_player_stats(df) %>% filter(jahr == 2023)

cumulative_stats <- df_player %>%
  arrange(Spieler, reihenfolge) %>%
  group_by(Spieler) %>%
  mutate(
    siegprozent = cumsum(spiele_gewonnen) / cumsum(spiele_gesamt)
  ) %>%
  select(Spieler, reihenfolge, siegprozent)

ggplot(df, mapping=aes(x = `jahr`, fill=`x3_satzer`)) +
  geom_bar(position='stack', color='black')

ggplot(
  df_player[order(df_player$jahr),],
  mapping=aes(x=spieler, y=spiele_gesamt, fill=jahr)
  ) +
  geom_bar(position='stack', stat='identity')

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
#

