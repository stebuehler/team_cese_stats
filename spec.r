library(ggplot2)
library(tidyverse)
library(formattable)

source("util.R")

df <- get_source_data()
df_player <- get_df_for_player_stats(df)

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
player_stats <- df_player %>% group_by(spieler) %>%
  summarise(
    spiele_gesamt = sum(spiele_gesamt),
    spiele_gewonnen = sum(spiele_gewonnen),
    spiele_gewonnen_prozent = spiele_gewonnen / spiele_gesamt,
    saetze_gesamt = sum(saetze_gesamt),
    saetze_gewonnen = sum(saetze_gewonnen),
    saetze_gewonnen_prozent = saetze_gewonnen / saetze_gesamt,
    punkte_gesamt = sum(punkte_gesamt),
    punkte_gewonnen = sum(punkte_gewonnen),
    punkte_gewonnen_prozent = punkte_gewonnen / punkte_gesamt,
    tiebreaker_column = spiele_gewonnen_prozent + 0.01 * saetze_gewonnen_prozent + 0.0001 * punkte_gewonnen_prozent,
  ) %>%
  as.data.frame() %>%
  arrange(., desc(tiebreaker_column))%>% # order rows
  mutate(rang = seq.int(nrow(.))) %>% 
  mutate(Rang = rang) %>% # pretty names
  mutate(Spieler = spieler) %>%
  mutate("Spiele gesamt" = spiele_gesamt) %>%
  mutate("Spiele gewonnen" = spiele_gewonnen) %>%
  mutate("Spiele gewonnen (%)" = percent(spiele_gewonnen_prozent, 1)) %>%
  mutate("Saetze gesamt" = saetze_gesamt) %>%
  mutate("Saetze gewonnen" = saetze_gewonnen) %>%
  mutate("Saetze gewonnen (%)" = percent(saetze_gewonnen_prozent, 1)) %>%
  mutate("Punkte gesamt" = punkte_gesamt) %>%
  mutate("Punkte gewonnen" = punkte_gewonnen) %>%
  mutate("Punkte gewonnen (%)" = percent(punkte_gewonnen_prozent, 1)) %>%
  select( # reorder columns
    Rang,
    Spieler,
    "Spiele gesamt",
    "Spiele gewonnen",
    "Spiele gewonnen (%)",
    "Saetze gesamt",
    "Saetze gewonnen",
    "Saetze gewonnen (%)",
    "Punkte gesamt",
    "Punkte gewonnen",
    "Punkte gewonnen (%)"
  )
