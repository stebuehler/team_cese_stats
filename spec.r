library(ggplot2)
library(plotly)
library(tidyr)
library(formattable)
library(gsubfn)

source("util.R")

df <- get_source_data()
df_player <- get_df_for_player_stats(df)
# jahr stats
# anzahl spieler needs workaround via spieler_df or via operation to concatenate all four player columns in original df
year_stats <- df %>%
  group_by(jahr) %>%
  summarise(
    anzahl_tage = max(reihenfolge_tag),
    anzahl_sessions = n_distinct(session),
    laengste_session = max(reihenfolge_in_session),
    anzahl_matches = n_distinct(reihenfolge),
    anzahl_saetze = sum(satze_gewonnen_team_a) + sum(satze_gewonnen_team_b),
    anzahl_punkte = sum(punkte_gesamt),
    anzahl_3_saetzer = sum(x3_satzer == TRUE),
    anzahl_3_saetzer_gekehrt = sum(x3_satzer_gekehrt == TRUE)
  ) %>%
  as.data.frame()
#
df_anzahl_spieler <- df_player %>%
  group_by(jahr) %>%
  summarise(anzahl_spieler = n_distinct(Spieler)) %>%
  as.data.frame()
#
year_stats <- year_stats %>%
  inner_join(df_anzahl_spieler, by = "jahr")

# bar chart
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

