library(ggplot2)
library(tidyverse)

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

