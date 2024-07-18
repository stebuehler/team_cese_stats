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
