# util functions to read and transform team cese dataframe for specific stats
library(tidyverse)
library(googlesheets4)
library(janitor)

get_source_data <- function(){
  df <- read_sheet('https://docs.google.com/spreadsheets/d/1lI-jzdEPqdN7T8rvJEr-7wfv-34ZE9DpFVnR5JRegEY')
  df <- df %>% drop_na(Jahr)
  df <- clean_names(df)
  return(df)
}

# TODO: drop unused / non-useful columns at the end
get_df_for_player_stats <- function(df){
  new_column_names <- c(
    'spieler',
    'partner',
    'spiele_gewonnen',
    'saetze_gewonnen',
    'punkte_gewonnen',
    'spiele_verloren',
    'saetze_verloren',
    'punkte_verloren',
    'resultat_string_sicht_spieler',
    'resultat_string_satz_1_sicht_spieler',
    'resultat_string_satz_2_sicht_spieler',
    'resultat_string_satz_3_sicht_spieler'
  )
  old_column_names_spieler_1 <- c(
    'spieler_1_team_a',
    'spieler_2_team_a',
    'spiel_gewonnen_team_a',
    'satze_gewonnen_team_a',
    'punkte_gewonnen_team_a',
    'spiel_gewonnen_team_b',
    'satze_gewonnen_team_b',
    'punkte_gewonnen_team_b',
    'resultat_string_sicht_team_a',
    'resultat_string_satz_1_sicht_team_a',
    'resultat_string_satz_2_sicht_team_a',
    'resultat_string_satz_3_sicht_team_a'
  )
  #
  old_column_names_spieler_2 <- old_column_names_spieler_1
  old_column_names_spieler_2[1] <- 'spieler_2_team_a'
  old_column_names_spieler_2[2] <- 'spieler_1_team_a'
  #
  old_column_names_spieler_3 <- c(
    'spieler_1_team_b',
    'spieler_2_team_b',
    'spiel_gewonnen_team_b',
    'satze_gewonnen_team_b',
    'punkte_gewonnen_team_b',
    'spiel_gewonnen_team_a',
    'satze_gewonnen_team_a',
    'punkte_gewonnen_team_a',
    'resultat_string_sicht_team_b',
    'resultat_string_satz_1_sicht_team_b',
    'resultat_string_satz_2_sicht_team_b',
    'resultat_string_satz_3_sicht_team_b'
  )
  #
  old_column_names_spieler_4 <- old_column_names_spieler_3
  old_column_names_spieler_4[1] <- 'spieler_2_team_b'
  old_column_names_spieler_4[2] <- 'spieler_1_team_b'
  #
  df_player1 <- data.frame(df)
  df_player2 <- data.frame(df)
  df_player3 <- data.frame(df)
  df_player4 <- data.frame(df)
  #
  for (i in 1:length(old_column_names_spieler_1)){
    colnames(df_player1)[colnames(df_player1) == old_column_names_spieler_1[i]] = new_column_names[i]
    colnames(df_player2)[colnames(df_player2) == old_column_names_spieler_2[i]] = new_column_names[i]
    colnames(df_player3)[colnames(df_player3) == old_column_names_spieler_3[i]] = new_column_names[i]
    colnames(df_player4)[colnames(df_player4) == old_column_names_spieler_4[i]] = new_column_names[i]
  }
  #
  df_out <- bind_rows(df_player1, df_player2, df_player3, df_player4)
  df_out$spiele_gesamt <- df_out$spiele_gewonnen + df_out$spiele_verloren
  df_out$saetze_gesamt <- df_out$saetze_gewonnen + df_out$saetze_verloren
  df_out$punkte_gesamt <- df_out$punkte_gewonnen + df_out$punkte_verloren
  df_out$punktedifferenz <- df_out$punkte_gewonnen - df_out$punkte_verloren
  return(df_out)
}