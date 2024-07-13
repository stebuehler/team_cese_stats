library(googlesheets4)
library(janitor)
library(formattable)
library(tidyr)

get_source_data <- function(){
  gs4_auth(cache=".secrets", email="stibe.buehler@gmail.com")
  df <- read_sheet('https://docs.google.com/spreadsheets/d/1lI-jzdEPqdN7T8rvJEr-7wfv-34ZE9DpFVnR5JRegEY')
  df <- df %>% drop_na(Jahr)
  df <- clean_names(df)
  df$spiele_gesamt <- df$spiel_gewonnen_team_a + df$spiel_gewonnen_team_b
  df$saetze_gesamt <- df$satze_gewonnen_team_a + df$satze_gewonnen_team_b
  df$punkte_gesamt <- df$punkte_gewonnen_team_a + df$punkte_gewonnen_team_b
  return(df)
}

# TODO: drop unused / non-useful columns at the end
get_df_for_player_stats <- function(df){
  new_column_names <- c(
    'Spieler',
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

get_df_for_team_stats <- function(df){
  new_column_names <- c(
    'Team',
    'gegner',
    'spiele_gewonnen',
    'saetze_gewonnen',
    'punkte_gewonnen',
    'spiele_verloren',
    'saetze_verloren',
    'punkte_verloren',
    'resultat_string_sicht_team',
    'resultat_string_satz_1_sicht_team',
    'resultat_string_satz_2_sicht_team',
    'resultat_string_satz_3_sicht_team'
  )
  old_column_names_team_a <- c(
    'team_a_unique',
    'team_b_unique',
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
  old_column_names_team_b <- c(
    'team_b_unique',
    'team_a_unique',
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
  df_team_a <- data.frame(df)
  df_team_b <- data.frame(df)
  #
  for (i in 1:length(old_column_names_team_a)){
    colnames(df_team_a)[colnames(df_team_a) == old_column_names_team_a[i]] = new_column_names[i]
    colnames(df_team_b)[colnames(df_team_b) == old_column_names_team_b[i]] = new_column_names[i]
  }
  #
  df_out <- bind_rows(df_team_a, df_team_b)
  df_out$spiele_gesamt <- df_out$spiele_gewonnen + df_out$spiele_verloren
  df_out$saetze_gesamt <- df_out$saetze_gewonnen + df_out$saetze_verloren
  df_out$punkte_gesamt <- df_out$punkte_gewonnen + df_out$punkte_verloren
  df_out$punktedifferenz <- df_out$punkte_gewonnen - df_out$punkte_verloren
  return(df_out)
}

get_df_for_matches_and_3satz_bar_and_line_chart <- function(df, grouping_column){
  chart_data <- df %>%
    group_by({{ grouping_column }}, spiele_gesamt) %>%
    summarize(
      count = n(),
      x3satz_percentage = mean(x3_satzer)
    )
  return(chart_data)
}

get_standard_stats <- function(df, groupby){
  if(groupby == "Spieler"){
    df_stats <- get_df_for_player_stats(df)
  } else if(groupby == "Team"){
    df_stats <- get_df_for_team_stats(df)
  } else{
    stop("not implemented")
    }
  #
  groupby_sym <- sym(groupby)
  stats <- df_stats %>% group_by({{groupby_sym}}) %>%
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
      {{groupby_sym}},
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
  return(stats)
}

get_player_stats_short <- function(df){
  player_stats <- get_standard_stats(df, "Spieler")
  player_stats <- player_stats %>%
  select(
    Rang,
    Spieler,
    "Spiele gesamt",
    "Spiele gewonnen (%)"
  )
  return(player_stats)
}
