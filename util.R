library(googlesheets4)
library(janitor)
library(formattable)
library(tidyr)
library(dplyr)

get_source_data <- function(){
  gs4_deauth()
  df <- read_sheet("https://docs.google.com/spreadsheets/d/1lI-jzdEPqdN7T8rvJEr-7wfv-34ZE9DpFVnR5JRegEY/edit?usp=sharing")
  df <- df %>% drop_na(Jahr) %>%
    clean_names() %>%
    mutate(spiele_gesamt = spiel_gewonnen_team_a + spiel_gewonnen_team_b) %>%
    mutate(saetze_gesamt = satze_gewonnen_team_a + satze_gewonnen_team_b) %>%
    mutate(punkte_gesamt = punkte_gewonnen_team_a + punkte_gewonnen_team_b) %>%
    mutate(reihenfolge_alltime = jahr + 0.01*reihenfolge) %>%
    mutate(session = replace(session, session == "- N/A N/A", NA)) %>%
    mutate(tag = paste(reihenfolge_tag, "-", wochentag_kurz))
  return(df)
}

get_all_players <- function(df){
  players <- append(df$spieler_1_team_a,df$spieler_2_team_a)
  players <- append(players, df$spieler_1_team_b)
  players <- append(players, df$spieler_2_team_b)
  return(unique(players))
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
    'resultat_string_satz_3_sicht_team',
    'punkte_gewonnen_satz_1_sicht_team',
    'punkte_gewonnen_satz_2_sicht_team',
    'punkte_gewonnen_satz_3_sicht_team',
    'punkte_verloren_satz_1_sicht_team',
    'punkte_verloren_satz_2_sicht_team',
    'punkte_verloren_satz_3_sicht_team'
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
    'resultat_string_satz_3_sicht_team_a',
    'punkte_satz_1_team_a',
    'punkte_satz_2_team_a',
    'punkte_satz_3_team_a',
    'punkte_satz_1_team_b',
    'punkte_satz_2_team_b',
    'punkte_satz_3_team_b'
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
    'resultat_string_satz_3_sicht_team_b',
    'punkte_satz_1_team_b',
    'punkte_satz_2_team_b',
    'punkte_satz_3_team_b',
    'punkte_satz_1_team_a',
    'punkte_satz_2_team_a',
    'punkte_satz_3_team_a'
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

get_df_for_set_stats <- function(df_team){
  new_column_names <- c(
    'satz_resultat_string',
    'satz_resultat_string_ordered',
    'punkte_gewonnen_satz',
    'punkte_verloren_satz'
  )
  old_column_names_satz_1 <- c(
    'resultat_string_satz_1_sicht_team',
    'resultat_string_satz_1',
    'punkte_gewonnen_satz_1_sicht_team',
    'punkte_verloren_satz_1_sicht_team'
  )
  old_column_names_satz_2 <- c(
    'resultat_string_satz_2_sicht_team',
    'resultat_string_satz_2',
    'punkte_gewonnen_satz_2_sicht_team',
    'punkte_verloren_satz_2_sicht_team'
  )
  old_column_names_satz_3 <- c(
    'resultat_string_satz_3_sicht_team',
    'resultat_string_satz_3',
    'punkte_gewonnen_satz_3_sicht_team',
    'punkte_verloren_satz_3_sicht_team'
  )
  df_satz_1 <- data.frame(df_team) %>% mutate(satz_nr = 1)
  df_satz_2 <- data.frame(df_team) %>% mutate(satz_nr = 2)
  df_satz_3 <- data.frame(df_team) %>% mutate(satz_nr = 3)
  #
  for (i in 1:length(old_column_names_satz_1)){
    colnames(df_satz_1)[colnames(df_satz_1) == old_column_names_satz_1[i]] = new_column_names[i]
    colnames(df_satz_2)[colnames(df_satz_2) == old_column_names_satz_2[i]] = new_column_names[i]
    colnames(df_satz_3)[colnames(df_satz_3) == old_column_names_satz_3[i]] = new_column_names[i]
  }
  #
  df_out <- bind_rows(df_satz_1, df_satz_2, df_satz_3)
  df_out <- df_out %>%
    mutate(punktedifferenz_satz = punkte_gewonnen_satz - punkte_verloren_satz)
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

get_df_for_satz_chart <- function(df_sets, grouping_column){
  groupby_sym <- sym(grouping_column)
  chart_data <- df_sets %>%
    filter(punktedifferenz_satz > 0) %>%
    group_by({{ groupby_sym }}) %>%
    summarize(
      count = n()
    )
  return(chart_data)
}

get_df_for_satz_stacked_bar_chart <- function(df_sets){
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
  return(df_set_bar_chart)
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
      tiebreaker_column = spiele_gewonnen_prozent + 0.01 * replace_na(saetze_gewonnen_prozent, 0) + 0.0001 * replace_na(punkte_gewonnen_prozent, 0),
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
    mutate("Punkte +/-" = 2 * punkte_gewonnen - punkte_gesamt) %>%
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
      "Punkte gewonnen (%)",
      "Punkte +/-"
    )
  return(stats)
}

get_year_stats <- function(df){
  df_player <- get_df_for_player_stats(df)
  df_teams <- get_df_for_team_stats(df)
  #
  stats <- df %>%
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
  df_anzahl_teams <- df_teams %>%
    group_by(jahr) %>%
    summarise(anzahl_teams = n_distinct(Team)) %>%
    as.data.frame()
  #
  stats <- stats %>%
    inner_join(df_anzahl_spieler, by = "jahr") %>%
    inner_join(df_anzahl_teams, by = "jahr") %>% # pretty names from here
    mutate("Jahr" = jahr) %>%
    mutate("Tage" = anzahl_tage) %>%
    mutate("Spieler" = anzahl_spieler) %>%
    mutate("Teams" = anzahl_teams) %>%
    mutate("Sessions" = anzahl_sessions) %>%
    mutate("Längste Session" = laengste_session) %>%
    mutate("Matches" = anzahl_matches) %>%
    mutate("Sätze" = anzahl_saetze) %>%
    mutate("Punkte" = anzahl_punkte) %>%
    mutate("3-Sätzer" = anzahl_3_saetzer) %>%
    mutate("3-Sätzer gekehrt" = anzahl_3_saetzer_gekehrt) %>%
    select( # reorder columns
      "Jahr",
      "Tage",
      "Spieler",
      "Teams",
      "Sessions",
      "Längste Session",
      "Matches",
      "Sätze",
      "Punkte",
      "3-Sätzer",
      "3-Sätzer gekehrt"
    )
  return(stats)
  #
  
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

get_stats_for_single_player <- function(df, player_name){
  stats <- get_standard_stats(df, "Spieler")
  stats <- stats %>%
    filter(Spieler == player_name)
  transposed_df <- data.frame(
    Variable = colnames(stats),
    Value = as.character(unlist(stats)),
    stringsAsFactors = FALSE
  )
  return(transposed_df)
}

get_df_for_cumulative_match_percentage <- function(df){
  df_player <- get_df_for_player_stats(df)
  # Data prep line chart
  cumulative_stats <- df_player %>%
    arrange(Spieler, reihenfolge_alltime) %>%
    group_by(Spieler) %>%
    mutate(
      siegprozent = cumsum(spiele_gewonnen) / cumsum(spiele_gesamt)
    ) %>%
    select(Spieler, reihenfolge_alltime, siegprozent, jahr, reihenfolge, wochentag_kurz)
  # pivot and unpivot to have same length entries for all players
  cumulative_stats <- pivot_wider(
    cumulative_stats,
    names_from = Spieler,
    values_from = siegprozent
  )
  cumulative_stats <- pivot_longer(
    cumulative_stats,
    cols = -c(reihenfolge_alltime, jahr, reihenfolge, wochentag_kurz),
    names_to = "Spieler",
    values_to = "siegprozent"
  )
  return(cumulative_stats)
}

give_x_ticks_for_cumulative_stats_plot <- function(df_in){
  # filter for just one player
  player <- df_in$Spieler[1]
  years <- unique(df_in$jahr)
  df <- df_in %>%
    filter(Spieler == player) %>%
    arrange(reihenfolge_alltime) %>%
    select(reihenfolge_alltime, jahr, wochentag_kurz)
  ticktext <- c()
  tickvals <- c()
  if (length(years) > 1){
    for(year in years){
      first_match <- min({df %>% filter(jahr == year)}$reihenfolge_alltime)
      rownum <- which(df[,1] == first_match)
      ticktext <- c(ticktext, year)
      tickvals <- c(tickvals, rownum)
    }
  }
  else{
    for(tag in unique(df$wochentag_kurz)){
      first_match <- min({df %>% filter(wochentag_kurz == tag)}$reihenfolge_alltime)
      rownum <- which(df[,1] == first_match)
      ticktext <- c(ticktext, tag)
      tickvals <- c(tickvals, rownum)
    }
  }
  return(list(as.list(ticktext), as.list(tickvals)))
}

get_orig_df_with_less_columns <- function(df){
  df_short <- df %>%
    select(
      jahr,
      reihenfolge,
      spieler_1_team_a,
      spieler_2_team_a,
      spieler_1_team_b,
      spieler_2_team_b,
      team_a_unique,
      team_b_unique,
      punkte_satz_1_team_a,
      punkte_satz_1_team_b,
      punkte_satz_2_team_a,
      punkte_satz_2_team_b,
      punkte_satz_3_team_a,
      punkte_satz_3_team_b,
      resultat_string_sicht_team_a,
      satze_gewonnen_team_a,
      satze_gewonnen_team_b,
      spiel_gewonnen_team_a,
      spiel_gewonnen_team_b,
      kommentar,
      session,
      reihenfolge_alltime
    )
  return(df_short)
}

get_matrix_for_matchup_chart <- function(df, total_order){
  df_matchup <- get_df_for_team_stats(df) %>%
    group_by(Team, gegner) %>%
    summarise(spiele_gewonnen_prozent = sum(spiele_gewonnen, na.rm = TRUE) / sum(spiele_gesamt, na.rm = TRUE)) %>%
    arrange(sapply(Team, function(y) which(y == total_order))) %>%
    pivot_wider(names_from = gegner, values_from = spiele_gewonnen_prozent)
  return(data.matrix(df_matchup[,total_order]))
}