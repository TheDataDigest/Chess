
library(tidyverse)
library(lubridate)

source(file = "R/2_helper_functions.R")

# load cleaned up Titled Tuesday 2024 results
TT <- readRDS(file = "input/2024_df_RDS/TitledTuesday_2024_df.RDS")


# 1) player_df ----
player_df <- TT %>% group_by(username) %>% 
  summarize(N_participations = length(unique(file_name)),
            min_score = min(score), 
            avg_score = round(mean(score), 2),
            median_score = median(score), 
            max_score = max(score), 
            best_Place = min(number), 
            avg_Place = round(mean(number), 1),
            median_Place = median(number), 
            lowest_Place = max(number),
            winning_chance = round(mean(number == 1), 4),
            winning_pct = round(mean(number == 1)* 100, 1),
            N_1st = sum(number == 1),
            N_Top3 = sum(number %in% 1:3),
            N_Top5 = sum(number %in% 1:5),
            N_Top10 = sum(number %in% 1:10),
            N_Top25 = sum(number %in% 1:25), 
            rating_avg = round(mean(rating), 1),
            rating_best = max(rating),
            rating_lowest = min(rating),
            avg_prize = round(mean(prize2), 2),
            total_prize = sum(prize2),
            N_all_games_played = sum(games_played == 11),
            avg_games_played = round(mean(games_played), 2), 
            median_games_played = median(games_played), 
            N_majority_played = sum(games_played > 5),
            avg_majority_played = round(mean(games_played > 5), 2),
            total_games_played = sum(games_played),
            min_BhC1 = round(min(buchholz_cut_1), 2),
            avg_BhC1 = round(mean(buchholz_cut_1), 2),
            median_BhC1 = round(median(buchholz_cut_1), 2),
            max_BhC1 = round(max(buchholz_cut_1), 2)) %>% 
              ungroup()

player_df$fed <- TT$fed[match(player_df$username, TT$username)]
player_df$name <- TT$name[match(player_df$username, TT$username)]
player_df$title_ordered <- TT$title_ordered[match(player_df$username, TT$username)]
player_df$title <- player_df$title_ordered
player_df$score <- round(player_df$avg_score, 2)

time_df <- TT %>% group_by(username, time) %>% 
  summarize(N = length(unique(file_name))) %>% 
  pivot_wider(names_from = time, values_from = N)

player_df <- player_df %>% left_join(time_df, by = "username")

saveRDS(object = player_df, file = "input/2024_df_RDS/player_2024_df.RDS")

# 2) TT_long - ALL players encounters ----
TT_long <- TT %>% 
  select(file_name, date_time, number, username, rating, fed, title, rnd1:rnd11) %>% 
  pivot_longer(cols = -c(file_name:title)) %>% 
  mutate(last_letter = substr(x = value, start = nchar(value), stop = nchar(value)),
         first_letter = substr(x = value, start = 1, stop = 1),
         color = if_else(last_letter == "W", "white", "black"),
         opponent_number = gsub(pattern = "[^0-9]", replacement = "", x = value),
         date_time_number = paste0(date_time, "_", number),
         date_time_opponent = paste0(date_time, "_", opponent_number)) %>% 
  left_join(data.frame(first_letter = c("W", "L", "D"), 
                       result = c("win", "lose", "draw")), by = "first_letter")  

names(TT_long)[8] <- "round"

opponent_df <- TT_long %>% select(date_time_number, username, rating)
TT$opponent_username <- TT$username ## what doing?
TT_long$opponent_username <- opponent_df$username[match(TT_long$date_time_opponent, opponent_df$date_time_number)]
TT_long$opponent_rating <- opponent_df$rating[match(TT_long$date_time_opponent, opponent_df$date_time_number)]

TT_opponent_names_df <- TT %>% select(opponent_username, name) %>% distinct()
TT_hero_names_df <- TT %>% select(username, name) %>% distinct()
names(TT_hero_names_df)[2] <- "hero_name"

TT_long <- TT_long %>% 
  left_join(TT_opponent_names_df, by = "opponent_username") %>% 
  na.omit()

TT_long <- TT_long %>% 
  left_join(TT_hero_names_df, by = "username")

names(TT_long)[17] <- "opponent_username"
names(TT_long)[19] <- "opponent_name"
names(TT_long)[5] <- "hero_rating"

matches_df <- TT_long
saveRDS(object = matches_df, file = "input/2024_df_RDS/matches_2024_df.RDS")

# 3) Results_df and games ----
games <- TT %>% select(username, contains("rnd")) 
games$N_played <- rowSums(games[, -1] != "U--")

results_df <- games %>% select(-N_played) %>% 
  pivot_longer(cols = -username) %>% 
  mutate(value_short = gsub(x = value, pattern = "[[:digit:]]", replacement = '')) %>% 
  group_by(username) %>% 
  summarize(win_White = sum(value_short == "WW"),
            draw_White = sum(value_short == "DW"),
            lose_White = sum(value_short == "LW"),
            win_Black = sum(value_short == "WB"),
            draw_Black = sum(value_short == "DB"),
            lose_Black = sum(value_short == "LB"))

results_df$games_w_White <- rowSums(results_df[, 2:4])
results_df$games_w_Black <- rowSums(results_df[, 5:7])

saveRDS(object = results_df, file = "input/2024_df_RDS/results_2024_df.RDS")

# 4) Streak_dfs ----
TT$week = week(TT$date)

# filter out players that only participated once
users_multi_part <- player_df %>% filter(N_participations > 1) %>% pull(username)

## create running tournament numbering
tournament_df <- data_frame(date_time = TT %>% pull(date_time) %>% unique() %>% sort(),
                            date_time_ID = 1:106)

TT <- TT %>% left_join(tournament_df, by = "date_time")

# calculate streak
week_streak_df <- TT %>% 
  filter(games_played > 0,
         username %in% users_multi_part) %>% 
  select(username, name, week) %>% 
  distinct() %>% 
  arrange(username, week) %>%
  group_by(username) %>%    
  mutate(Streak = c(1, diff(week)),
         a = Streak == 1) 

week_streak <- week_streak_df %>% group_by(username) %>% 
  summarize(longest_week_streak = longest_sequence(a)) %>% 
  arrange(desc(longest_week_streak)) %>% left_join(TT %>% select(username, name, fed, title) %>% distinct()) %>% 
  select(name, title, fed, longest_week_streak)

# tournament id
file_name <- TT %>% arrange(date, time) %>% pull(file_name) %>% unique()
tt_id <- 1:106
tour_ID <- data.frame(file_name, tt_id)
TT <- TT %>% left_join(tour_ID)

tournament_streak_df <- TT %>% 
  filter(games_played > 0,
         username %in% users_multi_part) %>% 
  select(username, name, tt_id) %>% 
  arrange(username, tt_id) %>%
  group_by(username) %>%
  mutate(Streak = c(1, diff(tt_id)),
         a = Streak == 1) 

tournament_streak <- tournament_streak_df %>% group_by(username) %>% 
  summarize(longest_tour_streak = longest_sequence(a)) %>% 
  arrange(desc(longest_tour_streak)) %>% left_join(TT %>% select(username, name, fed, title) %>% distinct()) %>% 
  select(name, title, fed, longest_tour_streak)


# winning streaks
winning_streak_df <- TT %>% select(file_name, username, name, title, fed, rnd1:rnd11) %>% 
  pivot_longer(cols = -c(file_name, username, name, title, fed), names_repair = "unique") %>% 
  mutate(result = substr(x =value, start = 1, stop = 1),
         win = result == "W")
names(winning_streak_df)[c(3,6)] <- c("name", "round")

winning_streak <- winning_streak_df %>% group_by(username, title, name, fed) %>% 
  summarize(winning_streak = longest_sequence(win)) %>% 
  arrange(desc(winning_streak)) %>% ungroup() %>% select(name, title, fed, winning_streak)


saveRDS(object = week_streak, file = "input/2024_df_RDS/week_streak_2024_df.RDS")
saveRDS(object = tournament_streak, file = "input/2024_df_RDS/tournament_streak_2024_df.RDS")
saveRDS(object = winning_streak, file = "input/2024_df_RDS/winning_streak_2024_df.RDS")
saveRDS(object = winning_streak_df, file = "input/2024_df_RDS/winning_streak_df_2024_df.RDS")
