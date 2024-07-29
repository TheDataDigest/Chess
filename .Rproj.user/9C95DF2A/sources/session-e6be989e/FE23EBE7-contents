## 0) Setup-up, load packages, clean data, save RDs files

Sys.setenv(lang = "en_US")
Sys.setlocale("LC_TIME", "English") # Important when local time is not english (as.Date() will only work on month that are identical in both languages. Like april, august, september, november in German but not december vs dezember)

# load packages
library(tidyverse)
library(readr)
library(stringr)
library(lubridate)
library(forcats)
library(janitor)


# 1) load and merge data ----
list_csv_files <- paste0("input/TitledTuesday_2023_csv_files/", list.files(path = "input/TitledTuesday_2023_csv_files"))

length(list_csv_files); head(list_csv_files); tail(list_csv_files)
# not in order, make sure to arrange final TT after date, early late, Number or Rank

TT_df_backup <- readr::read_csv(list_csv_files, id = "file_name")


# 2) basic mutates ----

# Input date string
TT <- TT_df_backup %>% 
  mutate(time = gsub(".*/(\\w+)-.*", "\\1", file_name),
         date_string = str_extract(file_name, "[a-zA-Z]+-\\d+-\\d+"),
         date = as.Date(x = date_string, format = "%b-%d-%Y"))

# Number of games_played
TT$games_played <- rowSums(TT[ ,9:19] != "U--")

# Clean names
TT <- TT %>% janitor::clean_names()

# Arrange TT after date, early/late, Number (final standing, ignore "rk")
TT <- TT %>% arrange(date, time, number)

# Add date_time (early vs late tournament on the same day)
TT$date_time <- paste(TT$date, TT$time, sep = "_")

# The first tournament had many draws (old system for tie braking)
TT %>% group_by(date_time) %>% count(number) %>% filter(n > 1)

TT$number[TT$date_time == "2023-01-03_early"] <- 1:length(
  TT$number[TT$date_time == "2023-01-03_early"])

# having unique positions is crucial for later player match-ups
# breaking the ties of the first tournament introduces minor inaccuracies with regards to average (number) tournament standing. 
# 2023-01-03_early Aleksei Sarana and Magnus Carlsen were both #16, after the code above Magnus is now #17

# Other tournaments had many draws for the lowest positions
TT$number[TT$date_time == "2023-11-14_early"] <- 1:length(
  TT$number[TT$date_time == "2023-11-14_early"])
TT$number[TT$date_time == "2023-01-31_late"] <- 1:length(
  TT$number[TT$date_time == "2023-01-31_late"])
TT$number[TT$date_time == "2023-10-24_late"] <- 1:length(
  TT$number[TT$date_time == "2023-10-24_late"])
TT$number[TT$date_time == "2023-05-09_early"] <- 1:length(
  TT$number[TT$date_time == "2023-05-09_early"])

# Ordering the titles of the players and extracting gender information
TT$title_ordered <- factor(TT$title, levels = c("GM", "IM", "FM", "CM", "NM", "WGM", "WIM", "WFM", "WCM", "WNM"))
TT <- TT %>% mutate(gender = ifelse(str_detect(string = TT$title, pattern = "W"), "Women", "Men"))

# Fixing non standard english alphabet names
TT$name[TT$name == "Димитрий Король"] <- "Dimitriy Korol"
TT$name[TT$name == "Корюн Гюламирян"] <- "Koryun Gyulamiryan"
TT$name[TT$name == "Sebasti&aacute;n Mar&iacute;n"] <- "Sebastian Marin"
TT$name[TT$name == "Платон Гальперин"] <- "Platon Galperin"
TT$name[TT$name == "Варвара Полякова"] <- "Varvara Polyakova"
TT$name[TT$name == "Алёна Гармаш"] <- "Aliona Garmash"



# Fixing players that have multiple usernames, feds, titles, names
username_name <- TT %>% count(username, name) %>% 
  count(username) %>% filter(n > 1) %>% pull(username)
# yes 410 cases

# function, fix name/username, user fed, that is most common? add_count()
#TT %>% count(username) %>% filter(username %in% username_tests) %>% arrange(desc(n))

name_fix <- TT %>% filter(username %in% username_name) %>% 
  count(username, name) %>%  
  mutate(name_length = nchar(name)) %>% 
  arrange(username, desc(name_length)) %>% 
  group_by(username) %>% 
  slice_head(n = 1) %>% ungroup() %>% select(-n, -name_length)

names(name_fix)[2] <- "name2"

TT <- TT %>% left_join(name_fix, by = "username")
TT$name2[is.na(TT$name2)] <- TT$name[is.na(TT$name2)]

# TT %>% filter(is.na(name)) %>% select(username,name, name2)

# TT$username[is.na(TT$name2)]
# 3645 name NAs remaining, overwrite with username!
TT$name2[is.na(TT$name2)] <- TT$username[is.na(TT$name2)]

TT %>% count(username, name2) %>% count(username) %>% filter(n > 1) # A tibble: 0 x 2, success!

# Fixing titles (some players might gain a better title throughout 2023, but I only use the best title)
username_title <- TT %>% count(username, title_ordered) %>% count(username) %>% filter(n > 1) %>% pull(username) # 76 cases

title_fix <- TT %>% filter(username %in% username_title) %>% 
  group_by(username, title_ordered) %>% 
  select(username, title_ordered) %>% distinct() %>% 
  ungroup() %>% 
  arrange(username, title_ordered) %>% 
  group_by(username) %>% 
  slice_head(n = 1) %>% ungroup()

names(title_fix)[2] <- "title2"

TT <- TT %>% left_join(title_fix, by = "username")
TT$title2[is.na(TT$title2)] <- TT$title[is.na(TT$title2)]

# Fix fed (sort by most common, add_count)
username_fed <- TT %>% count(username, fed) %>% count(username) %>% filter(n > 1) %>% pull(username) # 113 cases

fed_fix <- TT %>% filter(username %in% username_fed) %>% 
  count(username, fed) %>% 
  arrange(username, desc(n)) %>% 
  group_by(username) %>% 
  slice_head(n = 1) %>% ungroup() %>% select(-n)

names(fed_fix)[2] <- "fed2"

TT <- TT %>% left_join(fed_fix, by = "username")
TT$fed2[is.na(TT$fed2)] <- TT$fed[is.na(TT$fed2)]

# Replacing original variables and removing the temporary variables
TT$name <- TT$name2
TT$name2 <- NULL

TT$title <- TT$title2
TT$title2 <- NULL

TT$fed <- TT$fed2
TT$fed2 <- NULL

same_name_examples <- TT %>% count(username, name, title, fed) %>% group_by(name) %>% 
  add_count() %>% filter(nn > 1) %>% arrange(name)
# View(same_name_examples)
# 132 cases still
# In most cases a player had multiple chess.com accounts when playing the titled tuesday tournament
# Usernames are unique though

# Fix username (most often used one)
# "Fan_Of_Chucky" was used more often than "RaugintaBulba" by Augustinas Bazilius, but the latter name was used at the end of 2023 so it would be better to use the newest version.
username_name <- TT %>% count(username, name) %>% count(name) %>% filter(n > 1) %>% pull(name) # 113 cases

username_fix <- TT %>% filter(name %in% username_name) %>% 
  count(name, username) %>% 
  arrange(name, desc(n)) %>% 
  group_by(name) %>% 
  slice_head(n = 1) %>% ungroup() %>% select(-n)

names(username_fix)[2] <- "username2"

TT <- TT %>% left_join(username_fix, by = "name")
TT$username2[is.na(TT$username2)] <- TT$username[is.na(TT$username2)]

# Replacing original variables and removing the temporary variables
TT$username <- TT$username2
TT$username2 <- NULL

# manual FED changes of some players
TT$fed[TT$name == "Anish Giri"] <- "NED"
TT$fed[TT$name == "Christopher Woojin Yoo"] <- "USA"
TT$fed[TT$name == "Maxim Omariev"] <- "RUS"
TT$fed[TT$name == "Nihal Sarin"] <- "IND"
TT$fed[TT$name == "Vladimir Fedoseev"] <- "SVN"
TT$fed[TT$name == "Hovhannes Gabuzyan"] <- "ARM"
TT$fed[TT$name == "Pranesh M"] <- "IND"


# Adding prize money information (money amount payed to the first places)
# ignoring the $100 payed to the highest placed female
prize_money <- data.frame(number = 1:5,
                          prize = c(1000, 750, 350, 200, 100))

TT <- TT %>% left_join(prize_money, by = "number")
TT$prize2 <- TT$prize
TT$prize2[is.na(TT$prize2)] <- 0

saveRDS(object = TT, file = "input/TitledTuesday_2023_df.RDS")
