

ggsave(units = "in", filename = "fig2_prize_money_6by8_300dpi_noGrid.pdf",
       path = "E:/The Data Digest/GitHub/Chess/images/", 
       width = 8, height = 6, device='pdf', dpi=300)


ggsave(units = "in", filename = "fig5_hikaru_matches_8by6_300dpi_noGrid.png",
       path = "E:/The Data Digest/GitHub/Chess/images/", 
       width = 8, height = 6, device='png', dpi=300)

ggsave(units = "in", filename = "fig5_hikaru_matches_8by6_300dpi_noGrid.pdf",
       path = "E:/The Data Digest/GitHub/Chess/images/", 
       width = 8, height = 6, device='pdf', dpi=300)


ggsave(units = "in", filename = "fig6_magnus_matches_8by6_300dpi_noGrid.png",
       path = "E:/The Data Digest/GitHub/Chess/images/", 
       width = 8, height = 5, device='png', dpi=300)

ggsave(units = "in", filename = "fig6_magnus_matches_8by6_300dpi_noGrid.pdf",
       path = "E:/The Data Digest/GitHub/Chess/images/", 
       width = 8, height = 5, device='pdf', dpi=300)


TT_magnus <- TT %>% filter(username == "MagnusCarlsen") %>% pull(date_time)
TT_hikaru <- TT %>% filter(username == "Hikaru") %>% pull(date_time)

date_both <- intersect(TT_magnus, TT_hikaru)
#27 tournament

TT %>% filter(date_time %in% date_both) %>% 
  filter(username %in% c("MagnusCarlsen", "Hikaru")) %>% View()
  group_by(username) %>% 
  summarize(median_place = median(number),
            avg_place = mean(number),
            avg_score = mean(score),
            avg_prize = mean(prize2),
            avg_games_played = mean(games_played))
