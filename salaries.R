setwd("C:/Users/roymy/OneDrive/바탕화~2-DESKTOP-TTPA583-6709/STAT6341/Final Project")
source("library.R")
library(nflreadr)

sal <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds"))
salqb <- sal %>% 
  filter(position == "QB"& year_signed >= 2010) %>% 
  group_by(player, position)
salqb <- salqb %>%
  unnest_wider(cols, names_sep = "_")

cols_to_unnest <- c(
  "cols_year",
  "cols_team",
  "cols_base_salary",
  "cols_prorated_bonus",
  "cols_roster_bonus",
  "cols_guaranteed_salary",
  "cols_cap_number",
  "cols_cap_percent",
  "cols_cash_paid",
  "cols_workout_bonus",
  "cols_other_bonus",
  "cols_per_game_roster_bonus"
)

# Unnest the specified columns
salqb <- salqb %>%
  unnest(cols_to_unnest, names_sep = "_")
salqb <- salqb %>% 
  filter(cols_year>=2010)
salqb$player <- gsub("\\.", "", salqb$player)
salqb$player <- gsub("Matt Stafford", "Matthew Stafford", salqb$player)
salqb$player <- gsub("Gardner Minshew", "Gardner Minshew II", salqb$player)
salqb <- distinct(salqb)
salqb_filt <- salqb %>% 
  select(-team, -apy_cap_pct, -inflated_value, 
         -inflated_apy, -inflated_guaranteed, -cols_base_salary, -cols_prorated_bonus, 
         -cols_roster_bonus, -cols_guaranteed_salary, -cols_cap_number, -cols_cap_percent, -cols_cash_paid, -cols_workout_bonus, 
         -cols_other_bonus, -cols_per_game_roster_bonus)
salqb_filt <- salqb_filt %>%
  filter(cols_year >= year_signed)

salqb_filt <- salqb_filt %>%
  arrange(player, year_signed, cols_year) %>%
  group_by(player, cols_year) %>%
  filter(year_signed == max(year_signed)) %>%
  distinct(player, cols_year, .keep_all = TRUE) %>%
  ungroup()
salqb_filt <- salqb_filt %>% 
  select(-is_active, -player_page, -date_of_birth, -height, -weight, -college, -draft_year, -draft_round, -draft_overall, -draft_team,
         -cols_team)
colnames(salqb_filt)[colnames(salqb_filt) == "player"] <- "Player"
colnames(salqb_filt)[colnames(salqb_filt) == "cols_year"] <- "season"
save(salqb_filt, file ="salqb.Rdata")
load("salqb.Rdata")
write.csv(salqb_filt, file = "salqb.csv")


sal <- load_contracts(file_type = getOption("nflreadr.prefer", default = "rds"))
salRWT <- sal %>% 
  filter(position %in% c("RB", "WR" ,"TE") & year_signed >= 2010) 
salRWT <- salRWT %>%
  unnest_wider(cols, names_sep = "_")

cols_to_unnest <- c(
  "cols_year",
  "cols_team",
  "cols_base_salary",
  "cols_prorated_bonus",
  "cols_roster_bonus",
  "cols_guaranteed_salary",
  "cols_cap_number",
  "cols_cap_percent",
  "cols_cash_paid",
  "cols_workout_bonus",
  "cols_other_bonus",
  "cols_per_game_roster_bonus"
)
# Unnest the specified columns
salRWT <- salRWT %>%
  unnest(cols_to_unnest, names_sep = "_")
salRWT <- distinct(salRWT) %>% 
  filter(cols_year>=2010)
salRWT <- salRWT %>% 
  select(-team, -apy_cap_pct, -inflated_value, 
         -inflated_apy, -inflated_guaranteed, -cols_base_salary, -cols_prorated_bonus, 
         -cols_roster_bonus, -cols_guaranteed_salary, -cols_cap_number, -cols_cap_percent, -cols_cash_paid, -cols_workout_bonus, 
         -cols_other_bonus, -cols_per_game_roster_bonus)
salRWT <- distinct(salRWT)
salRWT <- salRWT %>% 
  select(-otc_id)
salRWT <- salRWT %>%
  filter(cols_year >= year_signed)
salRWT <- salRWT %>%
  arrange(player, position, year_signed, cols_year) %>%
  group_by(player, cols_year) %>%
  filter(year_signed == max(year_signed)) %>%
  distinct(player, cols_year, .keep_all = TRUE) %>%
  ungroup()

salRWT <- salRWT %>% 
  select(-player_page, -is_active, -date_of_birth, -height, -weight, -college, -draft_year, -draft_round, -draft_overall, -draft_team,
         -cols_team)
colnames(salRWT)[colnames(salRWT) == "cols_year"] <- "season"
salRWT$season <- as.numeric(salRWT$season)
save(salRWT, file ="salRWT.Rdata")
load("salRWT.Rdata")
write.csv(salRWT, file = "salRWT.csv")
