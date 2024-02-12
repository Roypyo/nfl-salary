
####
dfpbp <- load_pbp(2015:2022)
receiver <- dfpbp %>% 
  filter(!is.na(receiver_player_name))%>% 
  filter(season_type == "REG")

receiver <- receiver %>% 
  select(1:7, 39, 40, 73:74, 81:84, 93:97, 108:111, 122:128, 174:176, 286)

receiver <- receiver %>%
  mutate(receiving_yards = replace(receiving_yards, is.na(receiving_yards), 0)) %>% 
  mutate(yards_after_catch = replace(yards_after_catch, is.na(yards_after_catch), 0)) %>% 
  mutate(air_yards = replace(air_yards, is.na(air_yards), 0))

receiver <- receiver %>%
  group_by(game_id, receiver_player_name, receiver_player_id) %>%
  summarize(
    season = first(season),
    week = first(week),
    ep = sum(ep),
    epa = sum(epa),
    air_epa = sum(air_epa),
    yac_epa = sum(yac_epa),
    comp_air_epa = sum(comp_air_epa),
    comp_yac_epa = sum(comp_yac_epa),
    wp = sum(wp),
    wpa = sum(wpa),
    air_wpa = sum(air_wpa),
    yac_wpa = sum(yac_wpa),
    comp_air_wpa = sum(comp_air_wpa),
    comp_yac_wpa = sum(comp_yac_wpa),
    first_down_pass = sum(first_down_pass),
    first_down_penalty = sum(first_down_penalty),
    third_down_converted = sum(third_down_converted),
    third_down_failed = sum(third_down_failed),
    fourth_down_converted = sum(fourth_down_converted),
    fourth_down_failed = sum(fourth_down_failed),
    receiving_yards = sum(receiving_yards),
    yards_after_catch = sum(yards_after_catch),
    air_yards = sum(air_yards)
  )
receiver <- receiver %>%
  group_by(receiver_player_id) %>%
  mutate(receiver_player_name = first(receiver_player_name))
colSums(is.na(receiver))
receiver <- receiver %>%
  mutate_all(~replace(., is.na(.), 0))
receiver <- receiver %>%
  group_by(season, receiver_player_name, receiver_player_id) %>%
  summarize(
    ep = sum(ep),
    epa = sum(epa),
    air_epa = sum(air_epa),
    yac_epa = sum(yac_epa),
    comp_air_epa = sum(comp_air_epa),
    comp_yac_epa = sum(comp_yac_epa),
    wp = sum(wp),
    wpa = sum(wpa),
    air_wpa = sum(air_wpa),
    yac_wpa = sum(yac_wpa),
    comp_air_wpa = sum(comp_air_wpa),
    comp_yac_wpa = sum(comp_yac_wpa),
    first_down_pass = sum(first_down_pass),
    first_down_penalty = sum(first_down_penalty),
    third_down_converted = sum(third_down_converted),
    third_down_failed = sum(third_down_failed),
    fourth_down_converted = sum(fourth_down_converted),
    fourth_down_failed = sum(fourth_down_failed),
    receiving_yards = sum(receiving_yards),
    yards_after_catch = sum(yards_after_catch),
    air_yards = sum(air_yards)
  )

rusher <- dfpbp %>% 
  filter(!is.na(rusher_player_name))

rusher <- rusher %>% 
  select(1:7, 30, 73:74, 93:97, 121, 123:128, 177:179, 286)

colSums(is.na(rusher))
rusher <- rusher %>%
  mutate(wpa = ifelse(is.na(wpa), 0, wpa),
         rushing_yards = ifelse(is.na(rushing_yards), 0, rushing_yards),
         epa = ifelse(is.na(epa), 0, epa))

rusher <- rusher %>%
  group_by(rusher_player_id) %>%
  mutate(rusher_player_name = first(rusher_player_name))
rusher <- rusher %>%
  mutate(rusher_player_id = ifelse(rusher_player_name == "I.Pacheco" & is.na(rusher_player_id), "00-0037197", rusher_player_id))

rusher <- rusher %>%
  group_by(game_id, rusher_player_name, rusher_player_id) %>%
  summarize(
    season = first(season),
    week = first(week),
    ep = sum(ep),
    epa = sum(epa),
    wp = sum(wp),
    wpa = sum(wpa),
    first_down_rush = sum(first_down_rush),
    first_down_penalty = sum(first_down_penalty),
    third_down_converted = sum(third_down_converted),
    third_down_failed = sum(third_down_failed),
    fourth_down_converted = sum(fourth_down_converted),
    fourth_down_failed = sum(fourth_down_failed),
    rushing_yards = sum(rushing_yards)
  )

rusher <- rusher %>%
  group_by(season, rusher_player_name, rusher_player_id) %>%
  summarize(
    ep = sum(ep),
    epa = sum(epa),
    wp = sum(wp),
    wpa = sum(wpa),
    first_down_rush = sum(first_down_rush),
    first_down_penalty = sum(first_down_penalty),
    third_down_converted = sum(third_down_converted),
    third_down_failed = sum(third_down_failed),
    fourth_down_converted = sum(fourth_down_converted),
    fourth_down_failed = sum(fourth_down_failed),
    rushing_yards = sum(rushing_yards)
  )
rusher <- rusher %>%
  rename(player_id = rusher_player_id) %>% 
  rename(wp_change_rush = wp) %>% 
  rename(wpa_change_rush = wpa) %>% 
  rename(ep_rush = ep) %>% 
  rename(epa_rush = epa) %>% 
  rename(first_down_penalty_rush = first_down_penalty) %>% 
  rename(third_down_converted_rush = third_down_converted) %>% 
  rename(third_down_failed_rush = third_down_failed) %>% 
  rename(fourth_down_converted_rush = fourth_down_converted) %>% 
  rename(fourth_down_failed_rush = fourth_down_failed)
receiver <- receiver %>%
  rename(player_id = receiver_player_id) %>% 
  rename(wp_change_rec = wp) %>% 
  rename(wpa_change_rec = wpa) %>% 
  rename(ep_rec = ep) %>% 
  rename(epa_rec = epa) %>% 
  rename(first_down_penalty_rec = first_down_penalty) %>% 
  rename(third_down_converted_rec = third_down_converted) %>% 
  rename(third_down_failed_rec = third_down_failed) %>% 
  rename(fourth_down_converted_rec = fourth_down_converted) %>% 
  rename(fourth_down_failed_rec = fourth_down_failed)

player <- receiver %>%
  left_join(rusher, by = c("player_id", "season")) %>% 
  select(-rusher_player_name)
player <- player %>%
  mutate_all(~replace(., is.na(.), 0))

RWT2 <- RWT2 %>% 
  select(-rushing_yards, -rushing_first_downs, -rushing_epa, -rushing_first_downs, -receiving_yards, -receiving_air_yards, -receiving_yards_after_catch,
         -receiving_epa)
playerwpa <- player %>%
  left_join(RWT2, by = c("player_id", "season")) %>% 
  select(-Player)

colnames(playerwpa)[c(2)] <- c("Player")
playerwpa <- playerwpa %>%
  mutate(Player = ifelse(player_id == "00-0035676", "A.J. Brown", Player)) %>% 
  mutate(Player = ifelse(player_id == "00-0027942", "A.J. Green", Player)) %>% 
  mutate(Player = ifelse(player_id == "00-0031547", "DeVante Parker", Player)) %>% 
  mutate(Player = ifelse(player_id == "00-0032089", "Brian Parker", Player))%>%
  mutate(player_id = ifelse(Player == "D.Johnson" & player_id != "00-0026957", "00-0032187", player_id))

weekly <- calculate_player_stats(dfpbp, weekly = TRUE)
unique(weekly$position_group)
weekly <- weekly %>%
  dplyr::select(-player_name, -headshot_url, -position)
weekly[is.na(weekly)] <- 0

colSums(is.na(RWT))
RWT <- weekly %>% 
  filter(position_group %in% c("RB", "WR", "TE")) %>% 
  filter(season_type == "REG") %>% 
  group_by(player_display_name, season)

str(RWT)

RWT <- RWT %>% 
  select(-season_type, -interceptions, -sacks, -sack_yards, -sack_fumbles, -sack_fumbles_lost,
         -completions, -attempts, -passing_yards, -passing_tds, -passing_air_yards, -passing_yards_after_catch, -passing_first_downs,
         -passing_epa, -passing_2pt_conversions, -pacr, -dakota, -rushing_2pt_conversions, -receiving_2pt_conversions, -special_teams_tds,
         -fantasy_points, -fantasy_points_ppr)
RWT[is.na(RWT)] <- 0
RWT <- RWT %>%
  group_by(player_id,player_display_name, season) %>%
  
  summarize(
    carries = sum(carries),
    rushing_yards = sum(rushing_yards),
    rushing_tds = sum(rushing_tds),
    rushing_fumbles = sum(rushing_fumbles),
    rushing_fumbles_lost = sum(rushing_fumbles_lost),
    rushing_first_downs = sum(rushing_first_downs),
    rushing_epa = sum(rushing_epa),
    receptions = sum(receptions),
    targets = sum(targets),
    receiving_yards = sum(receiving_yards),
    receiving_tds = sum(receiving_tds),
    receiving_fumbles = sum(receiving_fumbles),
    receiving_fumbles_lost = sum(receiving_fumbles_lost),
    receiving_air_yards = sum(receiving_air_yards),
    receiving_yards_after_catch = sum(receiving_yards_after_catch),
    receiving_first_downs = sum(receiving_first_downs),
    receiving_epa = sum(receiving_epa),
    racr = mean(racr),
    target_share = sum(target_share),
    air_yards_share = sum(air_yards_share),
    wopr = mean(wopr)
  ) %>%
  ungroup()
RWT <- RWT %>% 
  select(1:3)

playerwpa <- left_join(RWT, playerwpa, by = c("player_id", "season"))  
save(playerwpa, file = "playerwpa.Rdata")
load("playerwpa.Rdata")
colnames(playerwpa)[c(2)] <- c("player")
playerwpa <- playerwpa %>%
  filter(Player != "David Johnson")
load("salRWT.Rdata")
finalplayer <- left_join(playerwpa, salRWT, by = c("player", "season"))
finalplayer <- finalplayer %>%
  filter(!is.na(value))##if the value is NA, the player was Kickreturner, FB etc outside of the RB, WR, TE
colSums(is.na(finalplayer))
finalplayer <- finalplayer %>% 
  filter(!is.na(carries))
save(finalplayer, file = "finalplayer.Rdata")
load("finalplayer.Rdata")


summary(finalplayer)
finalplayer <- finalplayer %>% 
  mutate(tot_epa = epa_rec + epa_rush) %>% 
  mutate(tot_wpa = wpa_change_rec + wpa_change_rush) %>% 
  select(-epa_rec, -epa_rush, -wpa_change_rec, -wpa_change_rush)

finalplayer <- finalplayer %>% 
  mutate(weightvalue = (value + log(value/(value-guaranteed)))/years) %>% 
  mutate(weightvalue = ifelse(is.infinite(weightvalue) | is.nan(weightvalue), apy, weightvalue))

finalplayer <- finalplayer %>% 
  select(-Player, -years, -value, -guaranteed)
save(finalplayer, file = "finalplayer.Rdata")
load("finalplayer.Rdata")

ryoe <- read.csv("15_23_ryoe_test.csv")
pyoe <- read.csv("15_23_pyoe_test.csv")
pyoe_level <- read.csv("15_23_pyoe_include_passlevel.csv")
playrate <- read.csv("playrate.csv")
playrate <- playrate %>% 
  filter(season >= 2015 & season <= 2022)
colnames(ryoe)[c(4)] <- c("player_id")
colnames(pyoe)[c(4)] <- c("player_id")
colnames(pyoe_level)[c(4)] <- c("player_id")
colnames(playrate)[c(4)] <- c("player_id")
ryoe <- ryoe %>% 
  select(-X, -posteam, -yards_per_carry)
pyoe <- pyoe %>% 
  select(-X, -posteam, -yards_per_rec)
pyoe_level <- pyoe_level %>% 
  select(-X, -posteam, -yards_per_rec, -receiver)
playrate <- playrate %>% 
  select(-X, -posteam, -rush, -pass)


final <- left_join(finalplayer, ryoe, by = c("player_id", "season"))
final <- left_join(final, pyoe_level, by = c("player_id", "season"))
final <- left_join(final, playrate, by = c("player_id", "season")) 
final <- final %>% 
  select(-rush_cnt, -rec_cnt, -rusher.x, -rusher.y, -tot_epa, -year_signed, -apy)
rows_with_zero_weightvalue <- final[final$weightvalue == 0, ]

final <- final %>%
  filter(targets >= 20 | carries >= 30)
colSums(is.na(final))
final[is.na(final)] <- 0
final <- final %>%
  distinct(player, season, .keep_all = TRUE)
save(final, file = "final.Rdata")

setwd("C:/Users/roymy/OneDrive/바탕화~2-DESKTOP-TTPA583-6709/STAT6341/Final Project")
source("library.R")
load("final.Rdata")

player_split <- initial_split(final, prop = 0.7)
player_train <- training(player_split)
player_test <- testing(player_split)

all.names <- colnames(final)[c(14:22, 25:45, 48, 56:60)]
player_train <- player_train %>% 
  dplyr::select(all_of(all.names))

playerrc <- recipe(weightvalue ~ ., data = player_train) %>% 
  step_normalize(all_predictors()) %>% 
  step_YeoJohnson(all_predictors())

player_prep <- prep(playerrc)
player_juice <- juice(player_prep)
player_bake <- bake(player_prep, final)
#Choose repeated 10-fold CV
set.seed(1111)
tidy_kfolds <- vfold_cv(player_train, v = 10, repeats = 5, strata = "weightvalue")

tidy_glmnet <- linear_reg() %>% 
  set_mode("regression") %>%
  set_engine("glm")

#Specify modeling procedure
tidy_wf <- workflow() %>% 
  add_recipe(playerrc) %>% 
  add_model(tidy_glmnet)

tidy_tune <- fit_resamples(
  tidy_wf,
  resamples = tidy_kfolds,
  metrics = metric_set(rmse, rsq, mae))

#1) Training Data RMSE
tidy_tune %>% collect_metrics()

tidy_tune %>% 
  show_best("rmse")

tidy_best_tune <- tidy_tune %>% select_best("rmse")
tidy_best_tune

tidy_final_workflow <- tidy_wf %>% 
  finalize_workflow(tidy_best_tune)
tidy_final_workflow

tidy_final_workflow %>%
  fit(player_train) %>%
  extract_fit_parsnip() %>%
  tidy()%>%
  print(n = 38)


#2) Test Data RMSE
test_data <- tidy_final_workflow %>% 
  last_fit(player_split, metrics = metric_set(rmse, rsq, mae)) %>% 
  collect_metrics() %>%
  filter(.metric=="rmse") %>%
  .[[".estimate"]]

predict <- tidy_final_workflow %>% 
  last_fit(player_split) %>% 
  collect_predictions

predict_all <- predict(tidy_final_workflow %>% fit(player_juice), player_bake)


predicted_values <- predict %>%
  select(.pred) %>%
  rename(predicted_weightvalue = .pred)

comparison_df <- bind_cols(player_test$season,player_test$player, player_test$position, player_test$weightvalue, predicted_values)

means_by_position <- comparison_df %>%
  group_by(...3) %>%
  summarize(
    mean_weightvalue = mean(...4, na.rm = TRUE),
    mean_predicted_weightvalue = mean(predicted_weightvalue, na.rm = TRUE)
  )

final <- bind_cols(final, predict_all)

###Random Forest
library(ranger)
library(gridExtra)
library(finetune)

final_ranger_tune <- rand_forest(trees = 10*(length(all.names)-1),
                                 mtry = tune(),
                                 min_n = tune()) %>% 
                      set_mode("regression") %>% 
                      set_engine("ranger", seed = 123, num.threads = 7)

final_ranger_wf <- workflow() %>% 
                    add_recipe(playerrc) %>% 
                    add_model(final_ranger_tune)

tuneboth_param <- extract_parameter_set_dials(final_ranger_tune) %>% 
  update(mtry = mtry(c(1, length(all.names)-1)))

final_grid_tune <- grid_max_entropy(tuneboth_param, size=10)

# final_ranger_tune_results <- tune_grid(
#   final_ranger_wf,
#   resamples = tidy_kfolds,
#   grid = final_grid_tune,
#   metrics = metric_set(rmse))
# save(final_ranger_tune_results, file = "tune_RF.Rdata")
load("tune_RF.Rdata")

final_ranger_tune_results %>% 
  collect_metrics()

top5 <- final_ranger_tune_results %>% 
  show_best("rmse", n = 5)

grid_best_tune <- final_ranger_tune_results %>% select_best("rmse")
grid_best_tune

final_ranger_tidy <- finalize_model(final_ranger_tune, grid_best_tune)

#Note that we need to update our workflow
ranger_wf3 <- workflow() %>% 
  add_recipe(playerrc) %>% 
  add_model(final_ranger_tidy)
ranger_wf3

ranger_wf3 %>% 
  last_fit(player_split, metrics = metric_set(rmse)) %>% 
  collect_predictions() %>%
  rmse(estimate=.pred, truth=weightvalue)

rf_final_workflow <- ranger_wf3 %>% 
  finalize_workflow(grid_best_tune)
rf_final_workflow

rf_rmse_test <- rf_final_workflow %>% 
  last_fit(player_split, metrics = metric_set(rmse, rsq, mae)) %>% 
  collect_metrics() %>%
  filter(.metric=="rmse") %>%
  .[[".estimate"]]


predict_rf <- rf_final_workflow %>% 
  last_fit(player_split) %>% 
  collect_predictions

predict_all_rf <- predict(rf_final_workflow %>% fit(player_juice), player_bake)

predicted_values_rf <- predict_rf %>%
  select(.pred) %>%
  rename(predicted_weightvalue = .pred)

comparison_df_rf <- bind_cols(player_test$season, player_test$player, player_test$position, player_test$weightvalue, predicted_values_rf)

means_by_position_rf <- comparison_df_rf %>%
  group_by(...3) %>%
  summarize(
    mean_weightvalue = mean(...4, na.rm = TRUE),
    mean_predicted_weightvalue = mean(predicted_weightvalue, na.rm = TRUE)
  )

final <- bind_cols(final, predict_all_rf)

ranger.final <- ranger(weightvalue ~ ., data = player_juice,
                       num.trees       = 2000,
                       mtry            = 5,
                       min.node.size   = 1,
                       replace         = TRUE,
                       sample.fraction = 0.75,
                       seed            = 123,
                       respect.unordered.factors = 'order',
                       importance      = 'impurity'
)


pfun.rf <- function(object, newdata)predict(object, data = newdata)$predictions

predictor.rf <- Predictor$new(model = ranger.final, data = player_juice[,-36], y = player_juice[,36], predict.fun = pfun.rf)

imp.rf <- FeatureImp$new(predictor.rf, loss = "rmse")
plot(imp.rf)

final.ale.rf <- FeatureEffect$new(predictor.rf, feature = "wopr", method='ale')
final.ale.rf$plot()

final.ale.rf <- FeatureEffect$new(predictor.rf, feature = "first_down_pass", method='ale')
final.ale.rf$plot()

final.ale.rf <- FeatureEffect$new(predictor.rf, feature = "target_share", method='ale')
final.ale.rf$plot()



##XGBoost
library(xgboost)
library(iml)
library(future)
library(future.callr)

final_xgb_tidy <- boost_tree(
  mode = "regression",
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  stop_iter = tune()) %>%
  set_engine("xgboost", nthread = 7)


final_xgb_param <- extract_parameter_set_dials(final_xgb_tidy) %>% 
  update(mtry = mtry(c(1, length(all.names)-1)))

xgboost_grid <- grid_max_entropy(final_xgb_param, size = 24)

final_xgb_wf <- workflow() %>% 
  add_recipe(playerrc) %>% 
  add_model(final_xgb_tidy)

# final_xgb_tune_results <- tune_grid(
#   final_xgb_wf,
#   resamples = tidy_kfolds,
#   grid = xgboost_grid,
#   metrics = metric_set(rmse))
# save(final_xgb_tune_results, file = "xgb.Rdata")
load("xgb.Rdata")

xgb_best_tune <- final_xgb_tune_results %>% select_best("rmse")
xgb_best_tune

final_xgb_tidy <- finalize_model(final_xgb_tidy, xgb_best_tune)

#Note that we need to update our workflow
xgb_wf <- workflow() %>% 
  add_recipe(playerrc) %>% 
  add_model(final_xgb_tidy)
xgb_wf

xgb_wf %>% 
  last_fit(player_split, metrics = metric_set(rmse)) %>% 
  collect_predictions() %>%
  rmse(estimate=.pred, truth=weightvalue)

xgb_final_workflow <- xgb_wf %>% 
  finalize_workflow(xgb_best_tune)
xgb_final_workflow

xgb_fit <- xgb_final_workflow %>%
  finalize_workflow(final_xgb_tune_results %>% select_best("rmse")) %>%
  last_fit(player_split, 
           metrics = metric_set(rmse, rsq, mae)) %>% 
  collect_metrics() %>% 
  select(-c(".estimator", ".config")) %>%
  rename(xgb_estimates = .estimate)

predict_xgb <- xgb_final_workflow %>% 
  last_fit(player_split) %>% 
  collect_predictions

predict_all_xgb <- predict(xgb_final_workflow %>% fit(player_juice), player_bake)


predicted_values_xgb <- predict_xgb %>%
  select(.pred) %>%
  rename(predicted_weightvalue = .pred)

comparison_df_xgb <- bind_cols(player_test$season, player_test$player, player_test$position, player_test$weightvalue, predicted_values_xgb)

means_by_position_xgb <- comparison_df_xgb %>%
  group_by(...3) %>%
  summarize(
    mean_weightvalue = mean(...4, na.rm = TRUE),
    mean_predicted_weightvalue = mean(predicted_weightvalue, na.rm = TRUE)
  )

final <- bind_cols(final, predict_all_xgb)

final$predicted_value_lm <- final$.pred...61
final$predicted_value_rf <- final$.pred...62
final$predicted_value_xgb <- final$.pred...63

final_comparison <- final[, c(2, 3, 14:22, 25:46, 48, 56:60, 64:66)]
final_comparison_value <- final_comparison[,c(1,2,33:34, 40:42)]
save(final_comparison_value, file = "final_comparison.Rdata")
load("final_comparison.Rdata")

final_xgb <- xgb_wf %>%
  finalize_workflow(final_xgb_tune_results %>% select_best("rmse")) %>%
  last_fit(player_split, 
           metrics = metric_set(yardstick::rmse)) %>%
  extract_fit_engine()

pfun.xgb <- function(model, newdata){
  newData_x <- xgb.DMatrix(data.matrix(newdata), missing = NA)
  results <- predict(model, newData_x)
  return(results)
}

predictor.xgb <- Predictor$new(model = final_xgb, data = player_juice[,-36], y = player_juice[,36],
                               predict.fun = pfun.xgb)

##Feature importance - uses permutations so takes a long time to run, but allows for CI's
imp.xgb <- FeatureImp$new(predictor.xgb, loss = "rmse")
plot(imp.xgb)

pitch.ale.xgb <- FeatureEffect$new(predictor.xgb, feature = "wopr", method='ale')
pitch.ale.xgb$plot()

pitch.ale.xgb <- FeatureEffect$new(predictor.xgb, feature = "target_share", method='ale')
pitch.ale.xgb$plot()

pitch.ale.xgb <- FeatureEffect$new(predictor.xgb, feature = "first_down_pass", method='ale')
pitch.ale.xgb$plot()

mean_values_by_position <- final_comparison_value %>%
  filter(position == "TE") %>% 
  summarise(
    mean_weightvalue = mean(weightvalue, na.rm = TRUE),
    mean_predicted_value_lm = mean(predicted_value_lm, na.rm = TRUE),
    mean_predicted_value_rf = mean(predicted_value_rf, na.rm = TRUE),
    mean_predicted_value_xgb = mean(predicted_value_xgb, na.rm = TRUE)
  ) 

mean_values_by_position_RB <- final_comparison_value %>%
  filter(position == "RB") %>% 
  summarise(
    mean_weightvalue = mean(weightvalue, na.rm = TRUE),
    mean_predicted_value_lm = mean(predicted_value_lm, na.rm = TRUE),
    mean_predicted_value_rf = mean(predicted_value_rf, na.rm = TRUE),
    mean_predicted_value_xgb = mean(predicted_value_xgb, na.rm = TRUE)
  ) 
mean_values_by_position_wr <- final_comparison_value %>%
  filter(position == "WR") %>% 
  summarise(
    mean_weightvalue = mean(weightvalue, na.rm = TRUE),
    mean_predicted_value_lm = mean(predicted_value_lm, na.rm = TRUE),
    mean_predicted_value_rf = mean(predicted_value_rf, na.rm = TRUE),
    mean_predicted_value_xgb = mean(predicted_value_xgb, na.rm = TRUE)
  ) 

final_comparison_mean_value <- bind_rows(mean_values_by_position, mean_values_by_position_RB, mean_values_by_position_wr)
final_comparison_mean_value$position <- c("TE","RB","WR")
save(final_comparison_mean_value, file = "final_comparison_mean_value.Rdata")


final_comparison <- final_comparison %>% 
  mutate(avg.predict = rowMeans(select(., 40:42), na.rm = T))

final_comparison <- final_comparison %>% 
  mutate(paid = ifelse(avg.predict > weightvalue, "Underpaid",
                       ifelse(avg.predict < weightvalue, "Overpaid", "Equal")))

percentage <- final_comparison %>% 
  group_by(position) %>% 
  summarise(
    underpaid = mean(paid == "Underpaid") * 100,
    overpaid = mean(paid == "Overpaid") * 100
  )

final_comparison <- final_comparison %>%
  mutate(underdifference = avg.predict - weightvalue)

top_underpaid <- final_comparison %>%
  arrange(desc(underdifference)) %>%
  head(10)

final_comparison <- final_comparison %>%
  mutate(overdifference = weightvalue - avg.predict)

top_overpaid <- final_comparison %>%
  arrange(desc(overdifference)) %>%
  head(10)
