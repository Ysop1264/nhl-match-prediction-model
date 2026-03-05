library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(stats) # For regressions
library(zoo) # For rolling means and lags
library(MASS) # For ordered-logit model


# Importing relevant datasets (please import from your own wd when replicating)
playergame_raw <- read.csv("C:/Users/Gebruiker/Desktop/UNI/Year 4/Blok 2/Sports Economics/Assignment1_SportsEcon/se_assignment1_14_playergamedata.csv")
gamedata_raw <- read.csv("C:/Users/Gebruiker/Desktop/UNI/Year 4/Blok 2/Sports Economics/Assignment1_SportsEcon/se_assignment1_14_gamedata.csv")

# Sanity checks
print(unique(playergame_raw$teamname))
print(n_distinct(playergame_raw$teamname))

# Team dataset where one row is one team game
teamdata_gamelvl <- playergame_raw %>%
  group_by(gameId, season ,teamname, teamid, home) %>%
  summarise(
    goals_team = sum(goals, na.rm = TRUE),
    assists_team = sum(assists, na.rm = TRUE),
    shots_team = sum(shots, na.rm = TRUE),
    plusMinus_team = sum(plusMinus, na.rm = TRUE),
    toi_team = sum(toi_seconds),
    team_goals_reg = first(team_goals_reg),
    home_goals_reg_team = first(home_goals_reg),
    away_goals_reg_team = first(away_goals_reg),
    result_reg = first(result_reg),
    .groups = 'drop'
  ) %>%
  mutate(
    team_win = as.integer((home == 1 & result_reg == 2) |
                            (home == 0 & result_reg == 0)),
    goal_diff = ifelse(home == 1,
                       home_goals_reg_team - away_goals_reg_team,
                       away_goals_reg_team - home_goals_reg_team),
    # goals for/against
    goals_for     = goals_team,
    goals_against = ifelse(home == 1,
                           away_goals_reg_team,
                           home_goals_reg_team)
  )

# opponent shots so we can get shots_against
shot_pairs <- teamdata_gamelvl %>%
  dplyr::select(gameId, teamid, shots_team)

teamdata_gamelvl <- teamdata_gamelvl %>%
  left_join(shot_pairs, by = "gameId", suffix = c("", "_opp")) %>%
  filter(teamid != teamid_opp) %>%
  mutate(
    shots_against = shots_team_opp
  ) %>%
  dplyr::select(-teamid_opp, -shots_team_opp) 

# Adding toi by position data
toi_by_pos <- playergame_raw %>%
  group_by(gameId, season, teamname, teamid, home) %>%
  summarise(
    toi_fwd = sum(ifelse(position %in% c("C","L","R"), toi_seconds, 0), na.rm = TRUE),
    toi_def = sum(ifelse(position %in% c("D"), toi_seconds, 0), na.rm = TRUE),
    .groups = "drop"
  )

# Merging with the existing team-game data
teamdata_gamelvl <- teamdata_gamelvl %>%
  left_join(toi_by_pos,
            by = c("gameId", "season", "teamname", "teamid", "home"))

# Calculating some rolling statistics by game
# Function to create lagged_rolling_stats
make_team_lagged_features <- function(teamdata_gamelvl, k){
  teamdata_gamelvl <- teamdata_gamelvl %>%
    arrange(teamid, season, gameId) %>% 
    group_by(teamid) %>%
    mutate(
      # rolling means EXCLUDING current game
      goals_roll_ex = lag(rollapply(goals_team, k, mean, fill = NA, align = "right")),
      shots_roll_ex = lag(rollapply(shots_team, k, mean, fill = NA, align = "right")),
      gd_roll_ex    = lag(rollapply(goal_diff, k, mean, fill = NA, align = "right")),
      win_roll_ex   = lag(rollapply(team_win, k, mean, fill = NA, align = "right")),
      toi_fwd_roll_ex = lag(rollapply(toi_fwd, k, mean, fill = NA, align = "right")),
      toi_def_roll_ex = lag(rollapply(toi_def, k, mean, fill = NA, align = "right")),
      goals_conc_roll_ex  = lag(rollapply(goals_against,  k, mean, fill = NA, align = "right")),
      shots_conc_roll_ex  = lag(rollapply(shots_against,  k, mean, fill = NA, align = "right"))
    ) %>%
    ungroup()
}

join_team_features <- function(gamedata_raw, teamdata_lagged){
  # Home team pre-match features
  home_feats <- teamdata_lagged %>%
    filter(home == 1) %>%
    dplyr::select(
      gameId,
      teamname,
      goals_lastk_home          = goals_roll_ex,
      shots_lastk_home          = shots_roll_ex,
      gd_lastk_home             = gd_roll_ex,
      win_lastk_home            = win_roll_ex,
      toi_fwd_lastk_home        = toi_fwd_roll_ex,
      toi_def_lastk_home        = toi_def_roll_ex,
      goals_conceded_lastk_home = goals_conc_roll_ex,   
      shots_conceded_lastk_home = shots_conc_roll_ex    
    )
  
  # Away team pre-match features
  away_feats <- teamdata_lagged %>%
    filter(home == 0) %>%
    dplyr::select(
      gameId,
      teamname,
      goals_lastk_away          = goals_roll_ex,
      shots_lastk_away          = shots_roll_ex,
      gd_lastk_away             = gd_roll_ex,
      win_lastk_away            = win_roll_ex,
      toi_fwd_lastk_away        = toi_fwd_roll_ex,
      toi_def_lastk_away        = toi_def_roll_ex,
      goals_conceded_lastk_away = goals_conc_roll_ex, 
      shots_conceded_lastk_away = shots_conc_roll_ex    
    )
  
  gamedata_raw %>%
    left_join(home_feats,
              by = c("gameId", "teamname_home" = "teamname")) %>%
    left_join(away_feats,
              by = c("gameId", "teamname_away" = "teamname"))
}

# Multiclass log-loss for A/D/H
logloss_multiclass <- function(actual, predicted_probs) {
  # actual: factor, result_3way with levels c("A","D","H")
  # predicted_probs: matrix/data.frame with columns named "A","D","H" (any order)
  
  if (is.data.frame(predicted_probs)) {
    predicted_probs <- as.matrix(predicted_probs)
  }
  
  classes <- levels(actual)
  N <- length(actual)
  
  # reorder/select columns to match factor levels
  predicted_probs <- predicted_probs[, classes, drop = FALSE]
  
  # sanity check
  if (nrow(predicted_probs) != N) {
    stop("Rows in predicted_probs (", nrow(predicted_probs),
         ") do not match length of actual (", N, ").")
  }
  
  # clipping probabilities
  eps <- 1e-15
  predicted_probs <- pmax(pmin(predicted_probs, 1 - eps), eps)
  
  # prob of the true class for each row
  idx <- cbind(1:N, as.integer(actual))
  p_true <- predicted_probs[idx]
  
  -mean(log(p_true))
}

windows <- c(3, 5, 7, 10, 15)
results <- data.frame(window = windows, logloss = NA_real_)

for (k in windows) {
  
  # Rolling features with window k
  teamdata_k <- make_team_lagged_features(teamdata_gamelvl, k)
  
  # Join to game-level and build diffs
  gamedata_k <- join_team_features(gamedata_raw, teamdata_k) %>%
    mutate(
      result_3way = factor(result_reg, levels = c(0, 1, 2),
                           labels = c("A", "D", "H")),
      shots_diff_lastk   = shots_lastk_home   - shots_lastk_away,
      gd_diff_lastk      = gd_lastk_home      - gd_lastk_away,
      win_diff_lastk     = win_lastk_home     - win_lastk_away,
      toi_fwd_diff_lastk = toi_fwd_lastk_home - toi_fwd_lastk_away,
      toi_def_diff_lastk = toi_def_lastk_home - toi_def_lastk_away,
      goals_conceded_diff_lastk = goals_conceded_lastk_home - goals_conceded_lastk_away,
      shots_conceded_diff_lastk = shots_conceded_lastk_home - shots_conceded_lastk_away
    ) %>%
    filter(
      !is.na(shots_diff_lastk), !is.na(gd_diff_lastk), !is.na(win_diff_lastk),
      !is.na(toi_fwd_diff_lastk), !is.na(toi_def_diff_lastk),
      !is.na(goals_conceded_diff_lastk), !is.na(shots_conceded_diff_lastk)
    )
  
  # Train/validation split
  train_k <- filter(gamedata_k, season <= 2018)
  valid_k <- filter(gamedata_k, season %in% c(2019, 2020))
  
  # Ordered logit using the diff features
  m_k <- polr(
    result_3way ~ 
      shots_diff_lastk + gd_diff_lastk + win_diff_lastk + toi_fwd_diff_lastk +
      toi_def_diff_lastk + goals_conceded_diff_lastk + 
      shots_conceded_diff_lastk,
    data   = train_k,
    method = "logistic"
  )
  
  # Predicted probs on validation set
  p_valid <- predict(m_k, newdata = valid_k, type = "probs")
  
  # Multiclass log-loss
  results$logloss[results$window == k] <-
    logloss_multiclass(valid_k$result_3way, p_valid)
}

results

k_opt <- results$window[which.min(results$logloss)]
k_opt

# Rolling features with optimal window
teamdata_lagged <- make_team_lagged_features(teamdata_gamelvl, k_opt)

gamedata_model <- join_team_features(gamedata_raw, teamdata_lagged) %>%
  mutate(
    result_3way = factor(result_reg, levels = c(0, 1, 2),
                         labels = c("A", "D", "H")),
    home_win = as.integer(result_reg == 2),  # keep if you still want binary later
    
    # Differences in rolling performance (home - away)
    shots_diff_lastk   = shots_lastk_home   - shots_lastk_away,
    gd_diff_lastk      = gd_lastk_home      - gd_lastk_away,
    win_diff_lastk     = win_lastk_home     - win_lastk_away,
    toi_fwd_diff_lastk = toi_fwd_lastk_home - toi_fwd_lastk_away,
    toi_def_diff_lastk = toi_def_lastk_home - toi_def_lastk_away,
    goals_conceded_diff_lastk = goals_conceded_lastk_home - goals_conceded_lastk_away,
    shots_conceded_diff_lastk = shots_conceded_lastk_home - shots_conceded_lastk_away
  ) %>%
  filter(
    !is.na(shots_lastk_home), !is.na(shots_lastk_away), !is.na(gd_lastk_home),
    !is.na(gd_lastk_away), !is.na(win_lastk_home), !is.na(win_lastk_away),
    !is.na(toi_fwd_lastk_home), !is.na(toi_fwd_lastk_away),
    !is.na(toi_def_lastk_home), !is.na(toi_def_lastk_away),
    !is.na(goals_conceded_lastk_home), !is.na(goals_conceded_lastk_away),
    !is.na(shots_conceded_lastk_home), !is.na(shots_conceded_lastk_away)
  )

# Train/test split for all later models
train_data <- gamedata_model %>% filter(season <= 2020)
test_data  <- gamedata_model %>% filter(season >= 2021)

# Notable Descriptive Statistics
# Number of matches in model dataset
n_matches <- nrow(gamedata_model)

# Seasons covered
season_range <- range(gamedata_model$season, na.rm = TRUE)

# Number of unique teams
n_teams <- length(unique(c(gamedata_model$teamname_home, gamedata_model$teamname_away)))

n_matches
season_range
n_teams

# Frequency table of results
result_table <- gamedata_model %>%
  count(result_3way) %>%
  mutate(prop = n / sum(n))

result_table

# Bar plot of result frequencies
ggplot(result_table, aes(x = result_3way, y = prop)) +
  geom_col() +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            vjust = -0.2) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribution of Match Outcomes",
    x = "Result (A / D / H)",
    y = "Proportion"
  ) +
  theme_minimal()

# Summary stats of goals
goals_summary <- gamedata_model %>%
  summarise(
    mean_home_goals = mean(team_goals_reg_home, na.rm = TRUE),
    mean_away_goals = mean(team_goals_reg_away, na.rm = TRUE),
    var_home_goals  = var(team_goals_reg_home, na.rm = TRUE),
    var_away_goals  = var(team_goals_reg_away, na.rm = TRUE),
    max_home_goals  = max(team_goals_reg_home, na.rm = TRUE),
    max_away_goals  = max(team_goals_reg_away, na.rm = TRUE)
  )

goals_summary

# Histogram: home goals
ggplot(gamedata_model, aes(x = team_goals_reg_home)) +
  geom_histogram(binwidth = 1, boundary = -0.5) +
  labs(
    title = "Distribution of Home Goals",
    x = "Home goals",
    y = "Count"
  ) +
  theme_minimal()

# Histogram: away goals
ggplot(gamedata_model, aes(x = team_goals_reg_away)) +
  geom_histogram(binwidth = 1, boundary = -0.5) +
  labs(
    title = "Distribution of Away Goals",
    x = "Away goals",
    y = "Count"
  ) +
  theme_minimal()

# Functions for model performance
# Accuracy function
accuracy_multiclass <- function(actual, predicted_class) {
  mean(actual == predicted_class)
}

# Multi-class Brier score
brier_multiclass <- function(actual, predicted_probs) {
  # actual: factor with levels c("A", "D", "H")
  # predicted_probs: matrix or data frame with columns in same order
  
  classes <- levels(actual)
  N <- length(actual)
  K <- length(classes)
  
  # Creating one-hot matrix
  o_mat <- matrix(0, nrow = N, ncol = K)
  for (i in seq_len(N)) {
    o_mat[i, which(classes == actual[i])] <- 1
  }
  
  # Compute multiclass Brier score
  mean(rowSums((predicted_probs - o_mat)^2))
}

# Model 1: Ordered LOGIT model (rolling stats)
m_roll_toi <- polr(
  result_3way ~ 
    shots_diff_lastk + gd_diff_lastk + win_diff_lastk +
    toi_fwd_diff_lastk + toi_def_diff_lastk + goals_conceded_diff_lastk + 
    shots_conceded_diff_lastk,
  data   = train_data,
  method = "logistic"
)

summary(m_roll_toi)

# Predicting on test set
p_roll_toi <- predict(m_roll_toi, newdata = test_data, type = "probs")

# Class prediction
pred_class_roll_toi <- colnames(p_roll_toi)[apply(p_roll_toi, 1, which.max)]

# Accuracy, Brier score and Log Loss
acc_roll_toi   <- accuracy_multiclass(test_data$result_3way, pred_class_roll_toi)
brier_roll_toi <- brier_multiclass(test_data$result_3way, p_roll_toi)
log_loss_roll_toi <- logloss_multiclass(test_data$result_3way, p_roll_toi)

acc_roll_toi
brier_roll_toi
log_loss_roll_toi

# ELO rating function (used for grid search to tune parameters)
run_elo_model <- function(K0, lambda, H, gamedata_model) {
  
  # Prepare rating table
  teams <- unique(c(gamedata_model$teamname_home,
                    gamedata_model$teamname_away))
  
  elo_table <- data.frame(
    team = teams,
    elo = 1500,
    last_game_date = as.Date(NA)
  )
  
  # Storage
  gamedata_model$elo_home_before <- NA_real_
  gamedata_model$elo_away_before <- NA_real_
  gamedata_model$elo_diff <- NA_real_
  
  # Looping through games chronologically
  for (i in seq_len(nrow(gamedata_model))) {
    row  <- gamedata_model[i,]
    home <- row$teamname_home
    away <- row$teamname_away
    
    R_home <- elo_table$elo[elo_table$team == home]
    R_away <- elo_table$elo[elo_table$team == away]
    
    # expected score
    expected_home <- 1 / (1 + 10 ^ (-(R_home + H - R_away)/400))
    
    # actual outcome
    outcome <- row$result_reg
    score_home <- ifelse(outcome == 2, 1,
                         ifelse(outcome == 1, 0.5, 0))
    score_away <- 1 - score_home
    
    # goal-diff scaling
    delta <- abs(row$team_goals_reg_home - row$team_goals_reg_away)
    K_adj <- K0 * (1 + delta)^lambda
    
    # store pre-match values
    gamedata_model$elo_home_before[i] <- R_home
    gamedata_model$elo_away_before[i] <- R_away
    gamedata_model$elo_diff[i] <- R_home - R_away
    
    # update ELO
    elo_table$elo[elo_table$team == home] <- 
      R_home + K_adj * (score_home - expected_home)
    elo_table$elo[elo_table$team == away] <- 
      R_away + K_adj * (score_away - (1 - expected_home))
    
    elo_table$last_game_date[elo_table$team == home] <- row$date
    elo_table$last_game_date[elo_table$team == away] <- row$date
  }
  
  return(gamedata_model)
}

# Starting grid search
K_grid      <- c(5, 10, 15, 20)
lambda_grid <- c(0.2, 0.3, 0.4, 0.5)
H_grid      <- c(30, 40, 50, 60, 70)

results_elo <- data.frame(
  K0 = numeric(),
  lambda = numeric(),
  H = numeric(),
  accuracy = numeric(),
  brier = numeric()
)

for (K0 in K_grid) {
  for (lambda in lambda_grid) {
    for (H in H_grid) {
      
      cat("Running K0 =", K0, "lambda =", lambda, "H =", H, "\n")
      
      # Run ELO with these parameters
      model_data <- run_elo_model(K0, lambda, H, gamedata_model)
      
      # Train/validation split (using only traning timespan)
      train_data <- model_data %>% filter(season <= 2018)
      valid_data <- model_data %>% filter(season %in% c(2019, 2020))
      
      # Fitting ordered logit
      m <- polr(result_3way ~ elo_diff, data=train_data, method="logistic")
      
      # Predicting on validation set
      p <- predict(m, newdata=valid_data, type="probs")
      pred_class <- colnames(p)[apply(p, 1, which.max)]
      
      acc <- mean(pred_class == valid_data$result_3way)
      br  <- brier_multiclass(valid_data$result_3way, p)
      
      # Storage of results
      results_elo <- rbind(
        results_elo,
        data.frame(K0=K0, lambda=lambda, H=H,
                   accuracy=acc, brier=br)
      )
    }
  }
}

results_elo %>%
  arrange(desc(accuracy)) %>%
  head(10)

# Inspecting best parameter by lowest Brier score
best_params <- results_elo %>% arrange(brier) %>% slice(1)
best_params

# Looks best setting is K0 = 5, H = 30, lambda = 0.5 by Brier score
# Manual values kept in to speed up runtime (feel free to run the grid to verify)

K0_best     <- 5
lambda_best <- 0.5
H_best      <- 30

gamedata_tuned <- run_elo_model(K0_best, lambda_best, H_best, gamedata_model)

train_data <- gamedata_tuned %>%
  filter(season <= 2020) %>%
  filter(
    !is.na(shots_lastk_home),!is.na(shots_lastk_away),is.na(gd_lastk_home),
    !is.na(gd_lastk_away), !is.na(elo_diff)
  )

test_data <- gamedata_tuned %>%
  filter(season >= 2021) %>%
  filter(
    !is.na(shots_lastk_home),
    !is.na(shots_lastk_away),
    !is.na(gd_lastk_home),
    !is.na(gd_lastk_away),
    !is.na(elo_diff)
  )

# Model 2
# Logit ELO only
summary(gamedata_tuned$elo_home_before)
summary(gamedata_tuned$elo_away_before)
summary(gamedata_tuned$elo_diff)
sd(gamedata_model$elo_diff, na.rm=TRUE)

m_elo_only <- polr(result_3way ~ elo_diff, data=train_data, method="logistic")
summary(m_elo_only)

# Prediction probabilities and classes
p_elo_only <- predict(m_elo_only, newdata=test_data, type="probs")
pred_class_elo <- colnames(p_elo_only)[apply(p_elo_only, 1, which.max)]

# Accuracy, Brier score and log loss
accuracy_elo_only <- mean(pred_class_elo == test_data$result_3way)
brier_elo_only <- brier_multiclass(test_data$result_3way, p_elo_only)
log_loss_elo_only <- logloss_multiclass(test_data$result_3way, p_elo_only)

accuracy_elo_only
brier_elo_only
log_loss_elo_only

# Building a model that accounts for rest days and fatigue
# Ensure date is Date in the *tuned* dataset
gamedata_tuned$date <- as.Date(gamedata_tuned$date)

# Build schedule (one row per team per game)
schedule_long <- gamedata_tuned %>%
  dplyr::select(gameId, season, date, teamname_home, teamname_away) %>%
  tidyr::pivot_longer(
    cols = c(teamname_home, teamname_away),
    names_to = "home_away",
    values_to = "teamname"
  ) %>%
  arrange(teamname, date, gameId) %>%
  group_by(teamname) %>%
  mutate(
    last_game_date = lag(date),
    days_rest = as.numeric(date - last_game_date),
    
    games_last7 = vapply(
      seq_along(date),
      function(i) {
        if (i == 1) return(0L)
        sum(date[1:(i-1)] >= date[i] - 7 & date[1:(i-1)] < date[i])
      },
      integer(1)
    )
  ) %>%
  ungroup()

# Home features from schedule
home_rest <- schedule_long %>%
  filter(home_away == "teamname_home") %>%
  dplyr::select(
    gameId,
    teamname_home = teamname,
    days_rest_home = days_rest,
    games_last7_home = games_last7
  )

# Away features from schedule
away_rest <- schedule_long %>%
  filter(home_away == "teamname_away") %>%
  dplyr::select(
    gameId,
    teamname_away = teamname,
    days_rest_away = days_rest,
    games_last7_away = games_last7
  )

# Joining into tuned dataset
gamedata_tuned <- gamedata_tuned %>%
  left_join(home_rest, by = c("gameId", "teamname_home")) %>%
  left_join(away_rest, by = c("gameId", "teamname_away")) %>%
  mutate(
    rest_diff        = days_rest_home    - days_rest_away,
    games_last7_diff = games_last7_home  - games_last7_away
  )

# Now building train/test from the tuned and rest dataset
train_data <- gamedata_tuned %>%
  filter(season <= 2020) %>%
  filter(
    !is.na(shots_lastk_home), !is.na(shots_lastk_away), !is.na(gd_lastk_home),
    !is.na(gd_lastk_away), !is.na(elo_diff),!is.na(rest_diff),
    !is.na(games_last7_diff)
  )

test_data <- gamedata_tuned %>%
  filter(season >= 2021) %>%
  filter(
    !is.na(shots_lastk_home), !is.na(shots_lastk_away), !is.na(gd_lastk_home),
    !is.na(gd_lastk_away), !is.na(elo_diff), !is.na(rest_diff),
    !is.na(games_last7_diff)
  )

# Model 3: Ordered Logit: ELO + rest (using tuned ELO)
m_elo_rest <- polr(
  result_3way ~ elo_diff + rest_diff + games_last7_diff,
  data   = train_data,
  method = "logistic"
)

# Predicted probabilties and classes
p_elo_rest <- predict(m_elo_rest, newdata = test_data, type = "probs")
pred_class_elo_rest <- colnames(p_elo_rest)[apply(p_elo_rest, 1, which.max)]

# Accuracy, Brier score, and Log Loss
accuracy_elo_rest <- mean(pred_class_elo_rest == test_data$result_3way)
brier_elo_rest    <- brier_multiclass(test_data$result_3way, p_elo_rest)
log_loss_elo_rest <- logloss_multiclass(test_data$result_3way, p_elo_rest)

accuracy_elo_rest
brier_elo_rest
log_loss_elo_only

# Model 4: Ordered LOGIT (ELO + rolling + rest)
m_combined <- polr(
  result_3way ~ elo_diff + rest_diff + games_last7_diff + shots_diff_lastk +
    gd_diff_lastk + win_diff_lastk + toi_fwd_diff_lastk +
    toi_def_diff_lastk + goals_conceded_diff_lastk + shots_conceded_diff_lastk,
  data = train_data,
  method = "logistic"
)

summary(m_combined)

p_combined <- predict(m_combined, newdata=test_data, type="probs")
pred_class_combined <- colnames(p_combined)[apply(p_combined, 1, which.max)]

accuracy_combined <- mean(pred_class_combined == test_data$result_3way)
brier_combined <- brier_multiclass(test_data$result_3way, p_combined)
log_loss_combined <- logloss_multiclass(test_data$result_3way, p_combined)

accuracy_combined
brier_combined
log_loss_combined 

# Model 5: Bayesian Dixon-Coles Poisson model
train_pois <- gamedata_tuned %>%
  filter(season <= 2020) %>%
  filter(
    !is.na(team_goals_reg_home),
    !is.na(team_goals_reg_away),
    !is.na(shots_lastk_home),
    !is.na(shots_lastk_away),
    !is.na(goals_conceded_lastk_home),
    !is.na(goals_conceded_lastk_away)
  )

test_pois <- gamedata_tuned %>%
  filter(season >= 2021) %>%
  filter(
    !is.na(team_goals_reg_home),
    !is.na(team_goals_reg_away),
    !is.na(shots_lastk_home),
    !is.na(shots_lastk_away),
    !is.na(goals_conceded_lastk_home),
    !is.na(goals_conceded_lastk_away)
  )

# Need to tune xi on validation data
xi <- 0.0005  

# Ensure date is Date
train_pois$date <- as.Date(train_pois$date)

max_date <- max(train_pois$date, na.rm = TRUE)

train_pois <- train_pois %>%
  mutate(
    days_ago = as.numeric(max_date - date),
    weight_dc = exp(-xi * days_ago)
  )

# Home goals model with weights
m_pois_home <- glm(
  team_goals_reg_home ~ goals_lastk_home + shots_lastk_home + 
    goals_conceded_lastk_away + shots_conceded_lastk_away + elo_diff,
  data   = train_pois,
  family = poisson,
  weights = weight_dc
)

m_pois_away <- glm(
  team_goals_reg_away ~ goals_lastk_away + shots_lastk_away + 
    goals_conceded_lastk_home + shots_conceded_lastk_home + elo_diff,
  data   = train_pois,
  family = poisson,
  weights = weight_dc
)

summary(m_pois_home)
summary(m_pois_away)

# Dixon-Coles tau correction
## Predict expected goals on test set
lambda_home <- predict(m_pois_home, newdata = test_pois, type = "response")
lambda_away <- predict(m_pois_away, newdata = test_pois, type = "response")

## Dixon–Coles tau function
# Using simpler if-functions instead ifelse
tau_dc <- function(gh, ga, rho) {
  if (gh == 0 && ga == 0) return(1 - rho)
  if (gh == 0 && ga == 1) return(1 + rho)
  if (gh == 1 && ga == 0) return(1 + rho)
  if (gh == 1 && ga == 1) return(1 - rho)
  1
}

rho <- 0.05   # small dependence parameter
# Converting lambdas to A/D/H probabilities with DC adjustment
# 0–8 covers virtually all mass upon seeing hists (11 is max, but we want to
# avoid giving weights to super unlikely events)
max_goals <- 8  

prob_pois <- matrix(NA_real_, nrow = nrow(test_pois), ncol = 3)
colnames(prob_pois) <- c("A", "D", "H")

for (i in seq_len(nrow(test_pois))) {
  lamH <- lambda_home[i]
  lamA <- lambda_away[i]
  
  pH <- 0; pD <- 0; pA <- 0
  
  for (gh in 0:max_goals) {
    for (ga in 0:max_goals) {
      tau <- tau_dc(gh, ga, rho)
      p   <- tau * dpois(gh, lamH) * dpois(ga, lamA)
      
      if (gh > ga)      pH <- pH + p
      else if (gh < ga) pA <- pA + p
      else              pD <- pD + p
    }
  }
  
  # normalise just in case of truncation and tau moves the sum slightly off 1
  row_sum <- pA + pD + pH
  prob_pois[i, ] <- c(pA, pD, pH) / row_sum
}

pred_class_pois <- colnames(prob_pois)[apply(prob_pois, 1, which.max)]

# Accuracy, Brier score, and Log Loss
accuracy_pois <- accuracy_multiclass(test_pois$result_3way, pred_class_pois)
brier_pois    <- brier_multiclass(test_pois$result_3way, prob_pois)
log_loss_pois <- logloss_multiclass(test_pois$result_3way, prob_pois)

accuracy_pois
brier_pois
log_loss_pois

# Model 6: Naive home-win benchmark
# Estimating historical probabilities from TRAINING 
train_outcomes <- train_data$result_3way   # factor with levels A, D, H

# empirical class probabilities
p_hist <- prop.table(table(train_outcomes))

# formatting as vector for convenience
pA <- as.numeric(p_hist["A"])
pD <- as.numeric(p_hist["D"])
pH <- as.numeric(p_hist["H"])

p_hist

# true labels (factor: A, D, H)
actual <- test_data$result_3way  

# creating prediction probabilities
pred_probs_homewin <- data.frame(
  A = rep(pA, nrow(test_data)),
  D = rep(pD, nrow(test_data)),
  H = rep(pH, nrow(test_data))
)

# predicted class
pred_class_homewin <- rep("H", nrow(test_data))

# Accuracy, brier score, and log loss
accuracy_homewin <- accuracy_multiclass(actual, pred_class_homewin)
brier_homewin <- brier_multiclass(actual, pred_probs_homewin)
logloss_homewin <- logloss_multiclass(actual, pred_probs_homewin)

accuracy_homewin
brier_homewin
logloss_homewin

# For plotting
results_df <- tibble(
  Model = c(
    "Ordered Logit (Rolling + TOI)",
    "Ordered Logit (ELO only)",
    "Ordered Logit (ELO + Rest)",
    "Ordered Logit (combined)",
    "Poisson (Goals → W/D/L)",
    "Naive Benchmark"
  ),
  Accuracy = c(
    acc_roll_toi,
    accuracy_elo_only,
    accuracy_elo_rest,
    accuracy_combined,
    accuracy_pois,
    accuracy_homewin
  ),
  Brier = c(
    brier_roll_toi,
    brier_elo_only,
    brier_elo_rest,
    brier_combined,
    brier_pois,
    brier_homewin
  ),
  LogLoss = c(
    log_loss_roll_toi,
    log_loss_elo_only,
    log_loss_elo_rest,
    log_loss_combined,
    log_loss_pois,
    logloss_homewin
  )
)

ggplot(results_df, aes(x = reorder(Model, Accuracy), y = Accuracy)) +
  geom_col() +
  geom_text(aes(label = round(Accuracy, 3)),
            vjust = -0.2, size = 4) +
  coord_flip() +
  labs(
    title = "Model Comparison: Accuracy",
    x = "Model",
    y = "Accuracy"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggplot(results_df, aes(x = reorder(Model, -Brier), y = Brier)) +
  geom_col() +
  geom_text(aes(label = round(Brier, 3)),
            vjust = -0.2, size = 4) +
  coord_flip() +
  labs(
    title = "Model Comparison: Brier Score",
    x = "Model",
    y = "Brier Score (lower is better)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggplot(results_df, aes(x = reorder(Model, -LogLoss), y = LogLoss)) +
  geom_col() +
  geom_text(aes(label = round(LogLoss, 3)),
            vjust = -0.2, size = 4) +
  coord_flip() +
  labs(
    title = "Model Comparison: Log Loss",
    x = "Model",
    y = "Log Loss (lower is better)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")


## Ordered LOGIT (ELO + Rest + Rolling) is the best model for betting
# Implmenting EV = fixed stake betting strategy
p_bets <- predict(m_combined, newdata = test_data, type = "probs")
head(p_bets)

actual <- test_data$result_3way   # factor A/D/H

odds_A <- test_data$odd_awaywin   
odds_D <- test_data$odd_draw
odds_H <- test_data$odd_winhome

# Calculating expected values
EV_A = p_bets[,"A"]*odds_A - 1
EV_D = p_bets[,"D"]*odds_D - 1
EV_H = p_bets[,"H"]*odds_H - 1

# Creating a bets dataframe
bets_df <- data.frame(
  actual,
  p_A = p_bets[,"A"],
  p_D = p_bets[,"D"],
  p_H = p_bets[,"H"],
  odds_A, odds_D, odds_H,
  EV_A, EV_D, EV_H
)

# Checking for highest EV
EV_mat <- as.matrix(bets_df[,c("EV_A","EV_D","EV_H")])
best_idx <- max.col(EV_mat, ties.method = "first")  # 1=A, 2=D, 3=H

bets_df$best_outcome <- factor(best_idx, levels = 1:3, labels = c("A","D","H"))
bets_df$best_EV      <- apply(bets_df[,8:10], 1, max)

bets_df <- bets_df %>% 
  filter(!is.na(EV_A), !is.na(EV_D), !is.na(EV_H))

# Only betting when EV is positive
bets_df$bet_flag <- as.integer(bets_df$best_EV > 0)
sum(bets_df$bet_flag)

# Strategy 1 - EV + fixed stake
stake_fixed <- 1

# Odds and probs of the chosen outcome
bets_df$chosen_odds <- ifelse(bets_df$best_outcome == "H", bets_df$odds_H,
                              ifelse(bets_df$best_outcome == "D", bets_df$odds_D,
                                     bets_df$odds_A))

bets_df$chosen_win <- as.integer(bets_df$best_outcome == bets_df$actual)

# Per-bet return for fixed stake (only where bet_flag = 1)
bets_df$ret_fixed <- 0
bets_df$ret_fixed[bets_df$bet_flag == 1 & bets_df$chosen_win == 1] <- 
  bets_df$chosen_odds[bets_df$bet_flag == 1 & bets_df$chosen_win == 1] - 1
bets_df$ret_fixed[bets_df$bet_flag == 1 & bets_df$chosen_win == 0] <- -1

# Only returns for bets actually placed
r_fixed <- bets_df$ret_fixed[bets_df$bet_flag == 1]

n_bets_fixed <- length(r_fixed)
profit_fixed <- sum(r_fixed)
roi_fixed    <- profit_fixed / (n_bets_fixed * stake_fixed)

sharpe_fixed <- mean(r_fixed) / sd(r_fixed)

n_bets_fixed
profit_fixed
roi_fixed
sharpe_fixed