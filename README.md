# When to Pull the Pitcher: An Absorbing Markov Chain Approach

James Chen & Charlie Ko, 11/6/2025

---

## 1. Problem Statement
During a close game in baseball, any tiny decision such as a substitute in  a pinch hitter or a reliever could dramatically change the outcome of the come. This study aims to provide a Markov chain analysis to assist coaching staff to evaluate the optimal timing to pull the starter in the inning to reduce points lost. To demonstrate the practical relevance of our model, we decide to apply it to a highly controversial real-world scenario: Houston manager A.J. Hinch’s decision to remove Zack Greinke in the 7th inning of World Series Game 7, which was widely regarded as one of the most controversial pitching substitutions in recent Major League Baseball (MLB) history. Beyond the famous 2019 case, we hope to develop a framework with the adapability to explore substitution strategies across a wide spectrum of game conditions.

---

## 2. Data and Methodology 

### 2.1 Scraping and construct play-by-play dataset

The 2024 Statcast data was collected using the [scraper](https://github.com/skdeshpande91/stat479_fall2025/blob/main/scripts/annual_statcast_query.R) that utilizes baseballr::statcastsearch(). After scraping the data, we kept only regular-season games and the columns of interest. Since we analyze only full innings, the dataset includes only the first eight innings of each game. 

```
#clean
```

To better understand each pitcher’s season-long performance, we construct a pitcher profile dataset that summarizes key metrics such as average, median, and maximum pitch counts, along with each pitcher’s repertoire. In addition, we also assign a role label, “starter,” “long reliever,” or “short reliever”, based on the maximum number of pitches thrown in a single game. This role classification is especially useful for determining appropriate fatigue-level thresholds in later analysis.
```
# Picture-level data construction+ half_inning _socre
```

Using our constructed dataset, we define conditioning variables: pitcher fatigue level, run differential, number of the times through the order, and a metric that evaluates the performance in the previous innings. 

To summarize a pitcher’s performance entering a new inning, we develop the Exponential Weighted Runs Allowed (EWRA) metric, a recency-aware score that emphasizes recent performance while gradually fading the influence of earlier innings. For each inning, we define Stress-Adjust Run Allowed (SARA) that includes both actual runs and a stress bump to account for demanding innings: innings in which a pitcher throws 20 or more pitches are treated as contributing an additional 0.5 effective runs, reflecting the well-established impact of stressful innings on subsequent performance. EWRA is then computed recursively using an exponential decay factor governed by a half-life parameter, ensuring that recent innings carry more weight while older, potentially anomalous events diminish smoothly. This produces a single, interpretable measure of a pitcher’s short-term form and stability entering the upcoming inning.
```
# EWRA formulas

```

We classify pitcher fatigue into five levels—Fresh, Settled-In, Working Load, Fatigued, and Redline. The thresholds between levels differ by role, since starters typically carry a higher pitch count than relievers(fg.1). To better filter the in-game circumstances, such as leading, trailing,  and pitcher’s performance entering the new inning, we classify the point difference into five levels and EWRA into three levels as well.(fg.2).

```
#fatigue, point difference, EWRA level threshold

```

To compute run expectancy and game-state transition probabilities, we collapse pitch-level “game_seq” to the at-bat level, keeping the batter, starting base/out state, pitcher info, pitch count per AB, times through the order, and the pitcher’s fatigue level over the AB. In our Markov-chain analysis, we set 3.000 (three outs) as the absorbing state, and each AB has a current and next state label.
```
# Get expected run 
```

### 2.2 Markov Chain Set Up

We first  isolate the game situations that match the context we want to study: a small lead with the starter performing well under our EWRA metric. The “keep” dataset includes starters who are either at a moderate fatigue level or facing the lineup for the third time, capturing moments when managers often debate whether to leave the starter in. The “sub” dataset includes relievers who enter the game right after the starter and are facing the lineup for the first time, representing a typical fresh bullpen option.

```
# scenario slices
```


### 2.3 Data Preprocessing and Feature Selection

To build a clean and informative dataset for model fitting, we first identified relevant variables, engineered new practical features, and removed noise from the raw data. One major challenge arose from combining the two half-courts, since the direction of play (left or right basket) does not affect shot quality. To standardize court orientation, we “flipped” all shots taken on the left side by multiplying both the x and y coordinates by −1 when x < 0, using (0, 0) as the center of the court.

However, this introduced a new issue: shots taken from behind half court could be incorrectly mirrored, leading to inaccurate expected percentages. To address this, we first extracted distance information from the descriptive text before performing the flip. Because long-distance and beyond-half-court attempts are typically described explicitly, we could reliably identify and filter them out—these shots have very low success rates.

Over-Half-Court Shot Success Rates
| Distance Threshold | Make Percentage |
|:-------------------|:----------------:|
| 47 - 59 ft | 2.48% |
| 60 - 74 ft | 1.68% |
| > 75 ft | 0.00% |

For cases where distance was missing, particularly for shots close to the basket, we manually calculated the missing distances using their exact coordinates to ensure accuracy.
```
# Shots expressed relative to the same basket
half_court <- 47
pbp_shot <- pbp_shot %>% 
  mutate(
    # Flip x,y for shots taken on the opposite half
    x_aligned = ifelse(coordinate_x < 0, -coordinate_x, coordinate_x),
    y_aligned = ifelse(coordinate_x < 0, -coordinate_y, coordinate_y),
    
    # update missing distance
    distance = ifelse(is.na(distance), sqrt((x_aligned-41.75)^2+y_aligned^2), distance)
  ) %>% 
  
   # Filter out half court shot here, can be added back if needed
  dplyr::filter(distance <= half_court)
```

Other features included in our analysis are the shot type (e.g., jump shot, layup), point difference and game situation (whether the team was leading, trailing, or tied) at the time of the shot, as well as the quarter and time remaining in the quarter.
```
useful_columns <- c("id", "type_text", "text", "away_score", "home_score", "period_number",
         "scoring_play", "score_value", "home_team_abbrev", "home_team_abbrev",
         "away_team_abbrev", "coordinate_x", "coordinate_y", "start_quarter_seconds_remaining",
         "team_id", "home_team_id" ,"away_team_id")

# Limit to regular season, only shooting play
pbp_shot <- pbp_raw %>%
  dplyr::filter(season_type == 2, shooting_play)

# Only keep columns that are useful for shot quality evaluation
pbp_shot <- pbp_shot %>% 
  dplyr::select(any_of(useful_columns)) %>% 
  dplyr::filter(score_value != 1) %>%
  mutate(distance = str_extract(text, "\\d+(?=-foot)") %>% as.numeric(),
         shooter_home = if_else(team_id == home_team_id, 1, -1),
         margin_before = if_else(shooter_home > 0,
                                 (home_score - score_value) - away_score, 
                                 home_score - (away_score - score_value)),
         shooter_margin = shooter_home * margin_before)

# Extract shot type from type_text
pbp_shot <- pbp_shot %>%
  mutate(shot_type = case_when(
    str_detect(type_text, regex("dunk", ignore_case = TRUE)) ~ "Dunk",
    str_detect(type_text, regex("layup", ignore_case = TRUE)) ~ "Layup",
    str_detect(type_text, regex("tip", ignore_case = TRUE)) ~ "Tip",
    str_detect(type_text, regex("hook", ignore_case = TRUE)) ~ "Hook",
    str_detect(type_text, regex("free throw", ignore_case = TRUE)) ~ "Free Throw",
    TRUE ~ "Jump Shot"
  )) %>% 
  filter(shot_type != "Free Throw")

# Identify if the shot worth two or three
pbp_shot <- pbp_shot %>%
  mutate(
    x_rot = y_aligned,
    y_rot = 47 - x_aligned,
    dist_rim    = sqrt((x_rot)^2 + (y_rot - rim_y)^2),
    arc3     = (abs(x_rot) <  tp_corner & dist_rim >= tp_r),
    non_arc3  = (abs(x_rot) >= tp_corner),
    point_value = ifelse(arc3 | non_arc3, 3L, 2L),
  ) %>%
  select(-dist_rim, -non_arc3, -arc3)


# Identify two_for_one shot case with already cleaned data, label first and second shot.
pbp_shot <- pbp_shot %>%
  mutate(two_for_one = case_when(
    id %in% tfo_first$id  ~ 1,
    id %in% tfo_second$id ~ 2,
    TRUE                  ~ 0
  ))
```

### 2.4  Shot Quality Model Developement
Our first and baseline model was a generalized additive model (GAM) that used only the shot coordinates to evaluate shot quality.
```
gam_shot_base <- bam(
  scoring_play ~ s(x_aligned, y_aligned),
  data = train,
  family = binomial(link = "logit")
)
```

Then, we added other features to fit an extended GAM. 
```
gam_shot_extended <- bam(
  scoring_play ~ 
    s(x_aligned, y_aligned, k=100) + 
    s(start_quarter_seconds_remaining, k=10) +
    s(shooter_margin, k=10) + 
    shot_type + 
    period_number,
  data = pbp_shot,
  family = binomial(link = "logit")
)
```

In addition to the two GAM models, we developed a hybrid model that combines the smooth interpretability of GAM with the predictive power of XGBoost. Specifically, we stacked the two approaches by using the expected percentage predicted by the base GAM as an additional input feature for the XGBoost model, alongside all other available variables. This design allows the hybrid model to capture both the smooth spatial patterns learned by the GAM and the complex nonlinear interactions identified by XGBoost.

```
# XGBoost training function
train_xgb <- function(train_x, train_y,test_x, test_y){
  dtrain <- xgb.DMatrix(data = train_x, label = train_y)
  dtest  <- xgb.DMatrix(data = test_x,  label = test_y)
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = 0.05,
    max_depth = 4,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 300,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = 25,
    verbose = 1
  )
  
  return(model)
}


# Create model matrices (numeric encoding for factors)
train_x <- model.matrix(
  scoring_play ~ pred_prob_base + shot_type + period_number +
    start_quarter_seconds_remaining + shooter_margin,
  data = train
)
train_y <- train$scoring_play

test_x <- model.matrix(
  scoring_play ~ pred_prob_base + shot_type + period_number +
    start_quarter_seconds_remaining + shooter_margin,
  data = test
)
test_y <- test$scoring_play

pbp_xgb <- model.matrix(
  scoring_play ~ pred_prob_base + shot_type + period_number +
    start_quarter_seconds_remaining + shooter_margin,
  data = pbp_shot
)

# train and evaluate xgb_hybrid
xgb_hybrid <- train_xgb(train_x, train_y,test_x, test_y)
```

Lastly, we leveraged the full capabilities of XGBoost to build two additional models. The first model used all available features, consistent with the extended GAM, while the second replaced the x and y coordinates with the distance column. This allowed us to examine whether shot location still influences shot quality even when distance is held constant.

XGBoost model with exact spatial data (x and y coordinate):
```
# Create model matrices (numeric encoding for factors)
train_x <- model.matrix(
  scoring_play ~ x_aligned + y_aligned + 
    shot_type + period_number +
    start_quarter_seconds_remaining + shooter_margin,
  data = train
)
train_y <- train$scoring_play

test_x <- model.matrix(
  scoring_play ~ x_aligned + y_aligned + 
    shot_type + period_number +
    start_quarter_seconds_remaining + shooter_margin,
  data = test
)
test_y <- test$scoring_play

pbp_xgb <- model.matrix(
  scoring_play ~ x_aligned + y_aligned + 
    shot_type + period_number +
    start_quarter_seconds_remaining + shooter_margin,
  data = pbp_shot
)

# train and evaluate xgb_no_spatial
xgb_spatial <- train_xgb(train_x, train_y,test_x, test_y)
```

XGBoost model with distance
```
# Create model matrices (numeric encoding for factors)
train_x <- model.matrix(
  scoring_play ~ distance + shot_type + period_number +
    start_quarter_seconds_remaining + shooter_margin,
  data = train
)
train_y <- train$scoring_play

test_x <- model.matrix(
  scoring_play ~  distance + shot_type + period_number +
    start_quarter_seconds_remaining + shooter_margin,
  data = test
)
test_y <- test$scoring_play

pbp_xgb <- model.matrix(
  scoring_play ~  distance + shot_type + period_number +
    start_quarter_seconds_remaining + shooter_margin,
  data = pbp_shot
)

# train and evaluate xgb_dist
xgb_dist <- train_xgb(train_x, train_y,test_x, test_y)
```

---

### 3. Main Results
On average, teams employing the strategy yield a positive 1.03 point advantage according to our histogram below. While many individual outcomes are neutral or negative, the positive outcomes are significant enough to make the strategy worthwhile on average.
```
# find for all shooting plays from team one, first and second shot of the two for one possession 
two_for_one_keys <- validated_sequences %>%
  distinct(game_id, period_number)

all_two_for_one_pbps <- pbp_with_shot_clock %>%
  semi_join(two_for_one_keys, by = c("game_id", "period_number")) %>%
  filter(seconds_remaining_qtr <= 42) 

#order the possession and extract first shot and second for team 1
team1_possession_ranks <- all_two_for_one_pbps %>%
  left_join(select(validated_sequences, game_id, period_number, team1), by = c("game_id", "period_number")) %>%
  filter(team_abbreviation == team1) %>%
  distinct(game_id, period_number, possession_id) %>%
  group_by(game_id, period_number) %>%
  mutate(team1_poss_rank = rank(possession_id, ties.method = "first")) %>%
  ungroup()

team1_first_shots <- all_two_for_one_pbps %>%
  semi_join(filter(team1_possession_ranks, team1_poss_rank == 1), 
            by = c("game_id", "period_number", "possession_id")) %>%
  filter(is_shot_attempt == TRUE &
           seconds_remaining_qtr >= 28 & 
           seconds_remaining_qtr <= 40 & 
           !stringr::str_detect(type_text, "Free Throw"))

team1_second_shots <- all_two_for_one_pbps %>%
  semi_join(filter(team1_possession_ranks, team1_poss_rank == 2), 
            by = c("game_id", "period_number", "possession_id")) %>%
  filter(is_shot_attempt == TRUE & !stringr::str_detect(type_text, "Free Throw"))
```
```
ggplot(validated_sequences, aes(x = point_differential)) +
    geom_histogram(binwidth = 1, fill = "#0072B2", color = "white", alpha = 0.8) +
    geom_vline(
        aes(xintercept = mean(point_differential)), 
        color = "red", 
        linetype = "dashed", 
        linewidth = 1,
        alpha = 0.5 
    ) +
    geom_text(
        aes(
            x = mean(point_differential), 
            y = Inf,
            label = paste("Mean =", round(mean(point_differential), 2))
        ),
        vjust = 2, hjust = -0.1, color = "red"
    ) +
    labs(
        title = "Two-for-One Overall Result",
        x = "Point Differential",
        y = "Number of Sequences"
    ) +
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )
```
<p align="center">
  <img src="plots/Two-for-One_Overall_Result-1.png" width="450">
  <br>
  <em>Figure 1. Two-for-One overall Result</em>
</p>


However, when using the strategy, both shots, especially the second one, are often rushed, leading teams to settle for lower-quality attempts.To understand the trade-off, we analyze the location and type of shots taken. We generate a dataset that captures all the shooting play-by-play data from the validated sequence in the step 2.1. The following histogram and shot charts visualize the initiating team’s shooting preference and the location of all "first shots" and "second shots". There isn’t a significant difference between two shots in terms of their shooting preference and location. 
```
# distance analysis

team1_first_shots <- team1_first_shots %>% 
  mutate(
    distance = str_extract(text, "\\d+(?=-foot)") %>% as.numeric(),
    x_aligned = ifelse(coordinate_x < 0, -coordinate_x, coordinate_x),
    y_aligned = ifelse(coordinate_x < 0, -coordinate_y, coordinate_y),
    distance = ifelse(is.na(distance), sqrt((x_aligned-41.75)^2+y_aligned^2), distance)) %>%
  mutate(
    # create categories for different shot zones
    shot_zone = case_when(
      points_scored == 3 ~ "Three Pointer",
      distance <= 6      ~ "At The Rim",
      distance > 6 & distance < 16 ~ "Mid-Range",
      distance >= 16 & distance <= 21 ~ "Long Two",
      TRUE ~ "Three Pointer" 
    ),
    shot_zone = factor(shot_zone, levels = c("At The Rim", "Mid-Range", "Long Two", "Three Pointer")))%>%
  mutate(shot_made = score_value > 0)


team1_second_shots <- team1_second_shots %>% 
  mutate(
    distance = str_extract(text, "\\d+(?=-foot)") %>% as.numeric(),
    # Flip x,y for shots taken on the opposite half
    x_aligned = ifelse(coordinate_x < 0, -coordinate_x, coordinate_x),
    y_aligned = ifelse(coordinate_x < 0, -coordinate_y, coordinate_y),
    distance = ifelse(is.na(distance), sqrt((x_aligned-41.75)^2+y_aligned^2), distance)) %>%
  mutate(
    # create categories for different shot zones
    shot_zone = case_when(
      points_scored == 3 ~ "Three Pointer",
      distance <= 6      ~ "At The Rim",
      distance > 6 & distance < 16 ~ "Mid-Range",
      distance >= 16 & distance <= 21 ~ "Long Two",
      TRUE ~ "Three Pointer" 
    ),
    shot_zone = factor(shot_zone, levels = c("At The Rim", "Mid-Range", "Long Two", "Three Pointer"))) %>%
  mutate(shot_made = score_value > 0)

```
```
# shot zone comparison 
shots_combined <- bind_rows(
    team1_first_shots %>% mutate(group = "First Shots"),
    team1_second_shots %>% mutate(group = "Second Shots")
)

shot_percentages <- shots_combined %>%
    group_by(group, shot_zone) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(group) %>%
    mutate(percent = count / sum(count) * 100)

shot_order <- shot_percentages %>%
    group_by(shot_zone) %>%
    summarise(avg_percent = mean(percent)) %>%
    arrange(-avg_percent) %>%
    pull(shot_zone)

ggplot(shot_percentages, aes(x = factor(shot_zone, levels = shot_order), 
                             y = percent, fill = group)) +
    geom_col(position = "dodge", alpha = 0.9) + 
    scale_fill_manual(values = c("First Shots" = "#8DB6CD", 
                                 "Second Shots" = "#CD5C5C")) +  
    labs(
        title = "Shot Type Comparison",
       # x = "Shot Type",
        y = "Percentage (%)",
        fill = "Shot Type"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )


```
<p align="center">
  <img src="plots/shot_type_comparison.png" width="450">
  <br>
  <em>Figure 2. Shot Type Comparison</em>
</p>

```

shot_colors <- c("TRUE" = "#00B050", "FALSE" = "#FF0000") 
shot_shapes <- c("TRUE" = 1, "FALSE" = 4)
line_col <- "gray30"
my_transparent_blue <- rgb(173/255, 216/255, 230/255, alpha = 0.5)


# court dimensions
half_len <- 47
half_wid <- 50
rim_y    <- 5.25
rim_r    <- 0.75
lane_hw  <- 8
ft_y     <- 19
ft_r     <- 6
ra_r     <- 4
tp_r     <- 23.75
tp_corner <- 22
tp_tan_y  <- rim_y + sqrt(tp_r^2 - tp_corner^2)

# geometry data frames
theta <- seq(0, 2*pi, length.out = 400)
rim <- data.frame(x = 0 + rim_r * cos(theta), y = rim_y + rim_r * sin(theta))
theta_ra <- seq(0, pi, length.out = 200)
ra_arc <- data.frame(x = 0 + ra_r * cos(theta_ra), y = rim_y + ra_r * sin(theta_ra))
ft_line <- data.frame(x=-lane_hw, y=ft_y, x2=lane_hw, y2=ft_y)
ft_circle <- data.frame(theta = theta, x = 0 + ft_r * cos(theta), y = ft_y + ft_r * sin(theta))
ft_circle_top <- ft_circle %>% filter(theta >= 0 & theta <= pi) %>% arrange(theta)
ft_circle_bottom <- ft_circle %>% filter(theta > pi & theta < 2*pi) %>% arrange(theta)
lane_lines <- data.frame(x  = c(-lane_hw,  lane_hw, -lane_hw,  lane_hw), y  = c(0, 0, ft_y, ft_y), x2 = c(-lane_hw,  lane_hw, -lane_hw,  lane_hw), y2 = c(ft_y, ft_y, 0, 0))
theta_left  <- pi - acos(tp_corner / tp_r)
theta_right <-      acos(tp_corner / tp_r)
theta_tp <- seq(theta_left, theta_right, length.out = 400)
tp_arc <- data.frame(x = 0 + tp_r * cos(theta_tp), y = rim_y + tp_r * sin(theta_tp))
corner_lines <- data.frame(x  = c(-tp_corner,  tp_corner), x2 = c(-tp_corner,  tp_corner), y  = c(0, 0), y2 = c(tp_tan_y, tp_tan_y))

# first shot distribution 
ggplot() +
    geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), colour = line_col) +
    geom_segment(aes(x = -25, y = 47, xend = 25, yend = 47), colour = line_col) +
    geom_segment(aes(x = -25, y = 0, xend = -25, yend = 47), colour = line_col) +
    geom_segment(aes(x = 25, y = 0, xend = 25, yend = 47), colour = line_col) +
    geom_segment(data = lane_lines, aes(x = x, y = y, xend = x2, yend = y2), colour = line_col) +
    geom_path(data = ft_circle_top, aes(x = x, y = y), colour = line_col) +
    geom_path(data = ft_circle_bottom, aes(x = x, y = y), colour = line_col, linetype = "dashed") +
    geom_segment(data = ft_line, aes(x = x, y = y, xend = x2, yend = y2), colour = line_col) +
    geom_path(data = rim, aes(x = x, y = y), linewidth = 1) +
    geom_path(data = ra_arc, aes(x = x, y = y), colour = line_col) +
    geom_segment(data = corner_lines, aes(x = x, y = y, xend = x2, yend = y2), colour = line_col) +
    geom_path(data = tp_arc, aes(x = x, y = y), colour = line_col) +
    
    geom_point(
        data = team1_first_shots, 
        aes(x = y_aligned, y = 47 - x_aligned, color = shot_made, shape = shot_made), 
        alpha = 0.6, 
        size = 3,    
        stroke = 1.2 
    ) +
    
    scale_color_manual(values = shot_colors, name = "Shot Outcome", labels = c("Missed", "Made")) +
    scale_shape_manual(values = shot_shapes, name = "Shot Outcome", labels = c("Missed", "Made")) +
    
    coord_fixed(ylim = c(-2, 50), xlim = c(-27, 27)) +
    theme_void() +
    labs(title = "Two-for-One First Shot Attempts") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.background = element_rect(fill = "white", color = NA))

```
```

# second shot distribution 
ggplot() +
    geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), colour = line_col) +
    geom_segment(aes(x = -25, y = 47, xend = 25, yend = 47), colour = line_col) +
    geom_segment(aes(x = -25, y = 0, xend = -25, yend = 47), colour = line_col) +
    geom_segment(aes(x = 25, y = 0, xend = 25, yend = 47), colour = line_col) +
    geom_segment(data = lane_lines, aes(x = x, y = y, xend = x2, yend = y2), colour = line_col) +
    geom_path(data = ft_circle_top, aes(x = x, y = y), colour = line_col) +
    geom_path(data = ft_circle_bottom, aes(x = x, y = y), colour = line_col, linetype = "dashed") +
    geom_segment(data = ft_line, aes(x = x, y = y, xend = x2, yend = y2), colour = line_col) +
    geom_path(data = rim, aes(x = x, y = y), linewidth = 1) +
    geom_path(data = ra_arc, aes(x = x, y = y), colour = line_col) +
    geom_segment(data = corner_lines, aes(x = x, y = y, xend = x2, yend = y2), colour = line_col) +
    geom_path(data = tp_arc, aes(x = x, y = y), colour = line_col) +
    
    geom_point(
        data = team1_second_shots, 
        aes(x = y_aligned, y = 47 - x_aligned, color = shot_made, shape = shot_made), 
        alpha = 0.6, 
        size = 3,    
        stroke = 1.2 
    ) +
    
    scale_color_manual(values = shot_colors, name = "Shot Outcome", labels = c("Missed", "Made")) +
    scale_shape_manual(values = shot_shapes, name = "Shot Outcome", labels = c("Missed", "Made")) +
    
    coord_fixed(ylim = c(-2, 50), xlim = c(-27, 27)) +
    theme_void() +
    labs(title = "Two-for-One Second Shot Attempts") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.background = element_rect(fill = "white", color = NA))
```
<p align="center">
  <img src="plots/first_shot_attempts.png" width="400">
  <img src="plots/second_002.png" width="400">
  <br>
  <em>Figure 3. Shot attempt locations: first vs. second TFO shots.</em>
</p>


Before showing model prediction, we also generated a shot map using the base GAM to visualize the expected shooting percentage and expected points across the court. The results aligned with current NBA trends, highlighting that teams prioritize three-point attempts and shots near the basket, as these yield the highest points per attempt.

Court dimension and lines
```
# Dimensions & refs (ft)
half_len <- 47      # baseline (0) to half-court (47)
half_wid <- 50      # sideline to sideline (-25..25)
rim_y   <- 5.25     # rim center is 5.25 ft from baseline
rim_r   <- 0.75     # rim radius ~ 9 in ≈ 0.75 ft
lane_hw <- 8        # lane half-width (key width = 16)
ft_y    <- 19       # free-throw line from baseline
ft_r    <- 6        # FT circle radius
ra_r    <- 4        # restricted area radius
tp_r    <- 23.75    # 3PT arc radius
tp_corner <- 22     # corner distance
tp_tan_y  <- rim_y + sqrt(tp_r^2 - tp_corner^2)  # where arc meets the corner lines


# Geometry (in rotated coords)
theta <- seq(0, 2*pi, length.out = 400)

# Rim
rim <- data.frame(
  x = 0 + rim_r * cos(theta),
  y = rim_y + rim_r * sin(theta)
)

# Restricted area semicircle
theta_ra <- seq(0, pi, length.out = 200)
ra_arc <- data.frame(
  x = 0 + ra_r * cos(theta_ra),
  y = rim_y + ra_r * sin(theta_ra)
)

# FT line & circle
ft_line <- data.frame(x=-lane_hw, y=ft_y, x2=lane_hw, y2=ft_y)

theta <- seq(0, 2*pi, length.out = 400)
ft_circle <- data.frame(
  theta = theta,
  x = 0 + ft_r * cos(theta),
  y = ft_y + ft_r * sin(theta)
)

ft_circle_top    <- ft_circle %>% 
  dplyr::filter(theta >= 0   & theta <= pi) %>% 
  arrange(theta)
ft_circle_bottom <- ft_circle %>%  
  dplyr::filter(theta >  pi  & theta <  2*pi)  %>% 
  arrange(theta)

# Lane (paint) rectangle edges
lane_lines <- data.frame(
  x  = c(-lane_hw,  lane_hw, -lane_hw,  lane_hw),
  y  = c(0, 0, ft_y, ft_y),
  x2 = c(-lane_hw,  lane_hw, -lane_hw,  lane_hw),
  y2 = c(ft_y, ft_y, 0, 0)
)

# 3PT arc (clipped to meet corners at |x| = 22)
theta_left  <- pi - acos(tp_corner / tp_r)
theta_right <-       acos(tp_corner / tp_r)
theta_tp <- seq(theta_left, theta_right, length.out = 400)
tp_arc <- data.frame(
  x = 0 + tp_r * cos(theta_tp),
  y = rim_y + tp_r * sin(theta_tp)
)

# Corner-3 verticals up to tangency height
tp_tan_y <- rim_y + sqrt(tp_r^2 - tp_corner^2)
corner_lines <- data.frame(
  x  = c(-tp_corner,  tp_corner),
  x2 = c(-tp_corner,  tp_corner),
  y  = c(0, 0),
  y2 = c(tp_tan_y, tp_tan_y)
)
```

Predict on half court grid
```
grid_step <- 0.5
grid <- expand.grid(
  x_aligned = seq(0, 47, by = grid_step),
  y_aligned = seq(-25, 25, by = grid_step)
)

# Rotate 90 degree clockwise (basket at bottom)
grid <- grid %>%
  mutate(x_rot = y_aligned,
         y_rot = 47 - x_aligned)

grid$pred_prob <- predict(gam_shot_base, newdata = grid, type = "response")

# Compute expected points
# Infer 3PT vs 2PT by geometry, then compute EP = p(make) * points
grid <- grid %>%
  mutate(
    dist_rim    = sqrt((x_rot)^2 + (y_rot - rim_y)^2),
    arc3     = (abs(x_rot) <  tp_corner & dist_rim >= tp_r),
    non_arc3  = (abs(x_rot) >= tp_corner),
    point_value = ifelse(arc3 | non_arc3, 3L, 2L),
    exp_points  = pred_prob * point_value
  ) %>%
  select(-dist_rim, -non_arc3, -arc3)
```

Visualization
```
line_col <- "gray30"


plot_surface <- function(col, plot_name,
                         palette = colorBlindness::Blue2DarkRed18Steps,
                         percent_scale = FALSE,
                         limits = NULL) {
  # col: string name of the column in `grid` to fill by (e.g., "pred_prob", "exp_points")

  ggplot() +
    geom_raster(
      data = grid,
      aes(x = x_rot, y = y_rot, fill = .data[[col]]),
      interpolate = TRUE
    ) +
    {
      if (percent_scale) {
        scale_fill_gradientn(
          colours = palette,
          name = plot_name,
          limits = if (is.null(limits)) c(0, 1) else limits,
          labels = percent_format(accuracy = 1)
        )
      } else {
        scale_fill_gradientn(
          colours = palette,
          name = plot_name,
          limits = limits
        )
      }
    } +
    coord_fixed(
      xlim = c(-half_wid/2 - 5, half_wid/2 + 5),
      ylim = c(-3, half_len + 3),
      expand = FALSE
    ) +
    
    # Boundaries (sidelines & baselines)
    geom_segment(aes(x = -half_wid/2, y = 0,        xend =  half_wid/2, yend = 0),
                 colour = line_col, linewidth = 1, inherit.aes = FALSE) +
    geom_segment(aes(x = -half_wid/2, y = half_len, xend =  half_wid/2, yend = half_len),
                 colour = line_col, linewidth = 1, inherit.aes = FALSE) +
    geom_segment(aes(x = -half_wid/2, y = 0,        xend = -half_wid/2, yend = half_len),
                 colour = line_col, linewidth = 1, inherit.aes = FALSE) +
    geom_segment(aes(x =  half_wid/2, y = 0,        xend =  half_wid/2, yend = half_len),
                 colour = line_col, linewidth = 1, inherit.aes = FALSE) +
    
    # Lane, FT line, FT circle
    geom_segment(data = lane_lines, aes(x = x, y = y, xend = x2, yend = y2),
             colour = line_col, linewidth = 1, inherit.aes = FALSE) +
    geom_path(data = ft_circle_top, aes(x = x, y = y),
          colour = line_col, linewidth = 1, inherit.aes = FALSE) +
    geom_path(data = ft_circle_bottom, aes(x = x, y = y),
          colour = line_col, linewidth = 1, linetype = "dashed", inherit.aes = FALSE) +
    geom_segment(data = ft_line, aes(x = x, y = y, xend = x2, yend = y2),  # draw last
             colour = line_col, linewidth = 1, inherit.aes = FALSE) +
  
    # Rim + restricted arc
    geom_path(data = rim, aes(x = x, y = y),
              colour = line_col, linewidth = 1.2, inherit.aes = FALSE) +
    geom_path(data = ra_arc, aes(x = x, y = y),
              colour = line_col, linewidth = 1, inherit.aes = FALSE) +
    
    # 3PT lines + arc
    geom_segment(data = corner_lines, aes(x = x, y = y, xend = x2, yend = y2),
                 colour = line_col, linewidth = 1, inherit.aes = FALSE) +
    geom_path(data = tp_arc, aes(x = x, y = y),
              colour = line_col, linewidth = 1, inherit.aes = FALSE) +
    labs(title = paste("Base GAM", plot_name), x = NULL, y = NULL) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "right",
      legend.title = element_text(face = "bold")
    )
}

# Call function
# FG%
plot_surface("pred_prob", "Expected FG%", percent_scale = TRUE)

# Expected points
plot_surface("exp_points", "Expected Points", percent_scale = FALSE)
```

<p align="center">
  <img src="plots/gam_base_xFG_perc.png" width="600">
  <img src="plots/gam_base_xPts.png" width="600">
  <br>
  <em>Figure 4. Base GAM models: 1. xFG% 2. xPts</em>
</p>


Across all evaluation metrics, model performance improved progressively from the base GAM to more advanced models. The extended GAM showed modest gains over the base version, indicating that adding contextual features such as shot type and game situation slightly enhanced prediction accuracy. The XGBoost models further improved calibration and discrimination, with the XGB Distance model achieving the best overall results (AUC = 0.693, Log Loss = 0.620, Brier = 0.217). This suggests that shot distance alone is a highly informative predictor of shot quality, capturing much of the spatial variation even more effectively than raw coordinates. 

| Model         | AUC   | Log Loss | MSE |
|----------------|:-----:|:---------:|:------:|
| GAM (Base)     | 0.642 | 0.654     | 0.231  |
| GAM (Extended) | 0.657 | 0.639     | 0.225  |
| XGB Hybrid     | 0.655 | 0.639     | 0.225  |
| XGB Spatial    | 0.658 | 0.639     | 0.225  |
| **XGB Distance** | **0.693** | **0.620** | **0.217** |

Model predictions show a clear decline in shot quality during Two-For-One (TFO) situations. Non-TFO shots have the highest expected percentages (54% for two-pointers and 36% for three-pointers), while both the first and especially the second TFO shots show lower predicted make rates. The second TFO three-pointer drops most sharply, averaging around 30%. This indicates that teams tend to rush their TFO attempts, particularly the second shot, resulting in reduced shot quality, even though the overall strategy remains beneficial due to the extra possession gained.

Expected percentage for 2 pointers:
| Context  | gam_base | gam_extended | hybrid | xgb_spatial | xgb_dist |
|:----------|:---------:|:-------------:|:--------:|:-------------:|:----------:|
| **League AVG** | 54.6% | 54.5% | 54.1% | 54.4% | 54.5% |
| **TFO 1st**    | 52.2% | 50.8% | 51.6% | 51.9% | 52.7% |
| **TFO 2nd**    | 52.4% | 47.4% | 48.1% | 48.5% | 48.5% |


Expected percentage for 3 pointers:
| Context  | gam_base | gam_extended | hybrid | xgb_spatial | xgb_dist |
|:----------|:---------:|:-------------:|:--------:|:-------------:|:----------:|
| **League AVG** | 36.0% | 36.2% | 36.7% | 36.4% | 36.3% |
| **TFO 1st**    | 36.0% | 33.9% | 35.3% | 35.1% | 35.2% |
| **TFO 2nd**    | 35.5% | 29.5% | 29.2% | 28.8% | 29.3% |


---

### 4. Limitation and Further Development

### 4.1 Limitation
First, the method for identifying two-for-one possessions is based on a strict, time-based heuristic. This approach cannot fully capture every intended two-for-one possession, as it measures the outcome of a sequence rather than the players' intent. Second, the shot quality assessment is based on proxies like shot location and distance, which are available in standard play-by-play data. However, our model does not account for several critical contextual variables. The most significant of these is defender proximity, as an open shot is fundamentally different from a contested one. Additionally, nuanced shot clock situations, such as the difference between a planned shot early in the clock and a rushed one as it expires, are not explicitly modeled.

### 4.2 Further Development

Develop an Enhanced Shot Quality Mode
Feature engineering with a more comprehensive data to enhanced shot quality model
 - defender_distance: The distance to the nearest defender.
 - Shot_type_detailed: Differentiate between catch-and-shoot, pull-up, and off-the-dribble shots.

Perform a Historical Trend Analysis of Two-for-One
 - Acquire play-by-play data for multiple past NBA seasons 
 - Run the same/new validation script on each season's data to calculate the frequency of two-for-one attempts year over year and present with visualization 













