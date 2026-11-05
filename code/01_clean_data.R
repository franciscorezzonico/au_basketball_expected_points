# =============================================================================
# Script:  code/01_clean_data.R
# Purpose: Prepare practice and game shot data, weights, and summary counts.
# Output:  data/preprocessed/cleaned_shots.rds
# Usage:   From the project root, run source("code/01_clean_data.R").
# Scope:   Preprocessing only; no model fitting or eligibility thresholds.
#
# Pipeline overview:
#   1. Read the raw practice and game CSVs and stack them into one table.
#   2. Repair Excel-mangled shot-clock labels and drop invalid rows.
#   3. Drop columns the models never use.
#   4. Compute practice-shot weights.
#   5. Collapse categories, build dummy (0/1) columns, snake_case the names.
#   6. Count shots per scenario and save everything as one list in an RDS file.
# =============================================================================

library(fastDummies)
library(janitor)
library(dplyr)

# Resolve input and output paths relative to the project root.
raw_dir <- file.path("data", "raw")
output_dir <- file.path("data", "preprocessed")
output_file <- file.path(output_dir, "cleaned_shots.rds")
input_files <- file.path(
  raw_dir,
  c("Practice_Cleaned_All.csv", "Game_Cleaned_All.csv")
)


# ---- 1. Read and combine the raw data ---------------------------------------

# Read practice and game data.
practice_shots <- read.csv(
  file.path(raw_dir, "Practice_Cleaned_All.csv"),
  stringsAsFactors = FALSE
)

game_shots <- read.csv(
  file.path(raw_dir, "Game_Cleaned_All.csv"),
  stringsAsFactors = FALSE
)

# Tag each shot with its source, then add any column that exists in only one
# file (filled with NA) so rbind() sees identical column sets.
practice_shots$is.game <- FALSE
game_shots$is.game <- TRUE

practice_shots[setdiff(names(game_shots), names(practice_shots))] <- NA
game_shots[setdiff(names(practice_shots), names(game_shots))] <- NA
df <- rbind(practice_shots, game_shots, stringsAsFactors = FALSE)


# ---- 2. Repair and validate categorical values ------------------------------

# Excel auto-converted the shot-clock labels "10-6" and "5-0" into dates
# ("6-Oct" and "May-00"). Convert them back to the intended labels.
df$Shot.Clock[df$Shot.Clock == "6-Oct"] <- "10-6"
df$Shot.Clock[df$Shot.Clock == "May-00"] <- "5-0"

# Lists of accepted values for each categorical column. Any row with a
# value outside these lists (typos, blanks, new tags) is dropped below.
valid_Results <- c("Made 3", "Made 2", "Miss 3", "Miss 2")

valid_Shot.Clocks <- c("5-0", "10-6", "21-11", "22+")

valid_Shot.Contests <- c(
  "Highly Contested",
  "Lightly Contested",
  "Wide Open",
  "Blocked"
)

valid_Shot.Locations <- c(
  "Rim",
  "Close Shot Left",
  "Close Shot Middle",
  "Close Shot Right",
  "Left Corner 2",
  "Left Corner 3",
  "Left Wing 2",
  "Left Wing 3",
  "Right Corner 2",
  "Right Corner 3",
  "Right Wing 2",
  "Right Wing 3",
  "Top of Key 2",
  "Top of Key 3"
)

valid_Shot.Types <- c(
  "Layup",
  "Catch & Shoot",
  "Pullup",
  "Fadeaway",
  "Hook",
  "Step Back",
  "Floater",
  "Putback",
  "Dunk",
  "Other"
)

# Returns TRUE only if every checked field in a one-row data frame is an
# accepted category and the shooter is present and nonempty.
isRowValid <- function(one_row) {
  if (
    one_row$Result %in%
      valid_Results &
      one_row$Shot.Clock %in% valid_Shot.Clocks &
      one_row$Shot.Contest %in% valid_Shot.Contests &
      one_row$Shot.Location %in% valid_Shot.Locations &
      one_row$Shot.Type %in% valid_Shot.Types &
      !is.na(one_row$Shooter) &
      one_row$Shooter != ""
  ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Apply the check row by row, then keep only the valid rows. drop = FALSE keeps
# each row as a data frame. Compute validation flags.
valid_rows <- sapply(1:nrow(df), function(i) {
  isRowValid(df[i, , drop = FALSE])
})

df <- df[valid_rows, ]

# ---- 3. Practice-shot weights -----------------------------------------------

# Default weight of 1 for every shot; practice rows are overwritten below.
df$wts <- 1

# Weight each player's practice shots by (share of team game shots) /
# (share of team practice shots), so practice data is re-balanced toward the
# game mix.
player_names <- unique(df$Shooter)

game_shot_proportions <- prop.table(table(factor(
  df$Shooter[df$is.game],
  levels = player_names
)))

practice_shot_proportions <- prop.table(table(factor(
  df$Shooter[!df$is.game],
  levels = player_names
)))

# Ratio of game share to practice share (>1 means the player shoots relatively
# more in games than in practice). Dividing by the maximum rescales weights
# to the range 0-1, so the most under-represented player's practice shots
# count fully and everyone else's count less.
# Caution: a player with no practice shots gives a 0 denominator (Inf/NaN),
# and a player with no game shots gets weight 0.
raw_player_practice_weights <- game_shot_proportions /
  practice_shot_proportions

player_practice_weights <- raw_player_practice_weights /
  max(raw_player_practice_weights)

# Look up each shot's weight by shooter name. Game shots always keep weight 1.
wt_lookup <- player_practice_weights[df$Shooter]
df$wts <- ifelse(df$is.game, 1, wt_lookup)


# ---- 4. Outcome, filtering and encoding -------------------------------------

# Raw field-goal points: the shot's value (2 or 3) if made, otherwise 0.
# Foul/free-throw adjustments are added later in 02_fit_models.R.
df$Points <- ifelse(df$MadeShot == TRUE, df$ShotValue, 0)

# Categorical predictors to one-hot encode, each paired (by position) with the
# reference category that is left out so the regression is not collinear.
cat_vars <- c("Shot.Clock", "Shot.Contest", "ShotZone", "Shot.Type")
cat_base_vals <- c("22+", "Wide Open", "In the Paint", "Layup")

# Ensure subsequent operations use data-frame (not tibble/data.table) subsetting.
df <- as.data.frame(df)

# Drop "Other" shot types and blocked shots; they are rare and hard to
# interpret, so they would add noise to the models.
df <- df[df$Shot.Type != "Other" & df$Shot.Contest != "Blocked", ]

# Merge the four jump-shot variants into one "Jump Shot" category.
df$Shot.Type[df$Shot.Type == "Catch & Shoot"] <- "Jump Shot"
df$Shot.Type[df$Shot.Type == "Fadeaway"] <- "Jump Shot"
df$Shot.Type[df$Shot.Type == "Pullup"] <- "Jump Shot"
df$Shot.Type[df$Shot.Type == "Step Back"] <- "Jump Shot"

# Save readable labels BEFORE dummy encoding replaces them.
shot_labels <- data.frame(
  shooter = df$Shooter,
  shot_zone = df$ShotZone,
  shot_type = df$Shot.Type,
  shot_contest = df$Shot.Contest,
  stringsAsFactors = FALSE
)

# One-hot encode the categorical columns (creates e.g. Shot.Clock_5-0).
df <- dummy_cols(df, select_columns = cat_vars)

# Build the dummy column names of the reference categories (e.g.
# "Shot.Clock_22+") and delete them, then delete the original text columns.
base_col_names <- vector(length = length(cat_vars))
for (i in 1:length(cat_vars)) {
  base_col_names[i] <- paste0(cat_vars[i], "_", cat_base_vals[i])
}
df <- df[, !(names(df) %in% base_col_names)]
df <- df[, !(names(df) %in% cat_vars)]

# Convert all column names to snake_case.
df <- clean_names(df)
base_df <- df


# ---- 5. Summary counts and output -------------------------------------------

# Raw counts of retained shots, with no eligibility thresholds; the model/app
# code decides what is shown.
# team_counts   : shots per (zone, type, contest) combination, all players.
# player_counts : the same, split by shooter.
# player_totals : total shots per shooter.
team_counts <- dplyr::count(
  shot_labels,
  shot_zone,
  shot_type,
  shot_contest,
  name = "n"
)

player_counts <- dplyr::count(
  shot_labels,
  shooter,
  shot_zone,
  shot_type,
  shot_contest,
  name = "n"
)

player_totals <- dplyr::count(shot_labels, shooter, name = "total_shots")


# Bundle everything downstream scripts need into one list.
cleaned_data <- list(
  schema_version = 1L,
  base_df = df,
  shot_labels = shot_labels,
  team_counts = team_counts,
  player_counts = player_counts,
  player_totals = player_totals,
  metadata = list(
    created_at = Sys.time(),
    input_files = input_files,
    r_version = R.version.string,
    cleaned_shots = nrow(base_df),
    game_shots = sum(base_df$is_game),
    practice_shots = sum(!base_df$is_game),
    zero_weight_shots = sum(base_df$wts == 0)
  )
)

# Save the complete object, creating parent directories if needed.
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(cleaned_data, output_file)

# Safety check: stop if the saved file is missing or empty.
if (!file.exists(output_file) || file.info(output_file)$size <= 0) {
  stop("The cleaned-data RDS was not created successfully.")
}

# Console summary of what was saved.
message("Saved cleaned data: ", output_file)
message("Cleaned shots: ", nrow(base_df))
message("Game shots: ", sum(base_df$is_game))
message("Practice shots: ", sum(!base_df$is_game))
message("Zero-weight shots retained: ", sum(base_df$wts == 0, na.rm = TRUE))
