# =============================================================================
# Script:  code/02_fit_models.R
# Purpose: Fit the foul, team, and player expected-points models.
# Output:  data/preprocessed/default_models.rds
# Usage:   From the project root, run source("code/02_fit_models.R"),
#          then fit_default_models().
# Scope:   Sourcing defines functions only; model fitting is explicit.
#
# Overview of what this file provides:
#   check_model_dependencies()  - verifies packages and R version.
#   load_cleaned_model_data()   - reads cleaned_shots.rds and derives lookups.
#   make_model_config()         - all tunable settings (thresholds, terms, FT%).
#   create_model_backend()      - returns fit_team(), fit_player() and
#                                 make_results(), sharing one data snapshot.
#   fit_default_models()        - fits everything and saves default_models.rds.
#   load_model_backend()        - reloads that bundle (used by App.R).
#
# Modeling idea: "expected points" for a shot scenario (zone x shot type x
# contest level) is estimated with a linear regression of points per shot,
# where points = field-goal points + expected free-throw points from fouls.
# A logistic foul model supplies foul probabilities for practice shots, since
# fouls are only recorded in games.
# =============================================================================

# Stops early with a clear message if a package or the R version is missing,
# then attaches dplyr/tidyr (the backend uses left_join, case_when, replace_na).
check_model_dependencies <- function() {
  packages <- c("dplyr", "tidyr")
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop("Install required packages: ", paste(missing, collapse = ", "))
  }
  if (getRversion() < "4.3.0") {
    stop("Use R 4.3.0 or newer.")
  }
  invisible(lapply(packages, function(p) {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }))
}


# Reads the RDS from 01_clean_data.R and builds the objects the models and the
# app need. Returns a named list (the "model data" snapshot).
load_cleaned_model_data <- function(cleaned_file) {
  if (!file.exists(cleaned_file)) {
    stop(
      "Cleaned data not found: ",
      cleaned_file,
      '. Run source("scripts/01_clean_data.R") from the project root first.'
    )
  }
  cleaned <- readRDS(cleaned_file)
  base_df <- as.data.frame(cleaned$base_df)
  shot_labels <- as.data.frame(cleaned$shot_labels)

  # Make sure no stale foul probabilities carry over from earlier runs.
  base_df$foul_prob <- NULL

  # TRIPLE identifies one "scenario": shot zone + shot type + contest level.
  # scenario_rows marks the FIRST row of each distinct scenario, so
  # scenario_data/scenario_labels have exactly one row per scenario. These are
  # the rows the fitted models predict on to build the heatmap.
  TRIPLE <- c("shot_zone", "shot_type", "shot_contest")
  scenario_rows <- !duplicated(shot_labels[TRIPLE])

  # These are descriptive counts.
  list(
    base_df = base_df,
    shot_labels = shot_labels,
    # Shots per scenario for the whole team.
    team_counts = dplyr::count(
      shot_labels,
      shot_zone,
      shot_type,
      shot_contest,
      name = "n"
    ),
    # Shots per scenario for each player.
    player_counts = dplyr::count(
      shot_labels,
      shooter,
      shot_zone,
      shot_type,
      shot_contest,
      name = "n"
    ),
    # Total shots per player (used for eligibility).
    player_totals = dplyr::count(shot_labels, shooter, name = "total_shots"),
    # One representative modeling row (dummy columns) per scenario.
    scenario_data = base_df[scenario_rows, , drop = FALSE],
    # The readable labels for those same scenario rows.
    scenario_labels = shot_labels[scenario_rows, TRIPLE, drop = FALSE]
  )
}


# Central place for every setting that shapes the models. Everything is built
# as a local variable and returned at the end via mget().
# `base_df` is only used here to discover the available column names.
make_model_config <- function(base_df) {
  # Legacy single-value thresholds (MIN_COMBINATION_SHOTS and
  # LIMITED_SAMPLE_MAX are used as defaults inside the backend;
  # MIN_PLAYER_SHOTS is kept in the config but DEFAULT_RULES is what
  # fit_default_models() actually reads).
  MIN_PLAYER_SHOTS <- 150L
  MIN_COMBINATION_SHOTS <- 10L
  LIMITED_SAMPLE_MAX <- 25L
  TRIPLE <- c("shot_zone", "shot_type", "shot_contest")

  # Free-throw percentage used for the team model and as a fallback for players
  # without their own entry in player_ft.
  team_ft_pct <- 0.751

  # Starting values for the dashboard's "Model rules" panel:
  #   *_total       minimum total shots before a player gets a model
  #   *_combination minimum shots in a scenario before an estimate is shown
  #   *_warning     upper count of the "limited sample" (asterisk) range
  DEFAULT_RULES <- list(
    player_total = 150L,
    player_combination = 10L,
    player_warning = 25L,
    team_combination = 10L,
    team_warning = 25L
  )

  # Columns that must never be used as predictors in the foul model:
  # identifiers, the outcome itself (foul), things that would leak the answer
  # (points, shot_value), and bookkeeping columns.
  foul_exclusions <- c(
    "result",
    "is_game",
    "shooter",
    "shot_location",
    "points",
    "shot_value",
    "foul",
    "scholarship",
    "position",
    "foul_prob",
    "wts"
  )

  # Foul model predictors = every remaining column (shot characteristics plus
  # whether the shot was made).
  foul_predictors <- setdiff(
    names(base_df),
    foul_exclusions
  )

  # made_shot is intentionally kept in the foul model (made shots can draw
  # "and-one" fouls). Dunks and putbacks are dropped from it.
  foul_predictors <- setdiff(
    foul_predictors,
    c("shot_type_dunk", "shot_type_putback")
  )

  # Expected-points predictors: the same pool, but WITHOUT made_shot, because
  # expected points are predicted from shot circumstances before the result is
  # known.
  ep_predictors <- setdiff(
    names(base_df),
    c(foul_exclusions, "made_shot")
  )

  # Interaction terms included by default for the team and for players with no
  # custom list.
  TEAM_INTERACTIONS <- c(
    "shot_clock_5_0:shot_zone_above_break_3s",
    "shot_clock_5_0:shot_zone_corner_3s",
    "shot_contest_highly_contested:shot_type_dunk",
    "shot_contest_highly_contested:shot_type_hook"
  )

  # Shot-clock terms removed from every expected-points model (including the
  # two shot-clock interactions listed above), so the estimates do not depend
  # on the clock.
  CLOCK_EXCLUSIONS <- c(
    "shot_clock_5_0",
    "shot_clock_10_6",
    "shot_clock_21_11",
    "shot_clock_5_0:shot_zone_above_break_3s",
    "shot_clock_5_0:shot_zone_corner_3s"
  )

  # Shot types removed from every expected-points model. Scenarios using them
  # are reported as not estimated (see fit_player) rather than silently
  # treated as the reference category (Layup).
  SHOT_EXCLUSIONS <- c(
    "shot_type_dunk",
    "shot_type_hook",
    "shot_type_putback"
  )

  # Final team regression terms: predictors + interactions, minus the clock
  # and shot-type exclusions. Note this also removes the two interactions in
  # TEAM_INTERACTIONS that involve dunk/hook (they would otherwise reference
  # variables no longer in the model).
  team_terms <- setdiff(
    c(ep_predictors, TEAM_INTERACTIONS),
    CLOCK_EXCLUSIONS
  )

  # Players treated as "Big"; everyone else is a "Guard". Used to pick which
  # position-specific foul model a player's practice shots are adjusted with.
  bigs <- c(
    "Dave Miller",
    "Mark Taylor",
    "Dan Wilson",
    "Steve Moore",
    "Mike Davis",
    "Paul Thomas"
  )

  # Individual free-throw percentages (named vector, looked up by player name).
  player_ft <- c(
    "Jim Smith" = 0.684,
    "Paul Thomas" = 0.739,
    "Mark Taylor" = 0.746,
    "Bob Johnson" = 0.702,
    "Mike Davis" = 0.823,
    "John Doe" = 0.818,
    "Steve Moore" = 0.744,
    "Bill Williams" = 0.716
  )

  # Collect the listed local variables into a named list (looked up only in
  # this function's own environment).
  mget(
    c(
      "MIN_PLAYER_SHOTS",
      "MIN_COMBINATION_SHOTS",
      "LIMITED_SAMPLE_MAX",
      "TRIPLE",
      "DEFAULT_RULES",
      "foul_predictors",
      "ep_predictors",
      "TEAM_INTERACTIONS",
      "CLOCK_EXCLUSIONS",
      "team_terms",
      "SHOT_EXCLUSIONS",
      "bigs",
      "player_ft",
      "team_ft_pct"
    ),
    envir = environment(),
    inherits = FALSE
  )
}


# Builds the set of model-fitting functions. All data and configuration
# objects are copied into this function's environment, so the inner functions
# below can use them directly (base_df, scenario_data, TRIPLE, ...) and the
# app can later fit extra players on demand using the same snapshot.
create_model_backend <- function(model_data, config) {
  # Snapshot shared by the saved defaults and future on-demand player fits.
  list2env(model_data, envir = environment())
  list2env(config, envir = environment())

  # --- Foul model ---------------------------------------------------------
  # Logistic regression predicting whether a shot drew a foul, fit on GAME
  # shots only (fouls are not recorded in practice). If `position` is given
  # ("Big" or "Guard"), only that position's game shots are used.
  fit_foul_model <- function(position = NULL) {
    g <- base_df[base_df$is_game, , drop = FALSE]

    if (!is.null(position)) {
      g <- g[
        !is.na(g$position) & g$position == position,
        ,
        drop = FALSE
      ]
    }

    # Convert the "Foul"/"No Foul" label to a 1/0 outcome.
    g$foul <- as.integer(g$foul == "Foul")

    # A logistic model needs both outcomes present.
    if (nrow(g) == 0 || length(unique(g$foul)) < 2) {
      stop("Insufficient game foul outcomes for this foul model.")
    }

    # na.fail makes the fit stop on any missing value instead of silently
    # dropping rows.
    model <- glm(
      reformulate(foul_predictors, response = "foul"),
      family = binomial(link = "logit"),
      data = g,
      na.action = na.fail
    )

    if (!isTRUE(model$converged)) {
      stop("Foul model did not converge.")
    }

    model
  }

  # --- Point adjustment ---------------------------------------------------
  # Turns each shot's raw points into points INCLUDING expected free throws:
  #   points = field-goal points + ft_pct * free_throw_attempts * P(foul)
  # Game shots use the recorded foul (1 or 0); practice shots use the foul
  # model's predicted probability.
  adjust_points <- function(d, foul_model, ft_pct) {
    # Rebuild from the raw outcome to avoid double adjustments.
    d$points <- ifelse(
      d$made_shot == 1,
      d$shot_value,
      0
    )

    # Free throws awarded if fouled: 1 on a made shot (and-one), otherwise as
    # many as the shot's value (2 or 3).
    attempts_if_fouled <- ifelse(
      d$made_shot == 1,
      1,
      d$shot_value
    )

    game_rows <- which(d$is_game)
    practice_rows <- which(!d$is_game)

    # Games: add the expected value of the free throws that actually occurred.
    d$points[game_rows] <- d$points[game_rows] +
      ft_pct *
        attempts_if_fouled[game_rows] *
        as.integer(d$foul[game_rows] == "Foul")

    # Practices: no recorded fouls, so use the predicted foul probability.
    if (length(practice_rows)) {
      foul_prob <- predict(
        foul_model,
        newdata = d[practice_rows, , drop = FALSE],
        type = "response"
      )

      if (any(!is.finite(foul_prob))) {
        stop("The foul model produced missing/nonfinite probabilities.")
      }

      d$points[practice_rows] <- d$points[practice_rows] +
        ft_pct *
          attempts_if_fouled[practice_rows] *
          foul_prob
    }

    d
  }

  # Predicts expected points for new scenario rows. rankdeficient = "NA" makes
  # predict.lm return NA (instead of a misleading number) for coefficients
  # that could not be estimated because of collinearity or missing data.
  predict_ep <- function(model, newdata) {
    as.numeric(
      predict(
        model,
        newdata = newdata,
        rankdeficient = "NA"
      )
    )
  }

  # --- Results table ------------------------------------------------------
  # Builds one row per scenario with the shot count `n`, the model's expected
  # points (only when n >= min_combination), and a status label:
  #   Insufficient shots / Not estimable / Limited sample / Meets sample rule.
  # `counts` holds the shot counts for the team or for one player. The
  # `player` argument is accepted for readability but is not used inside.
  make_results <- function(
    model,
    counts,
    player = NULL,
    min_combination = MIN_COMBINATION_SHOTS,
    warning_max = LIMITED_SAMPLE_MAX
  ) {
    # Start from every possible scenario and attach observed counts; scenarios
    # with no shots get n = 0.
    result <- scenario_labels |>
      left_join(
        counts |>
          select(all_of(TRIPLE), n),
        by = TRIPLE
      ) |>
      mutate(
        n = replace_na(n, 0L)
      )

    result$expected_points <- NA_real_

    # Only predict for scenarios with enough shots.
    supported <- result$n >= min_combination

    if (any(supported)) {
      result$expected_points[supported] <- predict_ep(
        model,
        scenario_data[supported, , drop = FALSE]
      )
    }

    # Flag limited-sample estimates for both team and player models.
    # Apply warnings to both team and player estimates.
    limited_sample <- (result$n >= min_combination & result$n <= warning_max)

    # case_when picks the first matching rule, so the order matters.
    result$status <- case_when(
      result$n < min_combination ~ "Insufficient shots",
      is.na(result$expected_points) ~ "Not estimable",
      limited_sample ~ "Limited sample",
      TRUE ~ "Meets sample rule"
    )

    result
  }

  # --- Individual player model --------------------------------------------
  # Fits one player's expected-points model and returns a list holding the
  # model, the foul model used, the results table and the training data.
  fit_player <- function(player) {
    if (length(player) != 1L || is.na(player) || !player %in% base_df$shooter) {
      stop("Select one player present in the cleaned data.")
    }

    # Pick the position-specific foul model (Big vs Guard).
    position <- if (player %in% bigs) "Big" else "Guard"

    foul_model <- fit_foul_model(position)

    # This player's shots only.
    p <- base_df[
      base_df$shooter == player,
      ,
      drop = FALSE
    ]

    # Use the player's own FT% if known, otherwise the team default.
    ft_pct <- if (player %in% names(player_ft)) {
      unname(player_ft[player])
    } else {
      0.751
    }

    # Add expected free-throw points to the player's shots.
    p <- adjust_points(p, foul_model, ft_pct)

    # Individual player models are fit unweighted: practice and game shots
    # count equally. Team-level weights in the cleaned data are untouched.
    p$wts <- 1

    # All players use the team's interaction terms (no player-specific overrides).
    interactions <- TEAM_INTERACTIONS

    excluded <- SHOT_EXCLUSIONS

    # Final term list: predictors + interactions, minus clock and
    # excluded shot-type main effects.
    terms <- setdiff(
      c(ep_predictors, interactions),
      c(CLOCK_EXCLUSIONS, excluded)
    )

    # Remove any interactions involving excluded shot-type predictors.
    # (strsplit breaks "a:b" into c("a", "b"); drop the term if any piece is
    # an excluded variable.)
    if (length(excluded)) {
      terms <- terms[
        !vapply(
          strsplit(terms, ":", fixed = TRUE),
          function(components) any(components %in% excluded),
          logical(1)
        )
      ]
    }

    # Weighted least squares; all weights are 1 here, so this is ordinary
    # least squares.
    model <- lm(
      reformulate(terms, response = "points"),
      data = p,
      weights = wts,
      na.action = na.fail
    )

    counts <- player_counts |>
      filter(shooter == player)

    result <- make_results(
      model,
      counts,
      player = player
    )

    # Do not silently interpret an excluded shot type as Layup: with those
    # dummies removed, a dunk/hook/putback scenario would be predicted as if
    # it were the reference category. Mark those scenarios as unestimated.
    # (Note: this uses the config default MIN_COMBINATION_SHOTS, whereas the
    # app re-applies this step with the user's current rule.)
    if (length(excluded)) {
      excluded_columns <- intersect(
        excluded,
        names(scenario_data)
      )

      if (length(excluded_columns)) {
        unsupported <- rowSums(
          scenario_data[, excluded_columns, drop = FALSE] == 1,
          na.rm = TRUE
        ) >
          0

        result$expected_points[unsupported] <- NA_real_

        result$status[
          unsupported &
            result$n >= MIN_COMBINATION_SHOTS
        ] <- "Excluded from player model"
      }
    }

    list(
      model = model,
      foul_model = foul_model,
      results = result,
      training_data = p
    )
  }

  # --- Team model ---------------------------------------------------------
  # Fits the all-player expected-points model. Unlike the player models, this
  # one uses the practice-shot weights (wts) from 01_clean_data.R.
  fit_team <- function() {
    # Team foul model uses all game shots (no position split).
    foul_model <- fit_foul_model()
    training <- adjust_points(base_df, foul_model, team_ft_pct)

    # Guard against a degenerate fit where every observation has zero weight.
    if (!any(training$wts > 0)) {
      stop("All team training weights are zero.")
    }

    model <- lm(
      reformulate(team_terms, response = "points"),
      data = training,
      weights = wts,
      na.action = na.fail
    )

    list(
      model = model,
      foul_model = foul_model,
      results = make_results(
        model,
        team_counts,
        min_combination = DEFAULT_RULES$team_combination,
        warning_max = DEFAULT_RULES$team_warning
      )
    )
  }

  # Expose only the three functions callers need.
  list(
    fit_team = fit_team,
    fit_player = fit_player,
    make_results = make_results
  )
}


# Fits the team model and a model for every player with enough shots, then
# saves everything (data snapshot, config, models, results, metadata) into one
# RDS bundle that App.R loads at startup. Returns the output path invisibly.
fit_default_models <- function(
  cleaned_file = file.path("data", "preprocessed", "cleaned_shots.rds"),
  output_file = file.path("data", "preprocessed", "default_models.rds")
) {
  check_model_dependencies()
  model_data <- load_cleaned_model_data(cleaned_file)
  config <- make_model_config(model_data$base_df)
  backend <- create_model_backend(model_data, config)

  message("Fitting default team models...")
  team <- backend$fit_team()

  # Players meeting the default total-shots rule.
  totals <- model_data$player_totals
  eligible <- totals$shooter[
    totals$total_shots >= config$DEFAULT_RULES$player_total
  ]

  # One slot per eligible player; failures are recorded rather than aborting
  # the whole run, so one bad player does not block the others.
  player_fits <- setNames(vector("list", length(eligible)), eligible)
  player_errors <- setNames(character(0), character(0))

  for (player in eligible) {
    message("Fitting player models: ", player)
    result <- tryCatch(backend$fit_player(player), error = function(e) e)
    if (inherits(result, "error")) {
      player_errors[player] <- conditionMessage(result)
      warning(player, ": ", conditionMessage(result), call. = FALSE)
    } else {
      player_fits[[player]] <- result
    }
  }

  # Everything the app needs, plus provenance (timestamps, input file hash,
  # package versions) so a stale or mismatched bundle can be recognized.
  bundle <- list(
    schema_version = 1L,
    model_data = model_data,
    config = config,
    team_model = team$model,
    team_foul_model = team$foul_model,
    team_results = team$results,
    player_fits = player_fits,
    player_errors = player_errors,
    metadata = list(
      prepared_at = Sys.time(),
      cleaned_file = cleaned_file,
      cleaned_file_md5 = tools::md5sum(cleaned_file),
      r_version = R.version.string,
      package_versions = vapply(
        c("stats", "dplyr", "tidyr"),
        function(p) as.character(utils::packageVersion(p)),
        character(1)
      ),
      eligible_players = eligible,
      successful_players = names(player_fits)[
        !vapply(player_fits, is.null, logical(1))
      ],
      failed_players = names(player_errors)
    )
  )

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(bundle, output_file)

  if (!file.exists(output_file) || file.info(output_file)$size <= 0) {
    stop("Model RDS was not created successfully.")
  }

  message("Saved: ", output_file)
  message(
    "Successful player fits: ",
    length(bundle$metadata$successful_players)
  )
  message("Failed player fits: ", length(player_errors))
  invisible(output_file)
}


# Loads the saved bundle and rebuilds the backend functions from the data and
# configuration stored inside it (functions themselves are not saved). The app
# uses `$bundle` for the pre-fit models and `$backend` to fit more players
# on demand when thresholds change.
load_model_backend <- function(
  file = file.path("data", "preprocessed", "default_models.rds")
) {
  check_model_dependencies()

  if (!file.exists(file)) {
    stop("Model bundle not found. Run fit_default_models() first.")
  }

  bundle <- readRDS(file)

  if (!identical(bundle$schema_version, 1L)) {
    stop("Unsupported model bundle schema.")
  }

  list(
    bundle = bundle,
    backend = create_model_backend(bundle$model_data, bundle$config)
  )
}
