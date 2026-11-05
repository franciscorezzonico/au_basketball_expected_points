# =============================================================================
# Script:  App.R
# Purpose: Shiny dashboard showing expected points by shot zone, shot type,
#          and contest level for the team and for individual players.
# Input:   data/preprocessed/default_models.rds (from 02_fit_models.R)
# Usage:   With au_basketball_expected_points as the working directory, run
#          shiny::runApp(). Default models must be prepared first.
# Scope:   Presentation and reactive logic only; default models are loaded,
#          not fitted (players are fitted on demand if the rules change).
#
# App structure:
#   1. Setup  - load packages, source the model backend, load saved models.
#   2. UI     - CSS theme, header, "Model rules" side panel, and three tabs
#               (Expected points, Player eligibility, Interpretation).
#   3. Server - reactive logic: applied rules, player-model cache, results
#               table, KPI cards, heatmap, eligibility table, CSV download.
#
# Workflow: run 01_clean_data.R, then fit_default_models() from
# 02_fit_models.R, then start this app.
# =============================================================================

library(shiny) # app framework
library(dplyr) # data manipulation in the server code
library(tidyr) # replace_na etc. (also needed by the backend)
library(ggplot2) # heatmap
library(DT) # interactive eligibility table


# ---- 1. Setup ---------------------------------------------------------------

# Fail early with a helpful message if the app is started from the wrong folder.
model_script <- file.path("code", "02_fit_models.R")
if (!file.exists(model_script)) {
  stop(
    "Cannot find scripts/02_fit_models.R. Run the app from the project root."
  )
}

# Defines the external functions; does not fit default models.
source(model_script, local = TRUE)

# Load the saved bundle (pre-fit models + data snapshot + settings) and a
# backend rebuilt from it, so the app can fit extra players if needed.
loaded_models <- load_model_backend(
  file.path("data", "preprocessed", "default_models.rds")
)
model_bundle <- loaded_models$bundle
model_backend <- loaded_models$backend

# Reuse the data/configuration snapshot saved with the default models, so the
# app always matches the models it displays.
base_df <- model_bundle$model_data$base_df
shot_labels <- model_bundle$model_data$shot_labels
team_counts <- model_bundle$model_data$team_counts
player_counts <- model_bundle$model_data$player_counts
player_totals <- model_bundle$model_data$player_totals
scenario_data <- model_bundle$model_data$scenario_data
scenario_labels <- model_bundle$model_data$scenario_labels

# Settings stored with the bundle.
TRIPLE <- model_bundle$config$TRIPLE
SHOT_EXCLUSIONS <- model_bundle$config$SHOT_EXCLUSIONS
default_rules <- model_bundle$config$DEFAULT_RULES

team_model <- model_bundle$team_model
team_foul_model <- model_bundle$team_foul_model
player_fits <- model_bundle$player_fits
player_errors <- model_bundle$player_errors

# Existing server calls now delegate to the external backend.
fit_player <- model_backend$fit_player
make_results <- model_backend$make_results

# Team baseline E[Points]: the team model's expected points per shot, averaged
# over every shot combination and weighted by how often each one occurs. It is
# computed once with a minimum of 1 shot, so it does not depend on the Model
# rules thresholds, the view, or any filter. Combinations the model cannot
# estimate (NA) are left out of the average.
team_baseline_ep <- local({
  baseline <- make_results(
    model = team_model,
    counts = team_counts,
    min_combination = 1L,
    warning_max = 1L
  )

  usable <- !is.na(baseline$expected_points) & baseline$n > 0

  if (any(usable)) {
    weighted.mean(
      baseline$expected_points[usable],
      w = baseline$n[usable]
    )
  } else {
    NA_real_
  }
})

# Players whose default model was fitted successfully (failed fits are NULL).
available_players <- names(player_fits)[
  !vapply(player_fits, is.null, logical(1))
]

# Dashboard colors.
AU_BLUE <- "#004FA2"
AU_RED <- "#E0263C"
AU_BACKGROUND <- "#F4F6FA"
AU_WHITE <- "#FFFFFF"
AU_TEXT <- "#253247"
AU_BORDER <- "#DCE3ED"


# ---- 2. User interface ------------------------------------------------------

ui <- fluidPage(
  # Main stylesheet. sprintf() fills the numbered placeholders from the color
  # constants listed at the end of the call:
  #   %1$s background, %2$s text, %3$s blue, %4$s red, %5$s border.
  # (Any literal percent sign inside this string would need to be written %%.)
  tags$head(
    tags$style(
      HTML(
        sprintf(
          "
          /* Overall page */
          body {
            background-color: %1$s;
            color: %2$s;
            font-family: Arial, Helvetica, sans-serif;
          }

          /* Dashboard heading */
          .au-header {
            display: flex;
            align-items: center;
            gap: 22px;
            background-color: %3$s;
            color: white;
            padding: 22px 26px;
            margin: 0 -15px 24px -15px;
            border-bottom: 6px solid %4$s;
          }
          
          .au-logo {
            width: 110px;
            height: auto;
            flex-shrink: 0;
            display: block;
            border-radius: 6px;
          }

          .au-header-text {
            min-width: 0;
          }

          .au-header h2 {
            color: white;
            margin: 0;
            font-size: 28px;
            font-weight: 700;
          }

          .au-header p {
            color: white;
            margin: 8px 0 0 0;
            font-size: 15px;
          }

          @media (max-width: 600px) {
            .au-header {
              gap: 14px;
              padding: 18px;
            }

            .au-logo {
              width: 75px;
            }

            .au-header h2 {
              font-size: 21px;
            }

            .au-header p {
              font-size: 13px;
            }
          }

          /* Sidebar */
          .well {
            background-color: white;
            border: 1px solid %5$s;
            border-top: 5px solid %3$s;
            border-radius: 10px;
            box-shadow: 0 3px 10px rgba(0, 0, 0, 0.05);
          }

          .control-label {
            color: %3$s;
            font-weight: 700;
          }

          .help-block {
            color: #526174;
          }

          /* Radio buttons and checkboxes */
          input[type='radio'],
          input[type='checkbox'] {
            accent-color: %3$s;
          }

          /* Dropdown focus */
          .selectize-input.focus {
            border-color: %3$s;
            box-shadow: 0 0 0 2px rgba(0, 79, 162, 0.15);
          }

          .selectize-dropdown .active {
            background-color: %3$s;
            color: white;
          }

          /* Tabs */
          .nav-tabs {
            border-bottom: 2px solid %3$s;
          }

          .nav-tabs > li > a {
            color: %3$s;
            font-weight: 600;
            border-radius: 6px 6px 0 0;
          }

          .nav-tabs > li > a:hover {
            background-color: #E8EFF8;
            border-color: transparent;
          }

          .nav-tabs > li.active > a,
          .nav-tabs > li.active > a:hover,
          .nav-tabs > li.active > a:focus {
            background-color: %3$s;
            color: white;
            border-color: %3$s;
            border-bottom: 3px solid %4$s;
          }

          /* Content area */
          .tab-content {
            background-color: white;
            padding: 20px;
            border: 1px solid %5$s;
            border-top: none;
            border-radius: 0 0 10px 10px;
          }

          /* Buttons */
          .btn-default {
            background-color: %3$s;
            color: white;
            border-color: %3$s;
            border-radius: 6px;
          }

          .btn-default:hover,
          .btn-default:focus,
          .btn-default:active {
            background-color: %4$s;
            color: white;
            border-color: %4$s;
          }

          /* Summary */
          #summary {
            color: %3$s;
            font-size: 17px;
            font-weight: 600;
            margin-bottom: 18px;
          }

          /* Data tables */
          table.dataTable thead th {
            background-color: %3$s;
            color: white;
            border-bottom: 3px solid %4$s !important;
          }

          table.dataTable tbody tr:hover {
            background-color: #EDF3FB !important;
          }

          /* Keyboard focus */
          a:focus-visible,
          button:focus-visible {
            outline: 3px solid %4$s;
            outline-offset: 3px;
          }

          /* KPI bar */
          .au-kpi-grid {
            display: grid;
            grid-template-columns: repeat(6, minmax(0, 1fr));
            gap: 14px;
            margin-bottom: 10px;
          }

          /* Individual KPI cards */
          .au-kpi-card {
            background-color: white;
            border: 1px solid %5$s;
            border-top: 4px solid %3$s;
            border-radius: 9px;
            padding: 16px;
            box-shadow: 0 3px 10px rgba(0, 0, 0, 0.05);
            min-width: 0;
          }

          /* Alternate red accent */
          .au-kpi-card.au-kpi-red {
            border-top-color: %4$s;
          }

          .au-kpi-label {
            color: #526174;
            font-size: 12px;
            font-weight: 700;
            letter-spacing: 0.5px;
            text-transform: uppercase;
            margin-bottom: 8px;
          }

          .au-kpi-value {
            color: %3$s;
            font-size: 29px;
            font-weight: 700;
            line-height: 1.15;
          }

          .au-kpi-detail {
            color: #667085;
            font-size: 12px;
            margin-top: 7px;
          }

          .au-kpi-note {
            color: #667085;
            font-size: 12px;
            margin-bottom: 20px;
          }

          /* Medium screens */
          @media (max-width: 1200px) {
            .au-kpi-grid {
              grid-template-columns: repeat(3, minmax(0, 1fr));
            }
          }

          /* Small screens */
          @media (max-width: 700px) {
            .au-kpi-grid {
              grid-template-columns: repeat(2, minmax(0, 1fr));
            }

            .au-kpi-value {
              font-size: 25px;
            }
          }

          /* Narrow mobile screens */
          @media (max-width: 420px) {
            .au-kpi-grid {
              grid-template-columns: 1fr;
            }
          }

          /* Rules button */
          .au-rules-toolbar {
            display: flex;
            justify-content: flex-end;
            margin-bottom: 16px;
          }

          /* Right-hand settings panel */
          .au-rules-panel {
            position: fixed;
            top: 0;
            right: 0;
            bottom: 0;
            width: 370px;
            max-width: 95vw;
            background-color: white;
            border-left: 5px solid %3$s;
            box-shadow: -6px 0 24px rgba(0, 0, 0, 0.18);
            padding: 24px;
            overflow-y: auto;
            z-index: 2000;
          }

          .au-rules-heading {
            display: flex;
            align-items: center;
            justify-content: space-between;
            gap: 12px;
          }

          .au-rules-heading h3 {
            color: %3$s;
            font-weight: 700;
            margin: 0;
          }

          .au-rules-panel h4 {
            color: %3$s;
            font-weight: 700;
          }

          .au-rules-description {
            color: #526174;
            margin-top: 16px;
          }

          .au-rules-panel #apply_rules {
            background-color: %3$s;
            border-color: %3$s;
            color: white;
          }

          .au-rules-panel #apply_rules:hover {
            background-color: %4$s;
            border-color: %4$s;
          }

          .au-rules-panel pre {
            background-color: #F4F6FA;
            border-color: %5$s;
            color: #253247;
            white-space: pre-wrap;
            font-size: 12px;
          }
          ",
          AU_BACKGROUND,
          AU_TEXT,
          AU_BLUE,
          AU_RED,
          AU_BORDER
        )
      )
    )
  ),

  # Second stylesheet for the filter panel. It is a plain string (no sprintf),
  # so the colors are written out directly instead of using the AU_* values.
  tags$style(
    HTML(
      "
      /* Horizontal filter panel */
      .au-filter-panel {
        background-color: #F4F6FA;
        border: 1px solid #DCE3ED;
        border-top: 4px solid #004FA2;
        border-radius: 9px;
        padding: 18px 20px;
        margin-bottom: 22px;
      }

      .au-filter-panel .form-group {
        margin-bottom: 12px;
      }

      /* Checkbox and download button */
      .au-filter-actions {
        display: flex;
        align-items: center;
        justify-content: space-between;
        flex-wrap: wrap;
        gap: 12px;
        margin-top: 4px;
      }

      .au-filter-actions .form-group {
        margin-bottom: 0;
      }

      .au-filter-actions .checkbox {
        margin-top: 0;
        margin-bottom: 0;
      }

      /* Supporting instructions */
      .au-filter-help {
        margin-top: 12px;
      }

      .au-filter-help .help-block {
        margin-bottom: 0;
        font-size: 12px;
      }

      /* Mobile layout */
      @media (max-width: 767px) {
        .au-filter-panel {
          padding: 16px;
        }

        .au-filter-actions {
          align-items: flex-start;
          flex-direction: column;
        }
      }
      
      /* Interpretation tab */
      .au-interpretation {
        max-width: 900px;
        line-height: 1.55;
      }

      .au-interpretation h4 {
        color: #004FA2;
        font-weight: 700;
        margin-top: 24px;
      }

      .au-interpretation h4:first-child {
        margin-top: 0;
      }

      .au-interpretation li {
        margin-bottom: 6px;
      }
      "
    )
  ),

  # Page header: logo and titles. The image is served from the app's www/
  # folder (www/au-basketball-logo.jpg).
  div(
    class = "au-header",

    tags$img(
      src = "au-basketball-logo.jpg",
      class = "au-logo",
      alt = "American University Men's Basketball logo"
    ),

    div(
      class = "au-header-text",

      h2(
        "American University Men's Basketball"
      ),

      p(
        "Expected Points Dashboard"
      )
    )
  ),

  # Public demonstration notice
  div(
    class = "alert alert-info",
    role = "note",

    tags$strong("Fictitious data — public demonstration. "),

    "All player names and data values were randomly chosen or generated ",
    "for privacy purposes and do not represent actual team or player ",
    "performance. Unusual results may arise from the artificial data; ",
    "the dashboard's functionality and analytical workflow remain the same."
  ),

  # Button above the dashboard
  div(
    class = "au-rules-toolbar",

    actionButton(
      "open_rules",
      "Model rules",
      icon = icon("sliders")
    )
  ),

  # Hidden right-hand settings panel.
  # Show/hide trick: the open and close buttons each count their clicks.
  # The panel is visible when the combined click count is odd (open pressed
  # once more than close), and hidden when it is even. `|| 0` covers the
  # initial NULL values before anything is clicked.
  conditionalPanel(
    condition = paste0(
      "((input.open_rules || 0) + ",
      "(input.close_rules || 0)) % 2 === 1"
    ),

    div(
      class = "au-rules-panel",
      role = "region",
      `aria-label` = "Model inclusion and display rules",

      div(
        class = "au-rules-heading",

        h3("Model rules"),

        actionButton(
          "close_rules",
          label = NULL,
          icon = icon("times"),
          title = "Close settings"
        )
      ),

      p(
        class = "au-rules-description",
        "Edit the thresholds below, then click Apply rules."
      ),

      tags$hr(),

      # Inputs below only DRAFT new thresholds; nothing changes until the
      # Apply rules button is pressed (see observeEvent(input$apply_rules)).
      # Default values mirror DEFAULT_RULES from the model config.
      h4("Player models"),

      numericInput(
        "rule_player_total",
        "Minimum total shots for a player model",
        value = 150,
        min = 1,
        step = 1
      ),

      numericInput(
        "rule_player_combination",
        "Minimum shots per player combination",
        value = 10,
        min = 1,
        step = 1
      ),

      numericInput(
        "rule_player_warning",
        "Limited-sample warning: upper count",
        value = 25,
        min = 1,
        step = 1
      ),

      helpText(
        "The warning applies from the player-combination minimum ",
        "through this upper count, inclusive."
      ),

      tags$hr(),

      h4("Team model"),

      numericInput(
        "rule_team_combination",
        "Minimum shots per team combination",
        value = 10,
        min = 1,
        step = 1
      ),

      numericInput(
        "rule_team_warning",
        "Limited-sample warning: upper count",
        value = 25,
        min = 1,
        step = 1
      ),

      helpText(
        "Team estimates are flagged from the team-combination ",
        "minimum through this upper count, inclusive."
      ),

      tags$hr(),

      actionButton(
        "apply_rules",
        "Apply rules",
        icon = icon("check"),
        class = "btn-block"
      ),

      br(),

      tags$strong("Currently applied"),

      # Text summary of the rules actually in effect (server: applied_rules_text).
      verbatimTextOutput("applied_rules_text"),

      helpText(
        "Counts include all cleaned shots, including zero-weight shots."
      ),

      helpText(
        "Combination thresholds hide or reveal estimates; ",
        "they do not remove shots from model training."
      )
    )
  ),

  tabsetPanel(
    # ---- Tab 1: expected points heatmap ----
    tabPanel(
      "Expected points",

      br(),

      # ========================================================
      # KPI BAR
      # ========================================================

      # Filled by output$kpi_bar in the server.
      uiOutput("kpi_bar"),

      # ========================================================
      # FILTERS BELOW THE KPI BAR
      # ========================================================

      div(
        class = "au-filter-panel",

        fluidRow(
          column(
            width = 3,

            # Team vs. individual player view.
            radioButtons(
              "view",
              "Model",
              choices = c("Team", "Player"),
              selected = "Team",
              inline = TRUE
            ),

            # Player picker appears only in Player view. Its choices are
            # refreshed by the server whenever eligibility changes.
            conditionalPanel(
              condition = "input.view == 'Player'",

              selectInput(
                "player",
                "Player",
                choices = available_players,
                width = "100%"
              )
            )
          ),

          # The three dropdowns below filter which scenarios are shown;
          # their choices come from the labels actually present in the data.
          column(
            width = 3,

            selectInput(
              "zone",
              "Shot zone",
              choices = c(
                "All",
                sort(unique(shot_labels$shot_zone))
              ),
              width = "100%"
            )
          ),

          column(
            width = 3,

            selectInput(
              "type",
              "Shot type",
              choices = c(
                "All",
                sort(unique(shot_labels$shot_type))
              ),
              width = "100%"
            )
          ),

          column(
            width = 3,

            selectInput(
              "contest",
              "Contest level",
              choices = c(
                "All",
                sort(unique(shot_labels$shot_contest))
              ),
              width = "100%"
            )
          )
        ),

        div(
          class = "au-filter-actions",

          # When checked, scenarios with no reportable estimate are hidden.
          checkboxInput(
            "available_only",
            "Show only available estimates",
            value = TRUE
          ),

          downloadButton(
            "download",
            "Download displayed results"
          )
        ),

        div(
          class = "au-filter-help",

          helpText(
            "Use Model rules to change player eligibility, ",
            "combination thresholds, and limited-sample warnings. ",
            "Rule changes take effect only after clicking Apply rules."
          )
        )
      ),

      # ========================================================
      # RESULTS
      # ========================================================

      textOutput("summary"),

      plotOutput(
        "heatmap",
        height = "650px"
      )
    ),

    # ---- Tab 2: which players have models, and why not ----
    tabPanel(
      "Player eligibility",

      br(),

      DTOutput("eligibility")
    ),

    # ---- Tab 3: static explanatory text ----
    tabPanel(
      "Interpretation",

      br(),

      div(
        class = "au-interpretation",

        h4("What the heatmap shows"),
        tags$p(
          "Each cell is one shot combination: a shot zone, a shot type, ",
          "and a contest level. The large number is the expected points ",
          "per shot, E[Points], for that combination. The n beneath it is ",
          "the number of recorded shots in that combination."
        ),

        h4("What E[Points] means"),
        tags$ul(
          tags$li(
            "E[Points] estimates what an average shot of that kind is ",
            "worth. It comes from a regression model rather than a raw ",
            "average, so each estimate also draws on information from ",
            "related shots."
          ),
          tags$li(
            "It counts field-goal points plus the expected free-throw ",
            "points from fouls drawn. Fouls are only recorded in games, ",
            "so for practice shots the chance of a foul is predicted from ",
            "a model fit on game shots."
          ),
          tags$li("Shot clock is not part of the estimates.")
        ),

        h4("Team and player models"),
        tags$ul(
          tags$li(
            tags$strong("Team: "),
            "uses every player's shots. Practice shots are down-weighted ",
            "for each player according to how their share of game shots ",
            "compares with their share of practice shots, so players who ",
            "shoot a lot in practice but rarely in games do not drive the ",
            "results."
          ),
          tags$li(
            tags$strong("Player: "),
            "uses only that player's shots, with practice and game shots ",
            "counted equally."
          ),
          tags$li(
            "The team model includes the selected player's shots, so the ",
            "comparison is not a leave-one-player-out comparison. In ",
            "Player view, the downloaded file adds the team estimate for ",
            "the same zone, shot type, and contest level, and the ",
            "difference."
          )
        ),

        h4("Reading the cells"),
        tags$ul(
          tags$li(
            tags$strong("n "),
            "is a raw count of cleaned practice and game shots, not a ",
            "weighted sample size."
          ),
          tags$li(
            tags$strong("An asterisk (*) "),
            "marks a limited sample: the count falls between the minimum ",
            "and the warning upper count in Model rules (10\u201325 by ",
            "default). Treat these estimates with extra caution."
          ),
          tags$li(
            tags$strong("A grey cell with a dash "),
            "has no reportable estimate. This is not zero expected ",
            "points. It means there were too few shots, the shot type is ",
            "not modeled for that player, or the prediction could not be ",
            "estimated. \"Show only available estimates\" hides these ",
            "cells."
          )
        ),

        h4("Model rules"),
        tags$p(
          "The thresholds only hide or reveal estimates; they do not ",
          "change which shots are used to train the models. The player ",
          "minimum decides who gets an individual model (see the Player ",
          "eligibility tab). Changes take effect only after clicking ",
          "Apply rules."
        ),

        h4("Summary cards"),
        tags$ul(
          tags$li(
            "Total, game, and practice shots and field-goal percentage ",
            "describe the selected team or player and shot filters, using ",
            "all cleaned shots."
          ),
          tags$li(
            tags$strong("Team E[Points] "),
            "is the team model's average per shot across all shots. It is ",
            "a fixed baseline and never changes with the selection."
          ),
          tags$li(
            tags$strong("Selected E[Points] "),
            "averages the estimates currently displayed, weighted by shot ",
            "count. Comparing it with the team baseline shows whether the ",
            "selected shots are worth more or less than the average shot."
          )
        ),

        h4("Use with care"),
        tags$p(
          "These are model-based estimates, and practice shots stand in ",
          "for game conditions. Small differences between cells are not ",
          "meaningful, especially for cells marked with an asterisk."
        )
      )
    )
  )
)


# ============================================================
# SHINY SERVER
# ============================================================

server <- function(input, output, session) {
  # ============================================================
  # APPLIED SETTINGS
  # Updated only by the Apply rules button.
  # ============================================================

  # reactiveVal holding the thresholds currently in effect. Editing the inputs
  # in the Model rules panel does NOT change this until Apply is pressed.
  applied_rules <- reactiveVal(default_rules)

  # Reuse the default player models loaded from the saved RDS.
  # These caches belong to the current user session: models fitted on demand
  # are stored here so each player is fitted at most once per session.
  player_model_cache <- reactiveVal(player_fits)
  player_error_cache <- reactiveVal(player_errors)

  # ============================================================
  # ELIGIBILITY UNDER THE APPLIED RULES
  # ============================================================

  # Players whose total shots meet the applied minimum.
  eligible_players_now <- reactive({
    rules <- applied_rules()

    player_totals |>
      filter(total_shots >= rules$player_total) |>
      pull(shooter)
  })

  # Eligible players who also have a successfully fitted model in the cache
  # (a player can be eligible but missing a model if fitting failed).
  available_players_now <- reactive({
    eligible <- eligible_players_now()
    cache <- player_model_cache()

    eligible[
      vapply(
        eligible,
        function(player) {
          !is.null(cache[[player]])
        },
        logical(1)
      )
    ]
  })

  # ============================================================
  # APPLY RULES
  # ============================================================

  # Runs when "Apply rules" is clicked: validates the inputs, fits any newly
  # eligible players that have no cached model, then commits the new rules.
  observeEvent(
    input$apply_rules,
    {
      values <- list(
        player_total = input$rule_player_total,
        player_combination = input$rule_player_combination,
        player_warning = input$rule_player_warning,
        team_combination = input$rule_team_combination,
        team_warning = input$rule_team_warning
      )

      # A threshold must be a single, finite, whole number >= 1 (this also
      # rejects blank inputs, which arrive as NA).
      valid_integer <- function(x) {
        is.numeric(x) &&
          length(x) == 1L &&
          !is.na(x) &&
          is.finite(x) &&
          x >= 1 &&
          x <= .Machine$integer.max &&
          x == floor(x)
      }

      if (!all(vapply(values, valid_integer, logical(1)))) {
        showNotification(
          "Enter a positive whole number for every threshold.",
          type = "error"
        )

        return()
      }

      new_rules <- lapply(values, as.integer)

      # The warning range must not end below where it starts.
      if (new_rules$player_warning < new_rules$player_combination) {
        showNotification(
          paste(
            "The warning upper count must be at least",
            "the minimum shots per player combination."
          ),
          type = "error"
        )

        return()
      }

      if (new_rules$team_warning < new_rules$team_combination) {
        showNotification(
          paste(
            "The team warning upper count must be at least",
            "the minimum shots per team combination."
          ),
          type = "error"
        )

        return()
      }

      # Players who would be eligible under the NEW total-shots rule.
      newly_eligible <- player_totals |>
        filter(
          total_shots >= new_rules$player_total
        ) |>
        pull(shooter)

      cache <- player_model_cache()
      errors <- player_error_cache()

      # Of those, the ones without a model yet (lowering the minimum can make
      # new players eligible).
      needs_fit <- newly_eligible[
        vapply(
          newly_eligible,
          function(player) {
            is.null(cache[[player]])
          },
          logical(1)
        )
      ]

      # Only fit players whose models are not already cached.
      if (length(needs_fit)) {
        withProgress(
          message = "Preparing eligible player models",
          value = 0,
          {
            for (player in needs_fit) {
              # Catch errors so one failing player does not stop the rest.
              result <- tryCatch(
                fit_player(player),
                error = function(e) e
              )

              if (inherits(result, "error")) {
                errors[player] <- conditionMessage(result)
              } else {
                cache[[player]] <- result

                # Clear any earlier error recorded for this player.
                if (player %in% names(errors)) {
                  errors <- errors[names(errors) != player]
                }
              }

              incProgress(
                1 / length(needs_fit),
                detail = player
              )
            }
          }
        )
      }

      player_model_cache(cache)
      player_error_cache(errors)

      # Commit the settings only after preparing the models.
      applied_rules(new_rules)

      showNotification(
        "Rules applied.",
        type = "message",
        duration = 3
      )
    },
    ignoreInit = TRUE # do not run at startup, only on real clicks
  )

  # ============================================================
  # UPDATE THE PLAYER SELECTOR
  # ============================================================

  # Keeps the Player dropdown in sync with the players that currently have
  # models, preserving the user's selection when it is still valid.
  observe({
    choices <- available_players_now()

    # isolate() so changing the selection itself does not retrigger this.
    previous_selection <- isolate(input$player)

    selected <- if (
      length(previous_selection) == 1L &&
        previous_selection %in% choices
    ) {
      previous_selection
    } else if (length(choices)) {
      choices[[1]]
    } else {
      character(0)
    }

    updateSelectInput(
      session,
      "player",
      choices = choices,
      selected = selected
    )
  })

  # ============================================================
  # CURRENTLY APPLIED SETTINGS TEXT
  # ============================================================

  # Plain-text readout shown at the bottom of the Model rules panel.
  output$applied_rules_text <- renderText({
    rules <- applied_rules()

    paste0(
      "Player total shots: ",
      rules$player_total,
      "\n",

      "Player combination minimum: ",
      rules$player_combination,
      "\n",

      "Player warning: ",
      rules$player_combination,
      "\u2013",
      rules$player_warning,
      " shots\n\n",

      "Team combination minimum: ",
      rules$team_combination,
      "\n",

      "Team warning: ",
      rules$team_combination,
      "\u2013",
      rules$team_warning,
      " shots"
    )
  })

  # ============================================================
  # RESULTS TABLE (feeds the heatmap and the CSV download)
  # ============================================================

  # One row per shot scenario for the selected model (team or player), with
  # estimates, counts and status computed under the applied rules, then
  # narrowed by the dropdown filters.
  current_results <- reactive({
    rules <- applied_rules()

    if (identical(input$view, "Team")) {
      # Recompute (cheap: predictions only) so the applied thresholds take
      # effect without refitting the model.
      result <- make_results(
        model = team_model,
        counts = team_counts,
        min_combination = rules$team_combination,
        warning_max = rules$team_warning
      )
    } else {
      available <- available_players_now()

      # Friendly on-screen message instead of an error when nobody qualifies.
      validate(
        need(
          length(available) > 0,
          paste(
            "No player models are available under the applied rules.",
            "Check the Player eligibility tab."
          )
        )
      )

      # Silently wait if the dropdown holds a stale or empty selection.
      req(input$player %in% available)

      player <- input$player
      cache <- player_model_cache()
      fit <- cache[[player]]

      counts <- player_counts |>
        filter(shooter == player)

      result <- make_results(
        model = fit$model,
        counts = counts,
        player = player,
        min_combination = rules$player_combination,
        warning_max = rules$player_warning
      )

      # Apply the current team threshold to the comparison too.
      # Pull the team's estimate and count for each scenario and attach them,
      # so the player can be compared against the team.
      comparison <- make_results(
        model = team_model,
        counts = team_counts,
        min_combination = rules$team_combination,
        warning_max = rules$team_warning
      ) |>
        select(
          all_of(TRIPLE),
          team_expected_points = expected_points,
          team_n = n
        )

      result <- result |>
        left_join(
          comparison,
          by = TRIPLE
        ) |>
        mutate(
          # Positive = the player is expected to score more than the team
          # average for that scenario (NA if either estimate is missing).
          difference_vs_team = expected_points - team_expected_points
        )
    }

    # Existing shot filters.
    if (input$zone != "All") {
      result <- result |>
        filter(shot_zone == input$zone)
    }

    if (input$type != "All") {
      result <- result |>
        filter(shot_type == input$type)
    }

    if (input$contest != "All") {
      result <- result |>
        filter(shot_contest == input$contest)
    }

    # Optionally hide scenarios with no estimate.
    if (isTRUE(input$available_only)) {
      result <- result |>
        filter(!is.na(expected_points))
    }

    result |>
      arrange(
        shot_zone,
        shot_type,
        shot_contest
      )
  })

  # ============================================================
  # DESCRIPTIVE DATA FOR KPI CARDS
  # Does not modify model data or fitted models.
  # ============================================================

  # Shot-level data (one row per cleaned shot) filtered by the same selections
  # as the heatmap, used for the descriptive KPI cards.
  kpi_data <- reactive({
    # Combine readable labels and original shot outcomes.
    # These objects have matching row order.
    d <- bind_cols(
      shot_labels,
      base_df |>
        select(
          is_game,
          made_shot
        )
    )

    # Apply the team/player selection.
    if (identical(input$view, "Player")) {
      req(input$player)

      d <- d |>
        filter(shooter == input$player)
    }

    # Apply the shot filters.
    if (!is.null(input$zone) && input$zone != "All") {
      d <- d |>
        filter(shot_zone == input$zone)
    }

    if (!is.null(input$type) && input$type != "All") {
      d <- d |>
        filter(shot_type == input$type)
    }

    if (!is.null(input$contest) && input$contest != "All") {
      d <- d |>
        filter(shot_contest == input$contest)
    }

    d
  })

  # ============================================================
  # KPI BAR
  # ============================================================

  # Renders the row of six summary cards above the filters.
  output$kpi_bar <- renderUI({
    d <- kpi_data()

    total_shots <- nrow(d)

    game_shots <- sum(d$is_game)
    practice_shots <- sum(!d$is_game)

    # Guard against dividing by zero when the filters match no shots.
    fg_pct <- if (total_shots > 0) {
      mean(d$made_shot == 1)
    } else {
      NA_real_
    }

    # Selected E[Points]: the estimates currently displayed in the heatmap,
    # averaged with each combination weighted by its shot count. current_results()
    # can stop silently (e.g. no player model available), so fall back to NA
    # instead of blanking the whole KPI bar.
    shown <- tryCatch(current_results(), error = function(e) NULL)

    if (!is.null(shown)) {
      shown <- shown[
        !is.na(shown$expected_points) & shown$n > 0,
        ,
        drop = FALSE
      ]
    }

    selected_ep <- if (!is.null(shown) && nrow(shown) > 0) {
      weighted.mean(shown$expected_points, w = shown$n)
    } else {
      NA_real_
    }

    # Number of shots behind that average (shots in combinations with estimates).
    selected_ep_shots <- if (!is.null(shown)) sum(shown$n) else 0L

    # Display helpers.
    # Number with thousands separators (e.g. 1,234).
    format_count <- function(x) {
      format(
        x,
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      )
    }

    # Share of the selected shots, as text (uses total_shots from above).
    format_share <- function(x) {
      if (total_shots == 0) {
        return("No shots selected")
      }

      sprintf(
        "%.1f%% of selected shots",
        100 * x / total_shots
      )
    }

    # Reusable card.
    kpi_card <- function(
      label,
      value,
      detail,
      red_accent = FALSE
    ) {
      div(
        class = paste(
          "au-kpi-card",
          if (red_accent) "au-kpi-red" else ""
        ),

        div(
          class = "au-kpi-label",
          label
        ),

        div(
          class = "au-kpi-value",
          value
        ),

        div(
          class = "au-kpi-detail",
          detail
        )
      )
    }

    tagList(
      div(
        class = "au-kpi-grid",

        kpi_card(
          label = "Total shots",
          value = format_count(total_shots),
          detail = "Cleaned recorded attempts"
        ),

        kpi_card(
          label = "Game shots",
          value = format_count(game_shots),
          detail = format_share(game_shots),
          red_accent = TRUE
        ),

        kpi_card(
          label = "Practice shots",
          value = format_count(practice_shots),
          detail = format_share(practice_shots)
        ),

        kpi_card(
          label = "Field-goal percentage",
          value = if (is.na(fg_pct)) {
            "\u2014"
          } else {
            sprintf("%.1f%%", 100 * fg_pct)
          },
          detail = "Made shots / total attempts",
          red_accent = TRUE
        ),

        # Fixed baseline: does not respond to the view or any filter.
        kpi_card(
          label = "Team E[Points]",
          value = if (is.na(team_baseline_ep)) {
            "\u2014"
          } else {
            sprintf("%.2f", team_baseline_ep)
          },
          detail = "Team average per shot, unaffected by selections"
        ),

        # Responds to the view, player, and shot filters.
        kpi_card(
          label = "Selected E[Points]",
          value = if (is.na(selected_ep)) {
            "\u2014"
          } else {
            sprintf("%.2f", selected_ep)
          },
          detail = if (is.na(selected_ep)) {
            "No estimates for this selection"
          } else {
            paste0(
              "Weighted by shots: ",
              format_count(selected_ep_shots),
              " with estimates"
            )
          },
          red_accent = TRUE
        )
      ),

      div(
        class = "au-kpi-note",

        paste(
          "Shot counts and field-goal percentage reflect the selected",
          "team/player and shot filters, and include all cleaned shots",
          "regardless of regression weight.",
          "Team E[Points] is the team model's average expected points per",
          "shot across all shots and never changes.",
          "Selected E[Points] averages the estimates shown in the heatmap,",
          "weighted by shot count, so combinations without a reportable",
          "estimate are not included."
        )
      )
    )
  })

  # One-line headline above the heatmap giving the number of cleaned shots
  # behind the selected model. It ignores the zone/type/contest filters.
  output$summary <- renderText({
    if (input$view == "Team") {
      paste(
        "Team model:",
        nrow(base_df),
        "cleaned recorded shots."
      )
    } else {
      req(input$player)

      player_row <- player_totals[
        player_totals$shooter == input$player,
        ,
        drop = FALSE
      ]

      req(nrow(player_row) == 1)

      paste(
        input$player,
        "—",
        player_row$total_shots,
        "cleaned recorded shots."
      )
    }
  })

  # ============================================================
  # HEATMAP
  # ============================================================

  # One panel per contest level; x = shot type, y = shot zone, tile color =
  # expected points (red low, white middle, blue high), with the number and
  # sample size printed in each tile.
  output$heatmap <- renderPlot({
    rules <- applied_rules()

    d <- current_results()

    # Show a message instead of an empty plot when filters match nothing.
    validate(
      need(
        nrow(d) > 0,
        "No combinations match the selected filters."
      )
    )

    # Build the text printed in each tile:
    #   no estimate     -> dash plus the count
    #   limited sample  -> value with an asterisk
    #   otherwise       -> value and count
    # (case_when takes the first match, so the NA check must come first.)
    d <- d |>
      mutate(
        cell_label = case_when(
          is.na(expected_points) ~ paste0(
            "—\n(n=",
            n,
            ")"
          ),
          status == "Limited sample" ~ sprintf(
            "%.2f*\n(n=%d)",
            expected_points,
            n
          ),
          TRUE ~ sprintf(
            "%.2f\n(n=%d)",
            expected_points,
            n
          )
        )
      )

    ggplot(
      d,
      aes(
        x = shot_type,
        y = shot_zone,
        fill = expected_points
      )
    ) +
      # Tiles are semi-transparent (alpha) so the labels on top stay legible.
      geom_tile(
        color = "white",
        linewidth = 0.6,
        alpha = 0.6
      ) +
      geom_text(
        aes(label = cell_label),
        size = 3.3,
        color = "#253247",
        alpha = 1
      ) +
      # One small panel per contest level.
      facet_wrap(
        ~shot_contest
      ) +
      # Red -> white -> blue; scenarios without an estimate are light grey.
      scale_fill_gradientn(
        colours = c(
          AU_RED,
          AU_WHITE,
          AU_BLUE
        ),
        na.value = "#E5E7EB",
        name = "Expected\npoints"
      ) +
      # The caption explains the asterisk using the currently applied range.
      labs(
        x = NULL,
        y = NULL,
        caption = if (identical(input$view, "Player")) {
          paste0(
            "* Limited sample: player estimate based on ",
            rules$player_combination,
            "\u2013",
            rules$player_warning,
            " shots. Grey cells have no reportable estimate."
          )
        } else {
          paste0(
            "* Limited sample: team estimate based on ",
            rules$team_combination,
            "\u2013",
            rules$team_warning,
            " shots. Grey cells have no reportable estimate."
          )
        }
      ) +
      theme_minimal(
        base_size = 12,
        base_family = "sans"
      ) +
      # Theme tweaks to match the AU color scheme.
      theme(
        panel.grid = element_blank(),

        plot.background = element_rect(
          fill = AU_WHITE,
          color = NA
        ),

        panel.background = element_rect(
          fill = AU_WHITE,
          color = NA
        ),

        axis.text = element_text(
          color = AU_TEXT
        ),

        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),

        strip.background = element_rect(
          fill = AU_BLUE,
          color = AU_BLUE
        ),

        strip.text = element_text(
          color = AU_WHITE,
          face = "bold",
          size = 11,
          margin = margin(8, 8, 8, 8)
        ),

        legend.title = element_text(
          color = AU_BLUE,
          face = "bold"
        ),

        plot.caption = element_text(
          color = AU_TEXT,
          hjust = 0,
          margin = margin(t = 15)
        )
      )
  })

  # ============================================================
  # PLAYER ELIGIBILITY TABLE
  # ============================================================

  # One row per player showing total shots, whether they meet the applied
  # minimum, whether a model exists, and any error message from a failed fit.
  output$eligibility <- renderDT({
    rules <- applied_rules()
    cache <- player_model_cache()
    errors <- player_error_cache()

    d <- player_totals |>
      # Defensive: drop these columns first in case they already exist.
      select(
        -any_of(
          c(
            "eligible",
            "model_status",
            "error_message"
          )
        )
      ) |>
      mutate(
        eligible = total_shots >= rules$player_total,

        model_available = vapply(
          shooter,
          function(player) {
            !is.null(cache[[player]])
          },
          logical(1)
        ),

        model_status = case_when(
          !eligible ~ paste0(
            "Not eligible: fewer than ",
            rules$player_total,
            " shots"
          ),
          model_available ~ "Available",
          TRUE ~ "Model fitting failed"
        ),

        # Show the stored error only for eligible players whose fit failed.
        error_message = ifelse(
          eligible & !model_available,
          unname(errors[shooter]),
          NA_character_
        )
      ) |>
      select(-model_available)

    datatable(
      d,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      )
    )
  })

  # ============================================================
  # CSV DOWNLOAD
  # ============================================================

  # Downloads exactly the rows currently displayed (after rules and filters).
  output$download <- downloadHandler(
    filename = function() {
      paste0(
        "expected-points-",
        tolower(input$view),
        ".csv"
      )
    },

    content = function(file) {
      write.csv(
        current_results(),
        file,
        row.names = FALSE
      )
    }
  )
}

# Launch the app.
shinyApp(ui, server)
