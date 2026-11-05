> Privacy notice — fictitious demonstration data
>
> All player names and data values used in this public repository are fictitious and were randomly chosen/generated for privacy purposes. They do not represent real players, actual American University basketball records, or real team or individual performance. The two demonstration datasets do not disclose the original values of the private source datasets.
>
> Because the data are artificial, the dashboard can show unusual patterns, implausible estimates, or unexpected rankings. These results are artifacts of the fictitious data and must not be interpreted as basketball findings. The application retains the same dashboard behavior and analytical workflow; the underlying inputs and resulting estimates are fictitious.

# American University Men's Basketball Expected Points Dashboard

An interactive R Shiny dashboard for exploring expected points by shot zone, shot type, and defensive contest level. This repository is a public demonstration of a coaching analytics project using fictitious data only.

Repository: `au_basketball_expected_points`

## Live demonstration

Live demo: https://franciscorezzonico-au-basketball-expected-points.share.connect.posit.cloud

## Features

- Team and player-specific expected-points views.
- Filters for shot zone, shot type, and contest level.
- An expected-points heatmap: red indicates lower values and blue indicates higher values.
- KPI cards for total shots, game shots, practice shots, field-goal percentage, overall team E[Points], and selected-shot E[Points].
- A hidden Model rules panel with an Apply rules button.
- Player eligibility checks and limited-sample warnings.
- An eligibility table and CSV export of the selected results.
- Saved default models and on-demand fitting for newly eligible players.

## Interpreting the expected-points cards

Both E[Points] cards average only available estimates, weighted by recorded shot counts rather than regression weights:

```text
Weighted E[Points] = sum(combination shot count × combination estimate)
                    / sum(shot counts for available estimates)
```

- Overall team E[Points] uses all cleaned game and practice shots and the saved default inclusion rules. It stays fixed within the loaded model/data snapshot, regardless of dashboard selections or applied rules.
- Selected shots E[Points] follows the selected team/player model, shot filters, and applied rules.
- Coverage reports the proportion of recorded shots represented by available estimates. Unavailable estimates are omitted, not treated as zero.
- Limited-sample estimates remain included when they are available.

## Default inclusion and warning rules

| Rule | Default |
|---|---:|
| Minimum total cleaned shots for an individual model | 150 |
| Minimum shots per player combination | 10 |
| Player limited-sample upper count, inclusive | 25 |
| Minimum shots per team combination | 10 |
| Team limited-sample upper count, inclusive | 25 |

A combination is a shot-zone × shot-type × contest-level triple. With the defaults, available estimates based on 10–25 shots receive a limited-sample flag. Passing these thresholds is not proof of statistical reliability.

Rule changes take effect after Apply rules. Combination thresholds control display availability, not the training rows. Lowering the overall player threshold fits newly eligible players only when their models are not already cached. The app does not overwrite the default model bundle.

Zero-weight shots remain in recorded-shot counts. They do not contribute to the weighted expected-points fit.

## Project structure

```text
au_basketball_expected_points/
├── App.R
├── README.md
├── .gitignore
├── code/
│   ├── 01_clean_data.R
│   └── 02_fit_models.R
├── data/
│   ├── raw/
│   │   ├── Practice_Cleaned_All.csv
│   │   └── Game_Cleaned_All.csv
│   └── preprocessed/
│       ├── cleaned_shots.rds       # generated locally; not committed
│       └── default_models.rds      # generated locally; not committed
└── www/
    └── [dashboard images and styles]
```

Only fictitious CSVs belong in `data/raw/`. 

## Run locally

### 1. Open the project

Clone or download this repository and make its root directory your R working directory. Use R 4.3.0 or newer. No machine-specific `setwd()` path should be required in the source files.

### 2. Install dependencies

```r
install.packages(c(
  "fastDummies", "janitor", "dplyr", "tidyr",
  "shiny", "ggplot2", "DT"
))
```

### 3. Prepare the fictitious data

```r
source("code/01_clean_data.R")
```

Expected output: `data/preprocessed/cleaned_shots.rds`.

### 4. Fit the demonstration models

```r
source("code/02_fit_models.R")
fit_default_models()
```

Expected output: `data/preprocessed/default_models.rds`.

The fitting script defines model configurations and fitting functions. Sourcing it alone does not fit the default models. All player-specific interaction lists, exclusions, position mappings, and free-throw-rate lookups in this public version must refer only to fictitious players and demonstration parameters.

### 5. Launch the app

```r
shiny::runApp(".")
```

The app loads the prepared bundle at startup.

## Modeling workflow

1. Combine the fictitious practice and game datasets and validate shot categories.
2. Apply the preprocessing, weighting, and feature-encoding pipeline.
3. Fit foul-probability logistic models and add an expected free-throw contribution.
4. Fit weighted linear models for expected points, using the configured team/player terms.
5. Save the default bundle with the matching cleaned-data and configuration snapshot.
6. Load that snapshot in Shiny and apply display rules without refitting existing models.

Linear expected-points predictions are not inherently constrained to a basketball-plausible range. Artificial data, sparse categories, and model limitations can produce strange estimates. The public demo is intended to demonstrate software behavior, not validate a predictive model or recommend real coaching decisions.
