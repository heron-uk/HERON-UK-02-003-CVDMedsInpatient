# Instructions for Running This Study

This document gives the main steps for running the diagnostics, study analysis, diagnostic Shiny app, and final report app. Each main folder is an RStudio project with its own package environment.

## General Setup

1. Download or clone this repository.
2. Open the relevant `.Rproj` file in RStudio.
3. Restore package versions when prompted, or run:

```r
renv::restore()
```

4. For diagnostics or study code, add database-specific connection details only in the relevant `CodeToRun.R` file.
5. The diagnostics and study scripts create the `cdm` reference with `CDMConnector::cdmFromCon()`. See the CDMConnector database connection examples for DBMS-specific connection patterns.

## Running Diagnostics Code

The diagnostics code is in `diagnostics_code/`.

1. Open `diagnostics_code/study_diagnostics.Rproj`.
2. Open `CodeToRun.R`.
3. Set `dbName`, `con`, `cdmSchema`, `writeSchema`, `writePrefix`, `achillesSchema`, and `minCellCount`. See [CDMConnector database connection examples](https://darwin-eu.github.io/CDMConnector/articles/DBI_connection_examples.html) to see how to populate the connection object.
4. Run `CodeToRun.R`. It restores packages, creates the `cdm` reference, and sources `RunStudy.R`.

Outputs are written under `diagnostics_code/Results/`.

## Running Diagnostics Shiny App

The diagnostics review app is in `diagnostics_shiny/`.

1. Open `diagnostics_shiny/PhenotypeRShiny.Rproj`.
2. Place diagnostic result exports in `data/raw/`.
3. Keep expectation files, if used, in `data/raw/expectations/`.
4. Run:

```r
shiny::runApp()
```

The app preprocesses files from `data/raw/` into `data/appData.qs` when needed.

## Running Study Code

The main analysis code is in `study/`.

1. Open `study/Study.Rproj`.
2. Open `CodeToRun.R`.
3. Set `dbName`, `con`, `cdmSchema`, `writeSchema`, `writePrefix`, and `min_cell_count`. See [CDMConnector database connection examples](https://darwin-eu.github.io/CDMConnector/articles/DBI_connection_examples.html) to see how to populate the connection object.
4. Run `CodeToRun.R`. It restores packages, creates the `cdm` reference, and sources `RunStudy.R`.

Outputs are written under `study/Results/`.

## Running Report and Study Shiny App

The final output app and Quarto report are in `report/`.

1. Open `report/report.Rproj`.
2. Add partner CSV exports to `rawData/`.
3. Preprocess the data:

```r
source(file.path("rawData", "preprocess.R"))
```

4. Run the app:

```r
shiny::runApp()
```

5. Render the Word report:

```r
quarto::quarto_render("report.qmd")
```

## Prepare Report Data

Place partner CSV exports in `report/rawData/`, then run preprocessing from the `report/` folder. This creates `report/data/studyData.RData`, which is used by both the app and `report.qmd`.

Do not edit `studyData.RData` manually. Regenerate it from `report/rawData/preprocess.R` when the raw CSV files change.

Raw CSV files in `report/rawData/` are ignored by git and excluded from Shiny deployment. Check the deployment bundle before publishing.
