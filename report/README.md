# MI/Stroke Inpatient Report and Shiny App

This project contains the final Shiny app and Quarto report for the MI/Stroke inpatient study outputs.

For full setup and running instructions, see [../INSTRUCTIONS.md](../INSTRUCTIONS.md#running-report-and-study-shiny-app).

## Repository Organisation

- `global.R`, `ui.R`, `server.R`: Shiny app source files.
- `functions.R`: shared helper functions.
- `report.qmd`: Quarto source for the Word report.
- `report.docx`: rendered Word report.
- `rawData/`: local partner CSV exports and preprocessing script.
- `data/`: processed `.RData` file used by the app and report.
- `_brand.yml`: shared plot and table styling.
- `www/`: image and logo assets used by the app.

## Quick Start

Open `report.Rproj`, run `renv::restore()` if needed, then preprocess and run the app:

```r
source(file.path("rawData", "preprocess.R"))
shiny::runApp()
```

To render the Word report:

```r
quarto::quarto_render("report.qmd")
```

Raw CSV files should stay in `rawData/`; processed app/report data are written to `data/studyData.RData`.
