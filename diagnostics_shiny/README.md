# Diagnostics Shiny App

This project provides the Shiny app used to inspect phenotype diagnostic outputs.

For the full workflow, see [../INSTRUCTIONS.md](../INSTRUCTIONS.md#running-diagnostics-shiny-app).

## Contents

- `global.R`, `ui.R`, `server.R`: Shiny app source files.
- `scripts/preprocess.R`: prepares app data from raw diagnostic exports.
- `data/raw/`: diagnostic result exports.
- `data/`: processed app data.
- `www/`: app logos and static assets.

## Quick Start

Open `PhenotypeRShiny.Rproj`, place diagnostic exports in `data/raw/`, run `renv::restore()`, then run `shiny::runApp()`.
