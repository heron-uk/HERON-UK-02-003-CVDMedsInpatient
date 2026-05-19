# Processed Data

This folder contains derived data used by the Shiny app and Quarto report.

The main file is `studyData.RData`, created from the report project root by running:

```r
source(file.path("rawData", "preprocess.R"))
```

Do not edit `studyData.RData` manually; regenerate it from the raw CSV files in `rawData/`.

For the full workflow, see [../../INSTRUCTIONS.md](../../INSTRUCTIONS.md#prepare-report-data).
