# Raw Data

This folder is for partner-level CSV exports used to build the processed Shiny/report data.

The preprocessing script reads every `.csv` file here and writes the derived data file to:

```text
../data/studyData.RData
```

CSV files in this folder are ignored by git through `.gitignore`, and the whole folder is ignored for Shiny deployment through `../.rscignore`.

For the full workflow, see [../../INSTRUCTIONS.md](../../INSTRUCTIONS.md#prepare-report-data).
