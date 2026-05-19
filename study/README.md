# Study Code

This project runs the main HERON inpatient cardiovascular medicines study analysis.

For the full workflow, see [../INSTRUCTIONS.md](../INSTRUCTIONS.md#running-study-code).

## Contents

- `CodeToRun.R`: edit database connection details and run the study.
- `RunStudy.R`: main analysis workflow.
- `Cohorts/`: cohort definition CSVs and cohort instantiation code.
- `Analyses/`: analysis scripts sourced by `RunStudy.R`.
- `Results/`: local study outputs.

## Quick Start

Open `Study.Rproj`, run `renv::restore()`, update `CodeToRun.R`, then run the script.
