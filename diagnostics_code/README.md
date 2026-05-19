# Diagnostics Code

This project runs phenotype diagnostics for the HERON inpatient cardiovascular medicines study.

For the full workflow, see [../INSTRUCTIONS.md](../INSTRUCTIONS.md#running-diagnostics-code).

## Contents

- `CodeToRun.R`: edit database connection details and run the diagnostics.
- `RunStudy.R`: main diagnostics workflow.
- `Cohorts/`: cohort definition CSVs and cohort instantiation code.
- `Codelists/`: supporting codelist scripts and reviewed files.
- `Results/`: local diagnostics outputs.

## Quick Start

Open `study_diagnostics.Rproj`, run `renv::restore()`, update `CodeToRun.R`, then run the script.
