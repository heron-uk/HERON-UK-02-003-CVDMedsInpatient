# PhenotypeR HERON UK year 2

## Overview
This repository contains code for the phenotyping of cardiovascular conditions and treatments for HERON year 2 study

## Getting Started

### Prerequisites

-   **R** and **RStudio** are required to run the code.
-   Ensure that you have access to the database and necessary credentials to connect.

### Setup Instructions

1.  **Download the Repository**\
    Download this repository:

    -   Either download as a ZIP file using `Code -> Download ZIP`, then unzip.
    -   Or, use GitHub Desktop to clone the repository.

2.  **Open the R Project**

    -   Navigate to the the project file `phenotypeR.Rproj` in RStudio.
    -   You should see the project name in the top-right corner of your RStudio session.

3.  **Run the Analysis Code**

    -   Open the `CodeToRun.R` file. This is the main script you’ll use.
    -   Follow the instructions within the file to add your database-specific information.
    -   Run the code as directed. This will generate a `Results` folder containing the outputs, including a ZIP file with the results for sharing.

4.  **OPTIONAL: Visualize Results in Shiny**

    -   Navigate to the `PhenotypeRshiny` folder and open the project file `shiny.Rproj` in RStudio.
    -   You should see the project name in the top-right corner of your RStudio session.
    -   Copy the generated result files (in .csv format) into the `Raw Data` folder located within the`PhenotypeRshiny` folder.
    -   Open the `global.R` script in the `shiny` folder.
    -   Click the *Run App* button in RStudio to launch the local Shiny app for interactive exploration of the results.

