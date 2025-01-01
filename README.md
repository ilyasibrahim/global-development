# global_development
A Short Exploration and Modeling of Global Development: One Dataset, Many Questions

This repository contains an R-based project that explores and models global socio-economic and environmental indicators. It covers data cleaning, imputation, normalization, exploratory data analysis (EDA), feature engineering, and various predictive modeling techniques.

---

## Contents

1. **Data Collection and Initial Review**  
   - Explains how data was imported and validated.
2. **Data Cleaning**  
   - Outlines imputation methods for missing data, normalization steps, and variable transformations.
3. **Exploratory Data Analysis**  
   - Showcases correlation matrices, boxplots, distribution histograms, etc.
4. **Feature Engineering**  
   - Demonstrates the creation of interaction terms, polynomial features, and custom indices.
5. **Predictive Modeling**  
   - Includes logistic/linear regressions, random forests, k-means clustering, etc.
6. **Results and Visualizations**  
   - Summarizes outlier maps, geo-plots, correlation scatterplots, development tiers, etc.

---

## Files and Structure

- **R Scripts and Code Files**  
  - `01_project_setup.R` 
    *Installs and loads all required packages.*  
    *Ensures that any missing packages are installed automatically.*

  - `02_helper_functions.R` 
    *Houses utility functions (e.g., data loading, transformations, map preparation).*

  - `03_data_collection_initial_review.R`
    *Loads the dataset (`country_data.csv`),*  
    *Defines thematic indicators and checks missing data,*  
    *Generates basic summaries and outlier boxplots.*

  - `04_data_cleaning_imputation.R`  
    *Performs data cleaning steps (e.g., removing certain rows),*  
    *Handles missing data imputation (median, KNN, MICE).*

  - `05_data_normalization.R`  
    *Applies log+z, min-max, or standard z-score transformations as appropriate.*

  - `06_exploratory_data_analysis.R`  
    *Creates correlation matrices, histograms, scatter plots,*  
    *Geo-plots (mapping fertility, education, infant mortality, etc.).*

  - `07_feature_engineering.R`  
    *Creates interaction terms, polynomial features, dummy variables,*  
    *Aggregated indexes (trade_index, human_capital_index, etc.).*

  - `08_machine_learning_economic.R`  
    *Covers economic-focused models (GDP growth prediction, trade balance, development tiers, downturn classification).*

  - `09_machine_learning_population.R`  
    *Focuses on population dynamics (fertility, population growth, stability, migration).*

  - `10_machine_learning_education.R`  
    *Models for education and employment (predicting human capital index, employment distributions, resilience).*

  - `11_machine_learning_health_safety.R` 
    *Life expectancy, infant mortality, health status classification, and crisis prediction.*

  - `12_machine_learning_environment_tourism.R` 
    *Environmental and tourism indicators (CO2 emissions, sustainability classification, tourism drivers).*

- **R Markdown / Markdown Files**  
  - `global-development.Rmd`  
    *Combines code chunks and explanatory narrative into a single report.*
    * Available upon request from the author.*

- **Data**  
  - `country_data.csv` 
    *Referenced by code scripts for the main analysis.*

- **README.md**  
  *(This file)*  
  *Provides overview, installation details, and project scope.*

- **LICENSE**  
  *A license document clarifying how others can use or modify your project.*

- **.gitignore**  
  *Lists files/folders to exclude from version control (e.g., caches, large data).*

---

## Installation and Requirements

1. **Clone or Download** the repository:
   ```bash
   git clone https://github.com/ilyasibrahim/global-development.git

## Installation and Usage

2. **Open the Project** in RStudio (or your preferred R environment), open the .Rproj file,
or set your working directory to the project folder.

3. **Install Necessary Packages**  

```r
required_packages <- c(
  "tidyverse", "data.table", "randomForest", "mice", 
  "DMwR2", "caret", "sf", "rworldmap", "viridis", 
  "RColorBrewer", "cowplot", "kableExtra", "plotly",
  "skimr", "knitr", "fastDummies", "janitor"
)
install.packages(required_packages)
```
4. **Load the Scripts in Order**  
  - Run `01_project_setup.R` to install/load libraries.
  - Then run each script sequentially (`02_helper_functions.R`, `03_data_collection_initial_review.R`, etc.) so that all required objects exist in your environment.

5. **Run the Analysis**  
  - Execute the R scripts step-by-step, **OR**
  - Knit the .Rmd file (e.g., global-development.Rmd) to reproduce the entire workflow, generating plots, model outputs, etc.

---

## How to Use

**Data Input**  
Ensure `country_data.csv` (or your main dataset) is placed in the correct folder (for instance, `/data` if the scripts reference it there).

**Step-by-Step Execution**  
1. data loading  
2. cleaning  
3. EDA  
4. feature engineering  
5. modeling  

Each script in your project corresponds to one of these phases.

**View Results**  
- The R Markdown (`.Rmd`) compiles analyses into HTML or PDF.  
- Inspect final model outcomes (e.g., Random Forest OOB errors, confusion matrices, regression summaries).

---

## Project Highlights

**Data Imputation**  
- Multiple strategies: median fill (for minimal MCAR), KNN (moderate MAR), MICE (for >20% missingness).

**Normalization**  
- Log+Z for skewed variables, min-max for bounded ones, standard Z for roughly symmetric distributions.

**Feature Engineering**  
- Interaction terms (like `fertility_SecFem`, `gdpPerCap_co2`), polynomial expansions (`fertility_sq`, `gdp_growth_sq`), aggregated indices (`trade_index`, `human_capital_index`).

**Predictive Models**  
- Logistic/Linear Regression, Random Forest (both regression & classification), and K-means clustering for development tiers.

**Mapping & Visualization**  
- Combines `sf` or `rworldmap` with choropleths (e.g., fertility vs. education, environment vs. tourism).

---

## Contributing

Contributions are welcome. To propose changes:
1. Fork this repository
2. Create a new branch
3. Make and commit edits
4. Submit a Pull Request describing modifications

For questions, open an Issue or email `ilyas.ibrahimm@hotmail.com`.

---

## License

See the [LICENSE](https://github.com/ilyasibrahim/global-development/blob/main/LICENSE.txt) file for details.

---

## References

- **Data Source**: Kaggle or other open-data platforms.  
- **Packages**: CRAN or GitHub repositories used.  
- **Academic Theories**: Demographic Transition, Environmental Kuznets Curve, Human Capability, etc.

---

_Thank you for exploring this global development project. Feel free to raise an issue or share feedback!_



