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
  - `01_data_loading.R` (example)  
    *Loads the dataset and defines thematic indicators.*
  - `02_data_cleaning.R`  
    *Handles missing data, imputation strategies (median, KNN, MICE), normalization, etc.*
  - `03_exploratory_analysis.R`  
    *Contains EDA scripts (boxplots, correlation heatmaps, distributions).*
  - `04_feature_engineering.R`  
    *Builds interaction terms, polynomial features, aggregated indices.*
  - `05_modeling.R`  
    *Performs logistic/linear regressions, random forests, and clustering (k-means).*
  - (Additional scripts for specialized plots, final results, or utility functions.)

- **R Markdown / Markdown Files**  
  - `project_analysis.Rmd`  
    *Combines code chunks and explanatory narrative into a single report.*

- **Data**  
  - `country_data.csv` (or equivalent)  
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
   git clone https://github.com/YourUsername/global-development.git

## Installation and Usage

2. **Open the Project** in RStudio (or your preferred R environment).

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
4. **Load the Project**  
Open the `.Rproj` file if present, or run scripts in sequential order (e.g., `01_data_loading.R`, `02_data_cleaning.R`, etc.).

5. **Run the Analysis**  
Execute the `.Rmd` file (for example, `project_analysis.Rmd`) to reproduce the entire workflow, generating plots, model outputs, etc.

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

See the `LICENSE` file for details.

---

## References

- **Data Source**: Kaggle or other open-data platforms.  
- **Packages**: CRAN or GitHub repositories used.  
- **Academic Theories**: For example, mention the Environmental Kuznets Curve if relevant.

---

_Thank you for exploring this global development project. Feel free to raise an issue or share feedback!_



