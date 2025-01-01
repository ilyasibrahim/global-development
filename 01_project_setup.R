##############################################
# 01 PROJECT SETUP
##############################################

# 1.1 LIST OF REQUIRED PACKAGES
required_packages <- c(
  "tidyverse",   # Data wrangling, piping, ggplot2, etc.
  "knitr",       # R Markdown and dynamic reporting
  "kableExtra",  # Advanced table formatting in knitr
  "viridis",     # Color palettes friendly to colorblind viewers
  "RColorBrewer",# Additional curated color palettes 
  "patchwork",   # Align/arrange multiple ggplots
  "data.table",  # High-performance data manipulation 
  "skimr",       # Quick, comprehensive data summaries
  "caret",       # Classification and Regression Training package
  "mice",        # Multiple Imputation by Chained Equations
  "DMwR2",       # Data mining incl. KNN-based imputation 
  "naniar",      # Tools for missing data exploration
  "sf",          # Simple Features for spatial data handling
  "rworldmap",   # Quick retrieval of world map data
  "cowplot",     # Advanced plot layouts and combining
  "ggrepel",     # Smart label placement in ggplot
  "fastDummies", # Creation of dummy (indicator) variables
  "janitor",     # Data cleaning functions (naming, tabulation)
  "randomForest" # Random Forest models for classification/regression
)

# 1.2 FUNCTION: INSTALL MISSING PACKAGES
install_if_missing <- function(packages) {
  missing_packages <- packages[!(packages %in% rownames(installed.packages()))]
  if (length(missing_packages)) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    tryCatch({
      install.packages(missing_packages, dependencies = TRUE)
    }, error = function(e) {
      stop("Error installing packages: ", paste(missing_packages, collapse = ", "), "\n", e)
    })
  } else {
    message("All required packages are already installed.")
  }
}

# 1.3 INSTALL AND LOAD PACKAGES
install_if_missing(required_packages)
suppressPackageStartupMessages({
  lapply(required_packages, library, character.only = TRUE)
})

message("Project setup complete. All libraries loaded.")
