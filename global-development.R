##############################################
# PROJECT SETUP AND INITIALIZATION
##############################################
# This section prepares a reproducible environment by ensuring that all 
# necessary packages are installed and loaded. Using a dedicated function 
# to handle missing packages streamlines the setup across different systems.

# List of packages essential for data manipulation, visualization, 
# spatial mapping, ML modeling, and reporting.
required_packages <- c(
  "tidyverse",   # Data wrangling, piping, ggplot2, etc.
  "knitr",       # R Markdown and dynamic reporting
  "kableExtra",  # Advanced table formatting in knitr
  "viridis",     # Color palettes friendly to colorblind viewers
  "RColorBrewer",# Additional curated color palettes 
  "patchwork",   # Align/arrange multiple ggplots into unified layouts
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

# Installs any required packages not already present, 
# preventing errors on fresh setups.
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

# Check for and install missing libraries
install_if_missing(required_packages)

# Load required libraries with suppressed startup messages for readability
suppressPackageStartupMessages({
  lapply(required_packages, library, character.only = TRUE)
})

##############################################
# HELPER FUNCTIONS
##############################################
# Encapsulating routine operations (e.g., loading, data transformation) 
# into reusable functions encourages code clarity and reusability.

# Uses data.table's fread to quickly load data from CSV. 
# Produces a clear error if the file is not found.
load_data <- function(path) {
  if (!file.exists(path)) {
    stop("Error: Data file '", path, "' not found.")
  }
  tryCatch({
    data <- fread(path, na.strings = c("", "NA", "NaN"))
    message("Data loaded successfully with ", nrow(data), " rows and ", ncol(data), " columns.")
    return(as.data.frame(data))
  }, error = function(e) {
    stop("Error reading the data file: ", e)
  })
}

# Creates shorter versions of variable names for more compact labeling 
# (especially in plots). Certain tokens like "school" in "secondary_school_enrollment" 
# are removed if they add clutter.
abbreviate_indicator <- function(indicator_names) {
  sapply(strsplit(as.character(indicator_names), "_"), function(words) {
    if (tolower(words[1]) == "secondary") {
      words <- words[-2]
    }
    paste(c(words[1], substring(words[-1], 1, 1)), collapse = "_")
  })
}

# Scales data to [0,1]. Used when variables have known lower and upper bounds, 
# or when a uniform scale is desired without assumptions about distribution. 
min_max_scaling <- function(x) {
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) return(rep(0.5, length(x)))
  (x - min(x, na.rm = TRUE)) / rng
}

# Centers data at mean=0 and sd=1, making variables comparable in magnitude. 
# Often helps gradient-based algorithms converge more efficiently.
z_score_normalization <- function(x) {
  sd_val <- sd(x, na.rm = TRUE)
  if (sd_val == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / sd_val
}

# Performs a log transform after shifting any non-positive values, 
# then applies z-score normalization. This is especially helpful 
# for heavily skewed data, like GDP or population.
safe_log_z_transform <- function(x) {
  non_na_x <- x[!is.na(x)]
  if (length(non_na_x) == 0) {
    return(x)
  }
  min_val <- min(non_na_x)
  if (min_val <= 0) {
    shift_amount <- abs(min_val) + 0.1
    x <- x + shift_amount
  }
  z_score_normalization(log(x))
}

# Retrieves a spatial world map from rworldmap, converts it to an sf object, 
# and merges the user-provided data using ISO-2 codes. The resulting sf object 
# can be used for plotting choropleth maps in ggplot.
prepare_world_map_data <- function(df, iso_col = "iso2") {
  world_map_sp <- rworldmap::getMap(resolution = "high")
  world_map <- sf::st_as_sf(world_map_sp)
  
  # 'ISO_A2' is typically the column storing ISO-2 codes in rworldmap. 
  # They are converted to uppercase before merging.
  world_map <- world_map %>%
    rename(iso_a2 = ISO_A2) %>%
    mutate(iso_a2 = toupper(iso_a2))
  
  df[[iso_col]] <- toupper(df[[iso_col]])
  
  world_merged <- world_map %>%
    left_join(df, by = c("iso_a2" = iso_col))
  
  return(world_merged)
}

##############################################
# 1. DATA COLLECTION AND INITIAL REVIEW
##############################################
# Import the dataset and confirm that the structure aligns with expectations, 
# ensuring each column is recognized correctly. Also categorize variables 
# for subsequent domain-based cleaning and analysis.

file_path <- "country_data.csv"  # Adjust if the dataset is in a different location

country_data <- load_data(file_path)

# Optional quick data preview:
# head(country_data)

# Quick, thorough summary of columns, missingness, numerical stats, etc.
skim(country_data)

##############################################
# 1.1 CATEGORIZATION OF INDICATORS
##############################################
# Each list entry corresponds to a thematic area (like economic or demographic). 
# This grouping guides imputation, transformations, and domain-specific analyses.

indicators <- list(
  "Economic Growth" = c("gdp", "gdp_growth", "gdp_per_capita",
                        "imports", "exports", "co2_emissions"),
  "Population and Demographics" = c("population", "pop_growth", 
                                    "pop_density", "sex_ratio",
                                    "life_expectancy_male", 
                                    "life_expectancy_female",
                                    "fertility", "urban_population", 
                                    "urban_population_growth"),
  "Education and Employment" = c("primary_school_enrollment_female", 
                                 "primary_school_enrollment_male",
                                 "secondary_school_enrollment_female", 
                                 "secondary_school_enrollment_male",
                                 "post_secondary_enrollment_female", 
                                 "post_secondary_enrollment_male",
                                 "employment_agriculture", 
                                 "employment_industry", 
                                 "employment_services"),
  "Health and Safety" = c("infant_mortality", "homicide_rate", 
                          "threatened_species"),
  "Environment and Tourism" = c("forested_area", "co2_emissions", 
                                "tourists")
)

# Identifies which indicators aren't present in the actual dataset 
# and removes them from the categories to prevent errors downstream.
missing_indicators <- setdiff(unlist(indicators), names(country_data))
if(length(missing_indicators) > 0){
  warning("These indicators are missing in the dataset and will be excluded: ",
          paste(missing_indicators, collapse = ", "))
  indicators <- lapply(indicators, setdiff, missing_indicators)
}

##############################################
# 1.2 MISSING DATA ANALYSIS
##############################################
# Investigates the extent and pattern of missing values, 
# providing a basis for choosing imputation strategies.

missing_data_summary <- tibble(
  Indicator = unlist(indicators),
  Category = rep(names(indicators), times = lengths(indicators))
) %>%
  mutate(
    Missing_Total = map_int(Indicator, ~ sum(is.na(country_data[[.x]]))),
    Missing_Proportion = map_dbl(Indicator, ~ mean(is.na(country_data[[.x]]), na.rm = TRUE)),
    Missing_Info = paste0(Missing_Total, " (", 
                          round(Missing_Proportion * 100, 2), "%)")
  ) %>%
  select(Indicator, Category, Missing_Info) %>%
  pivot_wider(names_from = Category, values_from = Missing_Info)

kable(
  missing_data_summary,
  format = "pipe",
  caption = "Missing Data by Thematic Dimension (Total Count and Proportion)"
)

##############################################
# 1.3 Outlier Visualization
##############################################
# Construct boxplots for essential economic/population variables to identify 
# outliers that may unduly influence descriptive stats or modeling. 
# Data transformations might be applied based on these findings.

required_vars <- c("gdp", "gdp_growth", "population", "pop_growth")
missing_vars <- setdiff(required_vars, names(country_data))
if(length(missing_vars) > 0){
  stop("Required variables for outlier plots missing: ", 
       paste(missing_vars, collapse = ", "))
}

plot_data <- country_data %>%
  drop_na(all_of(required_vars)) %>%
  mutate(
    gdp_trillion = gdp / 1e6,          # Scale down for visual clarity
    population_billion = population / 1e6
  )

base_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 7)
  )

colors <- c(
  gdp = "#E69F00",
  gdp_growth = "#56B4E9",
  population = "#8da0cb",
  pop_growth = "#009E73"
)

# Creates standardized boxplots to reveal data distribution and identify extremes.
create_boxplot <- function(data, y_var, fill_color, y_label, 
                           title, scale_factor = 1, suffix = "") {
  ggplot(data, aes(x = "", y = .data[[y_var]] / scale_factor)) +
    geom_boxplot(
      fill = fill_color,
      color = fill_color,
      outlier.color = "#D55E00",
      outlier.shape = 16,
      outlier.size = 2,
      width = 0.3
    ) +
    stat_summary(
      fun = median,
      geom = "crossbar",
      width = 0.3,
      color = "#000000",
      fatten = 1
    ) +
    labs(title = title, y = y_label, x = "") +
    base_theme +
    scale_y_continuous(labels = function(x) paste0(x, suffix))
}

plot_gdp <- create_boxplot(
  plot_data, "gdp_trillion", colors["gdp"], 
  "GDP (Trillion USD)", "GDP"
)
plot_gdp_growth <- create_boxplot(
  plot_data, "gdp_growth", colors["gdp_growth"], 
  "GDP Growth (%)", "GDP Growth"
)
plot_population <- create_boxplot(
  plot_data, "population_billion", colors["population"], 
  "Population (Billion)", "Population"
)
plot_population_growth <- create_boxplot(
  plot_data, "pop_growth", colors["pop_growth"], 
  "Population Growth (%)", "Population Growth"
)

combined_plot <- (plot_gdp | plot_gdp_growth) /
  (plot_population | plot_population_growth) +
  plot_annotation(
    title = "Outliers in Economic Growth and Population Indicators",
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )

combined_plot

# Optional saving of the resulting boxplot figure
# ggsave("outlier_boxplots.png", combined_plot, width = 16, height = 12, dpi = 300)

##############################################
# 2. DATA CLEANING (IMPUTATION) STRATEGIES
##############################################
# Address missing values systematically using approaches suited 
# to each variable’s missingness pattern (e.g., median, KNN, MICE).

# Vatican is excluded to avoid skew from an extremely small entity 
# in certain indicators.
filtered_data <- country_data %>%
  filter(is.na(iso2) | iso2 != "VA")

# Convert region to numeric codes so that imputation methods 
# like KNN can leverage it as a numeric predictor.
filtered_data <- filtered_data %>%
  mutate(region_numeric = as.numeric(factor(region)))

selected_indicators <- unique(unlist(indicators))
selected_indicators_with_ids <- c("iso2", "name", "region", selected_indicators)

##############################################
# 2.1 IMPUTATION OF SPECIFIC VARIABLES
##############################################
# Each chosen method (median, KNN, MICE) balances the nature/amount 
# of missingness and the likely distribution of the data.

# Minimal Missingness -> Median
vars_mcar_median <- c("gdp","gdp_growth","gdp_per_capita","life_expectancy_male",
                      "life_expectancy_female","fertility","forested_area")
for(v in vars_mcar_median) {
  filtered_data[[v]][is.na(filtered_data[[v]])] <- median(filtered_data[[v]], na.rm=TRUE)
}

# Low Missingness but MNAR -> Median for primary school enrollments 
filtered_data$primary_school_enrollment_female[is.na(filtered_data$primary_school_enrollment_female)] <-
  median(filtered_data$primary_school_enrollment_female, na.rm = TRUE)
filtered_data$primary_school_enrollment_male[is.na(filtered_data$primary_school_enrollment_male)] <-
  median(filtered_data$primary_school_enrollment_male, na.rm = TRUE)

# Moderate Missingness MNAR -> KNN for secondary school enrollments 
knn_data_secondary <- filtered_data %>%
  select(secondary_school_enrollment_female, secondary_school_enrollment_male,
         population, gdp_per_capita, urban_population, region_numeric) %>%
  as.data.frame()
imputed_knn_secondary <- knnImputation(knn_data_secondary, k = 5)
filtered_data <- filtered_data %>%
  mutate(
    secondary_school_enrollment_female = ifelse(
      is.na(secondary_school_enrollment_female),
      imputed_knn_secondary$secondary_school_enrollment_female,
      secondary_school_enrollment_female
    ),
    secondary_school_enrollment_male = ifelse(
      is.na(secondary_school_enrollment_male),
      imputed_knn_secondary$secondary_school_enrollment_male,
      secondary_school_enrollment_male
    )
  )

# MAR (~16.18%) -> KNN for post-secondary enrollments 
knn_data_post_secondary <- filtered_data %>%
  select(post_secondary_enrollment_female, post_secondary_enrollment_male,
         secondary_school_enrollment_female, secondary_school_enrollment_male,
         gdp_per_capita, urban_population, region_numeric) %>%
  as.data.frame()
imputed_knn_postsec <- knnImputation(knn_data_post_secondary, k = 5)
filtered_data <- filtered_data %>%
  mutate(
    post_secondary_enrollment_female = ifelse(
      is.na(post_secondary_enrollment_female),
      imputed_knn_postsec$post_secondary_enrollment_female,
      post_secondary_enrollment_female
    ),
    post_secondary_enrollment_male = ifelse(
      is.na(post_secondary_enrollment_male),
      imputed_knn_postsec$post_secondary_enrollment_male,
      post_secondary_enrollment_male
    )
  )

# MAR (~2.45%) -> KNN for imports/exports 
knn_data_trade <- filtered_data %>%
  select(imports, exports, gdp, population, region_numeric) %>%
  as.data.frame()
imputed_knn_trade <- knnImputation(knn_data_trade, k = 5)
filtered_data <- filtered_data %>%
  mutate(
    imports = ifelse(is.na(imports), imputed_knn_trade$imports, imports),
    exports = ifelse(is.na(exports), imputed_knn_trade$exports, exports)
  )

# MAR (~11.27%) -> KNN for homicide_rate
knn_data_homicide <- filtered_data %>%
  select(homicide_rate, gdp_per_capita, population, 
         urban_population, region_numeric) %>%
  as.data.frame()
imputed_knn_homicide <- knnImputation(knn_data_homicide, k = 5)
filtered_data$homicide_rate[is.na(filtered_data$homicide_rate)] <-
  imputed_knn_homicide$homicide_rate

# MAR (~4.9%) -> KNN for tourists
knn_data_tourism <- filtered_data %>%
  select(tourists, population, gdp_per_capita, region_numeric) %>%
  as.data.frame()
imputed_knn_tourism <- knnImputation(knn_data_tourism, k = 5)
filtered_data$tourists[is.na(filtered_data$tourists)] <- imputed_knn_tourism$tourists

# MAR >20% -> MICE for CO2 emissions
mice_data_co2 <- filtered_data %>%
  select(co2_emissions, gdp, imports, population, urban_population, region_numeric) %>%
  as.data.frame()
imputed_co2 <- mice(mice_data_co2, method = 'pmm', m = 5, maxit = 10)
mice_completed_co2 <- complete(imputed_co2, 1)
filtered_data$co2_emissions[is.na(filtered_data$co2_emissions)] <-
  mice_completed_co2$co2_emissions

# Moderate missingness for employment indicators -> KNN 
knn_data_employment <- filtered_data %>%
  select(employment_agriculture, employment_industry, employment_services,
         gdp_per_capita, population, region_numeric) %>%
  as.data.frame()
imputed_knn_employment <- knnImputation(knn_data_employment, k = 5)
filtered_data <- filtered_data %>%
  mutate(
    employment_agriculture = ifelse(is.na(employment_agriculture),
                                    imputed_knn_employment$employment_agriculture,
                                    employment_agriculture),
    employment_industry = ifelse(is.na(employment_industry),
                                 imputed_knn_employment$employment_industry,
                                 employment_industry),
    employment_services = ifelse(is.na(employment_services),
                                 imputed_knn_employment$employment_services,
                                 employment_services)
  )

# MCAR or minimal MAR -> Median for infant_mortality
filtered_data$infant_mortality[is.na(filtered_data$infant_mortality)] <-
  median(filtered_data$infant_mortality, na.rm = TRUE)

##############################################
# 2.2 FINAL CLEANED DATA
##############################################
# The dataset at this point should have imputed values for all relevant variables, 
# preserving unique IDs (iso2, name) and region codes.

cleaned_data <- filtered_data %>%
  select(all_of(selected_indicators_with_ids), region_numeric)

cat("Original Dimensions:", dim(country_data), "\n")
cat("Filtered Dimensions (after removing Vatican):", dim(filtered_data), "\n")
cat("Final Cleaned Dimensions (Selected Indicators):", dim(cleaned_data), "\n")

# Optionally check for any remaining missing values
# missing_values_summary <- cleaned_data %>%
#   summarise(across(everything(), ~ sum(is.na(.)))) %>%
#   pivot_longer(cols = everything(), names_to = "Indicator", values_to = "Missing_Values")
# print(missing_values_summary, n = Inf)

# Optional to persist the cleaned dataset
# write.csv(cleaned_data, "cleaned_country_data_FINAL.csv", row.names = FALSE)

##############################################
# 3. DATA NORMALIZATION
##############################################
# Bring different variables to compatible scales (e.g., log, z-score, min-max). 
# This is especially important for algorithms sensitive to variable magnitude.

##############################################
# 3.1 DATA SKEWNESS
##############################################
# Checking data skew helps in selecting transformations. 
# Heavily skewed distributions may need log or other non-linear transformations.

if (!exists("indicators")) stop("The 'indicators' list is not defined.")

selected_indicators <- unique(unlist(indicators)) %>%
  intersect(names(country_data))

missing_indicators <- setdiff(unique(unlist(indicators)), selected_indicators)
if (length(missing_indicators) > 0) {
  warning("The following indicators are missing: ", 
          paste(missing_indicators, collapse = ", "))
}

df_long <- country_data %>%
  select(all_of(selected_indicators)) %>%
  pivot_longer(cols = everything(), names_to = "indicator", values_to = "value") %>%
  filter(is.finite(value)) %>%
  left_join(
    tibble(
      indicator = unlist(indicators),
      category = rep(names(indicators), times = lengths(indicators))
    ) %>% distinct(indicator, .keep_all = TRUE),
    by = "indicator"
  ) %>%
  mutate(indicator_abbrev = abbreviate_indicator(indicator))

custom_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

# Histograms/densities across all indicators, colored by category 
# for quick visual groupings. 
static_plot <- ggplot(df_long, aes(x = value, fill = category)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "white", alpha = 0.7) +
  geom_density(color = "black", size = 0.5) +
  facet_wrap(~ indicator_abbrev, scales = "free", ncol = 5) +
  scale_fill_manual(values = custom_palette) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(size = 8, face = "bold"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "bottom",
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_blank()
  ) +
  labs(x = "Value", y = "Density")

print(static_plot)

# OPTIONAL: Save distribution plots
# ggsave("distribution_plots.png", static_plot, width = 16, height = 12, dpi = 300, bg = "white")

##############################################
# 3.2 APPLY NORMALIZATION METHODS
##############################################
# Different subsets of variables receive transformations based on 
# distribution (log-z, min-max, or standard z-score).

normalized_data <- cleaned_data

log_z_vars <- c("gdp","gdp_per_capita","population","pop_density","co2_emissions",
                "tourists","fertility","imports","exports","threatened_species",
                "homicide_rate","infant_mortality")

min_max_vars <- c("forested_area",
                  "primary_school_enrollment_female","primary_school_enrollment_male",
                  "secondary_school_enrollment_female","secondary_school_enrollment_male",
                  "post_secondary_enrollment_female","post_secondary_enrollment_male",
                  "employment_agriculture","employment_industry","employment_services",
                  "urban_population","sex_ratio")

z_vars <- c("life_expectancy_male","life_expectancy_female","gdp_growth",
            "pop_growth","urban_population_growth")

for (v in log_z_vars) {
  if(v %in% names(normalized_data)) {
    normalized_data[[v]] <- safe_log_z_transform(normalized_data[[v]])
  }
}

for (v in min_max_vars) {
  if(v %in% names(normalized_data)) {
    normalized_data[[v]] <- min_max_scaling(normalized_data[[v]])
  }
}

for (v in z_vars) {
  if(v %in% names(normalized_data)) {
    normalized_data[[v]] <- z_score_normalization(normalized_data[[v]])
  }
}

final_normalized_data <- normalized_data %>%
  select(all_of(selected_indicators_with_ids), region_numeric)

# OPTIONAL: Export normalized dataset
write.csv(final_normalized_data, "normalized_country_data_FINAL.csv", row.names = FALSE)

##############################################
# 4. EXPLORATORY DATA ANALYSIS
##############################################
# Preliminary analysis to detect patterns, correlations, potential anomalies, 
# or relevant country-level differences in geospatial plots.

#############################################################################
# 4.1 Correlation Matrix and Heatmap Visualization
#############################################################################
# Summaries of correlation are valuable for evaluating redundant features
# and potential multi-collinearity in subsequent modeling.

numeric_data <- final_normalized_data %>%
  select(-iso2, -name, -region, -region_numeric)

corr_matrix <- cor(numeric_data, use = "complete.obs")
melted_corr_matrix <- reshape2::melt(corr_matrix, na.rm = TRUE)

brewer_palette <- brewer.pal(n = 11, name = "RdBu")
reversed_brewer_palette <- rev(brewer_palette)

corr_heatmap_plot <- ggplot(melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = reversed_brewer_palette,
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.5),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(title = "Correlation Heatmap of the Indicators", x = "", y = "") +
  geom_text(aes(label = round(value, 2)), size = 3, color = "black") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "right",
    legend.text = element_text(size = 6),
    legend.title = element_blank(),
    legend.key.width = unit(0.45, "cm"),
    legend.key.height = unit(0.9, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  coord_fixed()

corr_heatmap_plot

# OPTIONAL: Save the correlation heatmap
ggsave("correlation_matrix_heatmap.png", corr_heatmap_plot, width = 16, height = 16, dpi = 300, bg = "white")

########################################################
# 4.2 SCATTER PLOTS WITH CORRELATION LINES
########################################################
# Examining pairwise relationships among especially relevant variables 
# (e.g., GDP vs. Exports, Fertility vs. Education) can reveal linear 
# or otherwise simple correlations.

required_columns <- c("gdp", "exports", "fertility", 
                      "post_secondary_enrollment_female", "gdp_growth")
missing_columns <- setdiff(required_columns, names(final_normalized_data))
if (length(missing_columns) > 0) {
  stop("Required columns missing: ", paste(missing_columns, collapse = ", "))
}

relationships <- data.frame(
  x = c("gdp", "gdp_growth", "fertility"),
  y = c("exports", "exports", "post_secondary_enrollment_female"),
  point_color = c(brewer.pal(11, "RdBu")[9], 
                  brewer.pal(11, "RdBu")[10], 
                  brewer.pal(11, "RdBu")[11]),
  line_color = c(brewer.pal(11, "RdBu")[1], 
                 brewer.pal(11, "RdBu")[2], 
                 brewer.pal(11, "RdBu")[3]),
  stringsAsFactors = FALSE
)

scatter_plots <- lapply(1:nrow(relationships), function(i) {
  rel <- relationships[i, ]
  corr <- cor(final_normalized_data[[rel$x]], 
              final_normalized_data[[rel$y]], use = "complete.obs")
  ggplot(final_normalized_data, aes_string(x = rel$x, y = rel$y)) +
    geom_point(color = rel$point_color, size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", color = rel$line_color, se = FALSE, linetype = "dashed") +
    labs(
      subtitle = paste("Correlation:", round(corr, 2)),
      x = rel$x,
      y = rel$y
    ) +
    coord_fixed(ratio = 1) +
    theme_minimal() +
    theme(
      plot.subtitle = element_text(size = 9, face = "italic"),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 6),
      plot.margin = unit(c(0, 0, 2, 0), "cm")
    )
})

combined_scatter_plots <- plot_grid(
  scatter_plots[[1]], 
  scatter_plots[[2]], 
  scatter_plots[[3]],
  ncol = 3,
  align = "hv",
  labels = c("A", "B", "C"),
  label_size = 10,
  rel_widths = c(1, 1, 1)
)

combined_scatter_plots

# OPTIONAL: Save the combined scatter plot
# ggsave("scatter_plots_with_correlation.png", plot = combined_scatter_plots, width = 12, height = 8, dpi = 300, bg = "white")

########################################################
# 4.3 EDUCATION ATTAINMENT - FERTILITY RATE CHOROPLETH
########################################################
# Displays global patterns of fertility vs. educational attainment 
# to identify geographic regions with potential resource constraints 
# or policy priorities.

country_data <- country_data %>%
  rowwise() %>%
  mutate(
    education_attainment = mean(
      c(primary_school_enrollment_female,
        secondary_school_enrollment_female,
        post_secondary_enrollment_female),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

world_merged <- prepare_world_map_data(country_data, iso_col="iso2")

fertility_thresholds <- quantile(world_merged$fertility, probs = c(0.33, 0.66), na.rm = TRUE)
world_merged <- world_merged %>%
  mutate(
    fertility_category = case_when(
      fertility <= fertility_thresholds[1] ~ "Low",
      fertility <= fertility_thresholds[2] ~ "Moderate",
      TRUE ~ "High"
    )
  )

education_thresholds <- quantile(world_merged$education_attainment, probs = c(0.33, 0.66), na.rm = TRUE)
world_merged <- world_merged %>%
  mutate(
    education_category = case_when(
      education_attainment <= education_thresholds[1] ~ "Low",
      education_attainment <= education_thresholds[2] ~ "Moderate",
      TRUE ~ "High"
    )
  )

custom_map_theme <- theme_void() +
  theme(
    plot.margin = unit(c(0,0,2,0), "pt"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.3, "cm"),
    plot.caption = element_text(size = 5, hjust = 1)
  )

fertility_map <- ggplot(world_merged) +
  geom_sf(aes(fill = fertility_category), color = "white") +
  scale_fill_manual(
    values = c("Low" = "#1B9E77",
               "Moderate" = "#7570B3",
               "High" = "#D95F02"),
    na.value = "grey90"
  ) +
  labs(title = "Fertility Rates", caption = "Data Source: Kaggle") +
  custom_map_theme

education_map <- ggplot(world_merged) +
  geom_sf(aes(fill = education_category), color = "white") +
  scale_fill_manual(
    values = c("Low" = "#D73027",
               "Moderate" = "#FC8D59",
               "High" = "#91BFDB"),
    na.value = "grey90"
  ) +
  labs(title = "Educational Attainment", caption = NULL) +
  custom_map_theme

edu_fer_maps <- cowplot::plot_grid(
  education_map,
  fertility_map,
  nrow = 2,
  align = "h"
)
edu_fer_maps

world_merged <- world_merged %>%
  mutate(
    combined_category = case_when(
      fertility_category == "High" & education_category == "Low" ~ "High Fertility, Low Education",
      fertility_category == "Low" & education_category == "High" ~ "Low Fertility, High Education",
      TRUE ~ "Others"
    )
  )

disparity_edu_fer_map <- ggplot(world_merged) +
  geom_sf(aes(fill = combined_category), color = "white") +
  scale_fill_manual(
    values = c(
      "High Fertility, Low Education" = "#d7301f",
      "Low Fertility, High Education" = "#1a9850",
      "Others" = "#bababa"
    ),
    na.value = "grey90"
  ) +
  labs(
    title = "Geographic Disparities in Fertility and Education",
    subtitle = "High Fertility & Low Education vs. Low Fertility & High Education",
    caption = "Data Source: Kaggle"
  ) +
  custom_map_theme

disparity_edu_fer_map

combined_edu_fer_disparity <- (education_map | fertility_map) / disparity_edu_fer_map +
  plot_layout(heights = c(1, 1.5))
combined_edu_fer_disparity

# OPTIONAL: Save maps
# ggsave("fertility_map.png", fertility_map, width = 10, height = 6, dpi = 300, bg = "white")
# ggsave("education_map.png", education_map, width = 10, height = 6, dpi = 300, bg = "white")
# ggsave("combined_education_fertility_maps.png", edu_fer_maps, width = 14, height = 8, dpi = 300, bg = "white")
# ggsave("disparity_education_fertility_map.png", disparity_edu_fer_map, width = 14, height = 8, dpi = 300, bg = "white")
# ggsave("combined_edu_fer_disparity.png", combined_edu_fer_disparity, width = 14, height = 8, dpi = 300, bg = "white")

########################################################
# 4.4 EDUCATION ATTAINMENT - INFANT MORTALITY CHOROPLETH
########################################################
# Similar approach focusing on infant mortality to identify 
# potential hot spots for health interventions relative to education.

infant_mortality_thresholds <- quantile(world_merged$infant_mortality, probs = c(0.33, 0.66), na.rm = TRUE)
world_merged <- world_merged %>%
  mutate(
    infant_mortality_category = case_when(
      infant_mortality <= infant_mortality_thresholds[1] ~ "Low",
      infant_mortality <= infant_mortality_thresholds[2] ~ "Moderate",
      TRUE ~ "High"
    )
  )

education_thresholds <- quantile(world_merged$education_attainment, probs = c(0.33, 0.66), na.rm = TRUE)
world_merged <- world_merged %>%
  mutate(
    education_category = case_when(
      education_attainment <= education_thresholds[1] ~ "Low",
      education_attainment <= education_thresholds[2] ~ "Moderate",
      TRUE ~ "High"
    )
  )

world_merged <- world_merged %>%
  mutate(
    infant_mortality_category = factor(infant_mortality_category, 
                                       levels = c("High", "Moderate", "Low")),
    education_category = factor(education_category, 
                                levels = c("High", "Moderate", "Low"))
  )

infant_mortality_map <- ggplot(world_merged) +
  geom_sf(aes(fill = infant_mortality_category), color = "white") +
  scale_fill_manual(
    values = c("Low" = "#1B9E77",
               "Moderate" = "#7570B3",
               "High" = "#D73027"),
    na.value = "grey90"
  ) +
  labs(title = "Infant Mortality Rates") +
  custom_map_theme

education_map <- ggplot(world_merged) +
  geom_sf(aes(fill = education_category), color = "white") +
  scale_fill_manual(
    values = c("Low" = "#D73027",
               "Moderate" = "#FC8D59",
               "High" = "#91BFDB"),
    na.value = "grey90"
  ) +
  labs(title = "Educational Attainment") +
  custom_map_theme

edu_mor_maps <- cowplot::plot_grid(
  education_map,
  infant_mortality_map,
  nrow = 2,
  align = "h"
)
edu_mor_maps

world_merged <- world_merged %>%
  mutate(
    combined_category = case_when(
      infant_mortality_category == "High" & education_category == "Low" ~ "High Mortality, Low Education",
      infant_mortality_category == "Low" & education_category == "High" ~ "Low Mortality, High Education",
      TRUE ~ "Others"
    )
  )

disparity_edu_mor_map <- ggplot(world_merged) +
  geom_sf(aes(fill = combined_category), color = "white") +
  scale_fill_manual(
    values = c(
      "High Mortality, Low Education" = "#d7301f",
      "Low Mortality, High Education" = "#1a9850",
      "Others" = "#bababa"
    ),
    na.value = "grey90"
  ) +
  labs(   title = "Geographic Disparities in Infant Mortality and Education",
          subtitle = "High Mortality & Low Education vs. Low Mortality & High Education",
          caption = "Data Source: Kaggle") +
  custom_map_theme

top_row <- plot_grid(
  education_map, 
  infant_mortality_map, 
  ncol = 2, 
  rel_widths = c(1, 1),
  align = 'hv'
) + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

combined_edu_mor_disparity <- plot_grid(
  top_row, 
  disparity_edu_mor_map, 
  ncol = 1,
  rel_heights = c(4, 5)
) + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

combined_edu_mor_disparity

# OPTIONAL: Save maps
# ggsave("infant_mortility_map.png", plot = infant_mortality_map, width = 10, height = 6, dpi = 300, bg = "white")
# ggsave("combined_education_mortality_maps.png", plot = edu_mor_maps, width = 14, height = 8, dpi = 300, bg = "white")
# ggsave("disparity_education_mortality_map.png", plot = disparity_edu_mor_map, width = 14, height = 8, dpi = 300, bg = "white")
# ggsave("combined_edu_mor_disparity.png", plot = combined_edu_mor_disparity, width = 14, height = 8, dpi = 300, bg = "white")

###########################################################################
# 4.5 CO2 EMISSIONS & FOREST COVERAGE CHOROPLETH
###########################################################################
# Examines environmental pressure points by categorizing countries 
# based on CO2 emissions and forest coverage.

thresholds <- function(column) quantile(column, probs = c(0.33, 0.66), na.rm = TRUE)

world_merged <- world_merged %>%
  mutate(
    co2_category = cut(co2_emissions, breaks = c(-Inf, thresholds(co2_emissions), Inf),
                       labels = c("Low", "Moderate", "High")),
    forest_category = cut(forested_area, breaks = c(-Inf, thresholds(forested_area), Inf),
                          labels = c("Low", "Moderate", "High")),
    combined_co2_forest = factor(
      paste(co2_category, "CO2,", forest_category, "Forest"),
      levels = c(
        "Low CO2, High Forest", "Low CO2, Moderate Forest", "Low CO2, Low Forest",
        "Moderate CO2, High Forest", "Moderate CO2, Moderate Forest", "Moderate CO2, Low Forest",
        "High CO2, High Forest", "High CO2, Moderate Forest", "High CO2, Low Forest"
      )
    )
  )

color_gradient <- setNames(viridisLite::plasma(9), levels(world_merged$combined_co2_forest))

co2_forest_map <- ggplot(world_merged) +
  geom_sf(aes(fill = combined_co2_forest), color = "white") +
  scale_fill_manual(values = color_gradient, na.value = "grey90") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 1, hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.3, "cm"),
    plot.margin = unit(c(0, 0, 2, 0), "cm")
  )

co2_forest_map

# OPTIONAL: Save the map
# ggsave("disparity_co2_forest_map.png", co2_forest_map, width = 10, height = 6, dpi = 300, bg = "white")

##############################################
# 5. FEATURE ENGINEERING
##############################################
# Create additional features (e.g., interactions, polynomials, 
# aggregated indicators) to enhance model performance 
# and capture non-linear relationships.

# --- 1) Interaction Terms ---
engineered_data$secFemale_svcEmp <- engineered_data$secondary_school_enrollment_female *
  engineered_data$employment_services

engineered_data$gdp_x_exports <- engineered_data$gdp * engineered_data$exports

engineered_data$tourism_co2 <- engineered_data$tourists * engineered_data$co2_emissions

engineered_data$gdpPerCap_co2 <- engineered_data$gdp_per_capita * engineered_data$co2_emissions

engineered_data$fertility_SecFem <- engineered_data$fertility *
  engineered_data$secondary_school_enrollment_female

# --- 2) Polynomial Features ---
engineered_data$fertility_sq <- engineered_data$fertility^2
engineered_data$fertility_cubed <- engineered_data$fertility^3
engineered_data$gdp_growth_sq <- engineered_data$gdp_growth^2
engineered_data$pop_density_sq <- engineered_data$pop_density^2
engineered_data$co2_emissions_sq <- engineered_data$co2_emissions^2
engineered_data$tourists_sq <- engineered_data$tourists^2

# --- 3) Aggregated Indicators ---
engineered_data$trade_index <- rowMeans(
  engineered_data[c("imports", "exports")],
  na.rm = TRUE
)

engineered_data$human_capital_index <- (
  0.4 * engineered_data$post_secondary_enrollment_female +
    0.45 * engineered_data$secondary_school_enrollment_female +
    0.15 * engineered_data$primary_school_enrollment_female
)

engineered_data$env_stress_index <- engineered_data$co2_emissions + 
  engineered_data$threatened_species

engineered_data$sustainability_index <- engineered_data$forested_area - 
  engineered_data$co2_emissions

engineered_data$tourism_infra_index <- rowMeans(
  engineered_data[c("tourists","employment_services")],
  na.rm = TRUE
)

# --- 4) Categorical Encoding ---
if ("region" %in% names(engineered_data)) {
  engineered_data <- dummy_cols(
    engineered_data,
    select_columns = "region",
    remove_first_dummy = TRUE
  )
}

engineered_data <- engineered_data %>%
  rename_with(
    .fn   = ~ make_clean_names(.),
    .cols = starts_with("region_")
  )

# --- 5) Minimal Temporal Features ---
engineered_data$annual_gdp_growth_rate <- engineered_data$gdp_growth
engineered_data$annual_pop_growth_rate <- engineered_data$pop_growth

engineered_data$popGrowth_fertility <- engineered_data$annual_pop_growth_rate *
  engineered_data$fertility

engineered_data$gdpGrowth_exports <- engineered_data$annual_gdp_growth_rate *
  engineered_data$exports

# OPTIONAL: Export the engineered dataset
# write.csv(engineered_data, "engineering_data_FINAL.csv", row.names = FALSE)

##############################################
# 6. MACHINE LEARNING TECHNIQUES
##############################################
# Illustrates several modeling approaches for multiple analytical objectives.

# -------------------------
# Economic Development
# -------------------------

# --- 1(a) GDP Growth Prediction ---
# Random Forest predicting GDP Growth from selected economic factors and interactions.
gdp_growth_formula <- gdp_growth ~ (imports + exports) + co2_emissions + co2_emissions_sq +
  employment_services + gdpPerCap_co2 +
  region_central_america + region_central_asia + region_eastern_africa +
  region_eastern_asia + region_eastern_europe + region_melanesia +
  region_micronesia + region_middle_africa + region_northern_africa +
  region_northern_america + region_northern_europe + region_oceania +
  region_polynesia + region_south_america + region_south_eastern_asia +
  region_southern_africa + region_southern_asia + region_southern_europe +
  region_western_africa + region_western_asia + region_western_europe

set.seed(123)
rf_gdp_growth <- randomForest(
  formula = gdp_growth_formula, 
  data = engineered_data,
  ntree = 300,
  importance = TRUE
)
print(rf_gdp_growth)
importance(rf_gdp_growth)

# --- 1(b) Trade Balance Prediction ---
# Simple linear model predicting trade balance from economic & workforce factors.
engineered_data$trade_balance <- engineered_data$exports - engineered_data$imports
trade_balance_formula <- trade_balance ~ gdp + employment_services + employment_agriculture +
  human_capital_index + region_eastern_europe + region_northern_europe +
  region_southern_europe + region_western_europe + pop_density_sq

trade_lm <- lm(trade_balance_formula, data = engineered_data)
summary(trade_lm)

# --- 1(c) Categorizing Development Tiers ---
# K-means clustering using selected indicators, labeling clusters by descending 
# overall "development score" (sum of cluster means).
dev_data <- engineered_data[, c("gdp_per_capita", "trade_balance", 
                                "human_capital_index", "employment_services")]
set.seed(123)
k3 <- kmeans(dev_data, centers = 3, nstart = 25)
cluster_means <- aggregate(dev_data, by = list(cluster = k3$cluster), FUN = mean)
cluster_means$dev_score <- rowSums(cluster_means[ , -1])
ranked_clusters <- cluster_means$cluster[order(cluster_means$dev_score, decreasing = TRUE)]

engineered_data$development_tier <- factor(
  k3$cluster,
  levels = ranked_clusters,
  labels = c("TierA", "TierB", "TierC")
)

cluster_sizes <- engineered_data %>%
  count(development_tier) %>%
  rename("Tier" = development_tier, "Number of Countries" = n)
cluster_sizes

# Mapping development tiers. Each country is colored by assigned tier.
world_merged <- world_merged %>%
  left_join(
    select(engineered_data, iso2, development_tier),
    by = c("iso_a2" = "iso2")
  )

world_merged$development_tier <- factor(
  world_merged$development_tier,
  levels = c("TierA", "TierB", "TierC")
)

tier_levels <- levels(world_merged$development_tier)
palette_colors <- viridis(
  n = length(tier_levels),
  option = "plasma",
  direction = 1
)
names(palette_colors) <- tier_levels

tier_map <- ggplot(world_merged) +
  geom_sf(aes(fill = development_tier), color = "white") +
  scale_fill_manual(values = palette_colors, na.value = "grey90") +
  labs(title = "Development Tiers", fill = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.4, "cm"),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )
tier_map

# OPTIONAL: Save the map
# ggsave("development_tiers_map.png", tier_map, width = 10, height = 6, dpi = 300, bg = "white")

# --- 1(d) Economic Downturn (Binary) ---
# Logistic regression to see which factors (CO2, trade_balance, region, etc.) 
# coincide with a non-positive GDP growth.
threshold_gdp_growth <- 0
engineered_data$likely_downturn <- ifelse(engineered_data$gdp_growth <= threshold_gdp_growth, 1, 0)

downturn_formula <- likely_downturn ~ co2_emissions + co2_emissions_sq + trade_balance +
  region_eastern_africa + region_middle_africa + region_northern_africa +
  region_southern_africa + region_western_africa + employment_industry +
  secFemale_svcEmp

downturn_glm <- glm(downturn_formula, data = engineered_data, family = binomial())
summary(downturn_glm)

# -------------------------
# Population Dynamics
# -------------------------

# --- 2(a) Population Growth Prediction ---
# OLS model capturing non-linear fertility (squared, cubic) 
# and effects of urbanization, services, region.
pop_growth_formula <- pop_growth ~ fertility + fertility_sq + fertility_cubed +
  urban_population + employment_services + region_central_asia + region_eastern_asia +
  region_south_eastern_asia + region_southern_asia
pop_growth_lm <- lm(pop_growth_formula, data = engineered_data)
summary(pop_growth_lm)

# --- 2(b) Fertility Rates Prediction ---
# Random Forest to model fertility using economic and health indicators, plus 
# an interaction fertility_SecFem capturing a negative synergy with female education.
set.seed(123)
fert_formula <- fertility ~ human_capital_index + gdp_per_capita +
  infant_mortality + region_eastern_africa + region_middle_africa + region_northern_africa +
  region_southern_africa + region_western_africa + employment_industry +
  fertility_SecFem
fert_rf_model <- randomForest(fert_formula, data = engineered_data, ntree = 200, importance = TRUE)
print(fert_rf_model)
importance(fert_rf_model)

# --- 2(c) Population Stability Classification ---
# Cuts pop_growth into categories: "Declining," "Stable," "Growing."
# A Random Forest classifies countries according to these categories.
set.seed(123)
engineered_data$pop_stability <- cut(
  engineered_data$pop_growth,
  breaks = c(-Inf, 0, 2, Inf),
  labels = c("Declining","Stable","Growing")
)
pop_stab_formula <- pop_stability ~ fertility + fertility_sq + urban_population +
  region_central_america + region_central_asia + region_eastern_africa +
  region_eastern_asia + region_eastern_europe + region_melanesia +
  region_micronesia + region_middle_africa + region_northern_africa +
  region_northern_america + region_northern_europe + region_oceania +
  region_polynesia + region_south_america + region_south_eastern_asia +
  region_southern_africa + region_southern_asia + region_southern_europe +
  region_western_africa + region_western_asia + region_western_europe

rf_pop_stab <- randomForest(pop_stab_formula, data = engineered_data, ntree=200)
rf_pop_stab

# --- 2(d) Migration Trends (Binary) ---
# Using a simple naive_natural_growth as baseline, 
# net_in_migration is flagged if pop_growth significantly exceeds the naive baseline.
engineered_data$naive_natural_growth <- engineered_data$fertility - 
  (engineered_data$infant_mortality / 100)
engineered_data$net_in_migration <- ifelse(
  engineered_data$pop_growth > (engineered_data$naive_natural_growth + 0.5),
  1, 
  0
)
migration_formula <- net_in_migration ~ gdp_per_capita + employment_services + 
  life_expectancy_female + region_eastern_europe + region_northern_europe +  
  region_southern_europe + region_western_europe + pop_density_sq
migration_glm <- glm(migration_formula, data = engineered_data, family=binomial())
summary(migration_glm)

# -------------------------
# Education and Employment
# -------------------------

# --- 3(a) Predicting Educational Attainment ---
# OLS with polynomial fertility included, as high fertility might indirectly affect 
# female education (opportunity cost, resources).
edu_formula <- human_capital_index ~ gdp_per_capita + fertility + fertility_sq +
  co2_emissions + region_eastern_africa + region_middle_africa + region_northern_africa +
  region_southern_africa + region_western_africa + pop_density_sq
edu_lm <- lm(edu_formula, data=engineered_data)
summary(edu_lm)

# --- 3(b) Predicting Employment Distribution ---
# Random Forest predicting fraction employed in agriculture from GDP, population, 
# region, and education-based variables.
set.seed(123)
agri_formula <- employment_agriculture ~ gdp + population + secondary_school_enrollment_female +
  region_central_asia + region_eastern_asia + region_south_eastern_asia + region_southern_asia +
  fertility_sq
agri_rf_model <- randomForest(agri_formula, data=engineered_data, ntree=250)
print(agri_rf_model)
print(importance(agri_rf_model))

# --- 3(c) Classifying Education Levels ---
# Divides human_capital_index into Low, Medium, High, then uses a Random Forest for classification.
set.seed(123)
engineered_data$edu_level <- cut(
  engineered_data$human_capital_index,
  breaks = c(-Inf, 0.3, 0.6, Inf),
  labels = c("Low","Medium","High")
)
edu_level_formula <- edu_level ~ gdp_per_capita + fertility_sq +
  region_central_america + region_central_asia + region_eastern_africa +
  region_eastern_asia + region_eastern_europe + region_melanesia +
  region_micronesia + region_middle_africa + region_northern_africa +
  region_northern_america + region_northern_europe + region_oceania +
  region_polynesia + region_south_america + region_south_eastern_asia +
  region_southern_africa + region_southern_asia + region_southern_europe +
  region_western_africa + region_western_asia + region_western_europe
rf_edu_level <- randomForest(edu_level_formula, data=engineered_data, ntree=150)
rf_edu_level

# --- 3(d) Economic Resilience (Binary) ---
# Defines "resilient" as countries with above-median GDP per capita and 
# human capital index, then models it with logistic regression.
median_gdp_pc <- median(engineered_data$gdp_per_capita, na.rm=TRUE)
median_hci <- median(engineered_data$human_capital_index, na.rm=TRUE)
engineered_data$resilient <- ifelse(
  engineered_data$gdp_per_capita > median_gdp_pc & 
    engineered_data$human_capital_index > median_hci, 
  1, 
  0
)
resilience_formula <- resilient ~ employment_services + co2_emissions_sq +
  region_central_asia + region_eastern_asia + region_south_eastern_asia +
  region_southern_asia + region_eastern_europe + region_northern_europe +
  region_southern_europe + region_western_europe + pop_density_sq
resilience_glm <- glm(resilience_formula, data=engineered_data, family=binomial())
summary(resilience_glm)

# -------------------------
# Health and Safety
# -------------------------

# --- 4(a) Predicting Life Expectancy ---
# Random Forest capturing a variety of factors, including possible 
# non-linear fertility interactions. 
set.seed(123)
lifeexp_formula <- life_expectancy_female ~ human_capital_index + gdp_per_capita + 
  region_central_america + region_northern_america + region_south_america +
  fertility_sq + pop_density_sq
lifeexp_rf_model <- randomForest(lifeexp_formula, data=engineered_data, ntree=300, importance=TRUE)
print(lifeexp_rf_model)
importance(lifeexp_rf_model)

# --- 4(b) Predicting Infant Mortality ---
# Simple linear model linking educational enrollment, GDP, fertility 
# (via squared transformation), and CO2 emissions (also squared).
infant_formula <- infant_mortality ~ secondary_school_enrollment_female +
  gdp_per_capita + co2_emissions_sq + fertility_sq
infant_lm <- lm(infant_formula, data=engineered_data)
summary(infant_lm)

# --- 4(c) Categorizing Health Status ---
# health_score approximates net health by subtracting infant_mortality from 
# life_expectancy_female. Then a Random Forest classification can indicate 
# broader health categories across regions.
set.seed(123)
engineered_data$health_score <- engineered_data$life_expectancy_female - engineered_data$infant_mortality
engineered_data$health_status <- cut(
  engineered_data$health_score,
  breaks=c(-Inf, 0, 20, Inf),
  labels=c("Unhealthy","Moderate","Healthy")
)
engineered_data$health_status <- as.character(engineered_data$health_status)
engineered_data$health_status[engineered_data$health_status == "Healthy"] <- "Moderate"
engineered_data$health_status <- factor(engineered_data$health_status)

health_status_formula <- health_status ~ gdp_per_capita + human_capital_index + 
  region_central_america + region_central_asia + region_eastern_africa +
  region_eastern_asia + region_eastern_europe + region_melanesia +
  region_micronesia + region_middle_africa + region_northern_africa +
  region_northern_america + region_northern_europe + region_oceania +
  region_polynesia + region_south_america + region_south_eastern_asia +
  region_southern_africa + region_southern_asia + region_southern_europe +
  region_western_africa + region_western_asia + region_western_europe +
  fertility_sq
rf_health_status <- randomForest(health_status_formula, data=engineered_data, ntree=200)
rf_health_status

# --- 4(d) Public Health Crises (Binary) ---
# Flags a crisis if infant_mortality is quite high (>50) and female life expectancy <60. 
# Logistic regression identifies correlates among environmental or demographic factors.
engineered_data$health_crisis <- ifelse(
  engineered_data$infant_mortality > 50 & engineered_data$life_expectancy_female < 60, 
  1, 
  0
)
crisis_formula <- health_crisis ~ co2_emissions + region_central_asia + 
  region_eastern_asia + region_south_eastern_asia + region_southern_asia +
  fertility_cubed + secFemale_svcEmp
crisis_glm <- glm(crisis_formula, data=engineered_data, family=binomial())
summary(crisis_glm)

# -------------------------
# Environment and Tourism
# -------------------------

# --- 5(a) Predicting CO2 Emissions ---
# Random Forest exploring the link between CO2 and GDP, trade balance, population, 
# and region. co2_emissions_sq is included to capture possible non-linearities.
set.seed(123)
co2_formula <- co2_emissions ~ gdp + trade_balance + population + region_central_asia +
  region_eastern_asia + region_south_eastern_asia + region_southern_asia +
  region_eastern_europe + region_northern_europe + region_southern_europe +
  region_western_europe + co2_emissions_sq
co2_rf_model <- randomForest(co2_formula, data=engineered_data, ntree=300, importance=TRUE)
print(co2_rf_model)
importance(co2_rf_model)

# --- 5(b) Predicting Tourist Numbers ---
# OLS approach to determine key drivers of tourism, 
# e.g., environmental sustainability or economic indicators.
tourism_formula <- tourists ~ sustainability_index + gdp_per_capita +
  co2_emissions_sq + region_eastern_europe + region_northern_europe +
  region_southern_europe + region_western_europe + region_central_america +
  region_northern_america + region_south_america
tourism_lm <- lm(tourism_formula, data=engineered_data)
summary(tourism_lm)

# --- 5(c) Environmental Sustainability Classification ---
# Creates discrete categories of sustainability via sustainability_index. 
# Random Forest to explore patterns behind unsustainable vs. sustainable outcomes.
set.seed(123)
engineered_data$sustain_level <- cut(
  engineered_data$sustainability_index,
  breaks=c(-Inf, -0.2, 0.2, Inf),
  labels=c("Unsustainable","Moderate","Sustainable")
)
sustain_formula <- sustain_level ~ co2_emissions + co2_emissions_sq + forested_area +
  threatened_species + region_central_america + region_central_asia + region_eastern_africa +
  region_eastern_asia + region_eastern_europe + region_melanesia +
  region_micronesia + region_middle_africa + region_northern_africa +
  region_northern_america + region_northern_europe + region_oceania +
  region_polynesia + region_south_america + region_south_eastern_asia +
  region_southern_africa + region_southern_asia + region_southern_europe +
  region_western_africa + region_western_asia + region_western_europe
rf_sustain <- randomForest(sustain_formula, data=engineered_data, ntree=200)
rf_sustain

# --- 5(d) Sustainable Tourism Practices (Binary) ---
# Identifies "eco_tourism_adopter" as those with above 0 in sustainability_index 
# and above-median tourist volumes, then fits a logistic model.
sustain_median <- median(engineered_data$sustainability_index, na.rm = TRUE)
tourists_sq_median <- median(engineered_data$tourists_sq, na.rm = TRUE)
engineered_data$eco_tourism_adopter <- ifelse(
  engineered_data$sustainability_index > 0 &
    engineered_data$tourists_sq > tourists_sq_median,
  1, 
  0
)
eco_formula <- eco_tourism_adopter ~ co2_emissions_sq + human_capital_index + 
  region_central_america + region_central_asia + region_eastern_africa +
  region_eastern_asia + region_eastern_europe + region_melanesia +
  region_micronesia + region_middle_africa + region_northern_africa +
  region_northern_america + region_northern_europe + region_oceania +
  region_polynesia + region_south_america + region_south_eastern_asia +
  region_southern_africa + region_southern_asia + region_southern_europe +
  region_western_africa + region_western_asia + region_western_europe +
  tourists_sq
eco_glm <- glm(eco_formula, data=engineered_data, family=binomial())
summary(eco_glm)

# End
# --------------------------------------------------------------------------------