##############################################
# 03 DATA COLLECTION & INITIAL REVIEW
##############################################

# 3.1 IMPORT DATA
file_path <- "country_data.csv"  # Adjust path if needed
country_data <- load_data(file_path)

# 3.2 OPTIONAL: Quick data preview
# head(country_data)

# 3.3 SUMMARY OF THE DATASET
skim(country_data)

# 3.4 CATEGORIZE INDICATORS
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

# Remove missing indicators from the categories
missing_indicators <- setdiff(unlist(indicators), names(country_data))
if(length(missing_indicators) > 0){
  warning("These indicators are missing in the dataset and will be excluded: ",
          paste(missing_indicators, collapse = ", "))
  indicators <- lapply(indicators, setdiff, missing_indicators)
}

# 3.5 MISSING DATA ANALYSIS
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

# 3.6 OUTLIER VISUALIZATION
required_vars <- c("gdp", "gdp_growth", "population", "pop_growth")
missing_vars <- setdiff(required_vars, names(country_data))
if(length(missing_vars) > 0){
  stop("Required variables for outlier plots missing: ", 
       paste(missing_vars, collapse = ", "))
}

plot_data <- country_data %>%
  drop_na(all_of(required_vars)) %>%
  mutate(
    gdp_trillion = gdp / 1e6,         
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

print(combined_plot)

# ggsave("outlier_boxplots.png", combined_plot, width = 16, height = 12, dpi = 300)
message("Data collection and initial review script complete.")
