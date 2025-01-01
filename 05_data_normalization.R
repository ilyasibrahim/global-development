##############################################
# 05 DATA NORMALIZATION
##############################################

# Ensure 'indicators' and 'cleaned_data' exist
if (!exists("indicators")) stop("The 'indicators' list is not defined.")
if (!exists("cleaned_data")) stop("The 'cleaned_data' object is not defined.")

# Check which indicators actually exist in the data
selected_indicators <- unique(unlist(indicators)) %>%
  intersect(names(cleaned_data))
missing_indicators <- setdiff(unique(unlist(indicators)), selected_indicators)
if (length(missing_indicators) > 0) {
  warning("The following indicators are missing: ", 
          paste(missing_indicators, collapse = ", "))
}

# 5.1 DEFINE VARIABLE GROUPS FOR TRANSFORMATION
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

# 5.2 APPLY TRANSFORMATIONS
normalized_data <- cleaned_data

for (v in log_z_vars) {
  if (v %in% names(normalized_data)) {
    normalized_data[[v]] <- safe_log_z_transform(normalized_data[[v]])
  }
}

for (v in min_max_vars) {
  if (v %in% names(normalized_data)) {
    normalized_data[[v]] <- min_max_scaling(normalized_data[[v]])
  }
}

for (v in z_vars) {
  if (v %in% names(normalized_data)) {
    normalized_data[[v]] <- z_score_normalization(normalized_data[[v]])
  }
}

final_normalized_data <- normalized_data %>%
  select(all_of(selected_indicators_with_ids), region_numeric)

write.csv(final_normalized_data, "normalized_country_data_FINAL.csv", row.names = FALSE)
message("Data normalization complete. 'normalized_country_data_FINAL.csv' saved.")
