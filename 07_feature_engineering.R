##############################################
# 07 FEATURE ENGINEERING
##############################################

# Assume 'final_normalized_data' is loaded in environment
# Create a working copy
engineered_data <- final_normalized_data

# 7.1 Interaction Terms
engineered_data$secFemale_svcEmp <- engineered_data$secondary_school_enrollment_female *
  engineered_data$employment_services

engineered_data$gdp_x_exports <- engineered_data$gdp * engineered_data$exports
engineered_data$tourism_co2 <- engineered_data$tourists * engineered_data$co2_emissions
engineered_data$gdpPerCap_co2 <- engineered_data$gdp_per_capita * engineered_data$co2_emissions
engineered_data$fertility_SecFem <- engineered_data$fertility *
  engineered_data$secondary_school_enrollment_female

# 7.2 Polynomial Features
engineered_data$fertility_sq <- engineered_data$fertility^2
engineered_data$fertility_cubed <- engineered_data$fertility^3
engineered_data$gdp_growth_sq <- engineered_data$gdp_growth^2
engineered_data$pop_density_sq <- engineered_data$pop_density^2
engineered_data$co2_emissions_sq <- engineered_data$co2_emissions^2
engineered_data$tourists_sq <- engineered_data$tourists^2

# 7.3 Aggregated Indicators
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

# 7.4 Categorical Encoding
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

# 7.5 Minimal Temporal Features
engineered_data$annual_gdp_growth_rate <- engineered_data$gdp_growth
engineered_data$annual_pop_growth_rate <- engineered_data$pop_growth

engineered_data$popGrowth_fertility <- engineered_data$annual_pop_growth_rate *
  engineered_data$fertility

engineered_data$gdpGrowth_exports <- engineered_data$annual_gdp_growth_rate *
  engineered_data$exports

# OPTIONAL: write.csv(engineered_data, "engineering_data_FINAL.csv", row.names = FALSE)
message("Feature engineering complete.")
