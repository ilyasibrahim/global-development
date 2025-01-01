##############################################
# 04 DATA CLEANING (IMPUTATION)
##############################################

# 4.1 FILTER / INITIAL CLEANUP
filtered_data <- country_data %>%
  filter(is.na(iso2) | iso2 != "VA")

# Convert region to numeric for KNN
filtered_data <- filtered_data %>%
  mutate(region_numeric = as.numeric(factor(region)))

selected_indicators <- unique(unlist(indicators))
selected_indicators_with_ids <- c("iso2", "name", "region", selected_indicators)

# 4.2 IMPUTATION STRATEGIES

# --- 4.2.1 Median Imputation
vars_mcar_median <- c("gdp","gdp_growth","gdp_per_capita","life_expectancy_male",
                      "life_expectancy_female","fertility","forested_area")
for(v in vars_mcar_median) {
  if(v %in% names(filtered_data)){
    filtered_data[[v]][is.na(filtered_data[[v]])] <- median(filtered_data[[v]], na.rm=TRUE)
  }
}

# Primary school enrollments -> Median
for (v in c("primary_school_enrollment_female","primary_school_enrollment_male")) {
  if(v %in% names(filtered_data)){
    filtered_data[[v]][is.na(filtered_data[[v]])] <- median(filtered_data[[v]], na.rm=TRUE)
  }
}

# --- 4.2.2 KNN Imputation for selected variables
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

# Post-secondary enrollments
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

# Imports/Exports
knn_data_trade <- filtered_data %>%
  select(imports, exports, gdp, population, region_numeric) %>%
  as.data.frame()
imputed_knn_trade <- knnImputation(knn_data_trade, k = 5)

filtered_data <- filtered_data %>%
  mutate(
    imports = ifelse(is.na(imports), imputed_knn_trade$imports, imports),
    exports = ifelse(is.na(exports), imputed_knn_trade$exports, exports)
  )

# Homicide rate
knn_data_homicide <- filtered_data %>%
  select(homicide_rate, gdp_per_capita, population, 
         urban_population, region_numeric) %>%
  as.data.frame()
imputed_knn_homicide <- knnImputation(knn_data_homicide, k = 5)
filtered_data$homicide_rate[is.na(filtered_data$homicide_rate)] <-
  imputed_knn_homicide$homicide_rate

# Tourists
knn_data_tourism <- filtered_data %>%
  select(tourists, population, gdp_per_capita, region_numeric) %>%
  as.data.frame()
imputed_knn_tourism <- knnImputation(knn_data_tourism, k = 5)
filtered_data$tourists[is.na(filtered_data$tourists)] <- imputed_knn_tourism$tourists

# --- 4.2.3 MICE Imputation for CO2 emissions
mice_data_co2 <- filtered_data %>%
  select(co2_emissions, gdp, imports, population, urban_population, region_numeric) %>%
  as.data.frame()
imputed_co2 <- mice(mice_data_co2, method = 'pmm', m = 5, maxit = 10)
mice_completed_co2 <- complete(imputed_co2, 1)
filtered_data$co2_emissions[is.na(filtered_data$co2_emissions)] <-
  mice_completed_co2$co2_emissions

# Employment indicators -> KNN
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

# Infant mortality -> median
if("infant_mortality" %in% names(filtered_data)){
  filtered_data$infant_mortality[is.na(filtered_data$infant_mortality)] <-
    median(filtered_data$infant_mortality, na.rm = TRUE)
}

# 4.3 FINAL CLEANED DATA
cleaned_data <- filtered_data %>%
  select(all_of(selected_indicators_with_ids), region_numeric)

cat("Original Dimensions:", dim(country_data), "\n")
cat("Filtered Dimensions (after removing Vatican):", dim(filtered_data), "\n")
cat("Final Cleaned Dimensions (Selected Indicators):", dim(cleaned_data), "\n")

# OPTIONAL: Check for any remaining NAs
# missing_values_summary <- cleaned_data %>%
#   summarise(across(everything(), ~ sum(is.na(.)))) %>%
#   pivot_longer(cols = everything(), names_to = "Indicator", values_to = "Missing_Values")
# print(missing_values_summary, n = Inf)

# OPTIONAL: write.csv(cleaned_data, "cleaned_country_data_FINAL.csv", row.names = FALSE)
message("Data cleaning and imputation complete.")
