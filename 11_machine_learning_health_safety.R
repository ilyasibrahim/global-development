##############################################
# 11 MACHINE LEARNING - HEALTH & SAFETY
##############################################

# 11.1 Predicting Life Expectancy (Random Forest)
set.seed(123)
lifeexp_formula <- life_expectancy_female ~ human_capital_index + gdp_per_capita + 
  region_central_america + region_northern_america + region_south_america +
  fertility_sq + pop_density_sq
lifeexp_rf_model <- randomForest(lifeexp_formula, data=engineered_data, ntree=300, importance=TRUE)
print(lifeexp_rf_model)
importance(lifeexp_rf_model)

# 11.2 Predicting Infant Mortality (OLS)
infant_formula <- infant_mortality ~ secondary_school_enrollment_female +
  gdp_per_capita + co2_emissions_sq + fertility_sq
infant_lm <- lm(infant_formula, data=engineered_data)
summary(infant_lm)

# 11.3 Categorizing Health Status
set.seed(123)
engineered_data$health_score <- engineered_data$life_expectancy_female - engineered_data$infant_mortality
engineered_data$health_status <- cut(
  engineered_data$health_score,
  breaks=c(-Inf, 0, 20, Inf),
  labels=c("Unhealthy","Moderate","Healthy")
)

# Combine 'Healthy' and 'Moderate'
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
print(rf_health_status)

# 11.4 Public Health Crises (Binary)
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

message("Health & safety ML scripts complete.")
