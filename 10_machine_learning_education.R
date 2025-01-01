##############################################
# 10 MACHINE LEARNING - EDUCATION & EMPLOYMENT
##############################################

# 10.1 Predicting Educational Attainment (OLS)
edu_formula <- human_capital_index ~ gdp_per_capita + fertility + fertility_sq +
  co2_emissions + region_eastern_africa + region_middle_africa + region_northern_africa +
  region_southern_africa + region_western_africa + pop_density_sq
edu_lm <- lm(edu_formula, data=engineered_data)
summary(edu_lm)

# 10.2 Predicting Employment Distribution (Random Forest)
set.seed(123)
agri_formula <- employment_agriculture ~ gdp + population + secondary_school_enrollment_female +
  region_central_asia + region_eastern_asia + region_south_eastern_asia + region_southern_asia +
  fertility_sq
agri_rf_model <- randomForest(agri_formula, data=engineered_data, ntree=250)
print(agri_rf_model)
print(importance(agri_rf_model))

# 10.3 Classifying Education Levels
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
print(rf_edu_level)

# 10.4 Economic Resilience (Binary)
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

message("Education & employment ML scripts complete.")
