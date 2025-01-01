##############################################
# 12 MACHINE LEARNING - ENVIRONMENT & TOURISM
##############################################

# 12.1 Predicting CO2 Emissions (Random Forest)
set.seed(123)
co2_formula <- co2_emissions ~ gdp + trade_balance + population + region_central_asia +
  region_eastern_asia + region_south_eastern_asia + region_southern_asia +
  region_eastern_europe + region_northern_europe + region_southern_europe +
  region_western_europe + co2_emissions_sq
co2_rf_model <- randomForest(co2_formula, data=engineered_data, ntree=300, importance=TRUE)
print(co2_rf_model)
importance(co2_rf_model)

# 12.2 Predicting Tourist Numbers (OLS)
tourism_formula <- tourists ~ sustainability_index + gdp_per_capita +
  co2_emissions_sq + region_eastern_europe + region_northern_europe +
  region_southern_europe + region_western_europe + region_central_america +
  region_northern_america + region_south_america
tourism_lm <- lm(tourism_formula, data=engineered_data)
summary(tourism_lm)

# 12.3 Environmental Sustainability Classification
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
print(rf_sustain)

# 12.4 Sustainable Tourism Practices (Binary)
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

message("Environment & tourism ML scripts complete.")
