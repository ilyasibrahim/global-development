##############################################
# 09 MACHINE LEARNING - POPULATION
##############################################

# 9.1 Population Growth Prediction (OLS)
pop_growth_formula <- pop_growth ~ fertility + fertility_sq + fertility_cubed +
  urban_population + employment_services + region_central_asia + region_eastern_asia +
  region_south_eastern_asia + region_southern_asia
pop_growth_lm <- lm(pop_growth_formula, data = engineered_data)
summary(pop_growth_lm)

# 9.2 Fertility Rates Prediction (Random Forest)
set.seed(123)
fert_formula <- fertility ~ human_capital_index + gdp_per_capita +
  infant_mortality + region_eastern_africa + region_middle_africa + region_northern_africa +
  region_southern_africa + region_western_africa + employment_industry +
  fertility_SecFem
fert_rf_model <- randomForest(fert_formula, data = engineered_data, ntree = 200, importance = TRUE)
print(fert_rf_model)
importance(fert_rf_model)

# 9.3 Population Stability Classification
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
print(rf_pop_stab)

# 9.4 Migration Trends (Binary)
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
migration_glm <- glm(migration_formula, data=engineered_data, family=binomial())
summary(migration_glm)

message("Population ML scripts complete.")
