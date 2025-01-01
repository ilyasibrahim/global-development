##############################################
# 08 MACHINE LEARNING - ECONOMIC
##############################################

# --- 8.1 GDP Growth Prediction (Random Forest)
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

# --- 8.2 Trade Balance Prediction (Linear Model)
engineered_data$trade_balance <- engineered_data$exports - engineered_data$imports
trade_balance_formula <- trade_balance ~ gdp + employment_services + employment_agriculture +
  human_capital_index + region_eastern_europe + region_northern_europe +
  region_southern_europe + region_western_europe + pop_density_sq

trade_lm <- lm(trade_balance_formula, data = engineered_data)
summary(trade_lm)

# --- 8.3 Development Tiers (K-means)
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
print(cluster_sizes)

# Map the development tiers
world_merged <- prepare_world_map_data(engineered_data, iso_col = "iso2")
world_merged$development_tier <- factor(
  world_merged$development_tier,
  levels = c("TierA", "TierB", "TierC")
)

tier_levels <- levels(world_merged$development_tier)
palette_colors <- viridis::viridis(
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
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.4, "cm"),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )
print(tier_map)

# --- 8.4 Economic Downturn (Binary)
threshold_gdp_growth <- 0
engineered_data$likely_downturn <- ifelse(engineered_data$gdp_growth <= threshold_gdp_growth, 1, 0)

downturn_formula <- likely_downturn ~ co2_emissions + co2_emissions_sq + trade_balance +
  region_eastern_africa + region_middle_africa + region_northern_africa +
  region_southern_africa + region_western_africa + employment_industry +
  secFemale_svcEmp

downturn_glm <- glm(downturn_formula, data = engineered_data, family = binomial())
summary(downturn_glm)

message("Economic ML scripts complete.")
