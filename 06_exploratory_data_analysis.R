##############################################
# 06 EXPLORATORY DATA ANALYSIS
##############################################

# 6.1 CHECK DISTRIBUTIONS / SKEWNESS
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

# 6.2 CORRELATION MATRIX & HEATMAP
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

print(corr_heatmap_plot)
ggsave("correlation_matrix_heatmap.png", corr_heatmap_plot, width = 16, height = 16, dpi = 300, bg = "white")

# 6.3 SCATTER PLOTS
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
  label_size = 10
)

print(combined_scatter_plots)

# 6.4 GEOSPATIAL PLOTS - FERTILITY & EDUCATION
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
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.3, "cm")
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
  labs(title = "Educational Attainment") +
  custom_map_theme

edu_fer_maps <- cowplot::plot_grid(
  education_map,
  fertility_map,
  nrow = 2,
  align = "h"
)
print(edu_fer_maps)

# Disparity map
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

combined_edu_fer_disparity <- (education_map | fertility_map) / disparity_edu_fer_map +
  plot_layout(heights = c(1, 1.5))
print(combined_edu_fer_disparity)

# 6.5 SIMILAR PLOTS FOR INFANT MORTALITY & EDUCATION, CO2 & Forest, etc.
# (Code is in the original script, omitted here for brevity or reuse as needed.)

message("Exploratory data analysis complete.")
