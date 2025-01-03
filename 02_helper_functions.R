##############################################
# 02 HELPER FUNCTIONS
##############################################

# Quick data loading via data.table's fread()
load_data <- function(path) {
  if (!file.exists(path)) {
    stop("Error: Data file '", path, "' not found.")
  }
  tryCatch({
    data <- fread(path, na.strings = c("", "NA", "NaN"))
    message("Data loaded successfully with ", nrow(data), " rows and ", ncol(data), " columns.")
    return(as.data.frame(data))
  }, error = function(e) {
    stop("Error reading the data file: ", e)
  })
}

# Abbreviates long indicator names (especially for plotting)
abbreviate_indicator <- function(indicator_names) {
  sapply(strsplit(as.character(indicator_names), "_"), function(words) {
    if (tolower(words[1]) == "secondary") {
      words <- words[-2]
    }
    paste(c(words[1], substring(words[-1], 1, 1)), collapse = "_")
  })
}

# Min-max scaling
min_max_scaling <- function(x) {
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) return(rep(0.5, length(x)))
  (x - min(x, na.rm = TRUE)) / rng
}

# Z-score normalization
z_score_normalization <- function(x) {
  sd_val <- sd(x, na.rm = TRUE)
  if (sd_val == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / sd_val
}

# Safe log transform + z-score
safe_log_z_transform <- function(x) {
  non_na_x <- x[!is.na(x)]
  if (length(non_na_x) == 0) {
    return(x)
  }
  min_val <- min(non_na_x)
  if (min_val <= 0) {
    shift_amount <- abs(min_val) + 0.1
    x <- x + shift_amount
  }
  z_score_normalization(log(x))
}

# Prepare world map for plotting in ggplot
# Copy paste all the files in the WB_countries_Admin0_10m folder into your project directory
prepare_world_map_data <- function(df, iso_col = "iso2", shapefile = "WB_countries_Admin0_10m.shp") {
  # 1) Read the shapefile
  world_map <- st_read(shapefile)
  
  # 2) Rename the ISO column if needed (assumes 'ISO_A2' exists in shapefile)
  #    Then convert to uppercase for consistent merging with your data.
  world_map <- world_map %>%
    rename(iso_a2 = ISO_A2) %>%
    mutate(iso_a2 = toupper(iso_a2))
  
  # 3) Convert the user data’s ISO column to uppercase as well
  df[[iso_col]] <- toupper(df[[iso_col]])
  
  # 4) Merge your data onto the shapefile by matching iso_a2 -> iso2
  world_merged <- world_map %>%
    left_join(df, by = c("iso_a2" = iso_col))
  
  return(world_merged)
}

message("Helper functions loaded.")
