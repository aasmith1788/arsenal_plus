library(data.table)

# Read data as data.table for maximum speed
data <- fread("C:/Users/aasmi/p3_summer_2025/Pitch Modeling Aaron-David/Get Statcast/2020_24Training.csv")

# Fast at-bat identification
data[, has_event := !is.na(events) & events != ""]
data[, atbat_group := cumsum(has_event), by = .(pitcher, game_year)]

# Keep only pitches that belong to complete at-bats
data_clean <- data[atbat_group > 0]

# Get unique pitch types for binary columns
unique_pitch_types <- unique(data_clean$pitch_type[!is.na(data_clean$pitch_type)])

# Create binary pitch type columns
for(pt in unique_pitch_types) {
  data_clean[, paste0("has_", pt) := as.integer(pitch_type == pt)]
}

# Fast aggregation using data.table
result <- data_clean[, {
  
  # Get outcome and final count from the row with events
  final_pitch_row <- which(!is.na(events) & events != "")[1]
  outcome_val <- events[final_pitch_row]
  final_balls <- balls[final_pitch_row]
  final_strikes <- strikes[final_pitch_row]
  
  # Pitch count
  total_pitches <- .N
  
  # Binary columns - any pitch type used in at-bat
  binary_cols <- lapply(unique_pitch_types, function(pt) {
    as.integer(any(get(paste0("has_", pt)) == 1, na.rm = TRUE))
  })
  names(binary_cols) <- unique_pitch_types
  
  # Velocity averages per pitch type
  vel_cols <- lapply(unique_pitch_types, function(pt) {
    speeds <- release_speed[pitch_type == pt & !is.na(release_speed)]
    if(length(speeds) > 0) mean(speeds) else 0
  })
  names(vel_cols) <- paste0(unique_pitch_types, "_avg_release_speed")
  
  # Movement averages per pitch type (pfx_x)
  pfx_x_cols <- lapply(unique_pitch_types, function(pt) {
    vals <- pfx_x[pitch_type == pt & !is.na(pfx_x)]
    if(length(vals) > 0) mean(vals) else 0
  })
  names(pfx_x_cols) <- paste0(unique_pitch_types, "_avg_pfx_x")
  
  # Movement averages per pitch type (pfx_z)
  pfx_z_cols <- lapply(unique_pitch_types, function(pt) {
    vals <- pfx_z[pitch_type == pt & !is.na(pfx_z)]
    if(length(vals) > 0) mean(vals) else 0
  })
  names(pfx_z_cols) <- paste0(unique_pitch_types, "_avg_pfx_z")
  
  # vx0 averages per pitch type
  vx0_cols <- lapply(unique_pitch_types, function(pt) {
    vals <- vx0[pitch_type == pt & !is.na(vx0)]
    if(length(vals) > 0) mean(vals) else 0
  })
  names(vx0_cols) <- paste0(unique_pitch_types, "_avg_vx0")
  
  # vy0 averages per pitch type
  vy0_cols <- lapply(unique_pitch_types, function(pt) {
    vals <- vy0[pitch_type == pt & !is.na(vy0)]
    if(length(vals) > 0) mean(vals) else 0
  })
  names(vy0_cols) <- paste0(unique_pitch_types, "_avg_vy0")
  
  # vz0 averages per pitch type
  vz0_cols <- lapply(unique_pitch_types, function(pt) {
    vals <- vz0[pitch_type == pt & !is.na(vz0)]
    if(length(vals) > 0) mean(vals) else 0
  })
  names(vz0_cols) <- paste0(unique_pitch_types, "_avg_vz0")
  
  # ax averages per pitch type
  ax_cols <- lapply(unique_pitch_types, function(pt) {
    vals <- ax[pitch_type == pt & !is.na(ax)]
    if(length(vals) > 0) mean(vals) else 0
  })
  names(ax_cols) <- paste0(unique_pitch_types, "_avg_ax")
  
  # ay averages per pitch type
  ay_cols <- lapply(unique_pitch_types, function(pt) {
    vals <- ay[pitch_type == pt & !is.na(ay)]
    if(length(vals) > 0) mean(vals) else 0
  })
  names(ay_cols) <- paste0(unique_pitch_types, "_avg_ay")
  
  # az averages per pitch type
  az_cols <- lapply(unique_pitch_types, function(pt) {
    vals <- az[pitch_type == pt & !is.na(az)]
    if(length(vals) > 0) mean(vals) else 0
  })
  names(az_cols) <- paste0(unique_pitch_types, "_avg_az")
  
  # General averages
  general_avgs <- list(
    avg_release_pos_x = mean(release_pos_x, na.rm = TRUE),
    avg_release_pos_y = mean(release_pos_y, na.rm = TRUE), 
    avg_release_pos_z = mean(release_pos_z, na.rm = TRUE),
    avg_release_extension = mean(release_extension, na.rm = TRUE)
  )
  
  # Combine all results
  c(list(outcome = outcome_val,
         pitch_count = total_pitches,
         final_balls = final_balls,
         final_strikes = final_strikes),
    binary_cols,
    vel_cols,
    pfx_x_cols,
    pfx_z_cols,
    vx0_cols,
    vy0_cols,
    vz0_cols,
    ax_cols,
    ay_cols,
    az_cols,
    general_avgs)
  
}, by = .(pitcher_id = pitcher, player_name, year = game_year, at_bat = atbat_group)]

# Clean up - remove helper columns and reorder
setcolorder(result, c("pitcher_id", "player_name", "year", "at_bat", "outcome", "pitch_count", "final_balls", "final_strikes"))

# Function to calculate convex hull area (arsenal area)
calculate_arsenal_area <- function(hor_values, vert_values) {
  # Remove NA values and zeros
  valid_indices <- !is.na(hor_values) & !is.na(vert_values) & hor_values != 0 & vert_values != 0
  hor_clean <- hor_values[valid_indices]
  vert_clean <- vert_values[valid_indices]
  
  # Need at least 3 points to form an area
  if (length(hor_clean) < 3) {
    return(0)
  }
  
  # Create points matrix
  points <- cbind(hor_clean, vert_clean)
  
  # Calculate convex hull
  hull_indices <- chull(points)
  hull_points <- points[hull_indices, ]
  
  # Calculate area using shoelace formula
  n <- nrow(hull_points)
  area <- 0
  for (i in 1:n) {
    j <- ifelse(i == n, 1, i + 1)
    area <- area + (hull_points[i, 1] * hull_points[j, 2] - hull_points[j, 1] * hull_points[i, 2])
  }
  area <- abs(area) / 2
  
  return(area)
}

# Get column names outside the data.table operation
pfx_x_cols <- names(result)[grepl("_avg_pfx_x$", names(result))]
pfx_z_cols <- names(result)[grepl("_avg_pfx_z$", names(result))]

# Add arsenal area calculation
result$Arsenal_Area <- apply(result, 1, function(row) {
  # Extract horizontal and vertical break values and convert to inches
  hor_values <- as.numeric(row[pfx_x_cols]) * 12
  vert_values <- as.numeric(row[pfx_z_cols]) * 12
  
  # Calculate arsenal area
  calculate_arsenal_area(hor_values, vert_values)
})
