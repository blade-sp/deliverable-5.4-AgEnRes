rm(list = ls())

##############################################################################
# Crop Simulation Data
##############################################################################

# Balkovic, J. (2025). LAMASUS - Simulated Crop Yields and Fertilizer 
# Application Rates for Nine Crops under Six Management Intensity Scenarios 
# in the EU [Data set]. Zenodo. https://doi.org/10.5281/zenodo.17119992

# Crops
# ----------------------
# CORN: corn
# POTA: potatoes 
# RAPE: winter rapeseed
# SBAR: spring barley
# SGBT: sugarbeet
# SOYB: soybeans
# SUNF: sunflower seeds
# WRYE: winter rye
# WWHT: winter wheat
# 
# Land
# ----------------------
# CRP: arable land
# 
# Variables
# ----------------------
# YLD: yield at common moisture (t/ha)
# FTN: nitrogen application rates (kgN/ha)
# 
# Intensity scenario
# ----------------------
# M1.rf: very-low intensity, rainfed
# M2.rf: low intensity, rainfed
# M3.rf: medium intensity, rainfed
# M4.rf: high intensity, rainfed
# M5.rf: very-high intensity, rainhfed
# M5.ir: very-high intensity, irrigated
# 
# Note:
# These intensity scenario simulations were used to calculate the yield response 
# surfaces in LAMASUS. Please note that the N fertilization gradient (FTN) 
# driving the yield responses varies across scenarios and differs between 
# countries. Crop yield (YLD) represents a long-term mean calculated for the 
# reference period 2001-2020.

# Load package
library(terra)
library(sf)
library(tidyverse)
library(tidyterra)
library(geodata)
library(stargazer)
library(robustbase)


# 1. Read the NetCDF file
nitrogen_file <- "1_Data/RawData/EPIC_LAMASUS_CropSimulationData/EPIC_LAMASUS_FTN_CRP_WWHT_v1.nc" 
yield_file <- "1_Data/RawData/EPIC_LAMASUS_CropSimulationData/EPIC_LAMASUS_YLD_CRP_WWHT_v1.nc"

r_nitrogen <- rast(nitrogen_file) # nitrogen application rates (kgN/ha)
r_yield <- rast(yield_file) # yield at common moisture (t/ha)

# 2. Inspect the object
r_nitrogen  
r_yield              # Shows number of layers, extent, resolution, CRS

names(r_nitrogen)
names(r_yield)       # List variable names (layers)
       
nlyr(r_nitrogen)        
nlyr(r_yield)        # Number of layers

summary(r_nitrogen)
summary(r_yield)     # Basic statistics

# 4. Plot  
plot(r_nitrogen)
plot(r_yield)


# Plot Germany only 
germany <- gadm(country = "DEU", level = 0, path = tempdir())
germany_sf <- st_as_sf(germany)

raster_germany <- crop(r_yield, germany)

ggplot() +
  geom_spatraster(data = raster_germany) +
  geom_sf(data = germany, fill = NA, color = "black", size = 0.5) +
  facet_wrap(~lyr, ncol = 3) + 
  scale_fill_viridis_c() +
  labs(fill = "Yield") +
  theme_minimal()


# Plot North Rhine-Westphalia only
germany_states <- gadm(country = "DEU", level = 1, path = tempdir())
nrw <- germany_states[germany_states$NAME_1 == "Nordrhein-Westfalen", ]
nrw <- st_as_sf(nrw)

raster_nrw <- crop(raster_germany, nrw)

ggplot() +
  geom_spatraster(data = raster_nrw) +
  geom_sf(data = nrw, fill = NA, color = "black", size = 0.5) +
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_viridis_c(na.value = "transparent") +
  labs(title = "Raster Layers for North Rhine-Westphalia",
       fill = "Value") +
  theme_minimal()


###############################################################################
# Extract values for Koln-Bucth area 
###############################################################################

# Define Koln-Bucht area 
vertex1 <- c(51.0206, 5.8339)
vertex2 <- c(51.2327, 6.5022)
vertex3 <- c(50.6223, 7.0810)

# Create triangle polygon for visualization
triangle_df <- data.frame(
  lat = c(vertex1[1], vertex2[1], vertex3[1], vertex1[1]),
  lon = c(vertex1[2], vertex2[2], vertex3[2], vertex1[2])
)

# Plot all layers for North Rhine-Westphalia with triangle
ggplot() +
  geom_spatraster(data = raster_nrw) +
  geom_sf(data = nrw, fill = NA, color = "black", size = 0.5) +
  geom_polygon(data = triangle_df, aes(x = lon, y = lat), 
               fill = NA, color = "red", size = 1) +
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_viridis_c(na.value = "transparent") +
  labs(title = "Cologne Lowland Area",
       fill = "Value") +
  theme_minimal()

### get representative coordinates
lat_center <- (vertex1[1] + vertex2[1] + vertex3[1]) / 3
lon_center <- (vertex1[2] + vertex2[2] + vertex3[2]) / 3

coord <- data.frame(lat = lat_center, lon = lon_center) 

ggplot() +
  geom_spatraster(data = raster_nrw) +
  geom_sf(data = nrw, fill = NA, color = "black", size = 0.5) +
  geom_polygon(data = triangle_df, aes(x = lon, y = lat), 
               fill = NA, color = "red", size = 1) +
  geom_point(data = coord, aes(x = lon, y = lat), 
             color = "red", size = 4, shape = 0) +  
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_viridis_c(na.value = "transparent") +
  labs(title = "North Rhine-Westphalia",
       fill = "Value") +
  theme_minimal() 

ggplot() +
  geom_spatraster(data = raster_nrw) +
  geom_sf(data = nrw, fill = NA, color = "black", size = 0.5) +
  geom_polygon(data = triangle_df, aes(x = lon, y = lat), 
               fill = NA, color = "red", size = 1) +
  geom_point(data = coord, aes(x = lon, y = lat), 
             color = "red", size = 4, shape = 0) +  
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_viridis_c(na.value = "transparent") +
  labs(title = "Cologne Lowland Area",
       fill = "Value") +
  theme_minimal() +
  coord_sf(
    xlim = range(triangle_df$lon) + c(-0.01, 0.01),
    ylim = range(triangle_df$lat) + c(-0.01, 0.01),
    expand = FALSE
  )

# Convert to sf object
coord_sf <- st_as_sf(coord, 
                     coords = c("lon", "lat"),
                     crs = 4326)  # WGS84 coordinate system

# Extract values for the coordinate
nitrogen_data <- terra::extract(r_nitrogen, coord_sf)
yield_data <- terra::extract(r_yield, coord_sf)

# exclude the irrigated layer
nitrogen_data <- nitrogen_data %>% select(-FTN_M5_ir)
yield_data <- yield_data %>% select(-YLD_M5_ir)

# convert and export data
nitrogen_data_long <- nitrogen_data %>% 
  pivot_longer(
    cols = -ID,
    names_to = "Layer",
    values_to = "N_application"
  ) %>% 
  mutate(Layer = substring(Layer, 5))

yield_data_long <- yield_data %>% 
  pivot_longer(
    cols = -ID,
    names_to = "Layer",
    values_to = "Simulated_Yield"
  ) %>% 
  mutate(Layer = substring(Layer, 5))


df_merged <- merge(nitrogen_data_long, yield_data_long, by = c("ID", "Layer"))
head(df_merged)

reg1_yield_function <- lmrob(Simulated_Yield ~ I(sqrt(N_application)) + N_application, 
                              data = df_merged, method = "M")
summary(reg1_yield_function)

# append residuals to dataframe
df_merged <- df_merged %>% 
  mutate(residuals = reg1_yield_function$residuals,
         abs_residuals = abs(residuals))

reg1_variation_function <- lmrob(abs_residuals ~ I(sqrt(N_application)), 
                                  data = df_merged, method = "M")
summary(reg1_variation_function)

###############################################################################
# Extract multiple points for Koln-Bucth area 
###############################################################################

# Function to get all grid points inside a triangle
grid_points_in_triangle <- function(v1, v2, v3, resolution = 0.0833333) {
  # v1, v2, v3 are the vertices of the triangle (latitude, longitude)
  # resolution is the grid resolution
  
  # Find bounding box of the triangle
  lat_min <- min(v1[1], v2[1], v3[1])
  lat_max <- max(v1[1], v2[1], v3[1])
  lon_min <- min(v1[2], v2[2], v3[2])
  lon_max <- max(v1[2], v2[2], v3[2])
  
  # Snap bounding box to grid
  lat_min <- floor(lat_min / resolution) * resolution
  lat_max <- ceiling(lat_max / resolution) * resolution
  lon_min <- floor(lon_min / resolution) * resolution
  lon_max <- ceiling(lon_max / resolution) * resolution
  
  # Generate all grid points in bounding box
  lat_seq <- seq(lat_min + resolution/2, lat_max - resolution/2, by = resolution)
  lon_seq <- seq(lon_min + resolution/2, lon_max - resolution/2, by = resolution)
  
  # Create all combinations
  grid_points <- expand.grid(lat = lat_seq, lon = lon_seq)
  
  # Function to check if a point is inside the triangle using barycentric coordinates
  is_inside_triangle <- function(p, v1, v2, v3) {
    # Calculate barycentric coordinates
    denom <- ((v2[2] - v3[2]) * (v1[1] - v3[1]) + (v3[1] - v2[1]) * (v1[2] - v3[2]))
    w1 <- ((v2[2] - v3[2]) * (p[1] - v3[1]) + (v3[1] - v2[1]) * (p[2] - v3[2])) / denom
    w2 <- ((v3[2] - v1[2]) * (p[1] - v3[1]) + (v1[1] - v3[1]) * (p[2] - v3[2])) / denom
    w3 <- 1 - w1 - w2
    
    # Point is inside if all weights are non-negative
    return(w1 >= 0 & w2 >= 0 & w3 >= 0)
  }
  
  # Filter points inside the triangle
  inside <- apply(grid_points, 1, function(p) {
    is_inside_triangle(p, v1, v2, v3)
  })
  
  grid_points <- grid_points[inside, ]
  
  return(grid_points)
}

# Get all grid points inside the triangle
grid_coords <- grid_points_in_triangle(vertex1, vertex2, vertex3, resolution = 0.0833333)

ggplot() +
  geom_spatraster(data = raster_nrw) +
  geom_sf(data = nrw, fill = NA, color = "black", size = 0.5) +
  geom_polygon(data = triangle_df, aes(x = lon, y = lat), 
               fill = NA, color = "red", size = 1) +
  geom_point(data = grid_coords, aes(x = lon, y = lat), 
             color = "red", size = 1, shape = 16) +  
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_viridis_c(na.value = "transparent") +
  labs(title = "Cologne Lowland Area",
       fill = "Value") +
  theme_minimal() +
  coord_sf(
    xlim = range(triangle_df$lon) + c(-0.01, 0.01),
    ylim = range(triangle_df$lat) + c(-0.01, 0.01),
    expand = FALSE
  )



# Convert to sf object
grid_coords_sf <- st_as_sf(grid_coords, 
                             coords = c("lon", "lat"),
                             crs = 4326)  # WGS84 coordinate system


nitrogen_data_mp <- terra::extract(r_nitrogen, grid_coords_sf)
yield_data_mp <- terra::extract(r_yield, grid_coords_sf)

# exclude the irrigated layer
nitrogen_data_mp <- nitrogen_data_mp %>% select(-FTN_M5_ir) 
yield_data_mp <- yield_data_mp %>% select(-YLD_M5_ir)

# convert and export data
nitrogen_data_mp_long <- nitrogen_data_mp %>% 
  pivot_longer(
    cols = -ID,
    names_to = "Layer",
    values_to = "N_application"
  ) %>% 
  mutate(Layer = substring(Layer, 5))

yield_data_mp_long <- yield_data_mp %>% 
  pivot_longer(
    cols = -ID,
    names_to = "Layer",
    values_to = "Simulated_Yield"
  ) %>% 
  mutate(Layer = substring(Layer, 5))


df_merged_mp <- merge(nitrogen_data_mp_long, yield_data_mp_long, by = c("ID", "Layer"))
head(df_merged_mp)

reg2_yield_function <- lmrob(Simulated_Yield ~ I(sqrt(N_application)) + N_application, 
                              data = df_merged_mp, method = "M")
summary(reg2_yield_function)

# append residuals to dataframe
df_merged_mp <- df_merged_mp %>% 
  mutate(residuals = reg2_yield_function$residuals,
         abs_residuals = abs(residuals))

reg2_variation_function <- lmrob(abs_residuals ~ I(sqrt(N_application)), 
                                  data = df_merged_mp, method = "M")
summary(reg2_variation_function)



stargazer(reg1_yield_function, 
          reg2_yield_function, 
          type = "text",  # or "html", "latex"
          title = "Comparison of Yield Functions Estimations",
          column.labels = c("Single point", "Multiple points"),
          dep.var.labels = "Simulated Yield",
          covariate.labels = c("sqrt(N)", "N"),  
          digits = 3)

stargazer(reg1_variation_function, 
          reg2_variation_function, 
          type = "text",  # or "html", "latex"
          title = "Comparison of Variance Functions Estimations",
          column.labels = c("Single point", "Multiple points"),
          dep.var.labels = "Yield Variance",
          covariate.labels = "sqrt(N)",  
          digits = 3)


ggplot(df_merged_mp, aes(x = N_application, y = Simulated_Yield)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", 
              formula = y ~ I(sqrt(x)) + x, 
              color = "blue", se = FALSE) +
  labs(title = "Yield Response Function (Multiple Points)",
       x = "Nitrogen Application (kgN/ha)",
       y = "Simulated Yield (t/ha)") +
  theme_minimal()

ggplot(df_merged_mp, aes(x = N_application, y = abs_residuals)) + 
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", 
              formula = y ~ I(sqrt(x)), 
              color = "blue", se = FALSE) +
  labs(title = "Yield Variation Function (Multiple Points)",
       x = "Nitrogen Application (kgN/ha)",
       y = "Absolute Residuals") +
  theme_minimal()

