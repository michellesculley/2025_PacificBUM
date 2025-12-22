## plot Pacific map centered on Hawaii
library(ggplot2)
library(sf)
library(rnaturalearth)  # likely will be asked to install rnaturalearthdata as well. Also had to install terra separately 
library(dplyr)

## Have to make sure you have sourced BUM_dataexploration.r first.

## convert your data into the correct projection for plotting

## here is some example data. You can replace grid_data with your own lat/lon data
# Create 5x5-degree grid within the Pacific region from 120°E to 120°W and latitudes from -10° to 50°
set.seed(42)



## extract the map of countries
worldMap <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_make_valid()

target_crs <- st_crs("+proj=eqc +x_0=0 +y_0=0 +lat_0=0 +lon_0=133")

# define a long & slim polygon that overlaps the meridian line & set its CRS to match
# that of world. This is used to crop the map data for the Pacific Ocean only.
# Centered in lon 133

offset <- 180 - 133


polygon <- st_polygon(x = list(rbind(
    c(-0.0001 - offset, 90),
    c(0 - offset, 90),
    c(0 - offset, -90),
    c(-0.0001 - offset, -90),
    c(-0.0001 - offset, 90)
))) %>%
    st_sfc() %>%
    st_set_crs(4326)


# modify world dataset to remove overlapping portions with world's polygons
world2 <- worldMap %>% st_difference(polygon)


# Transform
world3 <- world2 %>% st_transform(crs = target_crs)
world4 <- st_crop(
    x = world3,
    y = st_as_sfc(
        st_bbox(c(xmin = -100, ymin = -10, xmax = 120, ymax = 50), crs = 4326)
    ) %>% st_transform(target_crs)
)


## create lines for lat/lon every 5degrees, you can change this is you want finer/coarser tick marks
lons <- seq(120, 280, by = 5) # Every 5 degrees longitude
lats <- seq(-10, 50, by = 5) # Every 5 degrees latitude

# 2. Generate the graticule lines as an sf object
graticule <- st_graticule(
    # Set breaks to your desired 5-degree intervals
    lon = lons,
    lat = lats,
    # Define the bounding box of your data (optional, but good practice)
    ndiscr = 1000 # Increase density of line segments for better transformation
) %>%
    # Ensure the graticule is in the same custom CRS as your map
    st_transform(crs = target_crs)

## crop it with the polygon to match your map frame
graticule <- st_crop(
    x = graticule,
    y = st_as_sfc(
        st_bbox(c(xmin = -100, ymin = -10, xmax = 120, ymax = 50), crs = 4326)
    ) %>% st_transform(target_crs)
)

## plot
Pacific_BaseMap<-ggplot() +
    geom_sf(data = graticule, color = "grey50", linetype = "dotted", alpha = 0.5) +
    geom_sf(data = world4, aes(group = admin), fill = "grey80", color = "black") +
    labs( x = "Longitude", y = "Latitude") +
    theme_bw() +
     scale_size_continuous(name = "Value", range = c(2, 10)) # Scale point size based on 'value'
