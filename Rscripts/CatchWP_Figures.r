## Code to pull catch data for billfish assessments from LLDS summary tables


## BUM
# HI Longline quarterly catch in numbers
HI_QCatch <- HI_stats %>%
            filter(REPT_PERIOD == "Q") %>%
            group_by(YEAR, PERIOD_NUM) %>%
            summarise(
                C_BLUE_MARLIN = sum(C_BLUE_MARLIN, na.rm = TRUE)/1000
                ) %>%
                ungroup() %>%
            select(YEAR, PERIOD_NUM, C_BLUE_MARLIN)

#American Samoa longline quarterly catch in numbers
AS_QCatch <- AS_stats %>%
    filter(REPT_PERIOD == "Q") %>%
    group_by(YEAR, PERIOD_NUM) %>%
    summarise(
        C_BLUE_MARLIN = sum(C_BLUE_MARLIN, na.rm = TRUE)/1000
    ) %>%
    ungroup() %>%
    select(YEAR, PERIOD_NUM, C_BLUE_MARLIN)

## Small boat/other gears, catch in biomass
HI_Other<-HI_all_gears %>%
        filter(FAO_CODE=="BUM") %>% 
        mutate( C_BLUE_MARLIN = (PEL_LBS_KEPT - LL_ALL_LBS_KEPT)*0.000453592) %>%
        select(YEAR, C_BLUE_MARLIN)


## Annual catch by gear

#sum by year
HI_CatchYr<-aggregate(HI_QCatch$C_BLUE_MARLIN, by=list(HI_QCatch$YEAR), sum)
names(HI_CatchYr)=c("YEAR","C_BLUE_MARLIN")
HI_CatchYr<-subset(HI_CatchYr, YEAR<2025)
AS_CatchYr <- aggregate(AS_QCatch$C_BLUE_MARLIN, by = list(AS_QCatch$YEAR), sum)
names(AS_CatchYr) <- c("YEAR", "C_BLUE_MARLIN")
AS_CatchYr <- subset(AS_CatchYr, YEAR < 2025)


AnnualCatch<-bind_rows(
    "US HI Longline" = HI_CatchYr,
    "US AS Longline" = AS_CatchYr,
    "US Other Gears" = HI_Other,
    .id="Source"
)

## plot

AnnualLLCatch<-ggplot() +
    geom_col(aes(x=YEAR, y=C_BLUE_MARLIN, fill=Source), data = subset(AnnualCatch, Source!="US Other Gears")) +
    theme_bw() +
    scale_fill_manual(values=c("US AS Longline" = "mediumblue", "US HI Longline" = "grey70")) +
    scale_x_continuous(limits=c(1992, 2025), breaks=seq(1995,2025,5)) +
    scale_y_continuous(name="Thousands of Fish Caught")



## Mapping code



HI_CatchMapYR <- BUMMapping5 %>%
            group_by(Year, Lon5, Lat5) %>%
            mutate(
                Catch = sum(Catch, na.rm=TRUE)/1000
            ) %>%
         select(Year, Lon5, Lat5,Catch)

HI_CatchMapQTR <-  BUMMapping5Q %>%
        group_by(Quarter, Lon5,Lat5) %>%
    mutate(
        Catch = sum(Catch, na.rm = TRUE)/1000
    ) %>%
    select(Quarter, Lon5,Lat5,Catch)



HICatch_5yr<-HI_CatchMapYR

HICatch_5yr$YrGroup<-1
Group2 <- which(HICatch_5yr$Year >= 1995 & HICatch_5yr$Year < 2000)
Group3 <- which(HICatch_5yr$Year >= 2000 & HICatch_5yr$Year < 2005)
Group4 <- which(HICatch_5yr$Year >= 2005 & HICatch_5yr$Year < 2010)
Group5 <- which(HICatch_5yr$Year >= 2010 & HICatch_5yr$Year < 2015)
Group6 <- which(HICatch_5yr$Year >= 2015 & HICatch_5yr$Year < 2020)


HICatch_5yr$YrGroup[Group2]<-2
HICatch_5yr$YrGroup[Group3] <- 3
HICatch_5yr$YrGroup[Group4] <- 4
HICatch_5yr$YrGroup[Group5] <- 5
HICatch_5yr$YrGroup[Group6] <- 6


HICatch_5yr<-aggregate(Catch ~ YrGroup + Lat5 + Lon5, HICatch_5yr, sum)


YearLabels<-c("1" = "1995-1999", "2" = "2000 - 2004","3"="2005 - 2009","4"="2010 - 2014","5"="2015 - 2019","6"="2020 - 2024")
world_df <- map_data("world")
CatchMapYear<-ggplot() +
    # World map polygons
    geom_polygon(
        data = world_df,
        aes(x = long, y = lat, group = group),
        fill = "grey80", color = "gray50"
    ) +

    # Plotting the effort data as points with binned sizes
    geom_point(
        data = HICatch_5yr,
        aes(x = Lon5, y = Lat5, size = Catch),
        color = "red", alpha = 0.7
    ) +

    # Using discrete size scale for binned data
 #   scale_size_manual(
 #       values = seq(2, 6, length.out = n_bins), # Size range from 2 to 6
 #       labels = bin_labels,
 #       name = "Effort Range"
 #   ) +

    # Formatting X and Y axis labels with degrees
    scale_x_continuous(labels = function(x) paste0(abs(x), "°W")) +
    scale_y_continuous(labels = function(y) paste0(y, "°N")) +

    # Coordinate adjustments for map area
    coord_fixed(
        ratio = 1.3,
        xlim = c(-180, -100), # Adjust longitude range for Hawaii
        ylim = c(0, 40) # Adjust latitude range for Hawaii
    ) +

    # Customizing the legend
    guides(size = guide_legend(
        title = "Thousands of Fish Caught",
        override.aes = list(color = "red") # Ensure legend symbols are red
    )) +

    # Adding plot title and theme
    theme_minimal() +

    # Customizing the overall plot theme
    theme(
        #legend.position = c(0.8, 0.2),
        legend.background = element_rect(fill = alpha("white", 0.7), color = "black", linewidth = 0.5),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(20, 20, 20, 20),
        panel.grid.major = element_line(color = "gray70"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "white", color = NA) # Set white background
    ) + 
    facet_wrap(~YrGroup, labeller = labeller(YrGroup=YearLabels)) 


CatchMapQTR <- ggplot() +
    # World map polygons
    geom_polygon(
        data = world_df,
        aes(x = long, y = lat, group = group),
        fill = "grey80", color = "gray50"
    ) +

    # Plotting the effort data as points with binned sizes
    geom_point(
        data = HI_CatchMapQTR,
        aes(x = Lon5, y = Lat5, size = Catch),
        color = "red", alpha = 0.7
    ) +

    # Using discrete size scale for binned data
    #   scale_size_manual(
    #       values = seq(2, 6, length.out = n_bins), # Size range from 2 to 6
    #       labels = bin_labels,
    #       name = "Effort Range"
    #   ) +

    # Formatting X and Y axis labels with degrees
    scale_x_continuous(labels = function(x) paste0(abs(x), "°W")) +
    scale_y_continuous(labels = function(y) paste0(y, "°N")) +

    # Coordinate adjustments for map area
    coord_fixed(
        ratio = 1.3,
        xlim = c(-180, -100), # Adjust longitude range for Hawaii
        ylim = c(0, 40) # Adjust latitude range for Hawaii
    ) +

    # Customizing the legend
    guides(size = guide_legend(
        title = "Thousands of Fish Caught",
        override.aes = list(color = "red") # Ensure legend symbols are red
    )) +

    # Adding plot title and theme
    theme_minimal() +

    # Customizing the overall plot theme
    theme(
        # legend.position = c(0.8, 0.2),
        legend.background = element_rect(fill = alpha("white", 0.7), color = "black", linewidth = 0.5),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(20, 20, 20, 20),
        panel.grid.major = element_line(color = "gray70"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "white", color = NA) # Set white background
    ) +
    facet_wrap(~Quarter)






## plot Pacific map centered on Hawaiilibrary(ggplot2)
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
library(sf)
library(rnaturalearth)
library(dplyr)
library(ggplot2)

## convert your data into the correct projection for plotting
# grid_sf <- st_as_sf(
#     HI_CatchMap,
#     coords = c("LON", "LAT"), # Specify which columns contain longitude and latitude
#     crs = 4326 # Set the initial Coordinate Reference System (WGS84)
# )

# # Transform the 'sf' object to the target CRS (the one used for the map)
# grid_sf_transformed <- grid_sf %>%
#     st_transform(crs = target_crs)


# Create 5x5-degree grid within the Pacific region from 120°E to 120°W and latitudes from -10° to 50°
# set.seed(42)
# grid_data <- expand.grid(
#     lon = seq(120, 240, by = 5), # Longitude from 120°E to 240°E
#     lat = seq(-10, 50, by = 5) # Latitude from -10° to 50°
# ) %>%
#     mutate(
#         value = runif(n(), min = 10, max = 100) # Assign random values for plotting
#     )

# grid_sf <- st_as_sf(
#     grid_data,
#     coords = c("lon", "lat"), # Specify which columns contain longitude and latitude
#     crs = 4326 # Set the initial Coordinate Reference System (WGS84)
# )

# # Transform the 'sf' object to the target CRS (the one used for the map)
# grid_sf_transformed <- grid_sf %>%
#     st_transform(crs = target_crs)


# worldMap <- ne_countries(scale = "medium", returnclass = "sf") %>%
#     st_make_valid()

# target_crs <- st_crs("+proj=eqc +x_0=0 +y_0=0 +lat_0=0 +lon_0=133")

# # define a long & slim polygon that overlaps the meridian line & set its CRS to match
# # that of world
# # Centered in lon 133

# offset <- 180 - 133


# polygon <- st_polygon(x = list(rbind(
#     c(-0.0001 - offset, 90),
#     c(0 - offset, 90),
#     c(0 - offset, -90),
#     c(-0.0001 - offset, -90),
#     c(-0.0001 - offset, 90)
# ))) %>%
#     st_sfc() %>%
#     st_set_crs(4326)


# # modify world dataset to remove overlapping portions with world's polygons
# world2 <- worldMap %>% st_difference(polygon)


# # Transform
# world3 <- world2 %>% st_transform(crs = target_crs)
# world4 <- st_crop(
#     x = world3,
#     y = st_as_sfc(
#         st_bbox(c(xmin = -100, ymin = -25, xmax = 120, ymax = 50), crs = 4326)
#     ) %>% st_transform(target_crs)
# )


# ## create lines for lat/lon every 5degrees
# lons <- seq(120, 280, by = 5) # Every 5 degrees longitude
# lats <- seq(-25, 50, by = 5) # Every 5 degrees latitude

# # 2. Generate the graticule lines as an sf object
# graticule <- st_graticule(
#     # Set breaks to your desired 5-degree intervals
#     lon = lons,
#     lat = lats,
#     # Define the bounding box of your data (optional, but good practice)
#     ndiscr = 1000 # Increase density of line segments for better transformation
# ) %>%
#     # Ensure the graticule is in the same custom CRS as your map
#     st_transform(crs = target_crs)

# graticule <- st_crop(
#     x = graticule,
#     y = st_as_sfc(
#         st_bbox(c(xmin = -100, ymin = -25, xmax = 120, ymax = 50), crs = 4326)
#     ) %>% st_transform(target_crs)
# )



# ## transform your data: 
# grid_sf <- st_as_sf(
#     HICatch_5yr,
#     coords = c("Lon5", "Lat5"), # Specify which columns contain longitude and latitude
#     crs = 4326 # Set the initial Coordinate Reference System (WGS84)
# )

# # Transform the 'sf' object to the target CRS (the one used for the map)
# grid_sf_transformed <- grid_sf %>%
#     st_transform(crs = target_crs)



# ## plot
# Catch_2025<-ggplot() +
#     geom_sf(data = graticule, color = "grey50", linetype = "dotted", alpha = 0.5) +
#     geom_sf(data = world4, aes(group = admin), fill = "grey80", color = "black") +
#     labs(x = "Longitude", y = "Latitude") +
#     theme_bw() +
#     geom_sf(
#             data = subset(grid_sf_transformed, YrGroup==7),
#             aes(size = Catch),
#             color = "red", alpha = 0.7
#         ) +
#      scale_size_continuous(name = "Number of Fish Caught (1000s of fish)", range = c(2, 10))# +# Scale point size based on 'value'
     

# Catch_1519<-ggplot() +
#     geom_sf(data = graticule, color = "grey50", linetype = "dotted", alpha = 0.5) +
#     geom_sf(data = world4, aes(group = admin), fill = "grey80", color = "black") +
#     labs(x = "Longitude", y = "Latitude") +
#     theme_bw() +
#     geom_sf(
#             data = subset(grid_sf_transformed, YrGroup==6),
#             aes(size = Catch),
#             color = "red", alpha = 0.7
#         ) +
#      scale_size_continuous(name = "Number of Fish Caught (1000s of fish)", range = c(2, 10))# +# Scale point size based on 'value'

# Catch_1014<-ggplot() +
#     geom_sf(data = graticule, color = "grey50", linetype = "dotted", alpha = 0.5) +
#     geom_sf(data = world4, aes(group = admin), fill = "grey80", color = "black") +
#     labs(x = "Longitude", y = "Latitude") +
#     theme_bw() +
#     geom_sf(
#             data = subset(grid_sf_transformed, YrGroup==5),
#             aes(size = NUMKEPT),
#             color = "red", alpha = 0.7
#         ) +
#      scale_size_continuous(name = "Number of Fish Caught (1000s of fish)", range = c(2, 10))# +# Scale point size based on 'value'


# Catch_0509<-ggplot() +
#     geom_sf(data = graticule, color = "grey50", linetype = "dotted", alpha = 0.5) +
#     geom_sf(data = world4, aes(group = admin), fill = "grey80", color = "black") +
#     labs(x = "Longitude", y = "Latitude") +
#     theme_bw() +
#     geom_sf(
#             data = subset(grid_sf_transformed, YrGroup==4),
#             aes(size = NUMKEPT),
#             color = "red", alpha = 0.7
#         ) +
#      scale_size_continuous(name = "Number of fish caught (1000s of fish)", range = c(2, 10))# +# Scale point size based on 'value'
     

#      Catch_0004 <- ggplot() +
#          geom_sf(data = graticule, color = "grey50", linetype = "dotted", alpha = 0.5) +
#          geom_sf(data = world4, aes(group = admin), fill = "grey80", color = "black") +
#          labs(x = "Longitude", y = "Latitude") +
#          theme_bw() +
#          geom_sf(
#              data = subset(grid_sf_transformed, YrGroup == 3),
#              aes(size = NUMKEPT),
#              color = "red", alpha = 0.7
#          ) +
#          scale_size_continuous(name = "Number of fish caught (1000s of fish)", range = c(2, 10)) # +# Scale point size based on 'value'
# Catch_9599<-ggplot() +
#     geom_sf(data = graticule, color = "grey50", linetype = "dotted", alpha = 0.5) +
#     geom_sf(data = world4, aes(group = admin), fill = "grey80", color = "black") +
#     labs(x = "Longitude", y = "Latitude") +
#     theme_bw() +
#     geom_sf(
#             data = subset(grid_sf_transformed, YrGroup==2),
#             aes(size = NUMKEPT),
#             color = "red", alpha = 0.7
#         ) +
#      scale_size_continuous(name = "Number of fish caught (1000s of fish)", range = c(2, 10))# +# Scale point size based on 'value'

# Catch_9094<-ggplot() +
#     geom_sf(data = graticule, color = "grey50", linetype = "dotted", alpha = 0.5) +
#     geom_sf(data = world4, aes(group = admin), fill = "grey80", color = "black") +
#     labs(x = "Longitude", y = "Latitude") +
#     theme_bw() +
#     geom_sf(
#             data = subset(grid_sf_transformed, YrGroup==1),
#             aes(size = NUMKEPT),
#             color = "red", alpha = 0.7
#         ) +
#      scale_size_continuous(name = "Number of fish caught (1000s of fish)", range = c(2, 10))# +# Scale point size based on 'value'
          