library(terra)
library(sf)
library(tidyverse)
library(timeseriesTrajectories)
library(tmap)
library(diffeR)
library(magick)

# Preprocess----
dirims <- "LULC/"

# Additinoal polygons and points for building maps
muns <- st_read("gpkg/munRegions3.gpkg") |>
  st_transform(6372) |>
  group_by(Group) |>
  summarise()
letras <- st_read("gpkg/letrasRegions3_2.gpkg") |>
  st_transform(6372) 

# read images
images <- list.files(dirims,
                     glob2rx("LULC*.tif$"),
                     full.names = TRUE) |>
  rast() 

# Create a vector for the time points.
tps = seq(1993, 2024, 1)

# vertical units
vert_units <- "ha"

# AO area growth----
areas_year <- freq(rasstackY_v2) |>
  as_tibble() |>
  filter(value == 2) |>
  mutate(year = 1993:2024,
         # Pixels * pixel resolution * pixel resolution / 10000
         area_ha = count * 90 * 90 / 10000) 
areas_year |>
  ggplot(aes(x = year, 
             y = area_ha)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(1993, 2024, 5)) +
  labs(y = "Area (ha)", x = "Year") +
  cowplot::theme_cowplot()

ggsave(filename = "Plots/AvocBelt_area_orig.jpeg",
       width = 8,
       height = 6,
       dpi = 300)

# Maps of presence and change----
# Let's create the data for the rasstackY data
tmap_mode("plot")
# Get presence data
num_pres_change <- presenceData(rasstackY_v2)

im <- num_pres_change$`Data for number of presence`
# Absence of avocado set to NA
im[im == 0] <- NA
m1 <- tm_shape(im)+
  tm_raster(col = "sum",
            col.legend = tm_legend(title = "Years",
                                   position = tm_pos_out(cell.h = "center", 
                                                    
                                                         cell.v = "bottom", 
                                                         pos.v = "bottom"),
                                   orientation = "landscape"),
            
            col.scale = tm_scale_continuous(values = cols4all::c4a(palette = "brewer.reds"))) + 
  tm_shape(muns)+
  tm_borders(col = tm_const(),
             lwd = 1,
             col.legend = tm_legend(show = F)) +
  tm_shape(letras)+
  tm_text(text = "nom") +
  # tm_shape(muns) +
  # tm_borders(col = "gray20") +
  tm_scalebar(breaks = seq(0,50,25),
              position = c(0.008, 0.1),
              text.size = 0.7,
              lwd = 1,
              text.color = "gray10",
              color.dark = "gray10",
              color.light = "white",
              # just = "right",
              bg.color = "white",
              bg.alpha = 0.5) +
  tm_graticules(n.x = 3,
                n.y = 3,
                crs = "EPSG:4326",
                labels.show = T,
                labels.size = 0.7,
                labels.rot = c(0,90),
                labels.cardinal = T,
                ticks = T,
                lines = F) +
  tm_layout(legend.frame = FALSE,
            legend.frame.color = "#FFFFFF00")

tmap_save(tm = m1,
          filename = "Map/AvocBelt_AvocPresence.jpeg",
          # device = "tiff",
          height = 6,
          width = 8,
          dpi = 300)

# Trim empty spaces
map1 <- image_read("Map/AvocBelt_AvocPresence.jpeg")
map1 <- image_trim(map1)
# Add 20 px border in white
map1 <- image_border(map1, "#FFFFFF", "80x0")

# Image 2
im2 <- num_pres_change$`Data for number of changes`
im2[im2 == 0] <- NA
im2 <- as.factor(im2)
m2 <- tm_shape(im2)+
  tm_raster("sum",
            col.legend = tm_legend(title = "Times",
                                   position = tm_pos_out(cell.h = "center", 
                                                         cell.v = "bottom", 
                                                         pos.v = "bottom"),
                                   orientation = "landscape"),
            col.scale = tm_scale_categorical(values = cols4all::c4a(palette = "carto.ag_grn_yl",                                       reverse = FALSE))) + 
  tm_shape(muns)+
  tm_borders(col = tm_const(),
             lwd = 1,
            col.legend = tm_legend(show = F)) +
  tm_shape(letras)+
  tm_text(text = "nom") +
  tm_scalebar(breaks = seq(0,50,25),
              position = c(0.008, 0.1),
              text.size = 0.7,
              lwd = 1,
              text.color = "gray10",
              color.dark = "gray10",
              color.light = "white",
              # just = "right",
              bg.color = "white",
              bg.alpha = 0.5) +
  tm_graticules(n.x = 3,
                n.y = 3,
                crs = "EPSG:4326",
                labels.show = T,
                labels.size = 0.7,
                labels.rot = c(0,90),
                labels.cardinal = T,
                ticks = T,
                lines = F) +
  tm_layout(legend.frame = FALSE,
            legend.frame.color = "#FFFFFF00")

tmap_save(tm = m2,
          filename = "Map/AvocBelt_AvocNChanges.jpeg",
          # device = "tiff",
          height = 6,
          width = 8,
          dpi = 300)

# Trim empty spaces
map2 <- image_read("Map/AvocBelt_AvocNChanges.jpeg")
map2 <- image_trim(map2)
# Add 20 px border in white
map2 <- image_border(map2, "#FFFFFF", "80x0")

map1 <- image_annotate(map1, 
               "(a)", 
               size = 70, 
               gravity = "northwest",
               location = "+1+0",
               color = "black")

map2 <- image_annotate(map2, 
                       "(b)", 
                       size = 70, 
                       gravity = "northwest",
                       location = "+1+0",
                       color = "black")

img <- c(map1, map2)
map3 <- image_append(img, stack = TRUE)
map3 <- image_border(map3, "#FFFFFF", "20x20")

# Save final map
image_write(map3, 
            path = "Map/AvocBelt_Stack.jpeg", 
            format = "jpeg")

# Trajectory data map----
# Let's create trajectory data for the rasstackY data.
traj_data <- rastertrajData(rasstackY_v2,
                            zeroabsence = 'yes')

im <- traj_data$`Raster data for trajectory plot`

m1 <- tm_shape(im)+
  tm_raster("change",
            col.legend = tm_legend(title = "Change",
                                   # text.size =0.6,
                                   position = tm_pos_out(cell.h = "center", 
                                                         cell.v = "bottom", 
                                                         pos.v = "bottom"),
                                   orientation = "landscape"),
            col.scale = tm_scale_discrete(values = traj_data$`Attribute data for trajectory plot`$myCol,
                                          labels = traj_data$`Attribute data for trajectory plot`$cl)) +
  tm_shape(muns)+
  tm_borders(col = tm_const(),
             lwd = 1,
             col.legend = tm_legend(show = F)) +
  tm_shape(letras)+
  tm_text(text = "nom") +
  tm_scalebar(breaks = seq(0,50,25),
              position = c(0.008, 0.1),
              text.size = 0.7,
              lwd = 1,
              text.color = "gray10",
              color.dark = "gray10",
              color.light = "white",
              # just = "right",
              bg.color = "white",
              bg.alpha = 0.5) +
  tm_graticules(n.x = 3,
                n.y = 3,
                crs = "EPSG:4326",
                labels.show = T,
                labels.size = 0.7,
                labels.rot = c(0,90),
                labels.cardinal = T,
                ticks = T,
                lines = F) +
  tm_layout(legend.frame = FALSE,
            legend.frame.color = "#FFFFFF00")

tmap_save(tm = m1,
          filename = "Map/AvocBelt_AvocChange.jpeg",
          # device = "tiff",
          height = 6,
          width = 8,
          dpi = 300)

map1 <- image_read("Map/AvocBelt_AvocChange.jpeg")
map1 <- image_trim(map1)
# Add 20 px border in white
map1 <- image_border(map1, "#FFFFFF", "80x20")

writeRaster(im,
             "Raster/transitions.tif",
             datatype = "INT2S",
             overwrite = TRUE)

# Trajectory data plot----
# 1 year interval
stackbar_data <- rasterstackData(rasstackY_v2,
                                 timePoints = tps,
                                 spatialextent = 'unified',
                                 zeroabsence = 'yes',
                                 annualchange = 'yes',
                                 categoryName = 'Avocado orchards',
                                 regionName = 'region',
                                 varUnits = "(ha)",
                                 constant  = 1)

p1 <- stackbarPlot(stackbar_data,
                   axisSize = 10,
                   lbAxSize = 10,
                   lgSize = 7.5,
                   titleSize = 12,
                   datbreaks = "no",
                   upperlym = 35,
                   lowerlym = - 50,
                   lymby = 5,
                   upperlym2 = 0.5,
                   lymby2 = 0.1,
                   xAngle = 0)

ggsave(plot = p1[[1]] +
         scale_x_discrete(breaks = paste0(seq(1993, 2024, 5),
                                          "-",
                                          seq(1994, 2024, 5)),
                          labels = as.character(seq(1994, 2024, 5))) +
         labs(x = "End year", title = ""),
       filename = "Plots/AvocBelt_barschange.jpeg",
       # device = "tiff",
       height = 6,
       width = 8,
       dpi = 300)

p1 <- image_read("Plots/AvocBelt_barschange.jpeg")
p1 <- image_trim(p1)
# Add 20 px border in white
p1 <- image_border(p1, "#FFFFFF", "80x0")

map1 <- image_annotate(map1, 
                       "(a)", 
                       size = 70, 
                       gravity = "northwest",
                       location = "+1+0",
                       color = "black")

p1 <- image_annotate(p1, 
                       "(b)", 
                       size = 70, 
                       gravity = "northwest",
                       location = "+1+0",
                       color = "black")

img <- c(map1, p1)
map3 <- image_append(img, stack = TRUE)
map3 <- image_border(map3, "#FFFFFF", "20x20")

# Save final map
image_write(map3, 
            path = "Map/AvocBelt_ComponentsStack.jpeg", 
            format = "jpeg")
  

ggsave(plot = p1[[2]] +
         labs(title = ""),
       filename = "Plots/AvocBelt_barschangetotal.jpeg",
       # device = "tiff",
       height = 4,
       width = 4,
       dpi = 300)
