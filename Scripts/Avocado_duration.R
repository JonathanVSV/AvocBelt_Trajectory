library(terra)
library(sf)
library(tidyverse)
library(tmap)
library(cols4all)
library(patchwork)
library(magick)

# Preprocess----
dirims <- "LULC/"

# read images
images <- list.files(dirims,
                     glob2rx("LULC*.tif$"),
                     full.names = TRUE) |>
  rast()

# Transitions. This raster is exported from Trajectory_analyses.R
transitions <- rast("Raster/transitions.tif")

# Additinoal polygons and points for building maps
muns <- st_read("gpkg/munRegions3.gpkg") |>
  st_transform(6372) |>
  group_by(Group) |>
  summarise()
letras <- st_read("gpkg/letrasRegions3_2.gpkg") |>
  st_transform(6372) 

# Create a vector for the time points.
tps = seq(1993, 2024, 1)

# Loop over transitions 2 (Loss with alternation), 
# 4 (Gain with alternation), 
# 5 (All alternation, loss first), 
# 6 (All alternation, gain first)

meandurAO <- map(c(2, 4, 5, 6), function(transit){
  
  maskara <- transitions
  maskara[maskara != transit] <- NA
  
  vals <- map(1:nlyr(images), function(i){
    resul <- mask(images[[i]], maskara)
    resul <- values(resul)
    resul <- resul[!is.na(resul)]
    return(resul)
  }) |>
    setNames(as.character(tps))
  
  resul <- bind_cols(vals) |>
    mutate(id = row_number()) |>
    pivot_longer(cols = -id, 
                 names_to = "year",
                 values_to = "class") |>
    mutate(across(year, ~as.numeric(.x))) |>
    arrange(id, year) 
  
  # Identify years of change
  trajectories <- resul |>
    # anti_join(excl_id,
    #           by = "id") |>
    group_by(id) |>
    mutate(change = ifelse(class == lag(class), 0, 1),
           prechange = ifelse(class == lead(class), 0, 1)) |>
    mutate(num = row_number()) |>
    filter(change == 1 | num == 1) |>
    summarise(trajectory = paste(class, collapse = "-"),
              .groups = "drop") |>
    mutate(across(trajectory, ~stringr::str_replace_all(.x, "0", "NAO"))) |>
    mutate(across(trajectory, ~stringr::str_replace_all(.x, "2", "AO"))) 
  
  # Calculate duration
  duration <- resul |>
    # anti_join(excl_id,
    #           by = "id") |>
    group_by(id) |>
    mutate(change = ifelse(class == lag(class), 0, 1),
           prechange = ifelse(class == lead(class), 0, 1)) |>
    mutate(num = row_number()) |>
    filter(change == 1 | num == 1) |>
    mutate(duration = lead(num) - num) |>
    # Esto hace como un falso cambio
    # mutate(across(duration, ~ifelse(is.na(.x), 2024 - year, .x))) 
    mutate(across(class, ~ifelse(.x == 0, "NOA", "AO"))) |>
    filter(class == "AO") |>
    ungroup()
  
  return(list(trajectories, duration))
})

# 2: Loss with alternation
# 3: Gain without alternation
# 4: Gain with alternation
# 5: All alternation loss first
# 6: All alternation gain first

resul_traj <- meandurAO |>
  map(~.x |> 
        pluck(1)) |>
  set_names(c(2, 4, 5, 6)) |>
  bind_rows(.id = "trans") |>
  mutate(idchar = dplyr::case_match(trans,
                                    "2" ~ "Loss with alternation",
                                    "4" ~ "Gain with alternation",
                                    "5" ~ "All alternation loss first",
                                    "6" ~ "All alternation gain first"))

resul_dur <- meandurAO |>
  map(~.x |> 
        pluck(2)) |>
  set_names(c(2, 4, 5, 6)) |>
  bind_rows(.id = "trans") |>
  mutate(idchar = dplyr::case_match(trans,
                                "2" ~ "Loss with alternation",
                                "4" ~ "Gain with alternation",
                                "5" ~ "All alternation loss first",
                                "6" ~ "All alternation gain first"))

resul_dur |>
  summarise(med_dur = median(duration, na.rm = TRUE))
  
resul_dur |>
  group_by(duration) |>
  summarise(freq = n()) |>
  ggplot(aes(x = duration,
             y = freq)) +
  geom_col() +
  labs(x = "Years", y = "Frequency") +
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,32),
                     breaks = seq(0,32,5)) +
  # facet_wrap(~idchar) +
  cowplot::theme_cowplot()
 
ggsave(filename = "Plots/Alternation_AO_Duration.jpeg",
       # device = "tiff",
       height = 6,
       width = 8,
       dpi = 300)
