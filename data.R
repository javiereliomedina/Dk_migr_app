library(shiny)
library(tidyverse)
library(dint)
library(sf)
library(viridis)
library(tidytable)
# # if(!require("devtools"))  install.packages("devtools")
# if(!require("ggsflabel")) devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
library(danstat) 

# Data ----

## Local Administrative Units (LAU) ----
dk_lau <- readRDS(file = "DAGI/KOMMUNE.rds") %>% 
  mutate(KOMNAVN = factor(KOMNAVN)) %>% 
  st_transform(crs = 4326) %>% 
  group_by(KOMNAVN) %>%
  summarise(geometry = st_union(geometry)) %>% 
  ungroup() %>% 
  rename(LAU_NAME = KOMNAVN)

## LAU Centroids ----
dk_lau_cent <- st_centroid(dk_lau) %>%
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  st_set_geometry(NULL)

## Immigrants and their descendant in Denmark (Table: FOLK1C) ----
id_table <- "FOLK1C"
var_pop <- get_table_metadata(table_id = id_table, variables_only = TRUE)

countries <- as_tibble(var_pop$values[[5]]) %>% 
  mutate(text = factor(text))

dates <- var_pop$values[[6]]$id %>%
  gsub("K", "", .) %>% 
  as.integer() %>% 
  as_date_yq() %>% 
  first_of_quarter()

municipalities <- var_pop$values[[1]] %>% 
  as_tibble() %>% 
  mutate(id = as.numeric(id)) %>% 
  filter(id > 100)

## Internal movements (Table: FOLK1C) ----
id_table <- "FLY66"
internal_migr_meta <- get_table_metadata(table_id = id_table, variables_only = TRUE)

variables <- list(
  # TILKOMMUNE
  list(code = "TILKOMMUNE", values = NA),
  # FRAKOMMUNE
  list(code = "FRAKOMMUNE", values = NA),
  # First quarter of 2020
  list(code = "Tid", values = NA)
)

internal_migr_dwn <- get_data("FLY66", variables, language = "da")
internal_migr <- internal_migr_dwn %>%
  mutate(TILKOMMUNE = enc2native(TILKOMMUNE),
         FRAKOMMUNE = enc2native(FRAKOMMUNE)) %>%
  # Add centroids
  left_join.(dk_lau_cent, by = c("TILKOMMUNE" = "LAU_NAME")) %>%
  rename.(TIL_long = long, TIL_lat = lat) %>%
  left_join.(dk_lau_cent, by = c("FRAKOMMUNE" = "LAU_NAME")) %>%
  rename.(FRA_long = long, FRA_lat = lat)
