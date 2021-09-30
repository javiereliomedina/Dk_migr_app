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