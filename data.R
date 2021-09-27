# Plot immigrants and their descendant in Denmark

library(danstat) 
library(tidyverse)
library(readr)
library(dint)
library(patchwork)
library(gridExtra)
library(forcats)
library(RColorBrewer)
library(furrr)
library(sf)
library(giscoR)
library(viridis)
library(gganimate)
library(tidytable)
library(shiny)

# # if(!require("devtools"))  install.packages("devtools")
# if(!require("ggsflabel")) devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)

# Define theme for ggplot2
theme_plot <- function() {
  theme_bw() +
    theme(axis.text = element_text(size = 7),
          axis.title = element_text(size = 9),
          legend.title = element_text(size = 9, face = "bold"),
          plot.title = element_text(size = 11, face = "bold"),
          title = element_text(size = 9),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank())
}

# Local Administrative Units (LAU) ----
dk_lau <- readRDS(file = "DAGI/KOMMUNE.rds") %>% 
  mutate(KOMNAVN = factor(KOMNAVN)) %>% 
  st_transform(crs = 4326) %>% 
  group_by(KOMNAVN) %>%
  summarise(geometry = st_union(geometry)) %>% 
  ungroup() %>% 
  rename(LAU_NAME = KOMNAVN)

# Population data from Statistic Denmark (https://www.statbank.dk/10021)

# Table: FOLK1C
id_table <- "FOLK1C"
var_pop <- get_table_metadata(table_id = id_table, variables_only = TRUE)

countries <- as_tibble(var_pop$values[[5]]) %>% 
  mutate(text = factor(text))

# Plot only four years
periods <- as.Date(c("2008-01-01",
                     "2012-01-01",
                     "2016-01-01",
                     "2020-01-01"))

big_cities <- c("København", "Aarhus", "Odense", "Aalborg")
big_cities <- dk_lau %>%
  filter(LAU_NAME %in% big_cities) %>% 
  mutate(date = c("2008-01-01"),
         date = factor(date, levels = levels(factor(periods))),
         date = as.Date(date))

## Palette YlGnBu, with 9 colours
my_pal <- brewer.pal(9, "YlGnBu") 
## Add more colours to this palette :
my_pal <- rev(colorRampPalette(my_pal)(10))

cumsum__breaks <- seq(0, 100, 10)
