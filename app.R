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

if(!require("devtools"))  install.packages("devtools")
if(!require("ggsflabel")) devtools::install_github("yutannihilation/ggsflabel")
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

# App ----

ui <- fluidPage(
  
  fluidRow(
    column(6,
           selectInput("country", 
                       "Choose country",
                       choices = levels(countries$text),
                       width = "100%"
           )
    )
  ),
  
  fluidRow(
    column(4, "Number of immigrants and their descendants", 
           plotOutput("p_migr_year", width = "500px")),
    column(8, "Distribution over the years in Denmark",
           plotOutput("p_migr_muni", height = "700px"))
  )
  
)

server <- function(input, output, session) {
  
  selec_country <- reactive(countries %>% filter(text == input$country))
  selec_ID <- reactive(selec_country() %>% pull(id))
  variables <- reactive(list(list(code = "OMRÅDE", values = NA),
                             list(code = "HERKOMST", values = c("TOT", "4", "3")),
                             list(code = "IELAND", values = selec_ID()),
                             list(code = "Tid", values = NA)))
  get_pop <- reactive(get_data("FOLK1C", variables()) %>% 
                        rename(region = OMRÅDE,
                               ancestry = HERKOMST,
                               origin = IELAND,
                               date = TID, 
                               pop = INDHOLD) %>% 
                        mutate(date = gsub("Q", "", date),
                               date = as_date_yq(as.integer(date)),
                               date = first_of_quarter(date)) 
  )
  
  pop_lau <- reactive(dk_lau %>% 
                        left_join(get_pop(), by = c("LAU_NAME" = "region")) %>% 
                        st_as_sf() %>%
                        group_by(date, ancestry) %>% 
                        arrange(-pop) %>% 
                        mutate(pop_pct = 100 * pop / sum(pop, na.rm = TRUE),
                               pop_pct_cum = cumsum(pop_pct),
                               pop_pct_cum_brk = cut(pop_pct_cum, breaks = cumsum__breaks)) %>% 
                        ungroup()
  )
  
  
  output$p_migr_year <- renderPlot({
    
    get_pop() %>% 
      summarise.(pop = sum(pop), .by = c(date, ancestry)) %>% 
      ggplot() + 
      geom_line(aes(x = date, y = pop/1000, colour = ancestry)) + 
      scale_colour_manual(name = "Ancestry", 
                          values = c("#D55E00", "#0072B2", "#000000")) +
      theme_bw() +
      labs(y = "pop [x1000]")
    
  }, res = 96)
  
  output$p_migr_muni <- renderPlot({
    
    pop_lau() %>% 
      filter(date %in% periods, ancestry == "Total") %>% 
      ggplot() +
      geom_sf(aes(fill = pop_pct_cum_brk,
                  group = interaction(pop_pct_cum_brk, date)),
              color = "grey", 
              size = 0.05) +
      scale_fill_manual(name = "Cumulative\npercentage [%]",
                        values = my_pal,
                        labels = seq(10, 100, 10),
                        drop = FALSE) +
      
      labs(x = "",
           y = "")  + 
      ylim(54.50, 58.0) +
      theme_plot() +
      geom_sf_label_repel(data = big_cities,
                          aes(label = LAU_NAME),
                          force = 10,
                          nudge_y = 3,
                          nudge_x = 0.5,
                          seed = 10) +
      facet_wrap(~date)
    
  }, res = 96)
  
}

shinyApp(ui, server)
