# Get data

source("data.R")

big_cities <- c("København", "Aarhus", "Odense", "Aalborg")
big_cities <- dk_lau %>%
  filter(LAU_NAME %in% big_cities) %>% 
  mutate(date = c("2008-01-01"),
         date = factor(date, levels = levels(factor(periods))),
         date = as.Date(date))

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
