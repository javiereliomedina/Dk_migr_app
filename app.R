

source("data.R")

big_cities <- c("København", "Aarhus", "Odense", "Aalborg")
big_cities <- filter(dk_lau, LAU_NAME %in% big_cities) 

# App ----

ui <- fluidPage(
  
  tabsetPanel(
    
    tabPanel("Immigrants and their descendants",
             
             titlePanel("Immigrants and their descendants by country of origin in Denmark"), 
             
             fluidRow(
               
               column(4, selectInput("country", 
                                     "Choose country",
                                     choices = levels(countries$text),
                                     width = "100%")
               )
               
             ),
             
             fluidRow(
               
               column(12, "Total number by year", 
                      plotOutput("p_migr_year"))
               
             ),
             
             fluidRow(
               
               column(4, selectInput("dates",
                                     "Select quarter",
                                     choices = dates,
                                     width = "100%")
               )
             ),
             
             fluidRow(
               
               column(6, "Total number by municipality", 
                      dataTableOutput("tbl_pop_muni")),
               
               column(6, "Geographic distribution",
                      plotOutput("p_migr_muni"))
               
             )
             
    ),
    
    tabPanel("Internal migration",
             
             titlePanel("Internal movements between municipalities"),
             
             fluidRow(
               
               column(4, selectInput("target_muni", 
                                     "Choose municipality",
                                     choices = levels(factor(internal_migr$TILKOMMUNE)),
                                     width = "100%")
               ),
               
               column(4, selectInput("year", 
                                     "Choose year",
                                     choices = levels(factor(internal_migr$TID)),
                                     width = "100%")
               ),
               
               column(4, numericInput("top", 
                                      label = "Top N municipalities by movements",
                                      value = 15, 
                                      step = 5,
                                      max = 99,
                                      width = "100%")
               )
               
             ),
             
             fluidRow(
               
               column(6, "Out-migration", 
                      plotOutput("p_out_mov")),
               
               column(6, "In-migration", 
                      plotOutput("p_in_mov"))
               
             ),
             
             fluidRow(
               
               column(6, " ", 
                      dataTableOutput("tbl_out")),
               
               column(6, " ", 
                      dataTableOutput("tbl_in"))
               
             )
             
    )
    
  )
)

server <- function(input, output, session) {
  
  selec_country <- reactive(countries %>% filter(text == input$country))
  selec_ID <- reactive(selec_country() %>% pull(id))
  variables <- reactive(list(list(code = "OMRÅDE", values = municipalities$id),
                             list(code = "HERKOMST", values = c("TOT", "4", "3")),
                             list(code = "IELAND", values = selec_ID()),
                             list(code = "Tid", values = NA)))
  get_pop <- reactive(get_data("FOLK1C", variables()) %>% 
                        rename(muni = OMRÅDE,
                               ancestry = HERKOMST,
                               origin = IELAND,
                               date = TID, 
                               pop = INDHOLD) %>% 
                        mutate(date = gsub("Q", "", date),
                               date = as_date_yq(as.integer(date)),
                               date = first_of_quarter(date)) %>% 
                        mutate(muni = gsub("Copenhagen", "København", muni)) %>%
                        group_by(date, ancestry) %>% 
                        arrange(-pop) %>% 
                        mutate(pop_pct = 100 * pop / sum(pop, na.rm = TRUE),
                               pop_pct_cum = cumsum(pop_pct),
                               pop_pct_cum_brk = cut(pop_pct_cum,
                                                     breaks = seq(0, 100, 10))) %>% 
                        ungroup() 
  )
  
  pop_lau <- reactive(dk_lau %>% 
                        left_join(get_pop(), by = c("LAU_NAME" = "muni")) %>% 
                        st_as_sf() 
  )
  
  
  output$p_migr_year <- renderPlot({
    
    get_pop() %>% 
      summarise.(pop = sum(pop), .by = c(date, ancestry)) %>% 
      ggplot() + 
      geom_line(aes(x = date, y = pop/1000, colour = ancestry)) + 
      scale_colour_manual(name = "Ancestry", 
                          values = c("#D55E00", "#0072B2", "#000000")) +
      theme_bw() +
      labs(x = "",
           y = "pop [x1000]")
    
  }, res = 96)
  
  output$tbl_pop_muni <- renderDataTable(
    
    get_pop() %>% 
      filter(date == input$dates, ancestry == "Total") %>% 
      select(muni, pop, pop_pct, pop_pct_cum) %>% 
      mutate(across(where(is.numeric), round, 2)),
    options = list(pageLength = 5)
    
  )
  
  output$p_migr_muni <- renderPlot({
    
    pop_lau() %>% 
      filter(date == input$dates, ancestry == "Total") %>% 
      ggplot() +
      geom_sf(aes(fill = pop_pct),
              color = "grey", 
              size = 0.05) +
      scale_fill_viridis(name = "Percentage [%]",
                         option = "mako",
                         direction = -1) +
      labs(x = "",
           y = "",
           caption = "Data source: Statistics Denmark")  + 
      ylim(54.50, 58.0) +
      theme_void() +
      geom_sf_label_repel(data = big_cities,
                          aes(label = LAU_NAME),
                          force = 10,
                          nudge_y = 3,
                          nudge_x = 0.5,
                          seed = 10) 
    
  }, res = 96)
  
  get_out <- reactive(internal_migr %>% 
                        filter(FRAKOMMUNE == input$target_muni,
                               INDHOLD > 0,
                               TID == input$year) %>%
                        slice_max(INDHOLD, n = input$top))
  
  get_in <- reactive(internal_migr %>% 
                       filter(TILKOMMUNE == input$target_muni,
                              INDHOLD > 0,
                              TID == input$year) %>%
                       slice_max(INDHOLD, n = input$top))
  
  output$p_out_mov <- renderPlot({
    
    ggplot() +
      geom_sf(data = dk_lau) +
      geom_curve(data = get_out(),
                 aes(x = FRA_long,
                     y = FRA_lat,
                     xend = TIL_long,
                     yend = TIL_lat,
                     colour = INDHOLD),
                 size = 0.75,
                 alpha = 0.65,
                 curvature = -0.2, arrow = arrow(length = unit(0.01, "npc"))) +
      scale_colour_viridis(name = "Frequency",
                           option = "rocket",
                           trans = "log10",
                           direction = -1) +
      coord_sf() +
      theme_void()
    
  }, res = 96
  
  )
  
  output$p_in_mov <- renderPlot({
    
    ggplot() +
      geom_sf(data = dk_lau) +
      geom_curve(data = get_in(),
                 aes(x = FRA_long,
                     y = FRA_lat,
                     xend = TIL_long,
                     yend = TIL_lat,
                     colour = INDHOLD),
                 size = 0.75,
                 alpha = 0.65,
                 curvature = -0.2, arrow = arrow(length = unit(0.01, "npc"))) +
      scale_colour_viridis(name = "Frequency",
                           option = "mako",
                           trans = "log10",
                           direction = -1) +
      coord_sf() +
      theme_void()
    
  }, res = 96
  
  )
  
  output$tbl_out <- renderDataTable(
    
    get_out() %>% 
      select(TILKOMMUNE, INDHOLD) %>% 
      rename(To = TILKOMMUNE,
             Value = INDHOLD) %>% 
      arrange(-Value) %>% 
      mutate(across(where(is.numeric), round, 0)),
    options = list(pageLength = 5)
    
  )
  
  output$tbl_in <- renderDataTable(
    
    get_in() %>% 
      select(FRAKOMMUNE, INDHOLD) %>% 
      rename(From = FRAKOMMUNE,
             Value = INDHOLD) %>% 
      arrange(-Value) %>% 
      mutate(across(where(is.numeric), round, 0)),
    options = list(pageLength = 5)
    
  )
  
}

shinyApp(ui, server)
