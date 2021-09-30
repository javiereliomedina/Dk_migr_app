## Population data from Statistic Denmark (https://www.statbank.dk/10021)

# Immigrants and their descendant in Denmark (Table: FOLK1C) ----

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
  
## Palette YlGnBu, with 9 colours
my_pal <- brewer.pal(9, "YlGnBu") 
## Add more colours to this palette :
my_pal <- rev(colorRampPalette(my_pal)(10))

cumsum__breaks <- seq(0, 100, 10)
