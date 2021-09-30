# Local Administrative Units (LAU)
dk_lau <- readRDS(file = "DAGI/KOMMUNE.rds") %>% 
  mutate(KOMNAVN = factor(KOMNAVN)) %>% 
  st_transform(crs = 4326) %>% 
  group_by(KOMNAVN) %>%
  summarise(geometry = st_union(geometry)) %>% 
  ungroup() %>% 
  rename(LAU_NAME = KOMNAVN)
