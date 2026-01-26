library(tidyverse)

# Inspecting other moderators

meta.tab <- read_rds('Outputs/meta_tab.rds')

names(meta.tab)

meta.tab %>% 
  group_by(Insecticides, Variable) %>%
  count %>%
  pivot_wider(names_from = Variable, values_from = n) %>%
  dplyr::select(Parasitoid_abundance, Parasitism_rate, Pest_density, Yield)

pl_div_tab <- 
meta.tab %>% 
  group_by(Nectar_providing_plants_diversity, Variable) %>%
  count %>%
  pivot_wider(names_from = Variable, values_from = n) %>%
  dplyr::select(Parasitoid_abundance, Parasitism_rate, Pest_density, Yield) %>%
  mutate(n_pl = as.numeric(Nectar_providing_plants_diversity)) %>%
  arrange(n_pl) %>%
  dplyr::select(n_pl, everything()) %>%
  filter(!is.na(Nectar_providing_plants_diversity)) %>% # these correspond to AS treatments
  ungroup %>%
  dplyr::select(-Nectar_providing_plants_diversity)
# NA: Bone et al. 2009, Sann et al. 2018 -> spontaneous veget., prob. more than one species
   
pl_div_tab

pl_div_tab %>%
  mutate(n_pl_categ = ifelse(n_pl > 1 | is.na(n_pl), 2, 1)) %>%
  group_by(n_pl_categ) %>%
  summarise(across(c(Parasitoid_abundance, Parasitism_rate, Pest_density, Yield), ~sum(.x, na.rm = T)))

