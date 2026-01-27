# Some additional data exploration/manipulation

library(tidyverse)

meta.tab <- read_rds('Outputs/meta_tab.rds')

# A. Combining Fig. 3 and 4 ----
  # Need to load the initial part of script

tmt_distrib <- 
meta.tab %>%
  group_by(Variable, Tmt_sugar_name, Tmt_spatial) %>%
  summarise(Nb_included = n())

tmt_distrib %>%
  ungroup() %>%
  complete(Variable, Tmt_spatial, Tmt_sugar_name, fill = list(Nb_included = 0)) %>%
  ggplot(aes(x = Variable, y = Tmt_spatial, fill = Nb_included)) +
  geom_tile() +
  facet_wrap(~Tmt_sugar_name, ncol = 1, strip.position = "right") +
  scale_fill_viridis_c("Number of effect sizes") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6),
        legend.position = 'bottom') +
  labs(x = NULL, y = NULL) +
  ggtext::geom_richtext(aes(label = Nb_included), color = "white", size = 4)

# B. Map ----

p_layout <-   
ggplot() + 
  annotation_borders("world", colour = "gray40", fill = "gray80") +
  theme(axis.title = element_blank()) +
  scale_color_discrete("Food source tested") +
  scale_shape_manual("Included in the meta-analysis", values = c(23, 18)) +
  ylim(-60, NA) + 
  theme(legend.position = 'none')

points_map <- function(size) {
  geom_point(
    data = expTmt.table,
    aes(Longitude, Latitude, col = Tmt_sugar_name, shape = Included),
    size = size
  )
}

p_world <- p1 + points_map(size = 3) + theme(legend.position = 'bottom')
p_eu <- p1 + coord_quickmap(xlim = c(-10,25), ylim = c(35,65)) + points_map(size = 6) + theme(axis.text = element_blank())
p_usa <- p1 + coord_quickmap(xlim = c(-125, -75), ylim = c(30,55)) + points_map(size = 6) + theme(axis.text = element_blank())

ggsave('p_world.png', p_world,  unit = 'cm', width = 15, height = 9)
ggsave('p_eu.png', p_eu,  unit = 'cm', width = 6, height = 7)
ggsave('p_usa.png', p_usa,  unit = 'cm', width = 6, height = 6)


# C. Inspecting other moderators ----

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

