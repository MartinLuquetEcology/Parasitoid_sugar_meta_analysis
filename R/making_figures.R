# A new script to make proper figures for publication

############################### #
###### 0 - Initiation ######
############################### #

  # Clearing
rm(list = ls())

  # Packages
if(!require('pacman')) { 
  install.packages('pacman') 
  library('pacman') 
} 

pacman::p_load(
  # Data manipulation
  tidyverse, 
  # Graphics
  ggpubr, ggtext, maps
)

  # Data
  # cf main_script.R
paper.table <- read_rds('Outputs/paper_tab.rds')
meta.tab <- read_rds('Outputs/meta_tab.rds')
tmt.distrib <- read_rds('Outputs/tmt_distrib_complete.rds')
expTmt.table <- read_rds('Outputs/exp_tmt_tab.rds')

  # Functions
path.func <- "R/Functions"
files.func <- list.files(path.func)
path.files <- paste(path.func, files.func, sep = "/")
sapply(path.files, source)

  # User input
    # set to TRUE to save figures again
save_fig3 <- FALSE # fig. 3 
save_fig4 <- FALSE # fig. 4

#################### #
###### Figure 3 ######
#################### #

  # Merging previous fig. 3 and 4

  # Distribution of effect sizes
es_distrib <- 
  meta.tab %>%
  group_by(Variable, Tmt_sugar_name, Tmt_spatial) %>%
  summarise(Nb_included = n())

  # Adding missing cases
es_distrib_complete <- 
  es_distrib %>%
  ungroup %>%
    # Here
  complete(Variable, Tmt_spatial, Tmt_sugar_name, fill = list(Nb_included = 0)) %>%
    # + some additional recoding
  mutate(Tmt_sugar_name_recoded = 
           fct_recode(
             Tmt_sugar_name,
             'Artificial \nsugar source' = 'Artificial sugar source'
             )
         ) %>%
    # '0' can be changed to NA if we want to completely removed such cases from graph
    # but this will require some additional manipulation
  mutate(Nb_binary = ifelse(Nb_included == 0, '0', '>0')) %>%
  mutate(Nb_included_na = ifelse(Nb_included == 0, NA, Nb_included)) %>%
  mutate(Variable = gsub("_", " ", Variable))

  # Adding exp. info
es_distrib_complete <- 
tmt.distrib %>%
  mutate(Nb_exp = paste0('n = ', Nb_total, '/', Nb_included)) %>%
  dplyr::select(Tmt_spatial, Tmt_sugar_name, Nb_exp) %>%
  right_join(es_distrib_complete, by = c('Tmt_spatial', 'Tmt_sugar_name')) 

fig_3_init <- 
  es_distrib_complete %>%
  ggplot(aes(x = Variable, y = paste0(Tmt_spatial, '\n(', Nb_exp, ')'), fill = Nb_included_na)) +
  geom_tile() +
  facet_wrap(~Tmt_sugar_name_recoded, ncol = 1, strip.position = "right",
             scales = 'free_y') +
  scale_fill_viridis_c("Number of effect sizes") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.55),
        legend.position = 'bottom') +
  ggtext::geom_richtext(aes(label = Nb_included, 
                            col = Nb_binary,
                            fontface = ifelse(Nb_binary == '0', 'plain', 'bold')
                            ), 
                        
                        size = 4) +
  scale_color_manual(values = c('white','darkgrey')) +
  labs(x = NULL, y = NULL) +
  guides(col = 'none')

  # We add this to decrease the size of nb of exp
labeller_n <- function(x) {
  sub(
    "\n\\((n = [0-9]+/[0-9]+)\\)",
    "<br><span style='font-size:6pt'><i>(\\1)</i></span>",
    x
  )
}

fig_3 <- 
  fig_3_init +
  scale_y_discrete(labels = labeller_n) +
  theme(axis.text.y = ggtext::element_markdown())


if(save_fig3) {
  
  ggsave(
    'Outputs/Figures/fig_3.png',
    fig_3,
    unit = 'cm',
    height = 12,
    width = 16,
    dpi = 600
    )
  
}

#################### #
###### Figure 4 ######
#################### #

fig_4_layout <-   
  ggplot() + 
  annotation_borders("world", colour = "gray60", fill = "gray80") +
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

fig_4_world <- fig_4_layout + 
  points_map(size = 2)

fig_4_eu <- fig_4_layout + 
  coord_quickmap(xlim = c(-10,25), ylim = c(35,65)) + 
  points_map(size = 5) + 
  theme(axis.text = element_blank())

fig_4_usa <- fig_4_layout + 
  coord_quickmap(xlim = c(-125, -75), ylim = c(30,55)) + 
  points_map(size = 5) + 
  theme(axis.text = element_blank())

fig_4_map_legend_fs <- 
  ggpubr::get_legend(
    fig_4_world + 
      theme(legend.position = 'bottom', legend.title = element_text(face = "bold")) +
      guides(
        col = guide_legend(title.position = "top", title.hjust = 0.5, title.vjust = 2),
        shape = 'none')
  ) %>% as_ggplot

fig_4_map_legend_incl <- 
  ggpubr::get_legend(
    fig_4_world + 
      theme(legend.position = 'bottom', legend.title = element_text(face = "bold")) +
      guides(
        shape = guide_legend(title.position = "top", title.hjust = 0.5, title.vjust = 2),
        col = 'none')
    ) %>% as_ggplot

fig_4_dyn <- 
  plotDyn(paper.table, "Publication_year", "Year of publication", "papers", F) +
  labs(x = NULL, fill = NULL, title = NULL) + theme(legend.position = 'none')

fig_4_dyn_legend <- 
  ggpubr::get_legend(
    fig_4_dyn + 
      theme(legend.position = 'bottom', legend.title = element_text(face = "bold")) +
      guides(
        fill = guide_legend(title.position = "top", title.hjust = 0.5, title.vjust = 2))
  ) %>% as_ggplot
  

if(save_fig4) {
  
    # World
  ggsave(
    'Outputs/Figures/fig_4_world.png',
    fig_4_world,
    unit = 'cm',
    height = 9,
    width = 15,
    dpi = 600
  )
  
    # EU
  ggsave(
    'Outputs/Figures/fig_4_eu.png',
    fig_4_eu,
    unit = 'cm',
    height = 7,
    width = 6,
    dpi = 600
  )
  
    # USA
  ggsave(
    'Outputs/Figures/fig_4_usa.png',
    fig_4_usa,
    unit = 'cm',
    height = 6,
    width = 6,
    dpi = 600
  )
  
    # Map legends
  ggsave(
    'Outputs/Figures/fig_4_map_legend_fs.png',
    fig_4_map_legend_fs,
    dpi = 600
  )
  
  ggsave(
    'Outputs/Figures/fig_4_map_legend_incl.png',
    fig_4_map_legend_incl,
    dpi = 600
  )
  
  # Dynamics of publication
  ggsave(
    'Outputs/Figures/fig_4_dyn.png',
    fig_4_dyn,
    unit = 'cm',
    height = 4,
    width = 6,
    dpi = 600
  )
  
  # Dynamics of publication legend
  ggsave(
    'Outputs/Figures/fig_4_dyn_legend.png',
    fig_4_dyn_legend,
    dpi = 600
  )
  
}


