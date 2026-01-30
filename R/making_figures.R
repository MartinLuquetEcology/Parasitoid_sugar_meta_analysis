# A new script to make proper figures for publication

######################## #
###### Initiation ######
######################## #

### Clearing

rm(list = ls())

### Packages

if(!require('pacman')) { 
  install.packages('pacman') 
  library('pacman') 
} 

pacman::p_load(
  # Data manipulation
  tidyverse, 
  # Graphics
  ggpubr, ggtext, maps, scales, ggh4x
)

### User input

    # set to TRUE to save figures again
save_fig2 <- FALSE # fig. 2
save_fig3 <- FALSE # fig. 3
save_fig4 <- FALSE # fig. 4


#################### #
###### Figure 2 ######
#################### #

# Data needed (cf main script)
  # table with all treatments of all experiments
expTmt.table <- read_rds('Outputs/exp_tmt_tab.rds')
  # table with papers and their year of publication
paper.table <- read_rds('Outputs/paper_tab.rds')

# Functions
  # To plot the dynamics of publication over time
source('R/Functions/plotDyn.R')
  # To deal with point size on maps
source('R/Functions/pointsMap.R')

  # World map
fig_2_layout <-   
  ggplot() + 
  annotation_borders("world", colour = "gray60", fill = "gray80") +
  theme(axis.title = element_blank()) +
  scale_color_discrete("Food source tested") +
  scale_shape_manual("Included in the meta-analysis", values = c(23, 18)) +
  ylim(-60, NA) + 
  theme(legend.position = 'none')

fig_2_world <- fig_2_layout + 
  pointsMap(size = 2)

    # Extracting legends
fig_2_map_legend_fs <- 
  ggpubr::get_legend(
    fig_2_world + 
      theme(legend.position = 'bottom', legend.title = element_text(face = "bold")) +
      guides(
        col = guide_legend(title.position = "top", title.hjust = 0.5, title.vjust = 2),
        shape = 'none')
  ) %>% as_ggplot

fig_2_map_legend_incl <- 
  ggpubr::get_legend(
    fig_2_world + 
      theme(legend.position = 'bottom', legend.title = element_text(face = "bold")) +
      guides(
        shape = guide_legend(title.position = "top", title.hjust = 0.5, title.vjust = 2),
        col = 'none')
  ) %>% as_ggplot

  # Zooms on EU and USA
fig_2_eu <- fig_2_layout + 
  coord_quickmap(xlim = c(-10,25), ylim = c(35,65)) + 
  pointsMap(size = 3) + 
  theme(axis.text = element_blank())

fig_2_usa <- fig_2_layout + 
  coord_quickmap(xlim = c(-125, -75), ylim = c(30,55)) + 
  pointsMap(size = 3) + 
  theme(axis.text = element_blank())

  # Dynamics of publication
fig_2_dyn <- 
  plotDyn(paper.table, "Publication_year", "Year of publication", "papers", F) +
  labs(x = NULL, fill = NULL, title = NULL) + theme(legend.position = 'none') +
  theme(axis.text = element_text(size = 6), axis.title.y = element_text(size = 7))

fig_2_dyn_legend <- 
  ggpubr::get_legend(
    fig_2_dyn + 
      theme(legend.position = 'bottom', legend.title = element_text(face = "bold")) +
      guides(
        fill = guide_legend(title.position = "top", title.hjust = 0.5, title.vjust = 2))
  ) %>% as_ggplot


  # For now, all of these will be arranged by hand using PowerPoint
  # We recognise it's not best practice
  # Let's see if we get some time to do better

if(save_fig2) {
  
  # World
  ggsave(
    'Outputs/Figures/fig_2_world.png',
    fig_2_world,
    unit = 'cm',
    height = 9,
    width = 15,
    dpi = 600
  )
  
  # EU
  ggsave(
    'Outputs/Figures/fig_2_eu.png',
    fig_2_eu,
    unit = 'cm',
    height = 7,
    width = 6,
    dpi = 600
  )
  
  # USA
  ggsave(
    'Outputs/Figures/fig_2_usa.png',
    fig_2_usa,
    unit = 'cm',
    height = 6,
    width = 6,
    dpi = 600
  )
  
  # Map legends
  ggsave(
    'Outputs/Figures/fig_2_map_legend_fs.png',
    fig_2_map_legend_fs,
    dpi = 600
  )
  
  ggsave(
    'Outputs/Figures/fig_2_map_legend_incl.png',
    fig_2_map_legend_incl,
    dpi = 600
  )
  
  # Dynamics of publication
  ggsave(
    'Outputs/Figures/fig_2_dyn.png',
    fig_2_dyn,
    unit = 'cm',
    height = 4,
    width = 6,
    dpi = 600
  )
  
  # Dynamics of publication legend
  ggsave(
    'Outputs/Figures/fig_2_dyn_legend.png',
    fig_2_dyn_legend,
    dpi = 600
  )
  
}


#################### #
###### Figure 3 ######
#################### #

  # Merging previous fig. 3 and 4

# Data needed (cf main script)
  # Table used for meta analyses (with effect sizes)
meta.tab <- read_rds('Outputs/meta_tab.rds')
  # Distribution of treatments across experiments
tmt.distrib <- read_rds('Outputs/tmt_distrib_complete.rds')

  # Functions
    # A little function to reduce the size of 'n = ...' on plots
source('R/Functions/labellerN.R')


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

  # Making the figure
fig_3 <- 
  es_distrib_complete %>%
    # Re-ordering to plot variables in descending order of occurrences
  mutate(Variable = fct_reorder(Variable, Nb_included_na, .desc = T, .na_rm = T)) %>%
  ggplot(aes(x = Variable, y = paste0(Tmt_spatial, '\n(', Nb_exp, ')'), fill = Nb_included_na)) +
  geom_tile() +
  facet_wrap(~Tmt_sugar_name_recoded, ncol = 1, strip.position = "right",
             scales = 'free_y') +
  scale_fill_viridis_c("Number of effect sizes") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        legend.position = 'bottom') +
  ggtext::geom_richtext(aes(label = Nb_included, 
                            col = Nb_binary,
                            fontface = ifelse(Nb_binary == '0', 'plain', 'bold')
  ), 
  
  size = 4) +
  scale_color_manual(values = c('white','darkgrey')) +
  labs(x = NULL, y = NULL) +
  guides(col = 'none') +
    # Reducing 'n = ...' size using custom function labellerN()
  scale_y_discrete(labels = labellerN) +
  theme(axis.text.y = ggtext::element_markdown())

fig_3

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

  # Data needed (cf main script)
    # df used for meta analyses and results
allMa.mean.df <- read_rds('Outputs/ma_mean_df.rds')
allMA.mean.res <- read_rds('Outputs/ma_mean_res.rds')
allMa.var.df <- read_rds('Outputs/ma_var_df.rds')
allMA.var.res <- read_rds('Outputs/ma_var_res.rds')

  # Functions
    # To make the orchard plots we'll use this custom function
source('R/Functions/plotBeeSwarm.R')
    # To add vertical lines to guide interpretation
source('R/Functions/addPercentChange.R')


  # Mean figure
fig_4_mean <- 
  plotBeeSwarm(allMa.mean.df, allMA.mean.res, 'mean', 'lnRR') +
  # Adding independent facets using facet_grid2
  ggh4x::facet_grid2(
    fct_rev(Response)~., 
    scales = "free", switch = "y", independent = 'x'
  ) +
  theme(strip.text = element_text(size = 8)) +
  # Adding vertical lines (pct change) to guide interpretation
  addPercentChange(col = rep(c('blue', 'red'), 8)) + 
  # Doing this by hand (can't figure out a nice way with scales::pretty_breaks())
  scale_size_continuous(breaks = c(10, 30, 50))

  # Variance figure
fig_4_var <- 
  plotBeeSwarm(allMa.var.df, allMA.var.res, 'variance', 'lnCVR') +
  ggh4x::facet_grid2(fct_rev(Response)~., 
                     scales = "free", switch = "y",
                     independent = 'x',
                     # No text in facets as figures will be combined
                     strip = ggh4x::strip_nested(
                       text_y = element_blank()
                     ))  +
  addPercentChange(col = rep(c('blue', 'red'), 8)) +
  scale_size_continuous(breaks = c(1, 3, 5)) 

  # First combining, then adding the legend
fig_4_combined <- ggpubr::ggarrange(fig_4_mean, fig_4_var)

fig_4_legend <- plotBeeSwarm(allMa.mean.df, allMA.mean.res, NULL, NULL, 
                      move_legend = F) %>% 
  ggpubr::get_legend() %>%
  .$grobs %>%
  .[[2]]

fig_4 <- 
  ggpubr::ggarrange(
    fig_4_combined,
    fig_4_legend,
    ncol = 1,
    heights = c(20, 1.5)) +
  theme(panel.background = element_rect(fill='white'))


fig_4

  # Saving
if(save_fig4) {
  
  # World
  ggsave(
    'Outputs/Figures/fig_4.png',
    fig_4,
    unit = 'cm',
    height = 20,
    width = 16,
    dpi = 600
  )
  
}

