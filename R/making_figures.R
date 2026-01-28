# A new script to make proper figures for publication

######################## #
###### Initiation ######
######################## #

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
allMa.mean.df <- read_rds('Outputs/ma_mean_df.rds')
allMA.mean.res <- read_rds('Outputs/ma_mean_res.rds')
allMa.var.df <- read_rds('Outputs/ma_var_df.rds')
allMA.var.res <- read_rds('Outputs/ma_var_res.rds')

  # Functions
path.func <- "R/Functions"
files.func <- list.files(path.func)
path.files <- paste(path.func, files.func, sep = "/")
sapply(path.files, source)

  # User input
    # set to TRUE to save figures again
save_fig2 <- FALSE # fig. 2
save_fig3 <- FALSE # fig. 3
save_fig4 <- FALSE # fig. 4

#################### #
###### Figure 2 ######
#################### #

fig_2_layout <-   
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

fig_2_world <- fig_2_layout + 
  points_map(size = 2)

fig_2_eu <- fig_2_layout + 
  coord_quickmap(xlim = c(-10,25), ylim = c(35,65)) + 
  points_map(size = 3) + 
  theme(axis.text = element_blank())

fig_2_usa <- fig_2_layout + 
  coord_quickmap(xlim = c(-125, -75), ylim = c(30,55)) + 
  points_map(size = 3) + 
  theme(axis.text = element_blank())

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

# alternative : geom_pointrange + fatten (see old versions)

plot_beeswarm <- function(df, res, index, 
                          point_size = 2, ci_lwd = 1.2, pi_lwd = 0.4) {
  
  p <- 
    df %>%
    ggplot() +
    ggbeeswarm::geom_quasirandom(
      aes(y = yi, x = Response, size = 1/sqrt(vi), 
          colour = Response, fill = Response),
      alpha = 0.6) +
    geom_hline(yintercept = 0, lty = "dashed") +
    xlab(NULL) +
    coord_flip() +
    guides(fill = "none",
           colour = "none") +
    # PI
    geom_linerange(data = res, 
                   aes(x = Response, 
                       ymin = lowerPR, 
                       ymax = upperPR),
                   position = ggplot2::position_dodge2(width = 0.1),
                   linewidth = pi_lwd,
                   col = 'brown') +
    # CI
    geom_linerange(data = res, 
                   aes( 
                     x = Response, 
                     ymin = lowerCL, 
                     ymax = upperCL),
                   position = ggplot2::position_dodge2(width = 0.1),
                   linewidth = ci_lwd) +
    # Points
    geom_point(data = res, 
               aes(y = estimate, 
                   x = Response, 
                   shape = fct_rev(Overall_estimate)),
               position = ggplot2::position_dodge2(width = 0.1),
               size = point_size) +
    ggtitle(paste("Meta-analysis of", index)) +
    scale_shape_discrete("Overall estimate")
  
}

arrange_beeswarm <- function(plot) {
  
  plot +
    guides(shape = "none") +
    theme(legend.position.inside = c(1, 0),
          legend.justification.inside = c(1, 0),
          legend.position = "inside",
          legend.direction = "horizontal",
          legend.text = element_text(
            size = 6
          )) +
    labs(size = "Precision")
  
}



  #" A. Mean ----

fig_4_mean <- 
  plot_beeswarm(allMa.mean.df, allMA.mean.res, "mean") +
  facet_grid(fct_rev(metrics)~., space = "free", scales = "free", switch = "y") +
  ylab("lnRR") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(angle = 90, 
                                   hjust = 0.5, 
                                   size = 10))

fig_4_mean

#" B. Variance ----

fig_4_var <- 
  plot_beeswarm(allMa.var.df, allMA.var.res, "variance") +
  facet_grid(metrics~., space = "free", scales = "free", switch = "y") +
  ylab("lnCVR") +
  theme(legend.position = "bottom",
        axis.text.y = element_blank()) +
  scale_y_continuous(minor_breaks = seq(-3, 2, by = 1))

fig_4_var

#" C. Both ----

  # Combining figures
fig_4_combined <- 
  ggpubr::ggarrange(
      # Mean
    arrange_beeswarm(fig_4_mean) + 
      scale_size_continuous(breaks = c(10, 30, 50)), 
      # Variance
    arrange_beeswarm(fig_4_var) +
      scale_size_continuous(breaks = c(1, 3, 5)), widths = c(25, 24)
    )
                                    
  # Adding the legend
fig_4 <- 
  ggpubr::ggarrange(
    fig_4_combined,
    get_legend(fig_4_var)[[1]][[2]],
    ncol = 1,
    heights = c(20, 1)) +
  theme(panel.background = element_rect(fill='white'))

if(save_fig4) {
  
  ggsave(
    'Outputs/Figures/fig_4.png',
    fig_4,
    unit = 'cm',
    height = 16,
    width = 16,
    dpi = 600
  )
  
}

#" D. Alternative ----

library(ggh4x)

fig_4_2_mean <- 
plot_beeswarm(allMa.mean.df, allMA.mean.res, "mean") +
  ggh4x::facet_grid2(fct_rev(metrics) + Response~., 
                     scales = "free", switch = "y",
                     independent = 'x',
                     strip = ggh4x::strip_nested(
                       text_y = element_blank()
                     )) +
  ylab("lnRR") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(angle = 90, 
                                   hjust = 0.5, 
                                   size = 10))

fig_4_2_var <- 
  plot_beeswarm(allMa.var.df, allMA.var.res, "variance") +
  ggh4x::facet_grid2(metrics + Response~., 
                     scales = "free", switch = "y",
                     independent = 'x',
                     strip = ggh4x::strip_nested(
                       text_y = element_blank()
                     )) +
  ylab("lnCVR") +
  theme(legend.position = "bottom",
        axis.text.y = element_blank())

fig_4_2_combined <- 
  ggpubr::ggarrange(
    # Mean
    arrange_beeswarm(fig_4_2_mean) + 
      scale_size_continuous(breaks = c(10, 30, 50)), 
    # Variance
    arrange_beeswarm(fig_4_2_var) +
      scale_size_continuous(breaks = c(1, 3, 5)), widths = c(25, 24)
  )

# Adding the legend
fig_4_2 <- 
  ggpubr::ggarrange(
    fig_4_2_combined,
    get_legend(fig_4_2_var)[[1]][[2]],
    ncol = 1,
    heights = c(20, 1)) +
  theme(panel.background = element_rect(fill='white'))

if(save_fig4) {
  
  ggsave(
    'Outputs/Figures/fig_4_2.png',
    fig_4_2,
    unit = 'cm',
    height = 16,
    width = 16,
    dpi = 600
  )
  
}

#" Another alternative ----

fig_4_3_mean <- 
  plot_beeswarm(allMa.mean.df, allMA.mean.res, "mean") +
  ggh4x::facet_grid2(Response~., 
                     scales = "free", switch = "y",
                     independent = 'x') +
  ylab("lnRR") +
  theme(legend.position = "bottom",
        axis.text.y = element_blank()) +
  theme(strip.text.y = element_text(size = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(3)) +
  geom_hline(yintercept = log(1.25), lty = "dashed", col = 'red', lwd = 0.15) +
  geom_hline(yintercept = log(2), lty = "dashed", col = 'blue', lwd = 0.15) +
  geom_hline(yintercept = log(0.75), lty = "dashed", col = 'red', lwd = 0.15) +
  geom_hline(yintercept = log(0.5), lty = "dashed", col = 'blue', lwd = 0.15)

fig_4_3_var <- 
  plot_beeswarm(allMa.var.df, allMA.var.res, "variance") +
  ggh4x::facet_grid2(Response~., 
                     scales = "free", switch = "y",
                     independent = 'x',
                     strip = ggh4x::strip_nested(
                       text_y = element_blank()
                     )) +
  ylab("lnCVR") +
  theme(legend.position = "bottom",
        axis.text.y = element_blank()) +
  scale_y_continuous(breaks = scales::pretty_breaks(3)) +
  geom_hline(yintercept = log(1.25), lty = "dashed", col = 'red', lwd = 0.15) +
  geom_hline(yintercept = log(2), lty = "dashed", col = 'blue', lwd = 0.15) +
  geom_hline(yintercept = log(0.75), lty = "dashed", col = 'red', lwd = 0.15) +
  geom_hline(yintercept = log(0.5), lty = "dashed", col = 'blue', lwd = 0.15)



fig_4_3_combined <- 
  ggpubr::ggarrange(
    # Mean
    arrange_beeswarm(fig_4_3_mean) + 
      scale_size_continuous(breaks = c(10, 30, 50)), 
    # Variance
    arrange_beeswarm(fig_4_3_var) +
      scale_size_continuous(breaks = c(1, 3, 5)), widths = c(25, 24)
  )

# Adding the legend
fig_4_3 <- 
  ggpubr::ggarrange(
    fig_4_3_combined,
    get_legend(fig_4_3_var)[[1]][[2]],
    ncol = 1,
    heights = c(20, 1)) +
  theme(panel.background = element_rect(fill='white'))

if(save_fig4) {
  
  ggsave(
    'Outputs/Figures/fig_4_3.png',
    fig_4_3,
    unit = 'cm',
    height = 16,
    width = 16,
    dpi = 600
  )
  
}
