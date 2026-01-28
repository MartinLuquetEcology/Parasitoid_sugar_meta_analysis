############################### #
###### 0 - Initiation ######
############################### #

rm(list = ls())

  # To profile random effects, set to TRUE
profile_random <- FALSE
  # To run sensitivity analyses, set to TRUE
run_sens_ana <- FALSE
  # To load sensitivity analysis results, set to TRUE
load_sens_an <- TRUE
  # These objects will be needed to generate figures
    # To save them, set to TRUE
save_paper_table <- FALSE
save_exp_tmt_table <- FALSE
save_tmt_distrib <- FALSE
save_meta_table <- FALSE
save_meta_results <- TRUE

# For reproducibility (jitter, etc.)
set.seed(177)

################################ #
##### 1 - Loading libraries #####
################################ #

# Load pacman
if(!require('pacman')) { 
  install.packages('pacman') 
  library('pacman') 
} 

# To install orchaRd
  # note that it will install devtools as well
  # from https://daniel1noble.github.io/orchaRd/
if(!require('orchaRd')) {
  if(!require('devtools')) install.packages('devtools')
  devtools::install_github("daniel1noble/orchaRd", ref = "main", force = TRUE)
  pacman::p_load(devtools, tidyverse, metafor, patchwork, R.rsp, orchaRd, emmeans,
                 ape, phytools, flextable)
}

# Load packages
pacman::p_load(
    # Data manipulation
  tidyverse, 
    # Meta-analysis
  metafor, orchaRd, 
    # Graphics
  ggpubr, scales, ggtext, maps, ggnewscale
  )

############################### #
##### 2 - Loading the data #####
############################### #

  ## Loading the data

  # Data is downloaded from OSF if not already present in the Data folder
if( !file.exists("Data/data_main.csv") ) {
  
  if(!file.exists("Data")) dir.create("Data")
  
  pacman::p_load(osfr)
  osf_retrieve_file("https://osf.io/7sfgn?view_only=c61723dcf2e245d39d5462dc8f799d96") %>%
    osf_download(path = "Data")
  
}

full.table <- read_delim("Data/data_main.csv", 
                         delim = ";", escape_double = FALSE, 
                         locale = locale(decimal_mark = ","), 
                         trim_ws = TRUE)

  ## Functions
path.func <- "R/Functions"
files.func <- list.files(path.func)
path.files <- paste(path.func, files.func, sep = "/")
sapply(path.files, source)

############################### #
##### 3 - Preparing tables #####
############################### #

  ## --------------------------------------- #
  ###### '3A) Arranging the full dataset ----

  ### Let's add study and experiment ID numbers
full.table <- full.table %>%
    # For disambiguation between "Paper" and "Study"
  rename(Paper_name = "Study_Name") %>%
    # Paper ID
  mutate(
    # Paper ID
    Paper_ID = as.numeric(factor(Paper_name)),
    # Study ID: if two papers share at least one experiment they have the same ID
    sh_stud = ifelse(is.na(Shared_exp), Paper_name, Shared_exp),
    Study_ID = as.numeric(factor(sh_stud)),
    # Exp ID: if an experiment is shared across papers it has the same ID
    Exp_ID = as.numeric(factor(paste(sh_stud, Exp_Nb))))
    
  ### Giving full names to sugar treatments
full.table <- full.table %>%
  mutate(Tmt_sugar_name = fct_recode(Tmt_sugar, 
                                     'Artificial sugar source' = 'AS',
                                     'Honeydew' = 'H')
  )

  ### Keeping first year value when a season overlaps over two years
full.table <- full.table %>%
  mutate(Exp_year_cal =  as.numeric(sub("\\-.*", "", Exp_year)))
    # There are a few NAs because of unreported years
    # They won't be included in the meta-analysis anyway
  
    # We can see them here:
full.table %>%
  filter(is.na(Exp_year_cal)) %>%
  group_by(Paper_name, Exp_Nb, Included_in_meta_analysis) %>%
  summarise()

  ## ---------------------------------- #
  ###### '3B) Data subsets ----

  ### Data included in the meta-analysis
full.table.ma <- full.table %>%
  filter(Included_in_meta_analysis == "Yes")

### Paper-wise dataset
paper.table <- full.table %>%
    # Included will tell if at least some data is included in the meta-analysis
  group_by(Paper_name, Publication_year, Paper_ID, Study_ID) %>%
  mutate(Included = ifelse("Yes" %in% Included_in_meta_analysis, "Yes", "No")) %>% 
  group_by(Included, .add = T) %>%
  summarise()

  # saving for figures
if(save_paper_table) write_rds(paper.table, 'Outputs/paper_tab.rds')

### Study-wise dataset
study.table <- full.table %>%
  group_by(sh_stud, Study_ID) %>%
  mutate(Included = ifelse("Yes" %in% Included_in_meta_analysis, "Yes", "No")) %>% 
  group_by(Included, .add = T) %>%
  summarise()

### Experiment-wise dataset
exp.table <- full.table %>%
    # Just a lil trick here
    # The experiment is distributed over 2 fields very close together
    # We'll consider they have the same Long. and Lat. value for grouping purposes
  mutate(Longitude = ifelse(Paper_name == "Evans_et_al_2010", -112, Longitude),
         Latitude = ifelse(Paper_name == "Evans_et_al_2010", 41.86, Latitude)
         ) %>%
  group_by(sh_stud, Study_ID, Exp_Nb, Exp_ID, Latitude, Longitude,
           Exp_type_1, Exp_type_2) %>%
  mutate(Included = ifelse("Yes" %in% Included_in_meta_analysis, "Yes", "No"),
         FirstY = Exp_year_cal[1]
         ) %>% 
  group_by(Included, FirstY, .add = T) %>%
  summarise()

### Experiment * treatment wise dataset
  ### The thing here is that different treatments may have been tested in the same exp.
  ### e.g. Cappuccino et al. 2003
expTmt.table <- full.table %>%
  mutate(Longitude = ifelse(Paper_name == "Evans_et_al_2010", -112, Longitude),
         Latitude = ifelse(Paper_name == "Evans_et_al_2010", 41.86, Latitude)
  ) %>%
  group_by(sh_stud, Study_ID, Exp_Nb, Exp_ID, Latitude, Longitude, Exp_type_1, Exp_type_2,
           Tmt_sugar_name, Tmt_spatial) %>%
  mutate(Included = ifelse("Yes" %in% Included_in_meta_analysis, "Yes", "No"),
         FirstY = Exp_year_cal[1]
  ) %>% 
  group_by(Included, FirstY, .add = T) %>%
  summarise()

  # saving for figures
if(save_exp_tmt_table) write_rds(expTmt.table, 'Outputs/exp_tmt_tab.rds')

### Experiment-season
expSeas.table <- full.table %>%
  group_by(sh_stud, Study_ID, Exp_ID, Latitude, Longitude, 
           Exp_type_1, Exp_type_2, Exp_Nb, Exp_rep) %>%
  # There are some cases when the same experiment-season runs over several years
  # In such cases we keep track of the first year only
  mutate(Exp_year_cal = Exp_year_cal[1],
         Included = ifelse("Yes" %in% Included_in_meta_analysis, "Yes", "No")
  ) %>%
  group_by(Exp_year_cal, Included, .add = T) %>% 
  summarise() %>%
  # We remove cases with unreported years, at least for now
  filter(!is.na(Exp_year_cal))


### Experiment-season * treatment wise dataset
expTmtSeas.table <- full.table %>%
  mutate(Longitude = ifelse(Paper_name == "Evans_et_al_2010", -112, Longitude),
         Latitude = ifelse(Paper_name == "Evans_et_al_2010", 41.86, Latitude)
  ) %>%
  group_by(sh_stud, Study_ID, Exp_ID, Latitude, Longitude, 
           Exp_type_1, Exp_type_2, Exp_Nb, Exp_rep,
           Tmt_sugar_name, Tmt_spatial) %>%
  # There are some cases when the same experiment-season runs over several years
  # In such cases we keep track of the first year only
  mutate(Exp_year_cal = Exp_year_cal[1],
         Included = ifelse("Yes" %in% Included_in_meta_analysis, "Yes", "No")
  ) %>%
  group_by(Exp_year_cal, Included, .add = T) %>% 
  summarise() %>%
  # We remove cases with unreported years, at least for now
  filter(!is.na(Exp_year_cal))

########################################################################## #
##### 4 - Dataset description - Studies and experiments ####################
########################################################################## #

  ## ---------------------------------- #
  ###### '4A) General overview ----

  ### First, let's look at the number of experiments and studies
full.table %>%
  summarise(Nb_Papers = n_distinct(Paper_ID),
         Nb_Studies = n_distinct(Study_ID),
         Nb_Exp = n_distinct(Exp_ID),
         Nb_ExpSeas = n_distinct(paste(Exp_ID, Exp_rep))
  ) %>%
  mutate(Subset = "All papers") %>%
  bind_rows(
    full.table %>%
      filter(Included_in_meta_analysis == "Yes") %>%
      summarise(Nb_Papers = n_distinct(Paper_ID),
                Nb_Studies = n_distinct(Study_ID),
                Nb_Exp = n_distinct(Exp_ID),
                Nb_ExpSeas = n_distinct(paste(Exp_ID, Exp_rep))) %>%
      mutate(Subset = "Included in meta analysis")
  ) %>%
  select(Subset, everything())

  ## ------------------------------------------------ #
  ###### '4B) World distribution of experiments ----

  ### Let's generate a world map of all experiments
ggplot() + 
  borders("world", colour = "gray40", fill = "gray80") +
  geom_point(
    data = expTmt.table,
    aes(x = Longitude, 
        y = Latitude,
        col = Tmt_sugar_name,
        shape = Included),
    size = 5) +
  theme(axis.title = element_blank()) +
  scale_color_discrete("Food source tested") +
  scale_shape_manual("Included in the meta-analysis", values = c(23, 18)) +
  ylim(-60, NA)

    ## ---------------------------------------------- #
    ###### '4C) Dynamics of publication over time ----
  
  ### Let's look at the evolution in the number of studies and experiments over time
    ## To do so we use the custom plotDyn() function

    ## Publication year of studies

      # All papers
  plotDyn(paper.table, "Publication_year", "Year of publication", "papers", F)

      # Meta-analysis only
  plotDyn(paper.table, "Publication_year", "Year of publication", "papers", T)

    ## First year of experiments

      # All studies
  plotDyn(exp.table, "FirstY", "First year of experiment", "experiments", F)

      # Meta-analysis only
  plotDyn(exp.table, "FirstY", "First year of experiment", "experiments", T)

    ## Experimental years
  
      # All studies
  plotDyn(expSeas.table, "Exp_year_cal", "Experimental year", "experiment-seasons", F)
  
      # Meta-analysis only
  plotDyn(expSeas.table, "Exp_year_cal", "Experimental year", "experiment-seasons", T)
  
  ## -------------------------------------------------------- #
  ###### '4D) Distribution of experiments over treatments ----

    # Quick overview:
tmt.distrib <- 
expTmt.table %>%
  group_by(Tmt_sugar_name, Tmt_spatial) %>%
  mutate(Nb_total = n()) %>%
  filter(Included == "Yes") %>%
  group_by(Nb_total, .add = T) %>%
  summarise(Nb_included = n())

tmt.distrib

  # Adding missing cases
tmt.distrib.complete <- 
tmt.distrib %>%
  # As there is only one study for honeydew
  # It becomes uneligible for meta-analysis
  mutate(Nb_included = ifelse(Tmt_sugar_name == "Honeydew", 0, Nb_included)) %>%
  ungroup %>%
  complete(Tmt_spatial, Tmt_sugar_name, fill = list(Nb_total = 0,
                                                    Nb_included = 0))

if(save_tmt_distrib) write_rds(tmt.distrib.complete, 'Outputs/tmt_distrib_complete.rds')

    # Systematic map:
tmt.distrib.complete %>%
  mutate(Nb_included = ifelse(Tmt_sugar_name == "Honeydew", 0, Nb_included)) %>%
  ungroup() %>%
  complete(Tmt_spatial, Tmt_sugar_name, fill = list(Nb_total = 0,
                                                    Nb_included = 0)) %>%
  mutate(Nb = paste0("<b>", Nb_included, "</b> ", ' (', Nb_total, ')')) %>%
  ggplot(aes(x = Tmt_spatial, y = Tmt_sugar_name, fill = Nb_included)) +
  geom_tile() +
  ggtext::geom_richtext(aes(label = Nb), color = "white", size = 4)  +
  scale_fill_gradient("Number eligible for MA",
                      low = "#075AFF",
                      high = "#FF0000") +
  ggtitle("Number of experiments for each treatment",
          subtitle = "<b>Eligible for meta-analysis</b> (total)") +
  xlab("Spatial treatment") +
  ylab("Food source treatment") +
  theme(
    plot.subtitle = element_markdown()
  )

  ### Let us also look how experiments are distributed between different study designs
ggplot(exp.table, aes(x = Exp_type_1, fill = Exp_type_2)) +
  geom_bar() + 
  xlab(NULL) + 
  ylab("Number of experiments") +
  scale_fill_discrete("Type of experiment") +
  ggtitle("Number of experiment of each type")

######################################################## #
##### 5 - Dataset description - Systems and Insects ######
######################################################## #

  ### Here, let's look at the distribution of model systems 
  ### and species studied across experiments
  ### We'll work only on the data included in the meta analysis here

  ## ---------------------------------- #
  ###### '5A) Cropping systems ----

    ### First let's look at the studied agroecosystems (crop types)

# First we make a table at the experiment*crop level
  # Note that in some cases multiple crops can be studied in the same experiment
  # e.g. Pollier et al. 2019 or Tilman et al. 2015
  # So the numbers here are the number of experiments including each crop
expCrop.tab <- full.table.ma %>%
  group_by(sh_stud, Exp_ID, Crop_type_1, Crop_type_2, Crop) %>%
  summarise() %>%
  arrange(Exp_ID)

  # Here is a table to have the frequency of each crop:
expCrop.tab %>%
  group_by(Crop_type_1, Crop_type_2, Crop) %>%
  summarise(nb = n()) %>%
  arrange(desc(nb))

  # And then a graphics:
ggplot(expCrop.tab, aes(x = Crop_type_2, fill = Crop_type_2)) +
  facet_grid(~ Crop_type_1, scale = "free", space = 'free') +
  geom_bar() + 
  theme(axis.title = element_blank()) +
  ggtitle("Number of occurrences when a given crop was studied",
          subtitle = "Experiment level") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = scales::breaks_pretty())


  # Let's do the same but adding an 'Other' Category
  # To show only the main crop types (at least 3 experiments)
expCrop.tab <- expCrop.tab %>%
  group_by(Crop_type_1, Crop_type_2) %>%
  mutate(Crop_type_main = ifelse(n() >= 3, Crop_type_2, "Other"))

ggplot(expCrop.tab, aes(x = Crop_type_main, fill = Crop_type_main)) +
  facet_grid(~Crop_type_1, scale="free", space='free') +
  geom_bar()+
  theme(axis.title = element_blank()) +
  ggtitle("Number of occurrences when a given crop was studied",
          subtitle = "Experiment level") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = scales::breaks_pretty())

# Let's do the same thing at the experiment*season level
expCropSeas.tab <- full.table.ma %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Crop_type_1, Crop_type_2, Crop) %>%
  summarise() %>%
  arrange(Exp_ID) %>%
  group_by(Crop_type_1, Crop_type_2) %>%
  mutate(Crop_type_main = ifelse(n() >= 3, Crop_type_2, "Other"))

  #Table: 
expCropSeas.tab %>%
  group_by(Crop_type_1, Crop_type_2, Crop) %>%
  summarise(nb = n()) %>%
  arrange(desc(nb))

  # Graphics:
expCropSeas.tab %>%
  ggplot(aes(x = Crop_type_main, fill = Crop_type_main)) +
  facet_grid(~Crop_type_1, scale="free", space='free') +
  geom_bar()+
  theme(axis.title = element_blank()) +
  theme(axis.title = element_blank()) +
  ggtitle("Number of occurrences when a given crop was studied",
          subtitle = "Experiment*season level") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = scales::breaks_pretty())


  # Now let's look at what crop species were studied
  # at the experiment*season level
  # Again we create an "Other" category here (at least 3 experiments)
expCropSeas.tab <- expCropSeas.tab %>%
  group_by(Crop_type_1, Crop_type_main, Crop) %>%
  mutate(Crop_main = ifelse(n() >= 3, Crop, "Other")) %>%
  mutate(Crop_main_name = gsub("_", " ", Crop_main)) %>%
  mutate(Crop_main_name = ifelse(Crop_main_name == "Other",
                                 Crop_main_name,
                                 paste0("italic('", Crop_main_name, "')")))

  # Table:
expCropSeas.tab %>%
  group_by(Crop_type_1, Crop_type_2, Crop) %>%
  summarise(nb = n()) %>%
  arrange(desc(nb))

  # Graphics:
ggplot(expCropSeas.tab, aes(x = Crop_type_main, fill = fct_rev(Crop_main_name))) +
  facet_grid(~ Crop_type_1, scale = "free", space = 'free') +
  geom_bar() + 
  theme(axis.title = element_blank()) +
    # italicsGraph: a custom wrapper to write species name in italics
  scale_fill_discrete(labels = italicsGraph,
                      name = "Crop species") +
  ggtitle("Number of occurrences when a given crop species was studied",
          subtitle = "Experiment*season level") +
  scale_y_continuous(breaks = scales::breaks_pretty())

  ## ---------------------------------- #
  ###### '5B) Pests ----

    ### Now let's look at what pest insects are studied

      # First we make a table at the pest family level
      # We create a table to consider any pest family occurrence in a given experiment,
      # and season, regardless of variable
      # (Pest density, damage, parasitism rate, sex ratio)
      # We de-duplicate any pest studied in any experiment and season
expPestFam.tab <- full.table.ma %>%
  filter(!is.na(Pest)) %>%
  group_by(sh_stud, Exp_ID, Pest_order, Pest_family) %>%
  summarise() %>%
  arrange(Exp_ID)

# Table:
expPestFam.tab %>%
  group_by(Pest_order, Pest_family) %>% 
  summarise(nb = n()) %>%
  arrange(desc(nb))

# Graphics:
expPestFam.tab %>%
  group_by(Pest_family) %>%
  mutate(nb = n()) %>%
  mutate(Pest_family_main = ifelse(nb >= 3, Pest_family, "Other")) %>%
  mutate(Pest_order_family = paste(Pest_order, Pest_family_main, sep = ": ")) %>%
  ggplot(aes(x = Pest_order, fill = Pest_order_family)) + 
  geom_bar() +
  theme(axis.title = element_blank()) +
  ggtitle("Number of occurrences when a given pest family was studied",
          subtitle = "Experiment level") +
  scale_fill_discrete("Pest family") +
  scale_y_continuous(breaks = scales::breaks_pretty())

# Same at the experiment*season level
expSeasPestFam.tab <- full.table.ma %>%
  filter(!is.na(Pest)) %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Pest_order, Pest_family) %>%
  summarise() %>%
  arrange(Exp_ID)

# Table:
expSeasPestFam.tab %>%
  group_by(Pest_order, Pest_family) %>% 
  summarise(nb = n()) %>%
  arrange(desc(nb))

# Graphics: 
expSeasPestFam.tab %>%
  group_by(Pest_family) %>%
  mutate(nb = n()) %>%
  mutate(Pest_family_main = ifelse(nb >= 3, Pest_family, "Other")) %>%
  mutate(Pest_order_family = paste(Pest_order, Pest_family_main, sep = ": ")) %>%
  ggplot(aes(x = Pest_order, fill = Pest_order_family)) + 
  geom_bar() +
  theme(axis.title = element_blank()) +
  ggtitle("Number of occurrences when a given pest family was studied",
          subtitle = "Experiment level * season") +
  scale_fill_discrete("Pest family") +
  scale_y_continuous(breaks = scales::breaks_pretty()) 

# What lies in the "Various" category ?
  # Other orders represented, e.g. Orthoptera
full.table.ma %>%
  filter(Pest_order == "Various") %>%
  group_by(sh_stud, Pest_order, Pest_species_detail) %>%
  summarise()

  ### Now let's see what pest species were mostly studied
    # Keeping only measurements made on one species only

  # Experiment level

# Table
full.table.ma %>%
  filter(Pest_N == "Single") %>%
  group_by(sh_stud, Exp_ID, Pest_order, Pest_family, Pest_genus, Pest) %>%
  summarise() %>%
  group_by(Pest_order, Pest_family, Pest) %>%
  summarise(nb = n()) %>%
  arrange(desc(nb))

# Graphics
full.table.ma %>%
  filter(Pest_N == "Single") %>%
  group_by(sh_stud, Exp_ID, Pest_order, Pest_family, Pest_genus, Pest) %>%
  summarise() %>%
  arrange(Exp_ID) %>%
  ungroup() %>%
  group_by(Pest) %>%
  mutate(nb = n(),
         Pest_main = ifelse(nb >=3, Pest, "Other"),
         Pest_name = ifelse(Pest_main == "Other",
                            Pest_main,
                            paste0("italic('", Pest_main, "')")),
         Pest_name = gsub("_", " ", Pest_name)) %>% 
  ggplot(aes(y = Pest_family, fill = fct_rev(Pest_name))) + 
  facet_grid(Pest_order ~., space = "free", scales = "free") +
  geom_bar() +
  theme(axis.title = element_blank()) + 
  scale_fill_discrete("Pest species",
                      labels = italicsGraph) +
  ggtitle("Species that were studied the most",
          subtitle = "Experiment level") +
  xlab("Number of occurrences") +
  scale_x_continuous(breaks = scales::breaks_pretty())

  # Season level

# Table
full.table.ma %>%
  filter(Pest_N == "Single") %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Pest_order, Pest_family, Pest_genus, Pest) %>%
  summarise() %>%
  group_by(Pest_order, Pest_family, Pest) %>%
  summarise(nb = n()) %>%
  arrange(desc(nb))

# Graphics
full.table.ma %>%
  filter(Pest_N == "Single") %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Pest_order, Pest_family, Pest_genus, Pest) %>%
  summarise() %>%
  arrange(Exp_ID) %>%
  ungroup() %>%
  group_by(Pest) %>%
  mutate(nb = n(),
         Pest_main = ifelse(nb >=3, Pest, "Other"),
         Pest_name = ifelse(Pest_main == "Other",
                            Pest_main,
                            paste0("italic('", Pest_main, "')")),
         Pest_name = gsub("_", " ", Pest_name)) %>% 
  ggplot(aes(y = Pest_family, fill = fct_rev(Pest_name))) + 
  facet_grid(Pest_order ~., space = "free", scales = "free") +
  geom_bar() +
  theme(axis.title = element_blank()) + 
  scale_fill_discrete("Pest species",
                      labels = italicsGraph) +
  ggtitle("Species that were studied the most",
          subtitle = "Season level")


# Let's have a look at cases when species where studied in pools
pooled_species_pest <- 
full.table.ma %>%
  filter(!is.na(Pest_species_detail)) %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Pest_species_detail) %>%
  summarise() %>%
  filter(grepl("_", Pest_species_detail))

unlist(str_split(pooled_species_pest$Pest_species_detail, ",\\s*")) %>%
  grep("_", ., value = TRUE) %>%
  gsub("_\\([^)]*\\)", "", .) %>%
  .[!grepl("spp.", .)] %>% # Only one case : Manduca_spp.
  tibble(Species = .) %>%
  count(Species) %>%
  arrange(desc(n))

  ## ---------------------------------- #
  ###### '5C) Parasitoids ----

    ### Now let's look at what parasitoid insects are studied
      ## We follow the same procedure

      ## Let's first look how many occurrence of parasitoid orders we have
        ## at the experiment-season
full.table.ma %>%
  filter(!is.na(Parasitoid)) %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Parasitoid_order) %>%
  summarise() %>%
  group_by(Parasitoid_order) %>%
  summarise(nb = n())
    # As we could expect, mainly Hymenoptera

    ### Let's visualise this, first at the Experiment level, then at the 
    ### Experiment level
full.table.ma %>%
  filter(!is.na(Parasitoid)) %>%
  group_by(sh_stud, Exp_ID, Parasitoid_order, Parasitoid_family) %>%
  summarise() %>%
  ungroup() %>%
  group_by(Parasitoid_order) %>%
  mutate(nb_order = n()) %>%
  group_by(paste(Parasitoid_order, Parasitoid_family)) %>%
  mutate(nb_family = n()) %>%
  mutate(Parasitoid_order_main = ifelse(nb_order >= 10, Parasitoid_order, "Other")) %>%
  mutate(Parasitoid_family_main = ifelse(Parasitoid_order_main != "Other",
                                    ifelse(nb_family >= 3,
                                           paste(Parasitoid_order_main, Parasitoid_family, sep = ": "),
                                           "Other"),
                                    Parasitoid_order)) %>%
  ggplot(aes(x = Parasitoid_order_main, fill = Parasitoid_family_main)) + 
  geom_bar() +
  theme(axis.title = element_blank()) +
  ggtitle("Number of occurrences when a given Parasitoid order or family was studied",
          subtitle = "Experiment level") +
  scale_fill_discrete("Parasitoid order/family") +
  guides(fill = guide_legend(ncol = 1))

full.table.ma %>%
  filter(!is.na(Parasitoid)) %>%
  group_by(sh_stud, Exp_ID, Parasitoid_order, Parasitoid_family) %>%
  summarise() %>%
  group_by(Parasitoid_order) %>%
  mutate(nb_order = n()) %>%
  group_by(paste(Parasitoid_order, Parasitoid_family)) %>%
  mutate(nb_family = n()) %>%
  group_by(Parasitoid_order, Parasitoid_family, nb_order, nb_family) %>%
  summarise() %>%
  arrange(desc(nb_family))

### Season level
full.table.ma %>%
  filter(!is.na(Parasitoid)) %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Parasitoid_order, Parasitoid_family) %>%
  summarise() %>%
  ungroup() %>%
  group_by(Parasitoid_order) %>%
  mutate(nb_order = n()) %>%
  group_by(paste(Parasitoid_order, Parasitoid_family)) %>%
  mutate(nb_family = n()) %>%
  mutate(Parasitoid_order_main = ifelse(nb_order >= 20, Parasitoid_order, "Other")) %>%
  mutate(Parasitoid_family_main = ifelse(Parasitoid_order_main != "Other",
                                         ifelse(nb_family >= 3,
                                                paste(Parasitoid_order_main, Parasitoid_family, sep = ": "),
                                                "Other"),
                                         Parasitoid_order)) %>%
  ggplot(aes(x = Parasitoid_order_main, fill = Parasitoid_family_main)) + 
  geom_bar() +
  theme(axis.title = element_blank()) +
  ggtitle("Number of occurrences when a given Parasitoid order or family was studied",
          subtitle = "Experiment-Season level") +
  scale_fill_discrete("Parasitoid order/family") +
  guides(fill = guide_legend(ncol = 1))

full.table.ma %>%
  filter(!is.na(Parasitoid)) %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Parasitoid_order, Parasitoid_family) %>%
  summarise() %>%
  group_by(Parasitoid_order) %>%
  mutate(nb_order = n()) %>%
  group_by(paste(Parasitoid_order, Parasitoid_family)) %>%
  mutate(nb_family = n()) %>%
  group_by(Parasitoid_order, Parasitoid_family, nb_order, nb_family) %>%
  summarise() %>%
  arrange(desc(nb_family))

### Now let's see what parasitoid species were mostly studied
# Keeping only measurements made on one species only

# Experiment
full.table.ma %>%
  filter(Parasitoid_N == "Single") %>%
  group_by(sh_stud, Exp_ID, Parasitoid_order, Parasitoid_family, Parasitoid_genus, Parasitoid) %>%
  summarise() %>%
  arrange(Exp_ID) %>%
  ungroup() %>%
  group_by(Parasitoid) %>%
  mutate(nb = n(),
         Parasitoid_main = ifelse(nb >=3, Parasitoid, "Other"),
         Parasitoid_name = ifelse(Parasitoid_main == "Other",
                            Parasitoid_main,
                            paste0("italic('", Parasitoid_main, "')")),
         Parasitoid_name = gsub("_", " ", Parasitoid_name)) %>% 
  ggplot(aes(y = Parasitoid_family, fill = fct_rev(Parasitoid_name))) + 
  facet_grid(Parasitoid_order ~., space = "free", scales = "free") +
  geom_bar() +
  theme(axis.title = element_blank()) + 
  scale_fill_discrete("Parasitoid species",
                      labels = italicsGraph) +
  ggtitle("Species that were mostly studied",
          subtitle = "Experiment level")

full.table.ma %>%
  filter(Pest_N == "Single") %>%
  group_by(sh_stud, Exp_ID, Pest_order, Pest_family, Pest_genus, Pest) %>%
  summarise() %>%
  group_by(Pest_order, Pest_family, Pest) %>%
  summarise(nb = n()) %>%
  arrange(desc(nb))

# Season level
full.table.ma %>%
  filter(Parasitoid_N == "Single") %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Parasitoid_order, Parasitoid_family, Parasitoid_genus, Parasitoid) %>%
  summarise() %>%
  arrange(Exp_ID) %>%
  ungroup() %>%
  group_by(Parasitoid) %>%
  mutate(nb = n(),
         Parasitoid_main = ifelse(nb >=3, Parasitoid, "Other"),
         Parasitoid_name = ifelse(Parasitoid_main == "Other",
                            Parasitoid_main,
                            paste0("italic('", Parasitoid_main, "')")),
         Parasitoid_name = gsub("_", " ", Parasitoid_name)) %>% 
  ggplot(aes(y = Parasitoid_family, fill = fct_rev(Parasitoid_name))) + 
  facet_grid(Parasitoid_order ~., space = "free", scales = "free") +
  geom_bar() +
  theme(axis.title = element_blank()) + 
  scale_fill_discrete("Parasitoid species",
                      labels = italicsGraph) +
  ggtitle("Species that were mostly studied",
          subtitle = "Experiment * season level")

full.table.ma %>%
  filter(Parasitoid_N == "Single") %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Parasitoid_order, Parasitoid_family, Parasitoid_genus, Parasitoid) %>%
  summarise() %>%
  group_by(Parasitoid_order, Parasitoid_family, Parasitoid) %>%
  summarise(nb = n()) %>%
  arrange(desc(nb))

# Let's have a look at cases when species where studied in pools
pooled_species_para <- 
  full.table.ma %>%
  filter(!is.na(Parasitoid_species_detail)) %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Parasitoid_species_detail) %>%
  summarise() %>%
  filter(grepl("_", Parasitoid_species_detail))

unlist(str_split(pooled_species_para$Parasitoid_species_detail, ",\\s*")) %>%
  grep("_", ., value = TRUE) %>%
  gsub("_\\([^)]*\\)", "", .) %>%
  .[!grepl("spp.", .)] %>%
  tibble(Species = .) %>%
  count(Species) %>%
  arrange(desc(n))

  ## ---------------------------------- #
  ###### '5D) Nectar-producing plants ----

# Again, let's get unique nectar plants for each experiment or season
nectLevel.tab <- full.table.ma %>%
  filter(Tmt_sugar == "Nectar") %>%
  group_by(sh_stud, Exp_ID, Nectar_plant_family, Nectar_producing_plant,
           Nectar_producing_plant_status, Nectar_providing_plants_diversity,
           Management_type, Resource_selection_main, Tmt_spatial) %>%
  summarise() %>%
  arrange(Exp_ID)

    # Let's look at several things about these nectar plants

     # Management type
ggplot(nectLevel.tab,
       aes(y = Management_type,
           fill = Nectar_producing_plant_status)) +
  facet_grid(Tmt_spatial~., scales = 'free' ,space = 'free') +
  geom_bar()+
  xlab(NULL) +
  ylab("Way of providing nectar")

    # Reason for choosing
ggplot(nectLevel.tab,
       aes(y = Resource_selection_main,
           fill = Nectar_producing_plant_status)) +
  facet_grid(Tmt_spatial~., scales = 'free' ,space = 'free') +
  geom_bar()+
  xlab(NULL) +
  ylab("Main reason why nectar plants were chosen")

    # Plant diversity
ggplot(nectLevel.tab,
       aes(y = as.numeric(Nectar_providing_plants_diversity),
           fill = Nectar_producing_plant_status)) +
  facet_grid(Tmt_spatial~.) +
  geom_bar()+
  xlab(NULL) +
  ylab("Nectar plant diversity")

  # Now, let's look at what nectar plants are studied

    # All experiments
nectLevel.tab <- nectLevel.tab %>%
  group_by(Nectar_producing_plant) %>%
  mutate(Nectar_main = ifelse(n() >= 3, Nectar_producing_plant, "Other")) %>%
  mutate(Nectar_main_name = gsub("_", " ", Nectar_main)) %>%
  mutate(Nectar_main_name = ifelse(!(grepl(" ", Nectar_main_name)),
                                 Nectar_main_name,
                                 paste0("italic('", Nectar_main_name, "')"))) %>%
  group_by(Nectar_plant_family) %>%
  mutate(Nectar_family_main = ifelse(n() >= 3, Nectar_plant_family, "Other"))

ggplot(nectLevel.tab, aes(x = Nectar_family_main, fill = fct_rev(Nectar_main_name))) +
  geom_bar() + 
  xlab(NULL) +
  ylab("Number of occurrences") +
  scale_fill_discrete(labels = italicsGraph,
                      name = "Nectar species") +
  ggtitle("Most studied nectar-producing plants",
          subtitle = "Experiment level")

    # Same at seasonal level
full.table.ma %>%
  filter(Tmt_sugar == "Nectar") %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Nectar_plant_family, Nectar_producing_plant,
           Nectar_producing_plant_status, Nectar_providing_plants_diversity,
           Management_type, Resource_selection_main, Tmt_spatial) %>%
  summarise() %>%
  arrange(Exp_ID) %>%
  group_by(Nectar_producing_plant) %>%
  mutate(Nectar_main = ifelse(n() >= 3, Nectar_producing_plant, "Other")) %>%
  mutate(Nectar_main_name = gsub("_", " ", Nectar_main)) %>%
  mutate(Nectar_main_name = ifelse(!(grepl(" ", Nectar_main_name)),
                                   Nectar_main_name,
                                   paste0("italic('", Nectar_main_name, "')"))) %>%
  group_by(Nectar_plant_family) %>%
  mutate(Nectar_family_main = ifelse(n() >= 3, Nectar_plant_family, "Other")) %>%
  ggplot(aes(x = Nectar_family_main, fill = fct_rev(Nectar_main_name))) +
  geom_bar() + 
  xlab(NULL) +
  ylab("Number of occurrences") +
  scale_fill_discrete(labels = italicsGraph,
                      name = "Nectar species") +
  ggtitle("Most studied nectar-producing plants",
          subtitle = "Seasonal level")

    # More details in this table
nectLevel.tab %>%
  group_by(Nectar_plant_family, Nectar_producing_plant) %>%
  summarise(nb = n()) %>%
  arrange(desc(nb))
  
full.table.ma %>%
  filter(Tmt_sugar == "Nectar") %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Nectar_plant_family, Nectar_producing_plant,
           Nectar_producing_plant_status, Nectar_providing_plants_diversity,
           Management_type, Resource_selection_main, Tmt_spatial) %>%
  summarise() %>%
  group_by(Nectar_plant_family, Nectar_producing_plant) %>%
  summarise(nb = n()) %>%
  arrange(desc(nb))

# For now we only displayed plants that were studied as a sole nectar source
# And groupped plants that were studied in mixes in the "Various" category
# Let's look more closely at what plants were in those mixes
# Note that this may include nectar that do not produce nectar (e.g. herbs)
pooled.species.nectar <- 
full.table.ma %>%
  filter(!is.na(Nectar_plants_detail)) %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Nectar_plants_detail) %>%
  summarise()  %>%
  filter(grepl("_", Nectar_plants_detail))

  # They will all be stored in this table
all.nectar.plants <- 
unlist(str_split(pooled.species.nectar$Nectar_plants_detail, ",\\s*")) %>%
  grep("_", ., value = TRUE) %>%
  gsub("_\\([^)]*\\)", "", .) %>%
  .[!grepl("spp.", .)] %>%
  tibble(Species = .) %>%
  count(Species) %>%
  arrange(desc(n)) %>%
  mutate(Species_italics = gsub("_", " ", Species)) %>%
  mutate(Species_italics = ifelse(!(grepl(" ", Species_italics)),
                                   Species_italics,
                                   paste0("italic('", Species_italics, "')")))

head(all.nectar.plants, 10)

  # Graphical depiction
    # (Only plants included in at least 7 experiment-seasons)
all.nectar.plants %>%
  filter(n > 7) %>%
  ggplot(aes(x = reorder(Species_italics, n, decreasing = T),
             y = n)) +
  geom_bar(stat = "identity") + 
  labs(title = "Plants most included in nectar-producing mixes",
       subtitle = "Seasonal level",
       x = NULL,
       y = "Number of occurrences") +
  scale_x_discrete(labels = italicsGraph)

  ## ---------------------------------- #
  ###### '5E) Artificial sugar sources ----

  # Experiment level
    # Here again, note that was is represented is not the number of experiments
    # As several treatments can be tested in the same experiment
    # Rather, it is the number of times a treatment is tested as the experiment scale
    # The same reasoning applies for the seasonal scale

sugLevel.tab_exp <- full.table.ma %>%
  filter(Tmt_sugar == "AS") %>%
  group_by(sh_stud, Exp_ID, Artificial_sugar, Management_type) %>%
  summarise() %>%
  arrange(Exp_ID)

sugLevel.tab_exp

sugLevel.tab_exp %>%
  ggplot(aes(x = Artificial_sugar, fill = Management_type)) +
  geom_bar() + 
  xlab(NULL) + 
  ylab("Number of occurrences") +
  ggtitle("Most studied artificial sugar sources",
                      subtitle = "Experiment level")

  # Seasonal level
sugLevel.tab_seas <- full.table.ma %>%
  filter(Tmt_sugar == "AS") %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Artificial_sugar, Management_type) %>%
  summarise() %>%
  arrange(Exp_ID)

sugLevel.tab_seas

sugLevel.tab_seas %>%
  ggplot(aes(x = Artificial_sugar, fill = Management_type)) +
  geom_bar() + 
  xlab(NULL) + 
  ylab("Number of occurrences") +
  ggtitle("Most studied nectar-producing plants",
          subtitle = "Seasonal level") + 
  scale_y_continuous(breaks = breaks_pretty())

################################################### #
##### 6 - Preparing data for the meta-analysis ######
################################################### #

  ## ---------------------------------- #
  ###### '6A) Pooling distance measures ----

  ### In several papers, data are reported at different distances from the field border
  ### (Often, these are several distances from the sugar source)
  ### We need our data to be comparable across studies
  ### So we decided to pool these data
  ### So that they correspond more to the field scale

  # Then we take the subset of values corresponding to distance-dependent measures
  # And we calculate the Grand mean, Pooled N and Pooled SD for each case
    # We consider pooled N to be equal to the number of replicates for each distance here
    # For standard deviation we use a custom function (pooledSd())
pooled.dist <- full.table.ma %>%
  filter(Distance_dependence == 1) %>%
  group_by(sh_stud, Exp_ID, Exp_rep, Sub_exp, Sub_group_ctrl) %>%
  mutate(
    tmt_Mean = mean(tmt_Mean), 
    tmt_N = mean(tmt_N), 
    tmt_SD = pooledSd(tmt_N, tmt_SD), 
    ctrl_Mean = mean(ctrl_Mean), 
    ctrl_N = mean(ctrl_N), 
    ctrl_SD = mean(ctrl_SD), 
    Distance_from_border = "Pooled", 
    Distance_from_border_2 = paste(Distance_from_border_2, sep = "+", collapse = "/"),
    Distance_dependence = NA, 
    Distance_info = paste(Distance_info, sep = "+", collapse = ""),
    tmt_SE = tmt_SD/sqrt(tmt_N),
    ctrl_SE = ctrl_SD/sqrt(ctrl_N),
    Control = "Pooled distances, see original table",
    Treatment = "Pooled distances, see original table",
    Var_ID =  sub("_[^_]+$", "_pooled", Var_ID)
  ) %>%
  distinct()

  # Now we add this to the full dataset
    # We create a new ready.tab object
ready.tab <- full.table.ma %>%
  filter(is.na(Distance_dependence)) %>%
  bind_rows(pooled.dist)
  
  ## ---------------------------------- #
  ###### '6B) Shared controls ----

  ### There are several studies in which several treatments are compared to the same control group
  ### The sampling error of these effect sizes will be correlated
  ### Thus, we have to consider this source of dependence
  ### To do so, we divide the N of the control group by the number of shared controls
  ### As in Bishop & Nakagawa 2021 (See Higgins et al. 2019)
    # Note that one alternative could be to model the VCOV matrix explicitly
      # Yang et al. 2023, Nakagawa et al. 2023
ready.tab <- ready.tab %>%
  mutate(ctrl_N.corr = ifelse(is.na(Same_control_dependence), 
                              ctrl_N, 
                              ctrl_N/Nb_shared_controls),
          # Some sample sizes are now < 1 -> rounding them to 1
         ctrl_N.corr = ifelse(ctrl_N.corr < 1, 1, ctrl_N.corr)
  )

  ## ---------------------------------- #
  ###### '6C) Final adjustments ----

ready.tab <- ready.tab %>%
    ## We won't analyse honeydew in the meta-analysis
      ## (Not enough samples)
  filter(Tmt_sugar_name != 'Honeydew') %>%
  mutate(
    ## Creating moderator: merging spatial treatment and sugar treatment
    Mod = paste(Tmt_sugar, Tmt_spatial, sep = " - "),
    ## This will be useful for sensitivity analyses (just the name)
    ExpName = paste(sh_stud, Exp_Nb))

  # We also create a "calibrated" Publication year variable
  # This will be useful for the analysis of putative time-lag effects
ready.tab$year.c <- as.numeric(scale(ready.tab$Publication_year, scale = F))


########################### #
##### 7 - Effect sizes ######
########################### #

### First, let's look how many effect size rows we have for each variable

# Table, by decreasing order
sort(table(ready.tab$Variable), decreasing = T)

# How many of the effect sizes were sent by authors?
ready.tab %>%
  mutate(`Data origin` = ifelse(Data_Origin == "Sent_data", 
                              "Sent by authors", 
                              "Retrieved from paper")) %>%
  group_by(`Data origin`) %>%
  summarise(`Number of effect sizes` = n())

### Now let's compute effect sizes
## We calculate both Hedge's D (or G -> the corrected for small sample size version) and lnRR
## Note: there are a few NA, see that later

tab.es <- 
  ready.tab %>%
  ## Hedge's D (sensu Nakagawa et al. 2007)
  metafor::escalc(measure = "SMD",
         m1i = tmt_Mean,
         m2i = ctrl_Mean,
         sd1i = tmt_SD,
         sd2i = ctrl_SD,
         n1i = tmt_N,
         n2i = ctrl_N.corr,
         data = ready.tab
         ) %>% 
  metafor::summary.escalc() %>%
  rename_with(~ paste0("HedgeD_", .), .cols = (ncol(.) - 6):ncol(.)) %>%
  ##lnRR
  metafor::escalc(measure = "ROM",
         m1i = tmt_Mean,
         m2i = ctrl_Mean,
         sd1i = tmt_SD,
         sd2i = ctrl_SD,
         n1i = tmt_N,
         n2i = ctrl_N.corr,
         data = .
  ) %>% 
  metafor::summary.escalc() %>%
  rename_with(~ paste0("lnRR_", .), .cols = (ncol(.) - 6):ncol(.)) %>%
  ##lnCVR
  metafor::escalc(measure = "CVR",
                  m1i = tmt_Mean,
                  m2i = ctrl_Mean,
                  sd1i = tmt_SD,
                  sd2i = ctrl_SD,
                  n1i = tmt_N,
                  n2i = ctrl_N.corr,
                  data = .
  ) %>% 
  metafor::summary.escalc() %>%
  rename_with(~ paste0("lnCVR_", .), .cols = (ncol(.) - 6):ncol(.)) %>%
    # Giving an ID to each ES
  mutate(ES_ID = row_number())

## Just looking at NA's
tab.es %>%
  filter(if_any(starts_with("HedgeD"), is.na)) %>%
  select(sh_stud, Variable, Var_ID, tmt_Mean, 
         ends_with("Mean"), ends_with("SD"), ends_with("SE"),
         starts_with("HedgeD"))

tab.es %>%
  filter(if_any(starts_with("lnRR"), is.na)) %>%
  select(sh_stud, Variable, Var_ID, tmt_Mean, 
         ends_with("Mean"), ends_with("SD"), ends_with("SE"),
         starts_with("lnRR"))

tab.es %>%
  filter(if_any(starts_with("lnCVR"), is.na)) %>%
  select(sh_stud, Variable, Var_ID, tmt_Mean, 
         ends_with("Mean"), ends_with("SD"), ends_with("SE"),
         starts_with("lnCVR"))

## And checking their distribution
tab.es %>%
  filter(if_any(starts_with("HedgeD"), is.na)) %>% 
  pivot_longer(
    cols = c(ctrl_Mean, ctrl_SE, tmt_Mean, tmt_SE),
    names_to = c("Cond", ".value"),
    names_pattern = "(.*)_(.*)"
) %>%
  ggplot(aes(x = Mean, col = Cond, y = Var_ID)) +
  geom_pointrange(aes(xmin = Mean + SE, xmax = Mean - SE)) +
  ylab(NULL) +
  xlab("Mean value (+/- SE)") +
  ggtitle("Cases for which Hedge's G could not be computed") +
  scale_color_discrete("Condition", labels = c("Control", "Treatment"))

tab.es %>%
  filter(if_any(starts_with("lnRR"), is.na)) %>% 
  pivot_longer(
    cols = c(ctrl_Mean, ctrl_SE, tmt_Mean, tmt_SE),
    names_to = c("Cond", ".value"),
    names_pattern = "(.*)_(.*)"
) %>%
  ggplot(aes(x = Mean, col = Cond, y = Var_ID)) +
  geom_pointrange(aes(xmin = Mean + SE, xmax = Mean - SE)) +
  ylab(NULL) +
  xlab("Mean value (+/- SE)") +
  ggtitle("Cases for which lnRR could not be computed") +
  scale_color_discrete("Condition", labels = c("Control", "Treatment"))


tab.es %>%
  filter(if_any(starts_with("lnCVR"), is.na)) %>% 
  pivot_longer(
    cols = c(ctrl_Mean, ctrl_SE, tmt_Mean, tmt_SE),
    names_to = c("Cond", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  ggplot(aes(x = Mean, col = Cond, y = Var_ID)) +
  geom_pointrange(aes(xmin = Mean + SE, xmax = Mean - SE)) +
  ylab(NULL) +
  xlab("Mean value (+/- SE)") +
  ggtitle("Cases for which lnCVR could not be computed") +
  scale_color_discrete("Condition", labels = c("Control", "Treatment"))

### Now let's plot effect sizes
  # To do so we use the custom plotEs() function

  # ES on means (Hedge's D and lnRR)
plotES(tab.es, "Parasitism_rate", "HedgeD")
plotES(tab.es, "Parasitism_rate", "lnRR")
plotES(tab.es, "Parasitoid_abundance", "HedgeD")
plotES(tab.es, "Parasitoid_abundance", "lnRR")
plotES(tab.es, "Pest_density", "HedgeD")
plotES(tab.es, "Pest_density", "lnRR")
plotES(tab.es, "Yield", "HedgeD")
plotES(tab.es, "Yield", "lnRR")

  # ES on variance (lnCVR)
plotES(tab.es, "Parasitism_rate", "lnCVR")
plotES(tab.es, "Parasitoid_abundance", "lnCVR")
plotES(tab.es, "Pest_density", "lnCVR")
plotES(tab.es, "Yield", "lnCVR")

# lnRR is quite OK in terms of distribution but see the huge SEs sometimes
  # may be related to small sample sizes
  # see Nakagawa et al. 2022, Ecology letters
  # Check https://onlinelibrary.wiley.com/doi/10.1111/ele.14144
  # Implementation paragraph

# So let's check the assumption of normality
tab.es <- 
tab.es %>%
  mutate(
    CV_lnRR = sqrt(lnRR_vi)/lnRR_yi,
    n_ES = mean(c(tmt_N, ctrl_N)),
    norm_lnRR = (1/CV_lnRR) * (4*(n_ES^(3/2))) / (1 + 4 * n_ES),
    lnRR_outl = ifelse(norm_lnRR >= 3, "Ok", "Outlier")
    )

sum(tab.es$norm_lnRR >= 3, na.rm = T) / nrow(tab.es)

# About 20% of the data! Probably not enough for sensitivity tests
  # But we'll be cautious to model diagnostics
qplot(tab.es$norm_lnRR) +
  geom_vline(xintercept = 3, col = "red") +
  xlab("Normalized lnRR") +
  ggtitle("Distribution of normalized lnRR",
          subtitle = "Lajeunesse's test")

# Does it change something to plot without these data?
  # Not that much
plotES(tab.es %>% filter(norm_lnRR >= 3), "Parasitism_rate", "lnRR")
plotES(tab.es %>% filter(norm_lnRR >= 3), "Parasitoid_abundance", "lnRR")
plotES(tab.es %>% filter(norm_lnRR >= 3), "Pest_density", "lnRR")
plotES(tab.es %>% filter(norm_lnRR >= 3), "Yield", "lnRR")

# Final thing to do: computing the "effective sample size" as in
  # Nakagawa et al. (2022, 2023)
tab.es <- tab.es %>%
  mutate(ess_var = (1/ctrl_N.corr) + (1/tmt_N),
         ess_se = sqrt(ess_var))


########################### #
##### 8 - Meta-analyses #####
########################### #

# We'll do our best to model dependence in effect sizes due to shared studies,
# experiments, etc.
# However, many of our effect sizes also correspond to shared measurements
# And so we have sources of dependence in sampling variance as well
# (See Nakagawa et al. 2023)
# This includes (not exhaustively)
## Shared controls (Already taken care of by dividing sample sizes)
## Variables measured on the same samples, e.g. same plants or hosts
## Different methods used to measure the same variable
## Same variable measured on different species
## All of these combine multiple, intricate sources of dependence
## It will be difficult and probably overfitting to try modelling everything explicitly

## So we can use this set of rules:
## We will consider data as "independent" if
### They were obtained on different "sub_experiments"
### i.e. differentiated pairs of treatment AND control
### OR they were obtained on different samples AND concern different species
### So for instance count of pests on different parts of a plant
### Or parasitism rate on different host species by different parasitoid species

### When it is not the case, we will model data dependence 
### Defining variance-covariance matrices for each response variable
  ### (See Nakagawa et al. 2023)
### Assuming a correlation of 0.5 at first 
  ### but we'll try other values later in sensitivity analyses
    ### (Noble et al. 2017 Mol Ecol, Nakagawa et al. 2023)
    ### Bishop & Nakagawa 2021 for an example

# So for each Variable, we give a cluster ID to each ES
# To reflect this structure of dependence in the sampling variance
tab.es <- 
  tab.es %>%
  mutate(unit = factor(paste(Exp_ID, Exp_rep, sep = "_"))) %>%
  group_by(unit, Sub_exp) %>%
  # Same ID if
  # Same sub-exp
  # AND same sample or same species
  mutate(
    clusterID = cur_group_id() + group_indices(across(c(Variable_dep_sample, Variable_dep_taxon)))
  )

## Something that we will also do now
## is to exclude cases when we have one measurement on one species
## that was also included in a pool
## Dependence is probably very large here
  ## We could try to include them later in sensitivity analyses

# Here they are:
tab.es %>%
  filter(Parasitoid_pool == 1) %>%
  group_by(sh_stud, Var_ID, ES_ID) %>%
  summarise()

  # Quick and dirty way, let's improve this later by adding a column in the table
meta.tab <- tab.es %>%
  filter(!ES_ID %in% c(272, 415, 503, 504))

  # Saving this for figures
if(save_meta_table) write_rds(meta.tab, 'Outputs/meta_tab.rds')

  ## ---------------------------------- #
  ###### '8A) Pest density ----

    ## First, we extract the subset corresponding to pest density values only
meta.PD.dat <- meta.tab %>%
  filter(Variable == "Pest_density")

    ###### ''8A1) Meta-analysis of mean ----

  ## Let's start with analysing the mean
  ## i.e. is mean pest density different when sugar is available?

  ## Here we construct the VCOV matrix for dependence
    # Assuming rho = 0.5 for starters (Nakagawa et al. 2023)
vcov.PD.lnRR.05 <- 
  metafor::vcalc(vi = lnRR_vi, 
                 cluster = clusterID, 
                 obs = ES_ID, 
                 data = meta.PD.dat, 
                 rho = 0.5)


  ## Now we test the random structure (models are fitted with ML)
    # Following e.g. Bishop & Nakagawa 2021

    # Only Shared Study
meta.PD.Mean.1 <- rma.mv(lnRR_yi, vcov.PD.lnRR.05,
                   random = ~1|sh_stud, 
                   data = meta.PD.dat,
                   method = 'ML')
AIC(meta.PD.Mean.1) 

    # Shared Study AND Experiment (nested)
meta.PD.Mean.2 <- update(meta.PD.Mean.1,
                   random = ~1|sh_stud/Exp_Nb)
AIC(meta.PD.Mean.2)


    # Shared Experiment only
meta.PD.Mean.3 <- update(meta.PD.Mean.1,
                   random = ~1|Exp_ID)
AIC(meta.PD.Mean.3) 

    # Shared Experiment and Year
meta.PD.Mean.4 <- update(meta.PD.Mean.1,
                   random = ~1|Exp_ID/Exp_rep)
AIC(meta.PD.Mean.4) 
anova.rma(meta.PD.Mean.3, meta.PD.Mean.4)
  # significant difference with model with second lowest AIC

    # Shared Experiment and Sub_experiment
meta.PD.Mean.5 <- update(meta.PD.Mean.1,
                   random = ~1|Exp_ID/Sub_exp, 
                   data = ungroup(meta.PD.dat) %>% 
                     mutate(Sub_exp = ifelse(is.na(Sub_exp), 1, Sub_exp)))
AIC(meta.PD.Mean.5) 

anova.rma(meta.PD.Mean.3, meta.PD.Mean.5)
    # --> meta.PD.Mean.4 has the lowest AIC
    # (i.e. the one with Exp. season nested within Exp. ID without Study ID)

  ## So we can fit our intercept-only model using REML:
meta.PD.Mean.int <- update(meta.PD.Mean.4, method = 'REML')
summary(meta.PD.Mean.int)
mResults_PD.Mean.int <- orchaRd::mod_results(meta.PD.Mean.int, mod = "1", group = "Exp_ID")
print(mResults_PD.Mean.int)
orchard_plot(mResults_PD.Mean.int, xlab = "lnRR") +
  ggtitle("Meta-analysis of pest density mean",
          subtitle = "Intercept-only model")  +
  scale_colour_manual(values = "#CC6677") +
  scale_fill_manual(values = "#CC6677")

    # Let's just check that Random Effects were well estimated
      # Takes a few second (about 20s on my PC)
      # Ok, mainly for Experiment effect
if(profile_random) profile.rma.mv(meta.PD.Mean.int)

  ## This is the intercept-only model. What about moderators?
meta.PD.Mean.mods <- update(meta.PD.Mean.int, mods = ~ Mod - 1)
summary(meta.PD.Mean.mods)
mResults_PD.Mean.mods <- orchaRd::mod_results(meta.PD.Mean.mods, mod = "Mod", group = "Exp_ID")
print(mResults_PD.Mean.mods)
orchard_plot(mResults_PD.Mean.mods, xlab = "lnRR") +
  ggtitle("Meta-analysis of pest density mean",
          subtitle = "Model with moderators")

if(profile_random) profile.rma.mv(meta.PD.Mean.mods)

    # Test of moderators
anova.rma(meta.PD.Mean.int)
anova.rma(meta.PD.Mean.mods)

    # Model comparison
      # using custom modelCompTab() function
      # Note: models are re-fitted using ML here
modelCompTab("PD", "Mean")
  
  ## Analysing model performance and heterogeneity
      # using custom modelperfTab() function
modelperfTab("PD", "Mean")

  ## Checking model residual distribution

    # Now we'll focus on the intercept-only model
    # As we do not "need" the moderator model anymore
    # Again using custom functions
      # Note that these ignore precision/sample size for residuals :/
      # To be improved later
rmaQQplot("PD", "Mean", "Pest density", "lnRR")
rmaResidHist("PD", "Mean", "Pest density", "lnRR")

  ## Analysis of publication bias

    # Let's start with a funnel plot

      # Based on inverse standard error first ("classic" approach)
        # Distribution should by symmetrical
        # 95% of the data should be in the CI
funnel(meta.PD.Mean.int, yaxis = "seinv")
funnel(meta.PD.Mean.int, yaxis = "seinv", xlim = c(-5, 5))
funnel(meta.PD.Mean.int, yaxis = "seinv", ylim = c(0.1, 10), xlim = c(-5, 5))
funnel(meta.PD.Mean.int, yaxis = "seinv", ylim = c(0.1, 5), xlim = c(-5, 5))
        # Based on inverse standard error, there seems to be a small-study effect indeed!

      # Based on effective sample size (cf Nakagawa et al. 2022, 2023)
        # To do so we use custom essFunnel() function
essFunnel("PD", "Mean", "lnRR")

      # Proper test
meta.PD.Mean.int.pb <- update(meta.PD.Mean.int, mods = ~ ess_se + year.c)
        # Quick analysis
summary(meta.PD.Mean.int.pb)
anova(meta.PD.Mean.int.pb)
        # Graphical analysis
bubble_plot(meta.PD.Mean.int.pb, mod = "ess_se", group = "Exp_ID",
            xlab = "Effective sample size", ylab = "lnRR") +
  ggtitle("Analysis of publication bias for the effect on pest density mean",
          subtitle = "Decline effect")

bubble_plot(meta.PD.Mean.int.pb, mod = "year.c", group = "Exp_ID",
            xlab = "Publication year", ylab = "lnRR") +
  scale_x_continuous(
    labels = unique(round(meta.PD.dat$Publication_year / 5) * 5),
    breaks = unique(round(meta.PD.dat$year.c / 5) * 5)
  ) +
  ggtitle("Analysis of publication bias for the effect on pest density mean",
          subtitle = "Decline effect")

  ## Adjusting effect sizes
    # To adjust effect sizes for publication bias we use modResultsAdjPb()
    # custom function
mResults_PD.Mean.Adj <- modResultsAdjPb("PD", "Mean")
mResults_PD.Mean.Adj

    # Then we can plot unadjusted and adjusted estimates using plotAdjEst()
plotAdjEst("PD", "Mean", "lnRR", "Pest density")

  ## Sensitivity analysis

    # We will do several sensitivity analyses, using custom functions
      # saForest() (calling LOOTable() and LOOForest())
    # We'll re-run the model multiple times
    # Each time excluding one study or one experiment
    # We'll work only with the intercept-only model for now
      # As it was already selected over the model with moderators
      # Maybe we'll do more complex sensitivity analyses later

  # To re-run (and save) the analysis, set to TRUE
if(run_sens_ana) {
  
  meta.PD.Mean.int.SA.ESID <- 
    saForest(meta.PD.dat, meta.PD.Mean.int, "lnRR_vi", "ES_ID")
  meta.PD.Mean.int.SA.ExpID <- 
    saForest(meta.PD.dat, meta.PD.Mean.int, "lnRR_vi", "Exp_ID")
  meta.PD.Mean.int.SA.PaperID <- 
    saForest(meta.PD.dat, meta.PD.Mean.int, "lnRR_vi", "Paper_ID")

  saveSA(meta.PD.Mean.int.SA.ESID)
  saveSA(meta.PD.Mean.int.SA.ExpID)
  saveSA(meta.PD.Mean.int.SA.PaperID)
  
}

  # To load save analyses, set to TRUE
if(load_sens_an) {
  
  readSA("PD", "Mean", "ESID") +
    ggtitle("Sensitivity analysis of Pest density - Mean effect",
            "Removing Effect sizes one by one")
  
  readSA("PD", "Mean", "ExpID") +
    ggtitle("Sensitivity analysis of Pest density - Mean effect",
            "Removing Experiments one by one")
  
  readSA("PD", "Mean", "PaperID") +
    ggtitle("Sensitivity analysis of Pest density - Mean effect",
            "Removing Papers one by one")
  
}
    # So clearly there is something with exp. 54 in paper 45
    # By Helenius 1990
meta.PD.dat %>%
  filter(Paper_ID == 45) %>%
  select(Paper_name, Publication_year, Exp_ID, Exp_type_1, ctrl_Mean, tmt_Mean)

# So let's refit a model without this one
meta.PD.dat.postSA <- meta.PD.dat %>% filter(Paper_ID != 45)

vcov.PD.lnRR.05.postSA <- vcalc(vi = lnRR_vi, 
                               cluster = clusterID, 
                               obs = ES_ID, 
                               data = meta.PD.dat.postSA, 
                               rho = 0.5)
  
# Exploring results...
meta.PD.Mean.postSA.int <-
  update(meta.PD.Mean.int, V = vcov.PD.lnRR.05.postSA, 
         data = meta.PD.dat.postSA)

mResults_PD.Mean.postSA.int <- 
  orchaRd::mod_results(meta.PD.Mean.postSA.int, group = "Exp_ID")

mResults_PD.Mean.postSA.int %>%
  orchard_plot(xlab = "lnRR") +
  ggtitle("Meta-analysis of mean effect on pest density",
          subtitle = "After sensitivity analysis and paper exclusion")  +
  scale_colour_manual(values = "#CC6677") +
  scale_fill_manual(values = "#CC6677")

# Checking for publication bias again...
meta.PD.Mean.postSA.int.pb <- 
update(meta.PD.Mean.int.pb, V = vcov.PD.lnRR.05.postSA, 
       data = meta.PD.dat.postSA)

summary(meta.PD.Mean.postSA.int.pb)

bubble_plot(meta.PD.Mean.postSA.int.pb, mod = "ess_se", group = "Exp_ID") +
  xlab("Effective sample size") +
  ylab("LnRR") +
  ggtitle("Analysis of publication bias for the effect on pest density mean (after sensitivity analyses)",
          subtitle = "Small study effect")
  
bubble_plot(meta.PD.Mean.postSA.int.pb, mod = "year.c", group = "Exp_ID") +
  scale_x_continuous(
    labels = unique(round(meta.PD.dat$Publication_year / 5) * 5),
    breaks = unique(round(meta.PD.dat$year.c / 5) * 5)
  ) +
  xlab("Publication year") +
  ylab("LnRR") +
  ggtitle("Analysis of publication bias for the effect on pest density mean (after sensitivity analyses)",
          subtitle = "Decline effect")

mResults_PD.Mean.postSA.Adj <- 
  modResultsAdjPb("PD", "Mean", postSA = T)
mResults_PD.Mean.postSA.Adj 

meta.PD.Mean.modResultsAdjPb <- plotAdjEst("PD", "Mean", "lnRR", "Pest Density", postSA = T)
meta.PD.Mean.modResultsAdjPb

# Let's just check quickly again
if(run_sens_ana) {
  
  meta.PD.Mean.int.SA.ExpID.postSA <- 
    saForest(meta.PD.dat.postSA, meta.PD.Mean.postSA.int, "lnRR_vi", "Exp_ID")
  
  meta.PD.Mean.int.SA.PaperID.postSA <- 
    saForest(meta.PD.dat.postSA, meta.PD.Mean.postSA.int, "lnRR_vi", "Paper_ID")
  
  saveSA(meta.PD.Mean.int.SA.ExpID.postSA) 
  saveSA(meta.PD.Mean.int.SA.PaperID.postSA) 
  
}

if(load_sens_an) {
  
  readSA("PD", "Mean", "PaperID", postSA = T) +
    ggtitle("Sensitivity analysis of Pest density - Mean effect",
            "Removing Papers one by one (Helenius 1990 excluded)")
  
  readSA("PD", "Mean", "ExpID", postSA = T) +
    ggtitle("Sensitivity analysis of Pest density - Mean effect",
            "Removing Experiments one by one (Helenius 1990 excluded)")
  
}

# Still one weird paper
meta.PD.dat %>% filter(Paper_ID == 91) %>%
  select(Var_ID, ctrl_Mean, tmt_Mean)
  # But ok (doesn't change results qualitatively anyway)

# Now let's do some sensitivity analyses to methodological choices

  # Experimental type?
saForest(meta.PD.dat.postSA, meta.PD.Mean.postSA.int, "lnRR_vi", "Exp_type_1")
  # Distance category (cf datasheet)?
saForest(meta.PD.dat.postSA, meta.PD.Mean.postSA.int, "lnRR_vi", "Distance_category")
    # Overall ES changes when removing A but that's normal: most ES are here
    # Other categories do not change results qualitatively so we can keep them

  # We can also test for the effect of these as moderators: no effect
update(meta.PD.Mean.postSA.int, mods = ~ Exp_type_1 + Distance_category) %>%
  anova(meta.PD.Mean.postSA.int, refit = TRUE)

update(meta.PD.Mean.postSA.int, mods = ~ Exp_type_1 + Distance_category) %>%
  orchaRd::mod_results(group = "Exp_ID", mod = "Exp_type_1") %>%
  orchard_plot(xlab = "lnRR")

update(meta.PD.Mean.postSA.int, mods = ~ Exp_type_1 + Distance_category) %>%
  orchaRd::mod_results(group = "Exp_ID", mod = "Distance_category") %>%
  orchard_plot(xlab = "lnRR")

# Let's also do a sensitivity analysis to the correlation level
  # chosen for the variance-covariance matrix
  # of the dependent effect sizes
  # (Nakagawa et al. 2023)

  # We'll just generate 4 different values, from low to high correlation
    # (0.3 to 0.9)
listV.PD.Mean.postSA.int <- 
  lapply(c(0.3, 0.5, 0.7, 0.9), function(Rho) {
    vcalc(vi = lnRR_vi, 
          cluster = clusterID, 
          obs = ES_ID, 
          data = meta.PD.dat.postSA, 
          rho = Rho)
  } )

  # Now we re-run the models (the final ones)
    # And check how coefficients change
    # Arguably we could repeat the whole process but it would be very long

# Model unadjusted for publication bias
varCorLev_PD.Mean.postSA.int <- 
  lapply(listV.PD.Mean.postSA.int, function(v)
    update(meta.PD.Mean.postSA.int, V = v))

lapply(varCorLev_PD.Mean.postSA.int, 
       function(c) coefficients(summary(c))) %>%
  bind_rows(.id = "cor_level") %>%
  mutate(cor_level = c(0.3, 0.5, 0.7, 0.9)) %>%
  remove_rownames

# Model adjusted for publication bias
varCorLev_PD.Mean.postSA.int.pb <- 
  lapply(listV.PD.Mean.postSA.int, function(v)
    update(meta.PD.Mean.postSA.int.pb, V = v))

lapply(varCorLev_PD.Mean.postSA.int.pb, 
       function(c) {
         coefficients(summary(c)) %>%
           as.data.frame %>%
           rownames_to_column}) %>% 
  bind_rows(.id = "cor_level") %>%
  mutate(cor_level = rep(c(0.3, 0.5, 0.7, 0.9), each = 3))

  # From these we can conclude that
    # In the unadjusted version, results are unsensititive to the chosen
    # correlation level: we still have an overall effect no matter its value
    # In the adjusted version, we do not detect an effect of sugar on PD
      # But when choosing high correlation values
      # Evidence for publication bias disappears
      # In that case we might be willing to favour the unadjusted version of 
      # the model and conclude to a (small) negative effect of sugars on PD
      # But see how this depends on parameter values here: evidence is low


  ###### ''8A2) Meta-analysis of variance ----

    ## Here we construct the VCOV matrix for dependence
vcov.PD.lnCVR.05 <- 
  metafor::vcalc(vi = lnCVR_vi, 
                 cluster = clusterID, 
                 obs = ES_ID, 
                 data = meta.PD.dat, 
                 rho = 0.5)


    ## Now we test the random structure (models are fitted with ML)

      # Only Shared Study
meta.PD.Var.1 <- rma.mv(lnCVR_yi, vcov.PD.lnCVR.05,
                          random = ~1|sh_stud, 
                          data = meta.PD.dat,
                          method = 'ML')

      # Shared Study AND Experiment (nested)
meta.PD.Var.2 <- update(meta.PD.Var.1, random = ~1|sh_stud/Exp_Nb)
      # Shared Experiment only
meta.PD.Var.3 <- update(meta.PD.Var.1, random = ~1|Exp_ID)
      # Shared Experiment and Year
meta.PD.Var.4 <- update(meta.PD.Var.1, random = ~1|Exp_ID/Exp_rep)
      # Shared Experiment and Sub_experiment
meta.PD.Var.5 <- update(meta.PD.Var.1, random =~1|Exp_ID/Sub_exp,
                          data = ungroup(meta.PD.dat) %>% 
                            mutate(Sub_exp = ifelse(is.na(Sub_exp), 1, Sub_exp))
)
      # AIC comparison
AIC(meta.PD.Var.1, meta.PD.Var.2, meta.PD.Var.3,
    meta.PD.Var.4, meta.PD.Var.5) 
        # --> meta.PD.Var.3 has the lowest AIC
        # (shared experiment only)
        # maybe we do not have enough information for effects of other factors

    ## So we can fit our intercept-only model using REML:
meta.PD.Var.int.rand <- update(meta.PD.Var.3, method = 'REML')
summary(meta.PD.Var.int.rand)
mResults_PD.Var.int <- orchaRd::mod_results(meta.PD.Var.int.rand, mod = "1", group = "Exp_ID")
print(mResults_PD.Var.int)
orchard_plot(mResults_PD.Var.int, xlab = "lnRR") +
  ggtitle("Meta-analysis of pest density variance",
          subtitle = "Intercept-only model")  +
  scale_colour_manual(values = "#CC6677") +
  scale_fill_manual(values = "#CC6677")

      # Checking random effect fitting
if(profile_random) profile.rma.mv(meta.PD.Var.int)

      # Doesn't converge so we simplify (no random effect)
meta.PD.Var.int <- update(meta.PD.Var.int.rand, random = NULL)
summary(meta.PD.Var.int)
mResults_PD.Var.int <- orchaRd::mod_results(meta.PD.Var.int, mod = "1", group = "Exp_ID")
print(mResults_PD.Var.int)
orchard_plot(mResults_PD.Var.int, xlab = "lnRR") +
  ggtitle("Meta-analysis of pest density variance",
          subtitle = "Intercept-only model, no random structure")  +
  scale_colour_manual(values = "#CC6677") +
  scale_fill_manual(values = "#CC6677")

    ## Model with moderators
meta.PD.Var.mods <- update(meta.PD.Var.int, mods = ~ Mod - 1)
summary(meta.PD.Var.mods)
mResults_PD.Var.mods <- orchaRd::mod_results(meta.PD.Var.mods, mod = "Mod", group = "Exp_ID")
print(mResults_PD.Var.mods)
orchard_plot(mResults_PD.Var.mods, xlab = "lnCVR") +
  ggtitle("Meta-analysis of pest density variance",
          subtitle = "Model with moderators")

      # Checking random effect fitting
if(profile_random) profile.rma.mv(meta.PD.Var.mods)

      # Model comparison
anova.rma(meta.PD.Var.int)
anova.rma(meta.PD.Var.mods)
modelCompTab("PD", "Var")

    ## Analysing model performance and heterogeneity

      # Model performance
modelperfTab("PD", "Var")

      # Residual distribution
rmaQQplot("PD", "Var", "Pest density", "lnCVR")
rmaResidHist("PD", "Var", "Pest density", "lnCVR")

    ## Publication bias

      # Funnel plots
funnel(meta.PD.Var.int, yaxis = "seinv")
essFunnel("PD", "Var", "lnCVR")
        # Maybe a small study effect but due to a small group of papers?


      # Proper test
meta.PD.Var.int.pb <- update(meta.PD.Var.int, mods = ~ ess_se + year.c)
summary(meta.PD.Var.int.pb) # No clear evidence here
anova(meta.PD.Var.int.pb)

      # Graphical analysis
bubble_plot(meta.PD.Var.int.pb, mod = "ess_se", group = "Exp_ID") +
  ggtitle("Analysis of publication bias for the effect on pest density variance",
          subtitle = "Small study effect")
        # Indeed we can see the group of papers

bubble_plot(meta.PD.Var.int.pb, mod = "year.c", group = "Exp_ID") +
  scale_x_continuous(
    labels = seq(min(meta.PD.dat$Publication_year), max(meta.PD.dat$Publication_year), by = 6),
    breaks = seq(min(meta.PD.dat$year.c), max(meta.PD.dat$year.c), by = 6)
  ) +
  ggtitle("Analysis of publication bias for the effect on pest density variance",
          subtitle = "Decline effect")  
  # Helenius 1990 has an effect here too!

  # Actually we probably have some publication bias here,
  # though not enough data to be detected


    ## Sensitivity analysis

# This time we do it all at once
  # check sensAnCompl() custom function
sensAnCompl("PD", "Var", "lnCVR", "Pest density", run_sens_ana = run_sens_ana)
  # We can see the effect of Helenius 1990 here too
  # But doesn't change results that much

# Also quickly checking for sensitivity to varying correlation levels
  # check sensAnCompl() custom function
sensAnCor(meta.PD.dat, "lnCVR", meta.PD.Var.int, meta.PD.Var.int.pb)

    ## Final plot
meta.PD.Var.modResultsAdjPb <- plotAdjEst("PD", "Var", "lnCVR", "Pest density")
meta.PD.Var.modResultsAdjPb


  ###### ''8A3) Final plot for meta-analysis of pest density ----

meta.PD.modResultsAdjPb <-
ggpubr::ggarrange(meta.PD.Mean.modResultsAdjPb + 
            ggtitle("Meta-analysis of the effect on pest density", "Mean") + 
            ylab("lnRR") +
            theme(axis.text.y = element_blank()), 
          meta.PD.Var.modResultsAdjPb +
            ggtitle("", "Variance") + 
            ylab("lnCVR") +
            theme(axis.text.y = element_blank())
)

meta.PD.modResultsAdjPb

  ## ---------------------------------- #
  ###### '8B) Parasitoid abundance ----

## First, we extract the subset corresponding to parasitoid abundance values only
meta.ParAb.dat <- meta.tab %>%
  filter(Variable == "Parasitoid_abundance")

###### ''8B1) Meta-analysis of mean ----

  ## Here we construct the VCOV matrix for dependence
vcov.ParAb.lnRR.05 <- 
  metafor::vcalc(vi = lnRR_vi, 
                 cluster = clusterID, 
                 obs = ES_ID, 
                 data = meta.ParAb.dat, 
                 rho = 0.5)


  ## Now we test the random structure (models are fitted with ML)

# Only Shared Study
meta.ParAb.Mean.1 <- rma.mv(lnRR_yi, vcov.ParAb.lnRR.05,
                        random = ~1|sh_stud, 
                        data = meta.ParAb.dat,
                        method = 'ML')

# Shared Study AND Experiment (nested)
meta.ParAb.Mean.2 <- update(meta.ParAb.Mean.1, random = ~1|sh_stud/Exp_Nb)
# Shared Experiment only
meta.ParAb.Mean.3 <- update(meta.ParAb.Mean.1, random = ~1|Exp_ID)
# Shared Experiment and Year
meta.ParAb.Mean.4 <- update(meta.ParAb.Mean.1, random = ~1|Exp_ID/Exp_rep)
# Shared Experiment and Sub_experiment
meta.ParAb.Mean.5 <- update(meta.ParAb.Mean.1, random =~1|Exp_ID/Sub_exp,
                           data = ungroup(meta.ParAb.dat) %>% 
                             mutate(Sub_exp = ifelse(is.na(Sub_exp), 1, Sub_exp))
                           )
  # AIC comparison
AIC(meta.ParAb.Mean.1, meta.ParAb.Mean.2, meta.ParAb.Mean.3,
    meta.ParAb.Mean.4, meta.ParAb.Mean.5) 
  # -> Keeping model 1 (Study effect only)

  ## So we can fit our intercept-only model using REML:
meta.ParAb.Mean.int <- update(meta.ParAb.Mean.1, method = 'REML')
summary(meta.ParAb.Mean.int)
mResults_ParAb.Mean.int <- orchaRd::mod_results(meta.ParAb.Mean.int, mod = "1", group = "Exp_ID")
print(mResults_ParAb.Mean.int)
orchard_plot(mResults_ParAb.Mean.int, xlab = "lnRR") +
  ggtitle("Meta-analysis of parasitoid abundance mean",
          subtitle = "Intercept-only model")  +
  scale_colour_manual(values = "#CC6677") +
  scale_fill_manual(values = "#CC6677")

# Let's just check that Random Effects were well estimated
# Ok, mainly for Experiment effect
if(profile_random) profile.rma.mv(meta.ParAb.Mean.int)

  ## Model with moderators
meta.ParAb.Mean.mods <- update(meta.ParAb.Mean.int, mods = ~ Mod - 1)
summary(meta.ParAb.Mean.mods)
mResults_ParAb.Mean.mods <- orchaRd::mod_results(meta.ParAb.Mean.mods, mod = "Mod", group = "Exp_ID")
print(mResults_ParAb.Mean.mods)
orchard_plot(mResults_ParAb.Mean.mods, xlab = "lnRR") +
  ggtitle("Meta-analysis of parasitoid abundance mean",
          subtitle = "Model with moderators")

if(profile_random) profile.rma.mv(meta.ParAb.Mean.mods)

anova.rma(meta.ParAb.Mean.int)
anova.rma(meta.ParAb.Mean.mods)
modelCompTab("ParAb", "Mean")

  ## Analysing model performance and heterogeneity

    # Model performance
modelperfTab("ParAb", "Mean")

    # Residual distribution
rmaQQplot("ParAb", "Mean", "Parasitoid abundance", "lnRR")
rmaResidHist("ParAb", "Mean", "Parasitoid abundance", "lnRR")

  ## Publication bias

    # Funnel plots
funnel(meta.ParAb.Mean.int, yaxis = "seinv")
essFunnel("ParAb", "Mean", "lnCVR")

    # Proper test
meta.ParAb.Mean.int.pb <- update(meta.ParAb.Mean.int, mods = ~ ess_se + year.c)
summary(meta.ParAb.Mean.int.pb)
anova(meta.ParAb.Mean.int.pb)

    # Graphical analysis
bubble_plot(meta.ParAb.Mean.int.pb, mod = "ess_se", group = "Exp_ID") +
  ggtitle("Analysis of publication bias for the effect on parasitoid abundance mean",
          subtitle = "Small study effect")

bubble_plot(meta.ParAb.Mean.int.pb, mod = "year.c", group = "Exp_ID") +
  scale_x_continuous(
    labels = seq(min(meta.ParAb.dat$Publication_year), max(meta.ParAb.dat$Publication_year), by = 6),
    breaks = seq(min(meta.ParAb.dat$year.c), max(meta.ParAb.dat$year.c), by = 6)
  ) +
  ggtitle("Analysis of publication bias for the effect on parasitoid abundance mean",
          subtitle = "Decline effect")  

  ## Sensitivity analysis
sensAnCompl("ParAb", "Mean", "lnRR", "Parasitoid abundance", run_sens_ana = run_sens_ana)
sensAnCor(meta.ParAb.dat, "lnRR", meta.ParAb.Mean.int, meta.ParAb.Mean.int.pb)

  ## Final plot
meta.ParAb.Mean.modResultsAdjPb <- plotAdjEst("ParAb", "Mean", "lnCVR", "Pest Density")
meta.ParAb.Mean.modResultsAdjPb

  ###### ''8B2) Meta-analysis of variance ----

    ## Here we construct the VCOV matrix for dependence
vcov.ParAb.lnCVR.05 <- 
  metafor::vcalc(vi = lnCVR_vi, 
                 cluster = clusterID, 
                 obs = ES_ID, 
                 data = meta.ParAb.dat, 
                 rho = 0.5)


    ## Now we test the random structure (models are fitted with ML)

      # Only Shared Study
meta.ParAb.Var.1 <- rma.mv(lnCVR_yi, vcov.ParAb.lnCVR.05,
                            random = ~1|sh_stud, 
                            data = meta.ParAb.dat,
                            method = 'ML')

      # Shared Study AND Experiment (nested)
meta.ParAb.Var.2 <- update(meta.ParAb.Var.1, random = ~1|sh_stud/Exp_Nb)
      # Shared Experiment only
meta.ParAb.Var.3 <- update(meta.ParAb.Var.1, random = ~1|Exp_ID)
      # Shared Experiment and Year
meta.ParAb.Var.4 <- update(meta.ParAb.Var.1, random = ~1|Exp_ID/Exp_rep)
      # Shared Experiment and Sub_experiment
meta.ParAb.Var.5 <- update(meta.ParAb.Var.1, random =~1|Exp_ID/Sub_exp,
                            data = ungroup(meta.ParAb.dat) %>% 
                              mutate(Sub_exp = ifelse(is.na(Sub_exp), 1, Sub_exp))
)
      # AIC comparison
AIC(meta.ParAb.Var.1, meta.ParAb.Var.2, meta.ParAb.Var.3,
    meta.ParAb.Var.4, meta.ParAb.Var.5) 
        # -> Keeping model 1 (Study effect only)

  ## So we can fit our intercept-only model using REML:
meta.ParAb.Var.int <- update(meta.ParAb.Var.1, method = 'REML')
summary(meta.ParAb.Var.int)
mResults_ParAb.Var.int <- orchaRd::mod_results(meta.ParAb.Var.int, mod = "1", group = "Exp_ID")
print(mResults_ParAb.Var.int)
orchard_plot(mResults_ParAb.Var.int, xlab = "lnCVR") +
  ggtitle("Meta-analysis of parasitoid abundance variance",
          subtitle = "Intercept-only model")

    # Checking random effect fitting
if(profile_random) profile.rma.mv(meta.ParAb.Var.int)

  ## Model with moderators
meta.ParAb.Var.mods <- update(meta.ParAb.Var.int, mods = ~ Mod - 1)
summary(meta.ParAb.Var.mods)
mResults_ParAb.Var.mods <- orchaRd::mod_results(meta.ParAb.Var.mods, mod = "Mod", group = "Exp_ID")
print(mResults_ParAb.Var.mods)
orchard_plot(mResults_ParAb.Var.mods, xlab = "lnCVR") +
  ggtitle("Meta-analysis of parasitoid abundance variance",
          subtitle = "Model with moderators")

    # Checking random effect fitting
if(profile_random) profile.rma.mv(meta.ParAb.Var.mods)

    # Model comparison
anova.rma(meta.ParAb.Var.int)
anova.rma(meta.ParAb.Var.mods)
modelCompTab("ParAb", "Var")

  ## Analysing model performance and heterogeneity

    # Model performance
modelperfTab("ParAb", "Var")

    # Residual distribution
rmaQQplot("ParAb", "Var", "Parasitoid abundance", "lnCVR")
rmaResidHist("ParAb", "Var", "Parasitoid abundance", "lnCVR")

  ## Publication bias

    # Funnel plots
funnel(meta.ParAb.Var.int, yaxis = "seinv")
essFunnel("ParAb", "Var", "lnCVR")

    # Proper test
meta.ParAb.Var.int.pb <- update(meta.ParAb.Var.int, mods = ~ ess_se + year.c)
summary(meta.ParAb.Var.int.pb) # Decline effect?
anova(meta.ParAb.Var.int.pb)

    # Graphical analysis
bubble_plot(meta.ParAb.Var.int.pb, mod = "ess_se", group = "Exp_ID") +
  ggtitle("Analysis of publication bias for the effect on parasitoid abundance variance",
          subtitle = "Small study effect")

bubble_plot(meta.ParAb.Var.int.pb, mod = "year.c", group = "Exp_ID") +
  scale_x_continuous(
    labels = seq(min(meta.ParAb.dat$Publication_year), max(meta.ParAb.dat$Publication_year), by = 6),
    breaks = seq(min(meta.ParAb.dat$year.c), max(meta.ParAb.dat$year.c), by = 6)
  ) +
  ggtitle("Analysis of publication bias for the effect on parasitoid abundance variance",
          subtitle = "Decline effect")  

  ## Adjusting effect sizes
mResults_ParAb.Var.Adj <- modResultsAdjPb("ParAb", "Var")
print(mResults_ParAb.Var.Adj)

    # Let's quickly check the model fit again
rmaQQplot("ParAb", "Var", "Parasitoid abundance", "lnCVR", pb = T) # Better
rmaResidHist("ParAb", "Var", "Parasitoid abundance", "lnCVR", pb = T)

  ## Sensitivity analysis
sensAnCompl("ParAb", "Var", "lnCVR", "Parasitoid abundance", run_sens_ana = run_sens_ana)
sensAnCor(meta.ParAb.dat, "lnCVR", meta.ParAb.Var.int, meta.ParAb.Var.int.pb)

  # Okay there may be an effect masked by one paper
    # Dively et al. 2012
meta.ParAb.dat %>%
  filter(Paper_ID == 30)

meta.ParAb.dat %>%
  filter(Exp_ID == 36)

  ## Re-fitting the model

    # So we exclude data...
meta.ParAb.dat.postSA <- meta.ParAb.dat %>% filter(Paper_ID != 30)

    # Re-fit the model with the right VCOV Matrix...
vcov.ParAb.lnCVR.05.postSA <- vcalc(vi = lnCVR_vi, 
                                cluster = clusterID, 
                                obs = ES_ID, 
                                data = meta.ParAb.dat.postSA, 
                                rho = 0.5)

meta.ParAb.Var.postSA.int <-
  update(meta.ParAb.Var.int, V = vcov.ParAb.lnCVR.05.postSA, 
         data = meta.ParAb.dat.postSA)

mResults_ParAb.Var.postSA.int <- orchaRd::mod_results(meta.ParAb.Var.postSA.int, mod = "1", group = "Exp_ID")

  # Check for publication bias again: it is still detected
meta.ParAb.Var.postSA.int.pb <- 
  update(meta.ParAb.Var.int.pb, V = vcov.ParAb.lnCVR.05.postSA, 
         data = meta.ParAb.dat.postSA)

mResults_ParAb.Var.postSA.int.pb <- orchaRd::mod_results(meta.ParAb.Var.postSA.int.pb, mod = "1", group = "Exp_ID")
summary(meta.ParAb.Var.postSA.int.pb)

bubble_plot(meta.ParAb.Var.postSA.int.pb, mod = "year.c", group = "Exp_ID") +
  scale_x_continuous(
    labels = seq(min(meta.ParAb.dat$Publication_year), max(meta.ParAb.dat$Publication_year), by = 6),
    breaks = seq(min(meta.ParAb.dat$year.c), max(meta.ParAb.dat$year.c), by = 6)
  ) +
  ggtitle("Analysis of publication bias for the effect on parasitoid abundance variance",
          subtitle = "Decline effect, after sensitivity analyses")  

  # Checking residuals again
rmaQQplot("ParAb", "Var", "Parasitoid abundance", "lnCVR", pb = T, postSA = T)
rmaResidHist("ParAb", "Var", "Parasitoid abundance", "lnCVR", pb = T, postSA = T)

  # Sensitivity to correlation level in VCOV matrix
sensAnCor(meta.ParAb.dat.postSA, "lnCVR", meta.ParAb.Var.postSA.int, meta.ParAb.Var.postSA.int.pb)

  ## Final plot
mResults_ParAb.Var.postSA.Adj <- modResultsAdjPb("ParAb", "Var", postSA = T)
meta.ParAb.Var.modResultsAdjPb <- plotAdjEst("ParAb", "Var", "lnCVR", "Parasitoid abundance", postSA = T)
meta.ParAb.Var.modResultsAdjPb

  ###### ''8B3) Final plot for meta-analysis of parasitoid abundance ----

meta.ParAb.modResultsAdjPb <-
  ggpubr::ggarrange(meta.ParAb.Mean.modResultsAdjPb + 
              ggtitle("Meta-analysis of the effect on parasitoid abundance", "Mean") + 
              ylab("lnRR") +
              theme(axis.text.y = element_blank()), 
            meta.ParAb.Var.modResultsAdjPb +
              ggtitle("", "Variance") + 
              ylab("lnCVR") +
              theme(axis.text.y = element_blank())
  )


  ## ---------------------------------- #
  ###### 8C) Parasitism rate ----

meta.ParR.dat <- meta.tab %>%
  filter(Variable == "Parasitism_rate")

  ###### ''8C1) Meta-analysis of mean ----

    ## Here we construct the VCOV matrix for dependence
vcov.ParR.lnRR.05 <- 
  metafor::vcalc(vi = lnRR_vi, 
                 cluster = clusterID, 
                 obs = ES_ID, 
                 data = meta.ParR.dat, 
                 rho = 0.5)


    ## Now we test the random structure (models are fitted with ML)

      # Only Shared Study
meta.ParR.Mean.1 <- rma.mv(lnRR_yi, vcov.ParR.lnRR.05,
                            random = ~1|sh_stud, 
                            data = meta.ParR.dat,
                            method = 'ML')

      # Shared Study AND Experiment (nested)
meta.ParR.Mean.2 <- update(meta.ParR.Mean.1, random = ~1|sh_stud/Exp_Nb)
      # Shared Experiment only
meta.ParR.Mean.3 <- update(meta.ParR.Mean.1, random = ~1|Exp_ID)
      # Shared Experiment and Year
meta.ParR.Mean.4 <- update(meta.ParR.Mean.1, random = ~1|Exp_ID/Exp_rep)
      # Shared Experiment and Sub_experiment
meta.ParR.Mean.5 <- update(meta.ParR.Mean.1, random =~1|Exp_ID/Sub_exp,
                            data = ungroup(meta.ParR.dat) %>% 
                              mutate(Sub_exp = ifelse(is.na(Sub_exp), 1, Sub_exp))
)
      # AIC comparison
AIC(meta.ParR.Mean.1, meta.ParR.Mean.2, meta.ParR.Mean.3,
    meta.ParR.Mean.4, meta.ParR.Mean.5) 
    # -> Keeping model 4 clearly (Season nested within experiment)

  ## So we can fit our intercept-only model using REML:
meta.ParR.Mean.int <- update(meta.ParR.Mean.4, method = 'REML')
summary(meta.ParR.Mean.int)
mResults_ParR.Mean.int <- orchaRd::mod_results(meta.ParR.Mean.int, mod = "1", group = "Exp_ID")
print(mResults_ParR.Mean.int)
orchard_plot(mResults_ParR.Mean.int, xlab = "lnRR") +
  ggtitle("Meta-analysis of parasitism rate mean",
          subtitle = "Intercept-only model")  +
  scale_colour_manual(values = "#CC6677") +
  scale_fill_manual(values = "#CC6677")

    # Checking random effect fitting
if(profile_random) profile.rma.mv(meta.ParR.Mean.int)

  ## Model with moderators
meta.ParR.Mean.mods <- update(meta.ParR.Mean.int, mods = ~ Mod - 1)
summary(meta.ParR.Mean.mods)
mResults_ParR.Mean.mods <- orchaRd::mod_results(meta.ParR.Mean.mods, mod = "Mod", group = "Exp_ID")
print(mResults_ParR.Mean.mods)
orchard_plot(mResults_ParR.Mean.mods, xlab = "lnRR") +
  ggtitle("Meta-analysis of parasitism rate mean",
          subtitle = "Model with moderators")

    # Checking random effect fitting
if(profile_random) profile.rma.mv(meta.ParR.Mean.mods)

    # Model comparison
anova.rma(meta.ParR.Mean.int)
anova.rma(meta.ParR.Mean.mods)
modelCompTab("ParR", "Mean")

  ## Analysing model performance and heterogeneity

    # Model performance
modelperfTab("ParR", "Mean")

    # Residual distribution
rmaQQplot("ParR", "Mean", "Parasitism rate", "lnRR")
rmaResidHist("ParR", "Mean", "Parasitism rate", "lnRR")

  ## Publication bias

    # Funnel plots
funnel(meta.ParR.Mean.int, yaxis = "seinv")
funnel(meta.ParR.Mean.int, yaxis = "seinv",
       xlim = c(-5, 5), ylim = c(0.1, 15))
      # No clear evidence
essFunnel("ParR", "Mean", "lnRR")
      # Here maybe?

# Proper test
meta.ParR.Mean.int.pb <- update(meta.ParR.Mean.int, mods = ~ ess_se + year.c)
summary(meta.ParR.Mean.int.pb) # No evidence indeed
anova(meta.ParR.Mean.int.pb)

# Graphical analysis
bubble_plot(meta.ParR.Mean.int.pb, mod = "ess_se", group = "Exp_ID") +
  ggtitle("Analysis of publication bias for the effect on parasitism rate mean",
          subtitle = "Small study effect")

bubble_plot(meta.ParR.Mean.int.pb, mod = "year.c", group = "Exp_ID") +
  scale_x_continuous(
    labels = seq(min(meta.ParR.dat$Publication_year), max(meta.ParR.dat$Publication_year), by = 6),
    breaks = seq(min(meta.ParR.dat$year.c), max(meta.ParR.dat$year.c), by = 6)
  ) +
  ggtitle("Analysis of publication bias for the effect on parasitism rate mean",
          subtitle = "Decline effect")  
    # But the first paper (Leius et al. 1967) has a big effect though!


## Sensitivity analysis
sensAnCompl("ParR", "Mean", "lnRR", "Parasitism rate", run_sens_ana = run_sens_ana)
sensAnCor(meta.ParR.dat, "lnRR", meta.ParR.Mean.int, meta.ParR.Mean.int.pb)

meta.ParR.dat.noOutlier <- meta.ParR.dat %>% filter(lnRR_outl == "Ok")
vcov.ParR.lnRR.05.noOutlier <- 
  metafor::vcalc(vi = lnRR_vi, 
                 cluster = clusterID, 
                 obs = ES_ID, 
                 data = meta.ParR.dat.noOutlier, 
                 rho = 0.5)

update(meta.ParR.Mean.int, 
       data = meta.ParR.dat.noOutlier,
       V = vcov.ParR.lnRR.05.noOutlier)



## Final plot
meta.ParR.Mean.modResultsAdjPb <- plotAdjEst("ParR", "Mean", "lnRR", "Parasitism rate")
meta.ParR.Mean.modResultsAdjPb


  ###### ''8C2) Meta-analysis of variance ----

    ## Here we construct the VCOV matrix for dependence
vcov.ParR.lnCVR.05 <- 
  metafor::vcalc(vi = lnCVR_vi, 
                 cluster = clusterID, 
                 obs = ES_ID, 
                 data = meta.ParR.dat, 
                 rho = 0.5)

    ## Now we test the random structure (models are fitted with ML)

      # Only Shared Study
meta.ParR.Var.1 <- rma.mv(lnCVR_yi, vcov.ParR.lnCVR.05,
                           random = ~1|sh_stud, 
                           data = meta.ParR.dat,
                           method = 'ML')

      # Shared Study AND Experiment (nested)
meta.ParR.Var.2 <- update(meta.ParR.Var.1, random = ~1|sh_stud/Exp_Nb)
      # Shared Experiment only
meta.ParR.Var.3 <- update(meta.ParR.Var.1, random = ~1|Exp_ID)
      # Shared Experiment and Year
meta.ParR.Var.4 <- update(meta.ParR.Var.1, random = ~1|Exp_ID/Exp_rep)
      # Shared Experiment and Sub_experiment
meta.ParR.Var.5 <- update(meta.ParR.Var.1, random =~1|Exp_ID/Sub_exp,
                           data = ungroup(meta.ParR.dat) %>% 
                             mutate(Sub_exp = ifelse(is.na(Sub_exp), 1, Sub_exp))
)
    # AIC comparison
AIC(meta.ParR.Var.1, meta.ParR.Var.2, meta.ParR.Var.3,
    meta.ParR.Var.4, meta.ParR.Var.5) 
      # -> Keeping model 3 clearly (Experiment ID)

  ## So we can fit our intercept-only model using REML:
meta.ParR.Var.int <- update(meta.ParR.Var.3, method = 'REML')
summary(meta.ParR.Var.int)
mResults_ParR.Var.int <- orchaRd::mod_results(meta.ParR.Var.int, mod = "1", group = "Exp_ID")
print(mResults_ParR.Var.int)
orchard_plot(mResults_ParR.Var.int, xlab = "lnCVR") +
  ggtitle("Meta-analysis of parasitism rate variance",
          subtitle = "Intercept-only model")

    # Checking random effect fitting
if(profile_random) profile.rma.mv(meta.ParR.Var.int)

  ## Model with moderators
meta.ParR.Var.mods <- update(meta.ParR.Var.int, mods = ~ Mod - 1)
summary(meta.ParR.Var.mods)
mResults_ParR.Var.mods <- orchaRd::mod_results(meta.ParR.Var.mods, mod = "Mod", group = "Exp_ID")
print(mResults_ParR.Var.mods)
orchard_plot(mResults_ParR.Var.mods, xlab = "lnCVR") +
  ggtitle("Meta-analysis of parasitism rate variance",
          subtitle = "Model with moderators")

    # Checking random effect fitting
if(profile_random) profile.rma.mv(meta.ParR.Var.mods)

    # Model comparison
anova.rma(meta.ParR.Var.int)
anova.rma(meta.ParR.Var.mods)
modelCompTab("ParR", "Var")

  ## Analysing model performance and heterogeneity

    # Model performance
modelperfTab("ParR", "Var")

    # Residual distribution
rmaQQplot("ParR", "Var", "Parasitism rate", "lnCVR")
rmaResidHist("ParR", "Var", "Parasitism rate", "lnCVR")

  ## Publication bias

    # Funnel plots
funnel(meta.ParR.Var.int, yaxis = "seinv")
funnel(meta.ParR.Var.int, yaxis = "seinv",
       xlim = c(-5, 5), ylim = c(0.1, 3))
essFunnel("ParR", "Var", "lnCVR") # No clear evidence


    # Proper test
meta.ParR.Var.int.pb <- update(meta.ParR.Var.int, mods = ~ ess_se + year.c)
summary(meta.ParR.Var.int.pb) # No evidence indeed
anova(meta.ParR.Var.int.pb)

    # Graphical analysis
bubble_plot(meta.ParR.Var.int.pb, mod = "ess_se", group = "Exp_ID") +
  ggtitle("Analysis of publication bias for the effect on parasitism rate variance",
          subtitle = "Small study effect")

bubble_plot(meta.ParR.Var.int.pb, mod = "year.c", group = "Exp_ID") +
  scale_x_continuous(
    labels = seq(min(meta.ParR.dat$Publication_year), max(meta.ParR.dat$Publication_year), by = 6),
    breaks = seq(min(meta.ParR.dat$year.c), max(meta.ParR.dat$year.c), by = 6)
  ) +
  ggtitle("Analysis of publication bias for the effect on parasitism rate variance",
          subtitle = "Decline effect")  
    # See Leius et al. 1967 again though


  ## Sensitivity analysis
sensAnCompl("ParR", "Var", "lnCVR", "Parasitism rate", run_sens_ana = run_sens_ana)
meta.ParR.dat %>%
  filter(Paper_ID == 4) %>%
  select(Var_ID, ctrl_Mean, ctrl_SD, tmt_Mean, tmt_SD)
    # When removing this paper the effect disappears
    # Indeed we have important variances in this paper
      # Let's keep the results that way though
      # The paper does not de part as much from the distribution
      # as in other sensitivity analyses
      # Let's keep that in mind in other papers

  ## Final plot
meta.ParR.Var.modResultsAdjPb <- plotAdjEst("ParR", "Var", "lnCVR", "Parasitism rate")
meta.ParR.Var.modResultsAdjPb

  ###### ''8C3) Final plot for meta-analysis of parasitism rate ----

meta.ParR.modResultsAdjPb <-
  ggpubr::ggarrange(meta.ParR.Mean.modResultsAdjPb + 
              ggtitle("Meta-analysis of the effect on parasitism rate", "Mean") + 
              ylab("lnRR") +
              theme(axis.text.y = element_blank()), 
            meta.ParR.Var.modResultsAdjPb +
              ggtitle("", "Variance") + 
              ylab("lnCVR") +
              theme(axis.text.y = element_blank())
  )

meta.ParR.modResultsAdjPb

  ## ---------------------------------- #
  ###### 8D) Yield ----

meta.Yield.dat <- meta.tab %>%
  filter(Variable == "Yield")

  ###### '8D1) Meta-analysis of mean ----

    ## Here we construct the VCOV matrix for dependence
vcov.Yield.lnRR.05 <- 
  metafor::vcalc(vi = lnRR_vi, 
                 cluster = clusterID, 
                 obs = ES_ID, 
                 data = meta.Yield.dat, 
                 rho = 0.5)


    ## Now we test the random structure (models are fitted with ML)

      # Only Shared Study
meta.Yield.Mean.1 <- rma.mv(lnRR_yi, vcov.Yield.lnRR.05,
                           random = ~1|sh_stud, 
                           data = meta.Yield.dat,
                           method = 'ML')

      # Shared Study AND Experiment (nested)
meta.Yield.Mean.2 <- update(meta.Yield.Mean.1, random = ~1|sh_stud/Exp_Nb)
      # Shared Experiment only
meta.Yield.Mean.3 <- update(meta.Yield.Mean.1, random = ~1|Exp_ID)
      # Shared Experiment and Year
meta.Yield.Mean.4 <- update(meta.Yield.Mean.1, random = ~1|Exp_ID/Exp_rep)
      # Shared Experiment and Sub_experiment
meta.Yield.Mean.5 <- update(meta.Yield.Mean.1, random =~1|Exp_ID/Sub_exp,
                           data = ungroup(meta.Yield.dat) %>% 
                             mutate(Sub_exp = ifelse(is.na(Sub_exp), 1, Sub_exp))
)
      # AIC comparison
AIC(meta.Yield.Mean.1, meta.Yield.Mean.2, meta.Yield.Mean.3,
    meta.Yield.Mean.4, meta.Yield.Mean.5) 
        # -> Keeping model 4 although not that much more informative


  ## So we can fit our intercept-only model using REML:
meta.Yield.Mean.int.complex <- update(meta.Yield.Mean.4, method = 'REML')
summary(meta.Yield.Mean.int.complex)
mResults_Yield.Mean.int <- orchaRd::mod_results(meta.Yield.Mean.int.complex, mod = "1", group = "Exp_ID")
print(mResults_Yield.Mean.int)
orchard_plot(mResults_Yield.Mean.int, xlab = "lnRR") +
  ggtitle("Meta-analysis of parasitoid abundance mean",
          subtitle = "Intercept-only model")  +
  scale_colour_manual(values = "#CC6677") +
  scale_fill_manual(values = "#CC6677")

  # Checking random effect fitting
profile.rma.mv(meta.Yield.Mean.int.complex)
  # Does not converge for Exp.rep: let's keep it simple with model 3
meta.Yield.Mean.int <- update(meta.Yield.Mean.3, method = 'REML')
summary(meta.Yield.Mean.int)
mResults_Yield.Mean.int <- orchaRd::mod_results(meta.Yield.Mean.int, mod = "1", group = "Exp_ID")
print(mResults_Yield.Mean.int)
orchard_plot(mResults_Yield.Mean.int, xlab = "lnRR") +
  ggtitle("Meta-analysis of parasitoid abundance mean",
          subtitle = "Intercept-only model")  +
  scale_colour_manual(values = "#CC6677") +
  scale_fill_manual(values = "#CC6677")

  # Now it's ok
profile.rma.mv(meta.Yield.Mean.int)

  ## Model with moderators
meta.Yield.Mean.mods <- update(meta.Yield.Mean.int, mods = ~ Mod - 1)
summary(meta.Yield.Mean.mods)
mResults_Yield.Mean.mods <- orchaRd::mod_results(meta.Yield.Mean.mods, mod = "Mod", group = "Exp_ID")
print(mResults_Yield.Mean.mods)
orchard_plot(mResults_Yield.Mean.mods, xlab = "lnRR") +
  ggtitle("Meta-analysis of parasitoid abundance mean",
          subtitle = "Model with moderators")

    # Checking random effect fitting
if(profile_random) profile.rma.mv(meta.Yield.Mean.mods)

    # Model comparison
anova.rma(meta.Yield.Mean.int)
anova.rma(meta.Yield.Mean.mods)
modelCompTab("Yield", "Mean")

  ## Analysing model performance and heterogeneity

    # Model performance
modelperfTab("Yield", "Mean")

    # Residual distribution
rmaQQplot("Yield", "Mean", "Yield", "lnRR") # Ouch, bad fit
rmaResidHist("Yield", "Mean", "Yield", "lnRR")

  ## Publication bias

    # Funnel plots
funnel(meta.Yield.Mean.int, yaxis = "seinv")
funnel(meta.Yield.Mean.int, yaxis = "seinv",
       xlim = c(-5, 5), ylim = c(0.1, 25)) # Seems weird
    # No clear evidence
essFunnel("Yield", "Mean", "lnRR")  # Here too
    # Here maybe?

    # Proper test
meta.Yield.Mean.int.pb <- update(meta.Yield.Mean.int, mods = ~ ess_se + year.c)
summary(meta.Yield.Mean.int.pb) # Clear evidence of publication bias
anova(meta.Yield.Mean.int.pb)

    # Graphical analysis
bubble_plot(meta.Yield.Mean.int.pb, mod = "ess_se", group = "Exp_ID") +
  ggtitle("Analysis of publication bias for the effect on mean yield",
          subtitle = "Small study effect")

bubble_plot(meta.Yield.Mean.int.pb, mod = "year.c", group = "Exp_ID") +
  scale_x_continuous(
    labels = seq(min(meta.Yield.dat$Publication_year), max(meta.Yield.dat$Publication_year), by = 6),
    breaks = seq(min(meta.Yield.dat$year.c), max(meta.Yield.dat$year.c), by = 6)
  ) +
  ggtitle("Analysis of publication bias for the effect on mean yield",
          subtitle = "Decline effect")  

    ## Adjusting effect sizes

mResults_Yield.Mean.Adj <- modResultsAdjPb("Yield", "Mean")
print(mResults_Yield.Mean.Adj)

    # Let's quickly check the model fit again
rmaQQplot("Yield", "Mean", "Yield", "lnCVR", pb = T)
rmaResidHist("Yield", "Mean", "Yield", "lnCVR", pb = T)
  # Not very good -> keep in mind that model fit is poor

  ## Sensitivity analysis
sensAnCompl("Yield", "Mean", "lnRR", "Yield", run_sens_ana = run_sens_ana)

    # When removing that paper, the effect becomes significant
      # Maybe because of high variances?
meta.Yield.dat %>%
  filter(Paper_ID == 104)

meta.Yield.dat %>%
  filter(Exp_ID == 117)

meta.Yield.dat %>%
  filter(Exp_ID == 117) %>%
  dplyr::select(lnRR_vi,lnRR_sei)

  # But this is one on only 9 papers
  # And even if the effect becomes significant it is <2% increase anyway
  # Moreover, we didn't even consider publication bias here
  # Let's keep it that way?

  # Sensitivity to correlation levels
    # Very sensitive: not enough data -> impossible to answer
sensAnCor(meta.Yield.dat, "lnRR", meta.Yield.Mean.int, meta.Yield.Mean.int.pb)

## Final plot
meta.Yield.Mean.modResultsAdjPb <- plotAdjEst("Yield", "Mean", "lnRR", "Yield")
meta.Yield.Mean.modResultsAdjPb

  ###### '8D2) Meta-analysis of variance ----

## Here we construct the VCOV matrix for dependence
vcov.Yield.lnCVR.05 <- 
  metafor::vcalc(vi = lnCVR_vi, 
                 cluster = clusterID, 
                 obs = ES_ID, 
                 data = meta.Yield.dat, 
                 rho = 0.5)


## Now we test the random structure (models are fitted with ML)

# Only Shared Study
meta.Yield.Var.1 <- rma.mv(lnCVR_yi, vcov.Yield.lnCVR.05,
                          random = ~1|sh_stud, 
                          data = meta.Yield.dat,
                          method = 'ML')

# Shared Study AND Experiment (nested)
meta.Yield.Var.2 <- update(meta.Yield.Var.1, random = ~1|sh_stud/Exp_Nb)
# Shared Experiment only
meta.Yield.Var.3 <- update(meta.Yield.Var.1, random = ~1|Exp_ID)
# Shared Experiment and Year
meta.Yield.Var.4 <- update(meta.Yield.Var.1, random = ~1|Exp_ID/Exp_rep)
# Shared Experiment and Sub_experiment
meta.Yield.Var.5 <- update(meta.Yield.Var.1, random =~1|Exp_ID/Sub_exp,
                          data = ungroup(meta.Yield.dat) %>% 
                            mutate(Sub_exp = ifelse(is.na(Sub_exp), 1, Sub_exp))
)
# AIC comparison
AIC(meta.Yield.Var.1, meta.Yield.Var.2, meta.Yield.Var.3,
    meta.Yield.Var.4, meta.Yield.Var.5) 
# -> Keeping model 1

## So we can fit our intercept-only model using REML:
meta.Yield.Var.int <- update(meta.Yield.Var.1, method = 'REML')
summary(meta.Yield.Var.int)
mResults_Yield.Var.int <- orchaRd::mod_results(meta.Yield.Var.int, mod = "1", group = "Exp_ID")
print(mResults_Yield.Var.int)
orchard_plot(mResults_Yield.Var.int, xlab = "lnCVR") +
  ggtitle("Meta-analysis of parasitoid abundance variance",
          subtitle = "Intercept-only model")

# Checking random effect fitting
if(profile_random) profile.rma.mv(meta.Yield.Var.int)

## Model with moderators
meta.Yield.Var.mods <- update(meta.Yield.Var.int, mods = ~ Mod - 1)
summary(meta.Yield.Var.mods)
mResults_Yield.Var.mods <- orchaRd::mod_results(meta.Yield.Var.mods, mod = "Mod", group = "Exp_ID")
print(mResults_Yield.Var.mods)
orchard_plot(mResults_Yield.Var.mods, xlab = "lnCVR") +
  ggtitle("Meta-analysis of parasitoid abundance variance",
          subtitle = "Model with moderators")

# Checking random effect fitting
if(profile_random) profile.rma.mv(meta.Yield.Var.mods)

# Model comparison
anova.rma(meta.Yield.Var.int)
anova.rma(meta.Yield.Var.mods)
modelCompTab("Yield", "Var")

## Analysing model performance and heterogeneity

# Model performance
modelperfTab("Yield", "Var")

# Residual distribution
rmaQQplot("Yield", "Var", "Yield", "lnCVR") # ok
rmaResidHist("Yield", "Var", "Yield", "lnCVR")

## Publication bias

# Funnel plots
funnel(meta.Yield.Var.int, yaxis = "seinv")
essFunnel("Yield", "Var", "lnCVR") # Maybe?


# Proper test
meta.Yield.Var.int.pb <- update(meta.Yield.Var.int, mods = ~ ess_se + year.c)
summary(meta.Yield.Var.int.pb) # No clear evidence (there may be a trend, not enough data?)
anova(meta.Yield.Var.int.pb)

# Graphical analysis
bubble_plot(meta.Yield.Var.int.pb, mod = "ess_se", group = "Exp_ID") +
  ggtitle("Analysis of publication bias for the effect on parasitoid abundance variance",
          subtitle = "Small study effect")

bubble_plot(meta.Yield.Var.int.pb, mod = "year.c", group = "Exp_ID") +
  scale_x_continuous(
    labels = seq(min(meta.Yield.dat$Publication_year), max(meta.Yield.dat$Publication_year), by = 6),
    breaks = seq(min(meta.Yield.dat$year.c), max(meta.Yield.dat$year.c), by = 6)
  ) +
  ggtitle("Analysis of publication bias for the effect on parasitoid abundance variance",
          subtitle = "Decline effect")  
  # Could be a small one?

## Sensitivity analysis
sensAnCompl("Yield", "Var", "lnCVR", "Yield", run_sens_ana = run_sens_ana)
sensAnCor(meta.Yield.dat, "lnRR", meta.Yield.Var.int, meta.Yield.Var.int.pb)

## Final plot
meta.Yield.Var.modResultsAdjPb <- plotAdjEst("Yield", "Var", "lnCVR", "Yield")
meta.Yield.Var.modResultsAdjPb

  ###### '8D3) Final plot for meta-analysis of yield ----

meta.Yield.modResultsAdjPb <-
  ggpubr::ggarrange(meta.Yield.Mean.modResultsAdjPb + 
              ggtitle("Meta-analysis of the effect on parasitoid abundance", "Mean") + 
              ylab("lnRR") +
              theme(axis.text.y = element_blank()), 
            meta.Yield.Var.modResultsAdjPb +
              ggtitle("", "Variance") + 
              ylab("lnCVR") +
              theme(axis.text.y = element_blank())
  )

meta.Yield.modResultsAdjPb


  ## ---------------------------------- #
  # 8E) Final results ----

    # '8E1) Final plot ----
allMa.mean.df <- 
  bind_rows(
    list(
      'Pest density' = mResults_PD.Mean.int$data,
      'Parasitism rate' = mResults_ParR.Mean.int$data,
      'Yield' = mResults_Yield.Mean.int$data,
      'Parasitoid abundance' = mResults_ParAb.Mean.int$data
    ),
    .id = "Response"
  ) %>%
  mutate(Response = fct_rev(fct_relevel(Response,
                                        c("Parasitoid abundance",
                                          "Parasitism rate",
                                          "Pest density",
                                          "Yield")))) %>%
  mutate(metrics = "Mean (lnRR)")

allMA.mean.res <- 
  bind_rows(
    list(
      'Pest density' = mResults_PD.Mean.postSA.Adj %>%
        mutate(type = "ROM"),
      'Parasitism rate' = mResults_ParR.Mean.int$mod_table %>%
        mutate(Estimate = "Unadjusted",
               type = "ROM"),
      'Yield' = mResults_Yield.Mean.int$mod_table %>%
        mutate(Estimate = "Unadjusted",
               type = "ROM"),
      'Parasitoid abundance' = mResults_ParAb.Mean.int$mod_table %>%
        mutate(Estimate = "Unadjusted",
               type = "ROM")
    ),
    .id = "Response"
  ) %>%
  mutate(metrics = "Mean (lnRR)")  %>%
  group_by(Response) %>%
  mutate(Overall_estimate = ifelse(n() > 1 & Estimate == "Unadjusted",
                                   "Unadjusted (Evidence for pb)",
                                   "Unadjusted (No evidence for pb) \nor Adjusted (Evidence for pb)"))



final_plot_mean <- 
  allMa.mean.df %>%
  ggplot() +
  ggbeeswarm::geom_quasirandom(
    aes(y = yi, x = Response, size = 1/sqrt(vi), 
        colour = Response, fill = Response),
    alpha = 0.6) +
  geom_hline(yintercept = 0, lty = "dashed") +
  xlab(NULL) +
  ylab("lnRR") +
  coord_flip() +
  guides(fill = "none",
         colour = "none") +
  geom_linerange(data = allMA.mean.res, 
                 aes( 
                   x = Response, 
                   ymin = lowerCL, 
                   ymax = upperCL),
                 position = ggplot2::position_dodge2(width = 0.1),
                 size = 1.2) +
  geom_pointrange(data = allMA.mean.res, 
                  aes(y = estimate, 
                      x = Response, 
                      ymin = lowerPR, 
                      ymax = upperPR,
                      shape = fct_rev(Overall_estimate)),
                  position = ggplot2::position_dodge2(width = 0.1),
                  fatten = 6) +
  facet_grid(fct_rev(metrics)~., space = "free", scales = "free", switch = "y") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(angle = 90, 
                                   hjust = 0.5, 
                                   size = 12)) +
  ggtitle("Meta-analysis of mean") +
  scale_shape_discrete("Overall estimate")


allMa.var.df <- 
  bind_rows(
    list(
      'Pest density' = mResults_PD.Var.int$data,
      'Parasitism rate' = mResults_ParR.Var.int$data,
      'Yield' = mResults_Yield.Var.int$data,
      'Parasitoid abundance' = mResults_ParAb.Var.int$data
    ),
    .id = "Response"
  ) %>%
  mutate(Response = fct_rev(fct_relevel(Response,
                                        c("Parasitoid abundance",
                                          "Parasitism rate",
                                          "Pest density",
                                          "Yield")))) %>%
  mutate(metrics = "Variance (lnCVR)")

allMA.var.res <- 
  bind_rows(
    list(
      'Pest density' = mResults_PD.Var.int$mod_table %>%
        mutate(Estimate = "Unadjusted"),
      'Parasitism rate' = mResults_ParR.Var.int$mod_table %>%
        mutate(Estimate = "Unadjusted"),
      'Yield' = mResults_Yield.Var.int$mod_table %>%
        mutate(Estimate = "Unadjusted"),
      'Parasitoid abundance' = mResults_ParAb.Var.postSA.Adj
    ),
    .id = "Response"
  )  %>%
  group_by(Response) %>%
  mutate(Overall_estimate = ifelse(n() > 1 & Estimate == "Unadjusted",
                                   "Unadjusted (Evidence for pb)",
                                   "Unadjusted (No evidence for pb) \nor Adjusted (Evidence for pb)"))

final_plot_var <- 
  allMa.var.df %>%
  ggplot() +
  ggbeeswarm::geom_quasirandom(
    aes(y = yi, x = Response, size = 1/sqrt(vi), 
        colour = Response, fill = Response),
    alpha = 0.6) +
  geom_hline(yintercept = 0, lty = "dashed") +
  xlab(NULL) +
  ylab("lnCVR") +
  coord_flip() +
  guides(fill = "none",
         colour = "none") +
  geom_linerange(data = allMA.var.res, 
                 aes( 
                   x = Response, 
                   ymin = lowerCL, 
                   ymax = upperCL),
                 position = ggplot2::position_dodge2(width = 0.1),
                 size = 1.2) +
  geom_pointrange(data = allMA.var.res, 
                  aes(y = estimate, 
                      x = Response, 
                      ymin = lowerPR, 
                      ymax = upperPR,
                      shape = fct_rev(Overall_estimate)),
                  position = ggplot2::position_dodge2(width = 0.1),
                  fatten = 6)  +
  theme(legend.position = "bottom",
        axis.text.y = element_blank()) +
  facet_grid(metrics~., space = "free", scales = "free", switch = "y") +
  ggtitle("Meta-analysis of variance") +
  scale_shape_discrete("Overall estimate") +
  scale_y_continuous(minor_breaks = seq(-3, 2, by = 1))

final_plot <- 
  ggpubr::ggarrange(ggpubr::ggarrange(final_plot_mean + 
                                        guides(shape = "none") +
                                        theme(legend.position.inside = c(1, 0),
                                              legend.justification.inside = c(1, 0),
                                              legend.position = "inside",
                                              legend.direction="horizontal",
                                              legend.text = element_text(
                                                size = 6
                                              )) +
                                        labs(size = "Precision") +
                                        scale_size_continuous(breaks = c(10, 30, 50)), 
                                      final_plot_var + 
                                        guides(shape = "none") +
                                        theme(legend.position.inside = c(1, 0),
                                              legend.justification.inside = c(1, 0),
                                              legend.position = "inside",
                                              legend.direction="horizontal",
                                              legend.text = element_text(
                                                size = 6
                                              )) +
                                        labs(size = "Precision")+
                                        scale_size_continuous(breaks = c(1, 3, 5)),
                                      widths = c(25, 24)
  ),
  get_legend(final_plot_var)[[1]][[2]],
  ncol = 1,
  heights = c(20, 1)) +
  theme(panel.background = element_rect(fill='white'))


final_plot

if(save_meta_results) {
  
  write_rds(allMa.mean.df, 'Outputs/ma_mean_df.rds')
  write_rds(allMA.mean.res, 'Outputs/ma_mean_res.rds')
  write_rds(allMa.var.df, 'Outputs/ma_var_df.rds')
  write_rds(allMA.var.res, 'Outputs/ma_var_res.rds')
  
}

  # '8E2) Final table ----

  # check esSumm() and esFinalTable() for custom function description
meta.finalTable <- esFinalTable(c("PD", "ParAb", "ParR", "Yield"))
meta.finalTable %>% mutate(across(where(is.numeric), ~round(., 2)))

####################################################################### #
# 9) Descriptive analysis of variables not included in meta-anaysis #####
####################################################################### #
otherVar.dat <- meta.tab %>%
  filter(!Variable %in% c('Pest_density', 'Parasitoid_abundance',
                          'Parasitism_rate', 'Yield')) %>% 
  mutate(Var_aggregated =
           case_when(
             
             Variable %in% c("Parasitoid_longevity", "Parasitoid_fecundity", "Sex_ratio") ~ 
               "Life history traits",
             
             Variable %in% c("Parasitoid_feeding", "Parasitoid_nectar_feeding") ~ 
               "Sugar feeding",
             
             Variable %in% c("Parasitoid_diversity", "Trophic_link") ~ 
               "Diversity",
             
             Variable %in% c('Pest_damage') ~ 'Pest damage',
             
             .default = Variable
           ))

ggplot(otherVar.dat,
       aes(y = Var_aggregated,
           x = lnRR_yi,
           size = 1/(lnRR_sei),
           colour = Variable)) +
  geom_jitter() + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(Var_aggregated~., space = "free", scale = "free") +
  ylab(NULL) +
  xlab("Mean difference effect size (lnRR)") +
  ggtitle("Effect size (lnRR) for other variables related to parasitoids") +
  scale_size_continuous("Precision (1/SE)") + 
  theme(strip.text.y = element_blank())
  
####################### #
#### 10) Conclusions ####
####################### #


# Pest density ---

# Decline effect due mainly to one paper (but opposite to expected!)
# That clearly drives results
# Removing that paper reveals significant effect
# ... but there is evidence of publication bias for the mean (small-study effect)
# This removes significant effect!
# Sensitivity analyses may reveal a small significant effect
#### --> Still unclear but if there is an effect, it is likely small

# No effect on the variance


# Parasitoid abundance ---

# Mean : significant effect, no evidence of publication bias (!)
# Maybe thanks to multiple measures each time
# lnRR = 0.25
# That means that on average there are exp(0.25) = 1.3 times more parasitoids
# When sugar sources are provided

# Variance : publication bias due to decline effect 
# (initial studies suggested that sugar decreased abundance variance, but nope)
# no effect detected here after adjusting for that bias
# note that we removed one paper strongly driving results with just two ES
# following sensitivity analyses

# Parasitism rate

# Significant effect on mean AND variance (+ mean, - variance)
# Check prediction intervals and amount of unexplained variance: high variability in outcome
# No significant publication bias detected but early results may lead to over-evaluation

# Results on variance are sensitive to influential studies
# The detected effect is small but may even not exist


# Mean : lnRR = 0.23
# That means that on average there are exp(0.43) = 1.3 times more parasitoids
# When sugar sources are provided

# Variance: lnRR = 0.17
# That means that variation is reduced by about 16% (exp(-0.17) = 0.84)
# Note that these are CV so scaled to the mean

# Yield

# no effect detected whatsoever
# BUT results are very sensitive to analytical decisions
# (although results suggest that if an effect exists, it is very small)
# probably due to a lack of data

# So finally:
# Effect on parasitoid abundance that may reflect on parasitism rate (though weaker)
# Reduced variance of parasitism rate too: "stabilizing effect" of pest control
# ... but no evidence that it translates into lower pest levels
# And clearly, no detected effect on yield
# Evidence of publication bias for some variables (despite additional data)
# Explore this further by looking at added vs published data
# Even when not clearly detected there are some signs (precision asymmetry, high early values)

# Effects of moderators:
# No detected effect of sugar source but not enough data, clearly
# No clear effect of position of nectar sources (inside or outside the crop)
# Effects on abundance and parasitism are detected when sugar are either inside or outside the crop
# This goes against e.g. Vollhardt et al. 2010

# What is missing (but we won't do it, so for the future):
# What other moderators could influence results? (exploratory analysis)
# add them as moderators (and calculate e.g. R²)
# OR meta-analysis of residuals?
# Maybe first option

# Other stuff:
# Other response variables missing (cf "Other")
# Notably variables that could explain why PR does not translated into PD
# Life-history traits (longevity, fecundity), behaviour, actual sugar feeding
## Promising results from the data we have? But probably p. bias in such cases with few data published
## (cf Koricheva, Gurevitch)

# Alternative sugar sources: not much data
# You can also refer to the previous review (Wade et al. 2008) for more stuff
# Honeydew!!! Clearly studies are missing
# Although there are some examples when quantitative differences are measured

# Are parasitoids really sugar-limited in agroecosystems
# We can't really answer that (but maybe see the results about honeydew-feeding)