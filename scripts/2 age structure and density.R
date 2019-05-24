
# About -------------------------------------------------------------------
# make the clustering of all plots based on the age of two species
# plus their interquantile range of ages

# 0 Libraries and data ----------------------------------------------------
pacman::p_load(NbClust, tidyverse, broom, cowplot)

#- data
growth.df <- read_csv('data/growth.csv')
tree.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'tree')
core.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'core')

# 0 Support Functions -----------------------------------------------------
source('scripts/0_functions.R')

# 1. Age distributions of reliable age ------------------------------------
tree.use <- core.df %>% filter(!is.na(missing_years), missing_years <= 20) %>% distinct(plot_id, tree_id)

growth.df %>%
  group_by(plot_id, tree_id, species) %>%
  summarise(year = min(year),
            age = min(age)) %>%
  inner_join(tree.use,  by = c('plot_id', 'tree_id')) %>%
  mutate(year = year - age) %>%
  ungroup() ->
  age.df
  

# 2. Predict age of the rest of trees -------------------------------------

#' data to predict age
tree.df %>%
  filter(status %in% c(11)) %>%
  anti_join(tree.use,  by = c('plot_id', 'tree_id')) %>%
  select(plot_id, tree_id, species, dbh_mm) %>%
  mutate(sporig = species,
         species = recode(species, 'Acer pseudoplatanus' = 'Fagus sylvatica', 'Carpinus betulus' = 'Fagus sylvatica')) %>%
  filter(sporig %in% c('Fagus sylvatica', 'Abies alba')) ->
  tree.age



#' fit the equation and predict age
growth.df %>%
  inner_join(tree.use,  by = c('plot_id', 'tree_id')) %>%
  #group_by(species) %>%
  do(
    fit = lm(age ~ dbh_mm * species, data = .)
    #fit = nlme::lme(age ~ dbh_mm, ~1|plot_id/species, data=., method="ML", na.action = na.omit, correlation = nlme::corAR1())
  ) %>%
  # broom::glance(fit)
  broom::augment(fit, newdata = tree.age) %>%
  select(plot_id, tree_id, species = sporig, age = `.fitted`) %>%
  mutate(year = 2012 - round(age, 0),
         est = 'estimated age') %>%
  bind_rows(age.df %>% mutate(est = 'counted age')) %>%
  mutate(est = factor(est, levels = c('estimated age','counted age')))->
  age.df.pred



write_csv(age.df.pred, 'data/age_pred.csv')
# 3. Visualization --------------------------------------------------------
data.d <- age.df.pred #age.df
data.d <- age.df

#' Histogram of the age
quartz(width = 6.8, height = 2, pointsize = 12)

data.d %>%
  ggplot() +
  gstyle() +
  geom_histogram(aes(year, fill = species, alpha = est), breaks = seq(1600, 2000, 10)) +
  # geom_histogram(aes(year, fill = species), breaks = seq(1600, 2000, 1)) +
  scale_x_continuous("Calendar year", limits = c(1600,2000), breaks = seq(1600, 2000, 100)) +
  coord_cartesian( ylim = c(0,300))+ #300 # 60
  # facet_wrap(~species, ncol = 2) +
  ylab("Number of trees") +
  sinca_fill('') +
  scale_alpha_discrete('', range = c(0.3, 1))

ggsave( 'figs/fig 1 age non reliable 10 years.pdf', width = 3.2, height = 2, units = c("in"), dpi = 'retina', bg = "transparent")



#' Density of age
age.dig <- data.d %>%  group_by(species) %>% 
  do({ x <- .
  td <- density(x$year)
  data.frame(year = td$x[which.max(td$y)], dens = td$y[which.max(td$y)])
  })

quartz(width = 3.2, height = 2, pointsize = 12)

data.d %>%
  ggplot() +
  gstyle() +
  geom_density(aes(year, fill = species)) +
  sinca_fill_alpha('') +
  geom_vline(data = age.dig, aes(xintercept = year), linetype = 2, colour = 'grey60') +
  geom_text(data = age.dig, aes(year, dens + 0.0005, label = round(year, 0)), colour = 'grey20', size = rel(2.5)) +
  coord_cartesian(xlim = c(1600, 2000)) +
  xlab('Calendar year') +
  ylab('Density')


#' Plot level disturbances
quartz(width = 6.6, height = 9, pointsize = 12)
data.d %>% 
  ggplot() +
  gstyle() +
  geom_histogram(aes(year, fill = species, alpha = est), breaks = seq(1600, 2000, 10)) +
  scale_x_continuous("Calendar year", limits = c(1600,2000), breaks = seq(1600, 2000, 100)) +
  coord_cartesian( ylim = c(0,30))+
  ylab("Number of trees") +
  sinca_fill('') +
  scale_alpha_discrete('', range = c(0.3, 1)) +
  facet_wrap(~plot_id, ncol = 4)
