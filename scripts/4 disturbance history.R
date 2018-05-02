
# About -------------------------------------------------------------------
# growth trend analysis of the trees 

# 0 Libraries and data ----------------------------------------------------
pacman::p_load(tidyverse, broom,cowplot, zoo)

#- data
growth.df <- read_csv('data/growth.csv')
event.df <- read_csv('data/event.csv')
age_pred.df <- read_csv('data/age_pred.csv')
core.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'core')
tree.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'tree')


# 0 Support Functions -----------------------------------------------------
source('scripts/0_functions.R')

# 1. Prepare the dataset --------------------------------------------------
tree.use <- core.df %>% filter(!is.na(missing_years), missing_years <= 20) %>% distinct(plot_id, tree_id)

# sample depth
growth.df %>%
  group_by(plot_id, tree_id) %>%
  summarise(year = min(year) - min(age)) %>%
  inner_join(tree.use,  by = c('plot_id', 'tree_id')) %>%
  group_by(year) %>%
  count() %>%
  ungroup() %>%
  complete(year = c(1650:2000), fill = list(n = 0)) %>%
  mutate(n_depth = cumsum(n),
         n_per = n_depth * 100 / max(n_depth)) %>% 
  ungroup()  ->
  sample.depth.df
  
# proportion of events
event.df %>%
  rename(event = event_bl) %>%
  filter(!is.na(event)) %>%
  inner_join(tree.use,  by = c('plot_id', 'tree_id')) %>%
  mutate(year = ifelse(event %in% 'gap', year - age, year)) %>%
  group_by(event, year) %>%
  count() %>%
  ungroup() %>%
  complete(year = c(1650:2000), nesting(event), fill = list(n = 0)) %>%
  inner_join(sample.depth.df %>% select(year, n_per, n_depth), by = c('year')) %>%
  filter(n_per >= 10) %>%
  mutate(rel_per = n * 100 / n_depth)  %>%
  ungroup() %>%
  mutate(event = factor(event, levels = c('no event','moderate','major',  'gap')))->
  #mutate(event = factor(event, levels = c('no event','release',  'gap')))->
  event.perc.df
  
# Calculate MDS
event.perc.df %>%
  group_by(year) %>%
  summarise(rel_per = sum(rel_per, na.rm = T)) %>%
  ungroup() %>%
  complete(year = 1600:2030, fill = list(rel_per = 0)) %>%
  mutate(rel_per = mdsFun(rel_per, k = 30, bw = 5, st = 7)) %>%
  filter(year %in% c(1650:2000))->
  mds.df

# recruitment dencity

age_pred.df %>%
  group_by(year) %>%
  count() %>%
  ungroup() %>%
  complete(year = 1600:2030, fill = list(n = 0)) %>%
  mutate(n = mdsFun(n, k = 30, bw = 5, st = 100)) %>%
  filter(year %in% c(1650:2000))  ->
  age.dens


# 2. Visualize the histogram of event -------------------------------------
quartz(width = 3.2, height = 2)
event.perc.df %>%
  ggplot() +
  gstyle() +
  sinca_fill('') +
  geom_histogram(aes(year, weight = rel_per, fill = event), breaks = seq(1600, 2000, 1),alpha = 0.4)+
  scale_x_continuous("Calendar year", limits = c(1650,2000)) +
  scale_y_continuous( limits = c(0, 10), sec.axis = sec_axis(~.*10, name = 'Sampling depth (%)'))+
  ylab("% trees disturbed") +
  geom_line(data = sample.depth.df, aes(year, n_per / 10), color = "grey40", linetype = 2)+
  geom_line(data = mds.df, aes(year, rel_per / 10), colour = '#7CAE00', size = 1.2) +
  geom_line(data = age.dens, aes(year, n / 10), colour = '#78c2ef', linetype = 2, size = 1.2) +
  coord_cartesian(xlim=c(1800, 2000))
