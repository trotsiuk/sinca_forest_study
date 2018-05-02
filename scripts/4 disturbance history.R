
# About -------------------------------------------------------------------
# growth trend analysis of the trees 

# 0 Libraries and data ----------------------------------------------------
pacman::p_load(tidyverse, broom,cowplot)

#- data
growth.df <- read_csv('data/growth.csv')
event.df <- read_csv('data/event.csv')
core.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'core') %>% mutate_at(vars(missing_years, missing_mm), funs(as.numeric))
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
  rename(event = event_ai) %>%
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
  #mutate(event = factor(event, levels = c('no event','moderate','major',  'gap')))->
  mutate(event = factor(event, levels = c('no event','release',  'gap')))->
  event.perc.df
  

# 2. Visualize the histogram of event -------------------------------------
quartz(width = 3.2, height = 2)
event.perc.df %>%
  ggplot() +
  gstyle() +
  sinca_fill('') +
  geom_histogram(aes(year, weight = rel_per, fill = event), breaks = seq(1600, 2000, 10))+
  scale_x_continuous("Calendar year", limits = c(1650,2000)) +
  scale_y_continuous( limits = c(0, 30), sec.axis = sec_axis(~.*3.333333, name = 'Sampling depth (%)'))+
  ylab("% trees disturbed") +
  geom_line(data = sample.depth.df, aes(year, n_per / 3.333333), color = "grey40", linetype = 2)
