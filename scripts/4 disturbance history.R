
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
  group_by(plot_id, year) %>% #if plot level then group by  `plot_id`, `year` else `year`
  count() %>%
  group_by(plot_id) %>% # if per plot add this
  complete(year = c(1650:2000), fill = list(n = 0)) 
  mutate(n_depth = cumsum(n),
         n_per = n_depth * 100 / max(n_depth)) %>% 
  ungroup()  ->
  sample.depth.df
  
# proportion of events
event.df %>% 
  rename(event = event_bl) %>% # Please select the method: event_bl or event_ai
  filter(!is.na(event)) %>%
  inner_join(tree.use,  by = c('plot_id', 'tree_id')) %>%
  mutate(year = ifelse(event %in% 'gap', year - age, year)) %>%
  group_by(plot_id, event, year) %>% #if plot level then group by  `plot_id`, `event`,`year` else `event`,`year`
  count() %>%
  ungroup() %>%
  complete(year = c(1650:2000), nesting(event, plot_id), fill = list(n = 0)) %>% # if per plot add nesting(plot_id)
  inner_join(sample.depth.df %>% select(-n), by = c('plot_id','year')) %>% # if per plot add join by plot_id year
  filter(n_per >= 10, n_depth >= 3) %>%
  mutate(rel_per = n * 100 / n_depth)  %>%
  ungroup() %>%
  mutate(event = factor(event, levels = c('no event','moderate','major',  'gap')))->  # if the `event_bl` is selected
  #mutate(event = factor(event, levels = c('no event','release',  'gap')))->    # if the `event_ai` is selected
  event.perc.df
  



# 2. Visualize the histogram of event -------------------------------------

# 2.1 Disturbance history with absolute sample depth ----------------------

quartz(width = 3.2, height = 2)
event.perc.df %>%
  ggplot() +
  gstyle() +
  sinca_fill('') +
  geom_histogram(aes(year, weight = rel_per, fill = event), breaks = seq(1600, 2000, 10),alpha = 1)+
  scale_x_continuous("Calendar year", limits = c(1650,2000)) + #, breaks = seq(1600, 2000, 100)
  scale_y_continuous( limits = c(0, 30), sec.axis = sec_axis(~.*20, name = 'Sampling depth (n trees)'))+
  ylab("% trees disturbed") +
  geom_line(data = sample.depth.df, aes(year, n_depth / 20), color = "grey40", linetype = 2)


# 2.2 Plot level disturbance with absolute sample depth ----------------------

quartz(width = 6.6, height = 9, pointsize = 12)
event.perc.df %>%
  ggplot() +
  gstyle() +
  sinca_fill('') +
  geom_histogram(aes(year, weight = rel_per, fill = event), breaks = seq(1600, 2000, 10),alpha = 1)+
  scale_x_continuous("Calendar year", limits = c(1650,2000)) +
  scale_y_continuous( limits = c(0, 75), sec.axis = sec_axis(~.*1, name = 'Sampling depth (n trees)'))+
  ylab("% trees disturbed") +
  geom_line(data = sample.depth.df, aes(year, n_depth / 1), color = "grey40", linetype = 2) +
  facet_wrap(~plot_id, ncol = 4)


# 2.3 Yearly distribution with MDS ----------------------------------------

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

quartz(width = 3.2, height = 2)
event.perc.df %>%
  ggplot() +
  gstyle() +
  sinca_fill('') +
  geom_histogram(aes(year, weight = rel_per, fill = event), breaks = seq(1600, 2000, 1),alpha = 0.4)+
  scale_x_continuous("Calendar year", limits = c(1650,2000)) +
  scale_y_continuous( limits = c(0, 10), sec.axis = sec_axis(~.*60, name = 'Sampling depth (n trees)'))+
  ylab("% trees disturbed") +
  geom_line(data = sample.depth.df, aes(year, n_depth / 60), color = "grey40", linetype = 2)+
  geom_line(data = mds.df, aes(year, rel_per / 10), colour = '#7CAE00', size = 0.8) +
  geom_line(data = age.dens, aes(year, n / 10), colour = '#78c2ef', linetype = 2, size = 0.8) +
  coord_cartesian(xlim=c(1800, 2000))
