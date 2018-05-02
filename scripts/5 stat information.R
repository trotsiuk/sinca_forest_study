
# About -------------------------------------------------------------------
# growth trend analysis of the trees 

# 0 Libraries and data ----------------------------------------------------
pacman::p_load(stats, tidyverse, broom,cowplot, zoo)

#- data
growth.df <- read_csv('data/growth.csv')
event.df <- read_csv('data/event.csv')
core.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'core') %>% mutate_at(vars(missing_years, missing_mm), funs(as.numeric))
#tree.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'tree')


# 1. Prepare the dataset --------------------------------------------------
tree.use <- core.df %>% filter(!is.na(missing_years), missing_years <= 20) %>% distinct(plot_id, tree_id)



# 2. Table statistings on the tree growth and analysis --------------------
growth.df <- growth.df %>% inner_join(tree.use,  by = c('plot_id', 'tree_id'))
event.df <- event.df  %>% inner_join(tree.use,  by = c('plot_id', 'tree_id'))



# tree age
growth.df %>%
  group_by(species, plot_id, tree_id) %>%
  summarise(age = max(age, na.rm = T)) %>%
  group_by(species) %>%
  select(-tree_id) %>%
  summarise_at(vars(age), funs(median, sd, max, quantile(., 0.9))) %>%
  gather(variable, value, -species) %>%
  spread(species, value) %>%
  mutate(variable = paste0('age_', variable))->
  age.df
    
# dist history proportion
event.df %>% 
  group_by(species, event=event_bl) %>%
  filter(!is.na(event)) %>%
  count() %>%
  group_by(species) %>%
  mutate(n = n * 100 / sum (n)) %>%
  spread(species, n) %>%
  rename(variable = event)->
  eve.df
  

# tree growth and longevity
growth.df %>%
  group_by(species, plot_id, tree_id) %>%
  mutate(incr_mean = rollapply(incr_mm, width = 10, FUN = mean, fill = NA)) %>%
  summarise(gro_meadian = median(incr_mm, na.rm = T),
    gro_10_max = max(incr_mean, na.rm = T),
    gro_10_min = min(incr_mean, na.rm = T),
    gro_max = max(incr_mm, na.rm = T),
    gro_min = min(incr_mm, na.rm = T)) %>%
  group_by(species) %>%
  select(-tree_id) %>%
  summarise_all(funs(median, sd)) %>%
  gather(variable, value, -species) %>%
  spread(species, value)->
  gro.df

bind_rows(gro.df,  age.df, eve.df) %>%
  mutate_at(vars(`Abies alba`, `Fagus sylvatica`), funs(round(., 2))) %>%
  write_csv('figs/stat_table.csv')
