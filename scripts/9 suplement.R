
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

# 2. Boxplot of the trees with certain DBH --------------------------------

quartz(width = 3.2, height = 2, pointsize = 12)

growth.df %>%
  inner_join(tree.use,  by = c('plot_id', 'tree_id')) %>%
  group_by(plot_id, tree_id) %>%
  filter(dbh_mm %in% dbh_mm[which.min(abs(dbh_mm-160))]) %>%
  ungroup() %>%
  ggplot()+
  geom_boxplot(aes(species, age, fill = species)) +
  gstyle() +
  sinca_fill('') +
  xlab('') + ylab('DBH (mm)') +
  theme(legend.position = "none")


quartz(width = 3.2, height = 2, pointsize = 12)
growth.df %>%
  inner_join(tree.use,  by = c('plot_id', 'tree_id')) %>%
  group_by(species, plot_id, tree_id) %>%
  summarise(age_16 = age[which.min(abs(160-dbh_mm))],
    recruitment = min(year, na.rm = T)) %>%
  ungroup()%>%
  ggplot(aes(recruitment, age_16, colour = species))+
  geom_point(size = 0.8, alpha = 0.5) +
  geom_smooth() +
  gstyle() +
  sinca_color('') +
  coord_cartesian(ylim = c(0, 300)) +
  xlab('Calendar year of recruitment') + ylab('Age at DBH = 160mm ')



# 2. DBH distribution of the cored tree -----------------------------------

quartz(width = 3.2, height = 2, pointsize = 12)

growth.df %>%
  group_by(species, plot_id, tree_id) %>%
  filter(dbh_mm == max(dbh_mm),
    dbh_mm >= 160) %>%
  ungroup() %>%
  inner_join(tree.use,  by = c('plot_id', 'tree_id')) %>%
  ggplot()+
  gstyle()+
  sinca_fill('')+
  geom_histogram(aes(dbh_mm, fill = species), breaks = seq(0, 1100, 50))+
  scale_x_continuous("DBH (mm)", limits = c(0,1000), breaks = seq(0, 1200, 200)) +
  coord_cartesian( ylim = c(0,100))+
  ylab("Number of trees") 