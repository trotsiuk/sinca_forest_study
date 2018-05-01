
# About -------------------------------------------------------------------
# growth trend analysis of the trees 

# 0 Libraries and data ----------------------------------------------------
pacman::p_load(tidyverse, broom,cowplot)

#- data
growth.df <- read_csv('data/growth.csv')
event.df <- read_csv('data/event.csv')
core.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'core') %>% mutate_at(vars(missing_years, missing_mm), funs(as.numeric))
tree.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'tree')

ring.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'ring')

# 0 Support Functions -----------------------------------------------------
source('scripts/0_functions.R')

# 1. Growth trends of the trees for different storey ----------------------
tree.use <- core.df %>% filter(!is.na(missing_years), missing_years <= 20) %>% distinct(plot_id, tree_id)

growth.df %>%
  inner_join(tree.use,  by = c('plot_id', 'tree_id')) %>%
  left_join(event.df %>% select(plot_id, tree_id, year, event = event_bl), by = c('plot_id','tree_id', 'year')) %>%
  inner_join(tree.df %>% filter(layer %in% c("Overstorey", "Midstorey")) %>% select(plot_id, tree_id, layer), by = c('plot_id','tree_id')) %>%
  mutate(layer = factor(layer, levels = c("Overstorey", "Midstorey")))  ->
  growth.sel.df

# only gap recruit growth.sel.df <- growth.sel.df %>% inner_join(event.df %>% filter(event %in% 'gap') %>% distinct(tree_id), by = 'tree_id')

#' Growth trends for the species
growth.gg <- growth.sel.df %>%
  ggplot(aes(age, dbh_mm, color = species, group = interaction(plot_id, tree_id))) +
  geom_line(alpha = 0.1, size = rel(0.1)) +
  geom_smooth(aes(group = species), se = T, alpha = 1, size = rel(0.1), fill = 'grey80') +
  facet_wrap(~layer, ncol = 1) +
  gstyle() +
  sinca_color('')+
  xlab("Age (years)") + ylab("Cumulative DBH (mm)") +
  coord_cartesian(ylim = c(0, 700))


#' Growth releases
event.dbh.df <- growth.sel.df %>%
  filter(!is.na(event), !event %in% "gap") %>%
  arrange(tree_id, year) %>%
  group_by(plot_id, tree_id) %>%
  mutate(event_n = 1:n()) %>%
  filter(event_n <= 2) 

ntreesclustevent <- event.dbh.df %>%
  group_by(layer, event_n, species) %>%
  count() %>%
  spread(species, n)
  
#- remove the Synchronized    Midstorey     3                5            1
dbh_sign.df <- event.dbh.df %>%
  group_by( layer, event_n) %>%
  filter(n_distinct(species) == 2, n() > 4) %>%
  do(tidy(t.test(dbh_mm~ species, data=.))) %>%
  select(event_n, p.value) %>%
  mutate(signif = cut(p.value, breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1), labels = c("***", "**", "*", ".","ns"))) %>%
  ungroup() 

box.gg <- event.dbh.df %>%
  group_by(layer, event_n) %>%
  filter(n_distinct(species) == 2, n() > 4) %>%
  ungroup() %>%
  mutate(species = factor(species, levels = c("Fagus sylvatica", "Abies alba"))) %>%
  ggplot()+
  geom_boxplot(aes( as.factor(event_n), dbh_mm,  fill = species),
               size = 0.06, outlier.size =  0.06, 
               position = position_dodge2(preserve = "total"), width = 0.8)+
  geom_text(data = dbh_sign.df, aes (event_n, 700, label = signif, group = 1))+
  facet_wrap(~layer, ncol = 1) +
  gstyle() +
  sinca_fill('')+
  xlab("Event nuber") +ylab("DBH (mm)") +
  theme(legend.position = "none")



quartz(width = 6.3, height = 5, pointsize = 12)

plot_grid(growth.gg, box.gg,
          nrow = 1, ncol = 2,
          rel_widths = c(4.0, 2.3))
