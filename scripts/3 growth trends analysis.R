
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
base_size <- 5
gstyle <- list(
  theme_bw(base_size = base_size),
  theme(axis.text = element_text(size = base_size *1.2, angle = 0,  colour = "grey10"),
        axis.title=element_text(size=base_size*1.4),
        axis.ticks.length=unit(base_size*-0.15, "mm"),
        axis.ticks = element_line(size = base_size * 0.05),
        axis.text.x = element_text(margin=margin(2,0,1,0,"mm")),
        axis.text.y = element_text(margin=margin(0,2,0,1,"mm"))) ,
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_rect(colour = "black")) ,
  theme(strip.background = element_rect(colour = "white", fill = "white", size = base_size*1.1),
        strip.text.x = element_text(colour = "black", angle = 0, size = base_size*1.1,
                                    hjust = 0.5, vjust = 0.5)),
  theme(legend.justification=c(1,0), legend.position=c(0.95,0.05)),
  theme(legend.key.size =  unit(base_size*1, "mm"), legend.key.height =  unit(base_size*1, "mm")),
  theme(legend.text = element_text(size=base_size * 1.2),
        legend.title = element_text(size=base_size * 1.5))+
    theme(legend.key = element_rect(colour = 'white', fill = 'white', linetype='dashed', size =0.1))
)





# 1. Growth trends of the trees for different storey ----------------------
growth.df %>%
  left_join(event.df %>% select(tree_id, year, event), by = c('tree_id', 'year')) %>%
  inner_join(core.df %>% filter(!is.na(missing_years), missing_years <= 20) %>% distinct(tree_id), by = 'tree_id') %>%
  inner_join(tree.df %>% filter(layer %in% c("Overstorey", "Midstorey")) %>% select(plot_id, tree_id, layer), by = 'tree_id') %>%
  mutate(layer = factor(layer, levels = c("Overstorey", "Midstorey"))) ->
  growth.sel.df

# only gap recruit growth.sel.df <- growth.sel.df %>% inner_join(event.df %>% filter(event %in% 'gap') %>% distinct(tree_id), by = 'tree_id')


#--/ Growth trends for the species
growth.gg <- growth.sel.df %>%
  ggplot(aes(age, dbh_mm, color = species, group = tree_id)) +
  geom_line(alpha = 0.1, size = base_size * 0.01) +
  geom_smooth(aes(group = species), se = T, alpha = 1, size = base_size * 0.1, fill = 'grey80') +
  facet_wrap(~layer, ncol = 1) +
  gstyle +
  scale_color_manual('', values = c("Fagus sylvatica" = "#da2c3a", "Abies alba" = "#78c2ef"),
                     labels = c("Fagus sylvatica" = "European beech", "Abies alba" = "Silver fir"))+
  xlab("Age (years)") + ylab("Cumulative DBH (mm)") +
  coord_cartesian(ylim = c(0, 700))


#--/ Growth releases
event.dbh.df <- growth.sel.df %>%
  filter(!is.na(event), !event %in% "gap") %>%
  arrange(tree_id, year) %>%
  group_by(tree_id) %>%
  mutate(event_n = 1:n()) %>%
  filter(event_n <= 3) 

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
               size = base_size*0.06, outlier.size =  base_size*0.06, 
               position = position_dodge2(preserve = "total"), width = base_size * 0.15)+
  geom_text(data = dbh_sign.df, aes (event_n, 700, label = signif, group = 1))+
  facet_wrap(~layer, ncol = 1) +
  gstyle +
  scale_fill_manual('', values = c("Fagus sylvatica" = "#da2c3a", "Abies alba" = "#78c2ef"),
                     labels = c("Fagus sylvatica" = "European beech", "Abies alba" = "Silver fir"))+
  xlab("Event nuber") +
  theme(legend.position = "none")



quartz(width = 6.3, height = 5, pointsize = 12)

plot_grid(growth.gg, box.gg,
          nrow = 1, ncol = 2,
          rel_widths = c(4.0, 2.3))
