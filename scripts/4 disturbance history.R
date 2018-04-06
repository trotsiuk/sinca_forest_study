
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
base_size <- 5
gstyle <- list(
  theme_bw(base_size = base_size),
  theme(axis.text = element_text(size = base_size *1.2, angle = 0,  colour = "grey10"),
        axis.title=element_text(size=base_size*1.4),
        axis.ticks.length=unit(base_size*-0.15, "mm"),
        axis.ticks = element_line(size = base_size * 0.05),
        axis.text.x = element_text(margin=margin(2,0,1,0,"mm")),
        axis.text.y.right = element_text(margin=margin(0,2,0,2,"mm")),
        axis.text.y = element_text(margin=margin(0,2,0,2,"mm"))) ,
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_rect(colour = "black")) ,
  theme(strip.background = element_rect(colour = "white", fill = "white", size = base_size*1.1),
        strip.text.x = element_text(colour = "black", angle = 0, size = base_size*1.1,
                                    hjust = 0.5, vjust = 0.5)),
  theme(legend.key.size =  unit(base_size*1, "mm"), legend.key.height =  unit(base_size*1, "mm")),
  theme(legend.text = element_text(size=base_size * 1.2),
        legend.title = element_text(size=base_size * 1.5)),
  theme(legend.key = element_rect(colour = 'white', fill = 'white', linetype='dashed', size =0.1)),
  theme(legend.justification=c(0,1), legend.position=c(0,1)) 
)



# 1. Prepare the dataset --------------------------------------------------
tree.use <- core.df %>% filter(!is.na(missing_years), missing_years <= 20) %>% distinct(tree_id)

# sample depth
growth.df %>%
  group_by(tree_id) %>%
  summarise(year = min(year)) %>%
  inner_join(tree.use, by = 'tree_id') %>%
  mutate(plot_id = substr(tree_id, 0, 1)) %>%
  group_by(year) %>%
  count() %>%
  ungroup() %>%
  complete(year = c(1650:2000), fill = list(n = 0)) %>%
  mutate(n_per = cumsum(n) * 100 / sum(n)) %>%
  ungroup()  ->
  sample.depth.df
  
# proportion of events
event.df %>%
  inner_join(tree.use, by = 'tree_id') %>%
  mutate(plot_id = substr(tree_id, 0, 1)) %>%
  group_by(event, year) %>%
  count() %>%
  ungroup() %>%
  complete(year = c(1650:2000), nesting(event), fill = list(n = 0)) %>%
  inner_join(sample.depth.df %>% select(year, n_per), by = c('year')) %>%
  filter(n_per >= 10) %>%
  mutate(rel_per = n * 100 / n_per)  %>%
  ungroup() ->
  event.perc.df
  

# 2. Visualize the histogram of event -------------------------------------
quartz(width = 3.2, height = 2)
event.perc.df %>%
  ggplot() +
  gstyle +
  geom_histogram(aes(year, weight = rel_per, fill = event), binwidth = 10, breaks = seq(1600, 2000, 10))+
  scale_x_continuous("Calendar year", limits = c(1650,2000)) +
  scale_y_continuous( limits = c(0, 60), sec.axis = sec_axis(~.*1.666667, name = 'Sampling depth (%)'))+
  ylab("% trees disturbed") +
  scale_fill_manual('',values = c("gap" = "#7CAE00", "release" = "#da2c3a", "no event" = "#78c2ef")) +
  geom_line(data = sample.depth.df, aes(year, n_per / 1.666667), color = "grey40", linetype = 2)