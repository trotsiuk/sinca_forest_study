
# About -------------------------------------------------------------------
# make the clustering of all plots based on the age of two species
# plus their interquantile range of ages

# 0 Libraries and data ----------------------------------------------------
pacman::p_load(NbClust, tidyverse, broom,cowplot)

#- data
tree.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'tree')
core.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'core') %>% mutate_at(vars(missing_years, missing_mm), funs(as.numeric))
ring.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'ring')



# 0 Support Functions -----------------------------------------------------


# 1 Visualize the clusters ------------------------------------------------
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
                                    hjust = 0.5, vjust = 0.5))
)


age.df <- core.df %>%
  filter(missing_years <= 20,
    !is.na(missing_years)) %>%
  #mutate(missing_years = ifelse(is.na(missing_years), 0, missing_years)) %>%
  inner_join(ring.df %>% group_by(tree_id) %>% summarise(year = min(year)), by = 'tree_id') %>%
  inner_join(tree.df %>% select(plot_id, tree_id, species), by = 'tree_id') %>%
  mutate(year = year - missing_years + 1)


#--- Age structure
quartz(width = 3.2, height = 2, pointsize = 12)

age.df %>%
  ggplot() +
  gstyle +
  geom_histogram(aes(year, fill = species), binwidth = 10, breaks = seq(1600, 2000, 10))+
  scale_x_continuous("Calendar year", limits = c(1600,2000), breaks = seq(1600, 2000, 100)) +
  coord_cartesian( ylim = c(0,50))+
  ylab("Number of trees") +
  scale_fill_manual('', values = c("Fagus sylvatica" = "#da2c3a",
                               "Abies alba" = "#78c2ef"),
                    labels = c("Fagus sylvatica" = "European beech",
                               "Abies alba" = "Silver fir")) +
  theme(legend.key.size =  unit(base_size*1, "mm"), legend.key.height =  unit(base_size*1, "mm"))+
  theme(legend.text = element_text(size=base_size * 1.2),
        legend.title = element_text(size=base_size * 1.5))+
  theme(legend.key = element_rect(colour = 'white', fill = 'white', linetype='dashed', size =0.1)) +
  theme(legend.justification=c(0,1), legend.position=c(0.05,0.95))




#--- Age density of the trees
age.dig <- age.df %>%  group_by(species) %>% 
  do({ x <- .
  td <- density(x$year)
  data.frame(year = td$x[which.max(td$y)], dens = td$y[which.max(td$y)])
  })

quartz(width = 3.2, height = 2, pointsize = 12)
age.df %>%
  ggplot() +
  gstyle +
  geom_density(aes(year, fill = species)) +
  #scale_x_continuous("Calendar year", limits = c(1600,2000), breaks = seq(1600, 2000, 100)) +
  ylab("Density") +
  scale_fill_manual('', values = alpha(c("Fagus sylvatica" = "#da2c3a", "Abies alba" = "#78c2ef"), 0.5),
    labels = c("Fagus sylvatica" = "European beech", "Abies alba" = "Silver fir")) +
  theme(legend.key.size =  unit(base_size*1, "mm"), legend.key.height =  unit(base_size*1, "mm"))+
  theme(legend.text = element_text(size=base_size * 1.2),
    legend.title = element_text(size=base_size * 1.5))+
  theme(legend.key = element_rect(colour = 'white', fill = 'white', linetype='dashed', size =0.1)) +
  theme(legend.justification=c(0,1), legend.position=c(0.05,0.95)) +
  geom_vline(data = age.dig, aes(xintercept = year), linetype = 2, colour = 'grey60') +
  geom_text(data = age.dig, aes(year, dens +0.0005, label = round(year, 0)), colour = 'grey20', size = rel(2.5))