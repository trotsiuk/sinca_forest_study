
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



# 1. Prepare the data for clustering --------------------------------------
#--/ For the cluster analysis we use the following parameters
#- 1. Mean recruitment year per plot
#- 2. Range in years IQR (25-75 %) per plot
#- 3. Mean age of Beech recruitment 
#- 4. mean age of Fir recruitment

# All parameters were scaled; only trees with less than 20 missing years were used
# Also check how many available trees are per each class

#--/ Prepare the table for the cluster analysis. There are at least 10 trees per plot
core.df %>%
  filter(missing_years <= 20,
         !is.na(missing_years)) %>%
  inner_join(ring.df %>% group_by(tree_id) %>% summarise(year = min(year)), by = 'tree_id') %>%
  inner_join(tree.df %>% select(plot_id, tree_id, species), by = 'tree_id') %>%
  mutate(year = year - missing_years + 1) %>%
  group_by(plot_id) %>%
  dplyr::summarise(
    recruit_mean = mean(year),
    iqr = quantile(year, 0.75) - quantile(year, 0.25),
    recruit_beech = mean(year[species %in% "Fagus sylvatica"]),
    recruit_fir = mean(year[species %in% "Abies alba"])
  ) %>%
  mutate(recruit_difference = recruit_beech - recruit_fir) %>%
  select(plot_id, recruit_mean, iqr, recruit_beech, recruit_fir) %>%
  ungroup() %>% as.data.frame() ->
  dataCluster.df

pl.id <- dataCluster.df$plot_id
rownames(dataCluster.df) <- dataCluster.df$plot_id
dataCluster.df <- dataCluster.df %>% select(-plot_id)



# 2 Test for the possible number of clusters ------------------------------
set.seed(2)
nc <- NbClust(dataCluster.df %>% scale(), min.nc = 2, max.nc = 10, method = "kmeans")

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

# ***** Conclusion *****                            
#   * According to the majority rule, the best number of clusters is  2 




# 3 Do the clustering -----------------------------------------------------
clusterPlots <- kmeans(dataCluster.df, centers = 2, iter.max = 20, nstart = 25)

cluster.df <- dataCluster.df %>%
  as.data.frame.matrix() %>%
  mutate(plot_id = pl.id,
         plot_cluster = clusterPlots$cluster,
         plot_cluster = ifelse(!plot_cluster %in% 1, "Prolonged", "Synchronized"))

write_csv(cluster.df, "data/clusters.csv")



# 4 Visualize the clusters ------------------------------------------------
cluster.df <- read_csv("data/clusters.csv")

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


#--- Upper row
top_row <- core.df %>%
  filter(missing_years <= 20,
         !is.na(missing_years)) %>%
  inner_join(ring.df %>% group_by(tree_id) %>% summarise(year = min(year)), by = 'tree_id') %>%
  inner_join(tree.df %>% select(plot_id, tree_id, species), by = 'tree_id') %>%
  mutate(year = year - missing_years + 1) %>%
  inner_join( cluster.df %>% select(plot_id, plot_cluster), by = "plot_id") %>%
  mutate(plot_cluster = factor(plot_cluster, levels = c("Synchronized", "Prolonged"))) %>%
  ggplot() +
  gstyle +
  geom_histogram(aes(year, fill = species), binwidth = 10, breaks = seq(1600, 2000, 10))+
  facet_wrap(~plot_cluster, nrow = 1) +
  scale_x_continuous("Calendar year", limits = c(1600,2000), breaks = seq(1600, 2000, 100)) +
  coord_cartesian( ylim = c(0,45))+
  ylab("Number of trees") +
  scale_fill_manual('', values = c("Fagus sylvatica" = "#da2c3a",
                               "Abies alba" = "#78c2ef"),
                    labels = c("Fagus sylvatica" = "European beech",
                               "Abies alba" = "Silver fir")) +
  theme(legend.key.size =  unit(base_size*1, "mm"), legend.key.height =  unit(base_size*1, "mm"))+
  theme(legend.text = element_text(size=base_size * 1.2),
        legend.title = element_text(size=base_size * 1.5))+
  theme(legend.key = element_rect(colour = 'white', fill = 'white', linetype='dashed', size =0.1)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) +
  geom_text(data = cluster.df %>%
              mutate(plot_cluster = factor(plot_cluster, levels = c("Synchronized", "Prolonged"))) %>%
              group_by(plot_cluster) %>%
              dplyr::summarise(N = paste0("N plots - ",n())),
            aes(1650, 40, label=N), size = base_size*0.5, colour = "grey10")


#--- Lower panel with boxplot
cluster.gg <- cluster.df %>%
  select(-plot_id) %>%
  gather(., "Parameter","Observations",-plot_cluster) %>%
  mutate(plot_cluster = factor(plot_cluster, levels = c("Synchronized", "Prolonged")),
         Parameter = recode(Parameter, recruit_mean = 'Recruitment year \n mean (year)',
                            iqr = "Recruitment year IQR \n (year)", 
                            recruit_beech = "Recruitment year mean \n European beech (year)",
                            recruit_fir =  "Recruitment year mean \n Silver fir (year)"),
         Parameter = factor(Parameter, levels = c("Recruitment year \n mean (year)", "Recruitment year IQR \n (year)",
                                                  "Recruitment year mean \n European beech (year)", 
                                                  "Recruitment year mean \n Silver fir (year)")))

#--/ Signifficance in the boxplots
boxpl.signifficance <- cluster.gg %>%
  group_by(Parameter) %>%
  do(tidy(t.test(Observations~plot_cluster, data=.))) %>%
  select(Parameter, p.value) %>%
  mutate(signif = cut(p.value, breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1), labels = c("***", "**", "*", ".",""))) %>%
  ungroup() %>%
  mutate(
    x = 1.5,
    y = c(1870, 205, 1850, 1950)
  )

bottom_row <- cluster.gg %>%
  ggplot() +
  gstyle + 
  geom_boxplot(aes(as.factor(plot_cluster), Observations, group = plot_cluster), 
               size = base_size*0.06, outlier.size =  base_size*0.06, 
               position = position_dodge2(preserve = "total"), width = base_size * 0.15)+
  facet_wrap(~Parameter, nrow = 1, scales = "free_y") +
  xlab("")+
  ylab("") +
  geom_text(data = boxpl.signifficance, aes (x, y, label = signif))


quartz(width = 6.3, height = 4, pointsize = 12)

plot_grid(top_row, bottom_row,
          nrow = 2, ncol = 1,
          rel_heights = c(2.3, 1.7))