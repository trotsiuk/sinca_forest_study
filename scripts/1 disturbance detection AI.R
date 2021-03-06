
# About -------------------------------------------------------------------

# Detect the disturbances (gap origin and release) at the tree level. For 
# that purpose we will use the method developed by Fraver and White 2005
# caled Absolute Incease. As a threshold for the tree to be released or not 
# we will use the default of 1.25 standard deviation. For the gap recruitment
# we will use the growth over the first 15 years, shall it exceed the 
# threshold of 1.16 - fir and 1.16 - beech



# 0 Libraries and data ----------------------------------------------------
pacman::p_load(tidyverse, zoo, pracma)

#- data
tree.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'tree')
core.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'core') %>% mutate_at(vars(missing_years, missing_mm), funs(as.numeric))
ring.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'ring')



# 0 Support functions -----------------------------------------------------
priorGrowth <- function(x, windowLength = 10){
  rollapply( x, 
             width = windowLength,
             FUN = mean,
             fill = NA,
             align = "right",
             na.rm = T,
             partial = TRUE)
}

followGrowth <- function(x, windowLength = 10){
  rollapply( lead(x, 1), 
             width = windowLength,
             FUN = mean,
             fill = NA,
             align = "left",
             na.rm = T,
             partial = TRUE)
}

peakDetection <- function(x, threshold, mindist = 20, nups = 2){
  #' @description identify the index of year when release event occur
  #' @param x a vector of absolute increase change
  #' @param threshold a minimum ai value in mm
  #' @param mindist  minimum distance between two consecutive peaks in years
  #' @param nups number of increasing steps before the peak
  
  x <- ifelse(is.na(x), -0.2, x)
  
  x <- findpeaks(x, 
                 minpeakheight = threshold,
                 minpeakdistance = mindist,
                 nups = nups) 
  
  if(is.null(x)){
    NA
  }else{
    matrix(x, ncol = 4)[,2]
  }
}

keepRelease <- function(year, type, n = 20){
  #' @description calculate the distance between gap origin and releases
  #' @param year the vector of years for event
  #' @param type type of the event (release or gap)
  #' @param n number of years to be checked
  
  keep <- rep('yes', length(year))
  
  if(any(type %in% 'gap')){
    diffyear <- year - year[type %in% 'gap']
    keep[diffyear < n & type %in% 'release'] <- 'no'
  }
  keep
}


# 1 Analysis --------------------------------------------------------------
#--- For teh trees that have NA in the missing_years/missing_mm calculate
# the missing distance and age based on the radial growth and current dbh
ring.df %>%
  arrange(year) %>%
  group_by(tree_id) %>%
  summarise(dbh_ring = sum(incr_mm)*2,
            incr_average = mean(incr_mm[1:5])) %>%
  inner_join(core.df %>% filter(is.na(missing_years)), by = 'tree_id') %>%
  left_join(tree.df %>% select(tree_id, dbh_mm), by = 'tree_id') %>%
  mutate(missing_mm = (dbh_mm - dbh_ring) / 2,
         missing_mm = pmax(0, missing_mm),
         missing_years = round(missing_mm / incr_average, 0)) %>%
  select(tree_id, missing_years, missing_mm) %>%
  bind_rows(core.df %>% filter(!is.na(missing_years))) ->
  core.df


#---- Calculate the tree growth
inner_join(ring.df, core.df, by = 'tree_id') %>%
  inner_join(tree.df %>% select(tree_id, dbh_mm, species), by = 'tree_id') %>%
  arrange(tree_id, year) %>%
  group_by(tree_id) %>%
  mutate(dbh_growth = ifelse(row_number() == 1, incr_mm + missing_mm, incr_mm),
         dbh_growth = cumsum(dbh_growth) * 2,
         dbh_mm = ifelse(is.na(dbh_mm), max(dbh_growth), dbh_mm),
         dbh_coef = max(dbh_mm) / max(dbh_growth),
         dbh_growth = dbh_growth * dbh_coef,
         age = year - min(year) + missing_years + 1,
         pg = priorGrowth(incr_mm, windowLength = 10),
         fg = followGrowth(incr_mm, windowLength = 10),
         ai = fg - pg) %>%
  select(tree_id, species, year, incr_mm, age, dbh_mm = dbh_growth, ai, fg, pg) ->
  growth.df

#---- calcualte the absolute increase per species
aith <- growth.df %>% group_by(species) %>% summarise(ai.threshold = 1.25 * sd(ai, na.rm = T)) %>% deframe()
gapth <- c('Abies alba' = 1.16, 'Fagus sylvatica' = 1.16)
dbhth <- c('Abies alba' = 320, 'Fagus sylvatica' = 280)

#---- calculate the growth releases

growth.df %>%
  arrange(year) %>%
  group_by(tree_id) %>%
  mutate(event = ifelse(row_number() %in%  peakDetection(x = ai, threshold = aith[first(species)], nups = 1,  mindist = 30), 'release', NA),
         event = ifelse(lead(fg, 7) <= pg, NA, event),
         event = ifelse(lag(pg, 7) >= fg, NA, event)) %>%
  filter(dbh_mm <= dbhth[first(species)]) %>%
  filter(!is.na(event)) %>%
  select(tree_id, year, event) ->
  release.event

#---- calculate the gap origin
growth.df %>% 
  filter(age %in% c(0:15)) %>%
  arrange(year) %>%
  group_by(tree_id) %>%
  summarise(species = first(species),
            gapGrowth = mean(incr_mm, na.rm = T),
            N = n(),
            year = min(year),
            age = min(age)) %>%
  filter(N >= 5,
         gapGrowth >= gapth[species]) %>%
  mutate(event = 'gap',
         year = year - age + 1) %>%
  select(tree_id, year, event) ->
  gap.event


#--- add those trees that didn't show anything
growth.df %>%
  filter(!tree_id %in% c(unique(gap.event$tree_id), unique(release.event$tree_id))) %>%
  group_by(tree_id) %>%
  summarise(year = min(year)) %>%
  mutate(event = 'no event') ->
  no.event

#--- Put everything together
bind_rows(release.event, gap.event, no.event) %>%
  arrange(year) %>%
  group_by(tree_id) %>%
  mutate(keeprel = keepRelease(year, event, n = 30)) %>%
  ungroup() %>%
  filter(keeprel %in% 'yes') %>%
  inner_join(., growth.df, by = c('tree_id', 'year')) %>%
  select(tree_id, species, year, age, dbh_mm, ai, event) ->
  event.df


# 2 Visualise the results -------------------------------------------------
quartz(width = 6.4, height = 4)
event.df %>%
  ungroup() %>%
  filter(year >= 1700) %>%
  mutate(plot_id = substr(tree_id, 0, 1)) %>%
  ggplot(aes(year, fill = event)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~plot_id) +
  theme_bw(base_size = 5)+
  scale_fill_brewer('Event type', palette = 'Set2') +
  xlab('Calendar year') + ylab('Number of trees') +
  theme(legend.justification = c(1,0))

quartz(width = 6.4, height = 4)
growth.df %>%
  mutate(year = year - age + 1,
         plot_id = substr(tree_id, 0, 1)) %>%
  group_by(plot_id, tree_id, species) %>%
  summarise(year = min(year, na.rm = T)) %>%
  ungroup() %>%
  filter(year >= 1700) %>%
  ggplot(aes(year, fill = species)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~plot_id) +
  theme_bw(base_size = 5) +
  scale_fill_brewer('Species', palette = 'Set2') +
  xlab('Calendar year') + ylab('Number of trees') +
  theme(legend.justification = c(1,0))

# 9. Save the results -----------------------------------------------------
write_csv(growth.df, 'data/growth.csv')
write_csv(event.df, 'data/event.csv')

