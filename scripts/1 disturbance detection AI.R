
# About -------------------------------------------------------------------

#' Detect the disturbances (gap origin and release) at the tree level. For 
#' that purpose we will use the method developed by Fraver and White 2005
#' caled Absolute Incease and Balck and Abrams (Boundary line). As a threshold 
#' for the tree to be released or not we will use the default of 1.25 standard deviation. 
#' For the gap recruitment we will use the growth over the first 15 years, shall 
#' it exceed the  threshold of 1.16 - fir and 1.16 - beech.
#' For the boundary line we will use equations:
#' Fagus sylvatica: 3580.1915 *exp( -9.373 *pg)+ 715.8028 *exp( -1.2492 *pg)
#' Abies alba: PGC~2808.525 *exp( -8.9079 *pg)+ 881.7403 *exp( -1.0737 *pg)



# 0 Libraries and data ----------------------------------------------------
pacman::p_load(tidyverse, zoo, pracma)

#- data
tree.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'tree')
core.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'core')
ring.df <- readxl::read_excel('data/sinca_data.xlsx', sheet = 'ring')

# 0 Support functions -----------------------------------------------------
source('scripts/0_functions.R')

# 1 Analysis --------------------------------------------------------------
#' For the trees that have NA in the missing_years/missing_mm calculate
#' the missing distance and age based on the radial growth and current dbh
ring.df %>%
  arrange(year) %>%
  group_by(plot_id, tree_id) %>%
  summarise(dbh_ring = sum(incr_mm)*2,
    incr_average = mean(incr_mm[1:5])) %>%
  inner_join(core.df %>% filter(is.na(missing_years)), by = c('plot_id','tree_id')) %>%
  left_join(tree.df %>% select(plot_id, tree_id, dbh_mm), by = c('plot_id','tree_id')) %>%
  mutate(missing_mm = (dbh_mm - dbh_ring) / 2,
    missing_mm = pmax(0, missing_mm),
    missing_years = round(missing_mm / incr_average, 0)) %>% 
  select(plot_id, tree_id, missing_years, missing_mm) %>%
  bind_rows(core.df %>% filter(!is.na(missing_years))) ->
  core.df


#' Calculate the tree growth including previous and following growth
inner_join(ring.df, core.df, by = c('plot_id','tree_id')) %>%
  inner_join(tree.df %>% select(plot_id, tree_id, dbh_mm, species), by = c('plot_id','tree_id')) %>%
  arrange(plot_id, tree_id, year) %>%
  group_by(plot_id, tree_id) %>%
  mutate(dbh_growth = ifelse(row_number() == 1, incr_mm + missing_mm, incr_mm),
    dbh_growth = cumsum(dbh_growth) * 2,
    dbh_mm = ifelse(is.na(dbh_mm), max(dbh_growth), dbh_mm),
    dbh_coef = max(dbh_mm) / max(dbh_growth),
    dbh_growth = dbh_growth * dbh_coef,
    age = year - min(year) + missing_years + 1,
    m1 = f_m1(incr_mm),
    m2 = f_m2(incr_mm),
    pg = f_pg(incr_mm),
    pgc = 100 * (m2 - m1) / m1,
    ai = m2 - m1) %>%
  select(plot_id, tree_id, species, year, incr_mm, age, dbh_mm = dbh_growth, m1, m2, pg, pgc, ai) %>%
  ungroup() ->
  growth.df

#' calcualte the absolute increase per species
aith <- growth.df %>% group_by(species) %>% summarise(ai.threshold = 1.25 * sd(ai, na.rm = T)) %>% deframe()
gapth <- c('Abies alba' = 1.16, 'Fagus sylvatica' = 1.16)
#dbhth <- c('Abies alba' = 320, 'Fagus sylvatica' = 280)
dbhth <- c('Abies alba' = 9999, 'Fagus sylvatica' = 9999)
blth <- c('Abies alba' = '2808.525 *exp( -8.9079 *pg)+ 881.7403 *exp( -1.0737 *pg)', 'Fagus sylvatica' = '3580.1915 *exp( -9.373 *pg)+ 715.8028 *exp( -1.2492 *pg)')


#' calculate the growth releases,
#' first add also the pgc compare to the boundary line
#' Boundary line calculations

growth.df %>%
  arrange(plot_id, tree_id, year) %>%
  mutate(blf = blth[species],
    pgcbl = eval( parse( text = blf) ),
    pgcinc = 100 * pgc / pgcbl) %>% 
  group_by(plot_id, tree_id) %>%
  mutate(event = ifelse(row_number() %in%  peakDetection(x = pgcinc, threshold = 20, nups = 1,  mindist = 30), 'release', NA),
    event = ifelse(lead(m2, 7) <= m1, NA, event),
    event = ifelse(lag(m1, 7) >= m2, NA, event)) %>%
  ungroup() %>%
  filter(dbh_mm <= dbhth[first(species)]) %>%
  filter(!is.na(event)) %>%
  mutate(event = if_else(pgcinc >= 50, 'major', 'moderate')) %>%
  select(plot_id, tree_id, year, event) ->
  release.event.bl


#' Absolure increase release events
growth.df %>%
  arrange(plot_id, tree_id, year) %>%
  group_by(plot_id, tree_id) %>%
  mutate(event = ifelse(row_number() %in%  peakDetection(x = ai, threshold = aith[first(species)], nups = 1,  mindist = 30), 'release', NA),
    event = ifelse(lead(m2, 7) <= m1, NA, event),
    event = ifelse(lag(m1, 7) >= m2, NA, event)) %>%
  filter(dbh_mm <= dbhth[first(species)]) %>%
  filter(!is.na(event)) %>%
  select(plot_id, tree_id, year, event) ->
  release.event.ai


#' calculate the gap origin
growth.df %>% 
  filter(age %in% c(0:15)) %>%
  arrange(plot_id, tree_id, year) %>%
  group_by(plot_id, tree_id) %>%
  summarise(species = first(species),
    gapGrowth = mean(incr_mm, na.rm = T),
    N = n(),
    year = min(year),
    age = min(age)) %>%
  filter(N >= 5,
    gapGrowth >= gapth[species]) %>%
  mutate(event = 'gap') %>% # ,
  select(plot_id, tree_id, year, event) ->
  gap.event

#' detect trees for which no event was detected
#' bl
growth.df %>%
  filter(!tree_id %in% c(unique(gap.event$tree_id), unique(release.event.bl$tree_id))) %>%
  group_by(plot_id, tree_id) %>%
  summarise(year = min(year)) %>%
  mutate(event = 'no event') ->
  no.event.bl

#' ai
growth.df %>%
  filter(!tree_id %in% c(unique(gap.event$tree_id), unique(release.event.ai$tree_id))) %>%
  group_by(plot_id, tree_id) %>%
  summarise(year = min(year)) %>%
  mutate(event = 'no event') ->
  no.event.ai



# 2. Combine all the information ------------------------------------------
#' remove events that are next to each other
#' bl
bind_rows(release.event.bl, gap.event, no.event.bl) %>%
  arrange(plot_id, tree_id, year) %>%
  group_by(plot_id, tree_id) %>%
  mutate(keeprel = keepRelease(year, event, n = 30)) %>%
  ungroup() %>%
  filter(keeprel %in% 'yes') %>%
  select(-keeprel) %>%
  rename(event_bl = event) ->
  event.bl

#' ai
bind_rows(release.event.ai, gap.event, no.event.ai) %>%
  arrange(plot_id, tree_id, year) %>%
  group_by(plot_id, tree_id) %>%
  mutate(keeprel = keepRelease(year, event, n = 30)) %>%
  ungroup() %>%
  filter(keeprel %in% 'yes') %>%
  select(-keeprel) %>%
  rename(event_ai = event) ->
  event.ai

#' combine three datasets
#' 
full_join(event.bl, event.ai, by = c( 'plot_id', 'tree_id', 'year')) %>%
inner_join(growth.df, ., by = c( 'plot_id', 'tree_id', 'year')) %>%
  select(plot_id, tree_id, species, year, age, dbh_mm, event_bl, event_ai) ->
  event.df


# 9. Save the results -----------------------------------------------------
write_csv(growth.df, 'data/growth.csv')
write_csv(event.df, 'data/event.csv')

