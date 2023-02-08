library(tidyverse)

#### covid data
URL_prefix <- 'https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/'
files <- c(
  'containment_health_index_avg.csv',  # filenames with appropriate indices
  'economic_support_index.csv',
  'government_response_index_avg.csv',
  'stringency_index_avg.csv'
)
paths <- paste0(URL_prefix, files)

dfs <- lapply(paths, function(path) {
  read_csv(path)
})

### combine indices into one dataset
varnames <- c('containment_health_index', 'economic_support_index', 'government_response_index', 'stringency_index')

covid_data <- map2(
  dfs, varnames, function(df, varname) {
    df %>%
      filter(jurisdiction == 'NAT_TOTAL') %>%
      select(-c('...1', 'region_code', 'region_name', 'jurisdiction')) %>%
      gather(key = 'date', value = !!varname, -c('country_code', 'country_name')) %>%
      mutate(date = lubridate::as_date())
  }
)

View(covid_data[[2]])


#### parties & cabinets data
### exploration (parties)
## main ideology vars: left_right, state_market, liberty_authority
parties_df <- read_csv('https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_party.csv')
parties_data <- parties_df %>%
  select(c('country_name', 'left_right', 'state_market', 'liberty_authority',
           'party_name_english', 'country_id', 'party_id', 'family_id'))
# seems to be time-invariant here
ideology_stats <- psych::describe(parties_data[c('left_right', 'state_market', 'liberty_authority')])
ideology_stats_cntry <- bind_rows(
  lapply(
    psych::describeBy(parties_data[c('left_right', 'state_market', 'liberty_authority')],
                      parties_data[['country_name']]),
    function(lst) {
      lst %>%
        rownames_to_column('varname') %>%
        select(c('varname', 'range'))
    }
  ),
  .id = 'country_name'
)

ideology_stats_cntry[['index']] <- rep(1:length(unique(ideology_stats_cntry$country_name)), each = 3)
ideology_stats_cntry[['country_name']] <- factor(ideology_stats_cntry[['country_name']])

# facet-wrap plot: ideological scales
ggplot(parties_data %>%  # seems to have two main opposing groupings across ideological scales
         gather('varname', 'varvalue',
                c('left_right', 'state_market', 'liberty_authority')),
       aes(varvalue)) +
  geom_histogram(alpha = .7, color = 'black', bins = 20) +
  facet_wrap(vars(varname))

# facet-wrap plot: ideological scales ~ countries
ggplot(parties_data %>%  # seems to have ideological scales distribution almost identical across coutries
         gather('varname', 'varvalue',
                c('left_right', 'state_market', 'liberty_authority')),
       aes(country_name, varvalue)) +
  geom_boxplot(varwidth = TRUE) +
  facet_grid(rows = vars(varname))

# check using kruscal test (should use bonferroni pval ~= 0.017)
kruskal.test(left_right ~ country_name, data = parties_data)  # significantly dispersed
kruskal.test(state_market ~ country_name, data = parties_data)  # almost insignificant
kruskal.test(liberty_authority ~ country_name, data = parties_data)  # significantly dispersed

# probably, state_market variable will have no effect due to low cross-country variation


### exploration (cabinets)
# it's very important to check changes in ideological variable across time
cabinets_df <- read_csv('https://www.parlgov.org/data/parlgov-development_csv-utf-8/view_cabinet.csv') %>%
  select(c('country_name', 'start_date', 'cabinet_party', 'prime_minister',
           'seats', 'election_seats_total', 'party_name', 'party_id'))

## variables notes:
# prime_minister - may be important variable (for instance, ideology of prime-minister's party is more important)
# seats - should be included to compute mean ideology of a cabinet in order to account for party size
# election_seats_total - should be included as control variable in order to control for cabinet power
# party_name - 'no party affiliation' and 'one-seat' should be removed
# party_id - in order to join with parties_data

# NOTE: later it can be useful to include non-cabinet parties as opposing unit

# prepare final data
cabinets_data <- cabinets_df %>%
  filter(!(party_name %in% c('no party affiliation', 'one seat'))) %>%
  left_join(parties_data %>%
              select(c('party_id', 'left_right', 'state_market', 'liberty_authority')), by = 'party_id') %>%
  mutate(seats_ratio = seats / election_seats_total,
         left_right = left_right * seats_ratio,  # adjust ideology in order to account for party "power"
         state_market = state_market * seats_ratio,
         liberty_authority = liberty_authority * seats_ratio) %>%
  group_by(country_name, start_date, cabinet_party) %>%
  summarise(across(c('left_right', 'state_market', 'liberty_authority'), sum)) %>%
  ungroup() %>%
  rename(c('cabinet' = 'cabinet_party'))

## investigate time-variance in cabinets ideology
cabinets_data %>%
  filter(cabinet == 1) %>%
  mutate(year = lubridate::year(start_date),
         decade = year - year %% 2) %>%
  group_by(decade) %>%
  summarise(across(c('left_right', 'state_market', 'liberty_authority'), mean)) %>%
  gather('varname', 'varvalue',
         c('left_right', 'state_market', 'liberty_authority')) %>%
  ggplot(aes(decade, varvalue, color = varname)) +
    geom_line()

# NEXT STAGE: analyse COVID MEASURES as cabinet and non-cabinet ideologies are initially analysed


pc_data <- cabinets_data %>%
  left_join(parties_data, 'party_id') %>%
  group_by(country_name) %>%
  summarize(
    left_right = mean(left_right, na.rm = TRUE),
    state_market = mean(state_market, na.rm = TRUE),
    liberty_authority = mean(liberty_authority, na.rm = TRUE)
  ) %>%
  ungroup()


#### final join
all <- covid_data %>%
  full_join(pc_data, by = c('country' = 'country_name')) %>%
  drop_na(left_right) %>%
  slice(1:27)


#### modeling
model_lr <- glm(is_manvac ~ left_right, data = all, family = binomial(link = 'logit'))
summary(model)

model_sm <- glm(is_manvac ~ state_market, data = all, family = binomial(link = 'logit'))
summary(model)

model_la <- glm(is_manvac ~ liberty_authority, data = all, family = binomial(link = 'logit'))
summary(model)
