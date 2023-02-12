library(tidyverse)
library(memisc)

#### presuppositions
# only OECD countries
oecd_countries_ru <- c(
  'Австралия', 'Австрия', 'Бельгия', 'Великобритания', 'Венгрия', 'Германия',
  'Греция', 'Дания', 'Израиль', 'Ирландия', 'Исландия', 'Испания', 'Италия',
  'Канада', 'Колумбия', 'Коста-Рика', 'Латвия', 'Литва', 'Люксембург',
  'Нидерланды', 'Новая Зеландия', 'Норвегия', 'Мексика', 'Польша', 'Португалия',
  'Республика Корея', 'Словакия', 'Словения', 'США', 'Турция', 'Финляндия',
  'Франция', 'Чехия', 'Чили', 'Швейцария', 'Швеция', 'Эстония', 'Япония'
)

oecd_countries_en <- c(
  'Australia', 'Austria', 'Belgium', 'United Kingdom', 'Hungary', 'Germany',
  'Greece', 'Denmark', 'Israel', 'Ireland', 'Iceland', 'Spain', 'Italy',
  'Canada', 'Colombia', 'Costa Rica', 'Latvia', 'Lithuania', 'Luxembourg',
  'Netherlands', 'New Zealand', 'Norway', 'Mexico', 'Poland', 'Portugal',
  'South Korea', 'Slovak Republic', 'Slovenia', 'United States', 'Turkey',
  'Finland', 'France', 'Czech Republic', 'Chile', 'Switzerland', 'Sweden',
  'Estonia', 'Japan'
)

# UK is not in EU since 2021
eu_countries <- c(
  'Austria', 'Belgium', 'Hungary', 'Germany', 'Greece', 'Denmark', 'Ireland',
  'Iceland', 'Spain', 'Italy', 'Latvia', 'Lithuania', 'Luxembourg',
  'Netherlands', 'Norway', 'Poland', 'Portugal', 'Slovak Republic', 'Slovenia',
  'Finland', 'France', 'Czech Republic', 'Switzerland', 'Sweden', 'Estonia'
)

non_eu_countries <- c(
  'Australia', 'United Kingdom', 'Israel', 'Canada', 'Colombia', 'Costa Rica', 
  'New Zealand', 'Mexico', 'South Korea', 'United States', 'Turkey', 'Chile',
  'Japan'
)

# covid period
covid_start <- 2020


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
date_mapper <- c(
  'Jan' = '01',
  'Feb' = '02',
  'Mar' = '03',
  'Apr' = '04',
  'May' = '05',
  'Jun' = '06',
  'Jul' = '07',
  'Aug' = '08',
  'Sep' = '09',
  'Oct' = '10',
  'Nov' = '11',
  'Dec' = '12'
)

covid_data_list <- map2(
  dfs, varnames, function(df, varname) {
    df %>%
      filter(jurisdiction == 'NAT_TOTAL') %>%
      select(-c('...1', 'region_code', 'region_name', 'jurisdiction')) %>%
      gather(key = 'date', value = !!varname, -c('country_code', 'country_name')) %>%
      mutate(month = str_extract(date, '[A-z]+'),
             month = sapply(month, function(mnth) date_mapper[[mnth]]),
             date = paste0(substr(date, 1, 2), month, substr(date, 6, 9)),
             date = lubridate::as_date(date, format = '%d%m%Y')) %>%
      select(-month)
  }
)

covid_data <- covid_data_list[[1]] %>%
  full_join(covid_data_list[[2]], by = c('country_code', 'country_name', 'date')) %>%
  full_join(covid_data_list[[3]], by = c('country_code', 'country_name', 'date')) %>%
  full_join(covid_data_list[[4]], by = c('country_code', 'country_name', 'date')) %>%
  filter(country_name %in% oecd_countries_en) %>%
  mutate(in_eu = country_name %in% eu_countries)

### analyse indices
# government_response_index: overall response index (0-100, others as well)
# containment_health_index: restrictions on movement and gathering +
#                           public campaigns concerning medical issues: testing,
#                           vaccination, health spending
# economic_support_index: government economic support in pandemic context
# stringency_index: only restriction part from containment_health_index

## government_response_index
# eu
covid_data %>%
  filter(in_eu) %>%
  ggplot(aes(date, containment_health_index)) +
    geom_line() +
    facet_wrap(vars(country_name)) +
  theme_bw()

# non eu
covid_data %>%
  filter(!in_eu) %>%
  ggplot(aes(date, containment_health_index)) +
  geom_line() +
  facet_wrap(vars(country_name))


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
parties_covid_country_mapper <- c(
  'Slovakia' = 'Slovak Republic'
)

cabinets_data <- cabinets_df %>%
  filter(!(party_name %in% c('no party affiliation', 'one seat')),
         country_name %in% oecd_countries_en) %>%  # filter only oecds
  left_join(parties_data %>%
              select(c('party_id', 'left_right', 'state_market', 'liberty_authority')), by = 'party_id') %>%
  mutate(seats_ratio = seats / election_seats_total,
         left_right = left_right * seats_ratio,  # adjust ideology in order to account for party "power"
         state_market = state_market * seats_ratio,
         liberty_authority = liberty_authority * seats_ratio,
         country_name = sapply(country_name, function(cntry) {
           if (cntry %in% names(parties_covid_country_mapper)) {
             parties_covid_country_mapper[cntry]
           } else {
             cntry
           }
         })) %>%
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


#### final join for modeling
## aggregate covid
covid_to_join <- covid_data %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(country_name, year, in_eu) %>%
  summarise(across(c('containment_health_index', 'economic_support_index',
                     'government_response_index', 'stringency_index'),
                   \(x) mean(x, na.rm = TRUE))) %>%
  ungroup()

## aggregate parties
get_end_date <- function(country_df) {
  country_df %>%
    arrange(start_date) %>%
    mutate(end_date = lead(start_date) - lubridate::days(1),
           end_date = lubridate::as_date(ifelse(is.na(end_date), lubridate::today(), end_date))) %>%
    select(c('country_name', 'start_date', 'end_date', 'cabinet', 'left_right',
             'state_market', 'liberty_authority')) %>%
    return()
}

get_all_years <- function(row) {
  start_year <- unname(lubridate::year(row['start_date']))
  end_year <- unname(lubridate::year(row['end_date']))
  unique_years <- start_year:end_year
  
  df_to_return <- data.frame(
    'country_name' = unname(row['country_name']),
    'year' = unique_years,
    'cabinet' = unname(row['cabinet']),
    'left_right' = unname(row['left_right']),
    'state_market' = unname(row['state_market']),
    'liberty_authority' = unname(row['liberty_authority'])
  )
  
  return(df_to_return)
}

# non-cabinet
parties_to_join0 <- cabinets_data %>%
  filter(cabinet == 0) %>%
  group_by(country_name) %>%
  group_split() %>%
  lapply(., function(cdf) get_end_date(cdf)) %>%
  bind_rows()

# cabinet
parties_to_join1 <- cabinets_data %>%
  filter(cabinet == 1) %>%
  group_by(country_name) %>%
  group_split() %>%
  lapply(., function(cdf) get_end_date(cdf)) %>%
  bind_rows()

# both
parties_to_join <- bind_rows(parties_to_join0, parties_to_join1) %>%
  filter(end_date >= lubridate::as_date('2020-01-01', format = '%Y-%m-%d')) %>%
  apply(., 1, get_all_years, simplify = FALSE) %>%
  bind_rows() %>%
  mutate(across(c('cabinet', 'left_right', 'state_market', 'liberty_authority'),
                as.numeric)) %>%
  filter(year >= 2020, year <= 2022) %>%
  group_by(country_name, year, cabinet) %>%
  summarise(across(c('left_right', 'state_market', 'liberty_authority'), mean)) %>%
  ungroup() %>%
  drop_na()

cov_parties_data <- parties_to_join %>%
  full_join(covid_to_join, by = c('country_name', 'year')) %>%
  drop_na()

cov_parties_data0 <- cov_parties_data %>%
  filter(cabinet == 0)

cov_parties_data1 <- cov_parties_data %>%
  filter(cabinet == 1)


#### modeling
# government_response_index
model_gri <- lm(government_response_index ~ left_right + state_market + liberty_authority,
                data = cov_parties_data1)
summary(model_gri)

# containment_health_index
model_chi <- lm(containment_health_index ~ left_right + state_market + liberty_authority,
                data = cov_parties_data1)
summary(model_chi)

# economic_support_index
model_esi <- lm(economic_support_index ~ left_right + state_market + liberty_authority,
                data = cov_parties_data1)
summary(model_esi)

# stringency_index
model_si <- lm(stringency_index ~ left_right + state_market + liberty_authority,
                data = cov_parties_data1)
summary(model_si)

mtable(model_gri, model_chi, model_esi, model_si)
