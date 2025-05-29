# {simulist} showcase demo script -----------------------------------------

# all examples use {simulist}
library(simulist)


## Getting started --------------------------------------------------------

# simulate line list with defaults
linelist <- simulist::sim_linelist()

# simulate contact tracing data with defaults
contacts <- simulist::sim_contacts()

# simulate line list and contact tracing data with defaults
outbreak <- simulist::sim_outbreak()

# simulate anonymous line list
linelist <- simulist::sim_linelist(anonymise = TRUE)

# simulate line list with mostly confirmed cases (see Ct values column)
linelist <- simulist::sim_linelist(
  case_type_probs = c(suspected = 0.05, probable = 0.05, confirmed = 0.9)
)

# simulate contact tracing data with an overdispersed contact pattern
contacts <- simulist::sim_contacts(
  contact_distribution = function(x) dnbinom(x = x, mu = 2, size = 0.5),
  prob_infection = 0.6
)

# simulate a line list with custom onset-to-hospitalisation, -death and -recovery
# (see date_outcome column)
linelist <- simulist::sim_linelist(
  onset_to_hosp = function(x) rlnorm(n = x, meanlog = 1.5, sdlog = 0.5),
  onset_to_death = function(x) rweibull(n = x, shape = 1, scale = 5),
  onset_to_recovery = function(x) rweibull(n = x, shape = 2, scale = 4)
)

# simulate a line list without deaths
# (the same can be done with hospital admission)
linelist <- simulist::sim_linelist(
  onset_to_death = NULL,
  hosp_death_risk = NULL,
  non_hosp_death_risk = NULL
)


## Simulate and plot line list data ---------------------------------------

library(incidence2)
library(ggplot2)

set.seed(1)
linelist <- simulist::sim_linelist()

# create <incidence> object with daily aggregation
daily <- incidence(
  x = linelist,
  date_index = "date_onset",
  interval = "daily",
  complete_dates = TRUE
)
plot(daily)

# show plot is ggplot object so can be modified with ggplot2 layers
plot(daily) +
  ggplot2::scale_y_continuous(name = "Number of daily cases")

# tidy line list to plot incidence of cases, hospitalisations and deaths
library(tidyr)
library(dplyr)

linelist <- linelist |>
  tidyr::pivot_wider(
    names_from = outcome,
    values_from = date_outcome
  ) |>
  dplyr::rename(
    date_death = died,
    date_recovery = recovered
  )

daily <- incidence(
  linelist,
  date_index = c(
    onset = "date_onset",
    hospitalisation = "date_admission",
    death = "date_death"
  ),
  interval = "daily",
  complete_dates = TRUE
)
plot(daily)


## Simulate and plot contacts ---------------------------------------------

# {epicontacts} used for interactive plotting
library(epicontacts)

set.seed(2)
outbreak <- simulist::sim_outbreak()

epicontacts <- make_epicontacts(
  linelist = outbreak$linelist,
  contacts = outbreak$contacts,
  id = "case_name",
  from = "from",
  to = "to",
  directed = TRUE
)
plot(epicontacts)


## Simulate with <epiparameter> -------------------------------------------

library(epiparameter)

# create contact distribution (not available from {epiparameter} database)
contact_distribution <- epiparameter(
  disease = "COVID-19",
  epi_name = "contact distribution",
  prob_distribution = create_prob_distribution(
    prob_distribution = "pois",
    prob_distribution_params = c(mean = 2)
  )
)

# create infectious period (not available from {epiparameter} database)
infectious_period <- epiparameter(
  disease = "COVID-19",
  epi_name = "infectious period",
  prob_distribution = create_prob_distribution(
    prob_distribution = "gamma",
    prob_distribution_params = c(shape = 1, scale = 1)
  )
)

# create onset-to-hospitalisation delay distribution
onset_to_hosp <- epiparameter(
  disease = "COVID-19",
  epi_name = "onset to hospitalisation",
  prob_distribution = create_prob_distribution(
    prob_distribution = "lnorm",
    prob_distribution_params = c(meanlog = 1, sdlog = 0.5)
  )
)

# get onset to death from {epiparameter} database
onset_to_death <- epiparameter_db(
  disease = "COVID-19",
  epi_name = "onset to death",
  single_epiparameter = TRUE
)

set.seed(1)

linelist <- simulist::sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death
)


## Real-time outbreak snapshot --------------------------------------------

set.seed(1)

linelist <- simulist::sim_linelist(
  reporting_delay = function(x) rlnorm(n = x, meanlog = 1, sdlog = 1)
)

# truncate the outbreak 10 days before the end of the outbreak
linelist_trunc <- simulist::truncate_linelist(
  linelist = linelist,
  truncation_day = 10,
  unit = "days",
  direction = "backwards"
)

# truncate the outbreak 1 month before the end of the outbreak
linelist_trunc <- simulist::truncate_linelist(
  linelist = linelist,
  truncation_day = 1,
  unit = "month",
  direction = "backwards"
)

# truncate the outbreak 2 months after the start of the outbreak
linelist_trunc <- simulist::truncate_linelist(
  linelist = linelist,
  truncation_day = 1,
  unit = "month",
  direction = "backwards"
)

# truncate the outbreak by date
linelist_trunc <- simulist::truncate_linelist(
  linelist = linelist,
  truncation_day = as.Date("2023-03-01")
)


## Simulate with time-varying death risk ----------------------------------

library(incidence2)

set.seed(3)

# by default death risk is constant throughout the outbreak simulation

# exponential decline in case fatality risk over time
config <- simulist::create_config(
  time_varying_death_risk = function(risk, time) risk * exp(-0.05 * time)
)

# simulate line list with higher case fatality risks to illustrate when plotting
linelist <- simulist::sim_linelist(
  hosp_death_risk = 0.8,
  non_hosp_death_risk = 0.6,
  outbreak_size = c(500, 1000),
  config = config
)

linelist <- linelist |>
  tidyr::pivot_wider(
    names_from = outcome,
    values_from = date_outcome
  ) |>
  dplyr::rename(
    date_death = died,
    date_recovery = recovered
  )

daily <- incidence2::incidence(
  linelist,
  date_index = c(
    onset = "date_onset",
    death = "date_death"
  ),
  interval = "daily",
  complete_dates = TRUE
)

plot(daily)


## Simulate with age-structured population --------------------------------

library(ggplot2)

set.seed(1)

# create an age-structured population with a most young demographic
age_struct <- data.frame(
  age_limit = c(1, 10, 30, 60, 75),
  proportion = c(0.4, 0.3, 0.2, 0.1, 0)
)

linelist <- simulist::sim_linelist(
  population_age = age_struct,
  outbreak_size = c(100, 1e4)
)

# code to prepare line list for plotting an age pyramid (this can be ignored)
linelist_m <- subset(linelist, subset = sex == "m")
age_cats_m <- as.data.frame(table(floor(linelist_m$age / 5) * 5))
colnames(age_cats_m) <- c("AgeCat", "Population")
age_cats_m <- cbind(age_cats_m, sex = "m")
linelist_f <- subset(linelist, subset = sex == "f")
age_cats_f <- as.data.frame(table(floor(linelist_f$age / 5) * 5))
colnames(age_cats_f) <- c("AgeCat", "Population")
age_cats_f$Population <- -age_cats_f$Population
age_cats_f <- cbind(age_cats_f, sex = "f")
age_cats <- rbind(age_cats_m, age_cats_f)
breaks <- pretty(range(age_cats$Population), n = 10)
labels <- abs(breaks)

# plot age pyramid of simulated line list
ggplot(age_cats) +
  geom_col(mapping = aes(x = Population, y = factor(AgeCat), fill = sex)) +
  scale_y_discrete(name = "Lower bound of Age Category") +
  scale_x_continuous(name = "Population", breaks = breaks, labels = labels) +
  scale_fill_manual(values = c("#F04A4C", "#106BA0")) +
  theme_bw()


## Clean messy line list data ---------------------------------------------

library(cleanepi)

set.seed(1)

# simulate a default line list
linelist <- simulist::sim_linelist()

# make the line list messy with default settings
messy_linelist <- simulist::messy_linelist(linelist)

clean_linelist <- messy_linelist |>
  cleanepi::convert_to_numeric(target_columns = c("id", "age")) |>
  cleanepi::remove_duplicates()

attr(clean_linelist, "report")
