
# this fits a one-way linear model of water content vs d50, 
# separately by test type and % coarse addition. 
# It also extracts the slopes for each % coarse addition.
# These are then plotted with a separate script 

# take command line argument for output

library(purrr)

# read data and filter out adhesion limit data

experiment_1_data <- readr::read_csv(
  './data/experiment-1-data.csv'
) %>% 
  dplyr::filter(test_type %in% c('LL', 'PL'))

# fit model to test the effect 
 
atterberg_limits_vs_d50_by_coarse_pct_models <- experiment_1_data %>% 
  dplyr::group_by(test_type, coarse_pct) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(
    model = purrr::map(data, ~lm(data = ., formula = water_content ~ log10(d50))),
    coefficients = purrr::map(model, "coefficients"),
    slope = abs(purrr::map_dbl(coefficients, "log10(d50)")))


car::Anova(mod = atterberg_limits_vs_d50_by_coarse_pct_models)
