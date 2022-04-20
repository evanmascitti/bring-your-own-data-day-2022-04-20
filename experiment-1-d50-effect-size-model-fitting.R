
# this fits a two-way linear model with interaction and runs an ANOVA 
# test with type III Sums of Squares

library(purrr)

# read data and filter out adhesion limit data

experiment_1_data <- readr::read_csv(
  './data/experiment-1-data.csv',
  show_col_types = F,
  na = "-"
) %>% 
  dplyr::filter(test_type %in% c("LL","PL"))

# fit model to test the effect 
 
crossed_lm <- lm(
  data = experiment_1_data,
  formula = linear_law_deviation ~ I(log10(d50)^2) * coarse_pct
)


# make ANOVA table 

car::Anova(crossed_lm)
