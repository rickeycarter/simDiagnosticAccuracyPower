You can install the development version from GitHub with:

# install.packages("devtools")
devtools::install_github("overdodactyl/diagnosticSummary")



`diagnosticSummary` is designed to quickly create diagnostic summaries
and reports for binary classification data.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rickeycarter/simDiagnosticAccuracyPower/")
```

## Example

``` r
library(simDiagnosticAccuracyPower)

## Example Estimate 1 - Study with only 2 groups, ~8% positive, 92% negative
ex1_vector <- c(.08, 0, 0, 0, 0, 0, 0, 0, .92)
ex1_df <- gen_sim_data(n_sample_size = 1000, n_sims = 2000, p_vec = ex1_vector) %>%
  bind_rows(gen_sim_data(n_sample_size = 2000, n_sims = 2000, p_vec = ex1_vector)) %>%
  bind_rows(gen_sim_data(n_sample_size = 3000, n_sims = 2000, p_vec = ex1_vector)) %>%
  bind_rows(gen_sim_data(n_sample_size = 4000, n_sims = 2000, p_vec = ex1_vector)) %>%
  bind_rows(gen_sim_data(n_sample_size = 8000, n_sims = 2000, p_vec = ex1_vector)) %>%
  bind_rows(gen_sim_data(n_sample_size = 10000, n_sims = 2000, p_vec = ex1_vector)) %>%
  bind_rows(gen_sim_data(n_sample_size = 12000, n_sims = 2000, p_vec = ex1_vector))
ex1_power <- summarize_power(ex1_df)

ex1_power$powerplot

ex1_power$powerdata %>% knitr::kable

```