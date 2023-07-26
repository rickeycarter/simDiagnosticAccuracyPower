#' Summarize empirical power based on an input dataset of simulated experiments. Will return a list of results: plot of power curves (powerplot) and data frame of power results (powerresults)
#'
#' @param n_sample_size Sample size for the overall study
#' @param n_sims Number of simulation replicates to run (default = 2000)
#' @param est_sensitivity Estimate (under alternative) of the sensitivity of the test
#' @param est_specificity Estimate (under alternative) of the specificity of the test
#' @param sens_lower_boundary Lower limit for the Exact confidence interval
#' @param spec_lower_boundary Lower limit for the Exact confidence interval
#' @param ppv_lower_boundary Lower limit for the Exact confidence interval
#' @param npv_lower_boundary Lower limit for the Exact confidence interval
#' @param p_vec The multinomial vector of probabilities across 9 possible bins for participants. Vector entries may be zero but must sum to 1.  Using the pos_group and neg_group, these categories can be groupped into higher order positive and negative groups
#' @param pos_group A set of indices (1 -9 as integers) for the classes that are considered disease positive
#' @param neg_group A set of indices (1 - 9 as integers) for the classes that are considered negative for the disease. Any indices in the 1 to 9 range not in the pos or neg group will be considered indeterminate 
#' @param alpha The level of significance for the tests.  Defaults to 0.05.  Decrease to allow for multiple testing (e.g., 0.0125 for tests of Sens, Spec, PPV, and NPV)
#' @export

gen_sim_data <- function( n_sample_size = 2000,  
                          n_sims = 2000, 
est_sensitivity = .84,
est_specificity = .85,
sens_lower_boundary = .8,
spec_lower_boundary = .8,
ppv_lower_boundary = 0.4,
npv_lower_boundary = 0.9,
p_vec = c(.25, .10, .05, .05, .1, .02, .01, .07, .35),
pos_group = c(1,2,3,7),
neg_group = c(4,5,6,9),
alpha=0.05
){

# Start of the function calculations

p_total <- sum(p_vec)

## Build out the case distribution and allow for the exclusion of the indeterminate cases 

ind_group <- setdiff(1:9, c(pos_group, neg_group))


raw_vector<-t(rmultinom(n_sims, size = n_sample_size, prob = p_vec))



n_pos <- rowSums(raw_vector[, pos_group])
n_neg <- rowSums(raw_vector[, neg_group])
n_ind <- rowSums(raw_vector) - n_pos - n_neg



n_total <- n_pos + n_neg + n_ind
n_effective <- n_total - n_ind

## Apply sens & specificity to the counts to get the part of the confusion table
## Based on a random binomial

n_tp <- rbinom(n=rep(1, n_sims), size=n_pos,prob = rep(est_sensitivity, n_sims))
n_fn <- n_pos - n_tp
n_pos ==n_tp + n_fn


n_tn <- rbinom(n=rep(1, n_sims), size = n_neg,  prob = rep(est_specificity, n_sims))
n_fp <- n_neg - n_tn
n_neg == n_tn + n_fp



## Derive measures of diagnostic performance

set_conf_level <- rep(1 - alpha, n_sims)

sim_sens <- n_tp / n_pos
sim_sens_lci <- binom::binom.exact(n_tp, n_pos, conf.level = set_conf_level)$lower

sim_spec <- n_tn / n_neg
sim_spec_lci <- binom::binom.exact(n_tn, n_neg, conf.level = set_conf_level)$lower

sim_ppv <- n_tp / (n_tp + n_fp)
sim_ppv_lci <- binom::binom.exact(n_tp, n_tp+n_fp, conf.level = set_conf_level)$lower

sim_npv <- n_tn / (n_fn + n_tn)
sim_npv_lci <- binom::binom.exact(n_tn, n_fn+n_tn, conf.level = set_conf_level)$lower

## make some objects with references of the parameters

est_sensitivity_vec = rep(est_sensitivity, n_sims)
est_specificity_vec = rep(est_specificity, n_sims)
sens_lower_boundary_vec = rep(sens_lower_boundary, n_sims)
spec_lower_boundary_vec = rep(spec_lower_boundary, n_sims)
ppv_lower_boundary_vec = rep(ppv_lower_boundary, n_sims)
npv_lower_boundary_vec = rep(npv_lower_boundary, n_sims)


## make a combined data frame with all results

outdf <- data.frame(set_conf_level,
  est_sensitivity_vec,
                    est_specificity_vec,
                    sens_lower_boundary_vec,
                    spec_lower_boundary_vec,
                    ppv_lower_boundary_vec,
                    npv_lower_boundary_vec,
                    raw_vector, 
                    n_pos, n_neg, n_ind, n_total, n_effective, 
                    n_tp, n_fn, n_tn, n_fp, 
                    sim_sens, sim_sens_lci, 
                    sim_spec, sim_spec_lci, 
                    sim_ppv, sim_ppv_lci, 
                    sim_npv, sim_npv_lci)


## Do some calculations to set up power 
outdf <- outdf %>%
  mutate(
    sim_sens_pass = case_when(
      sim_sens_lci > sens_lower_boundary ~ 1,
      TRUE ~ 0
    ),
    sim_spec_pass = case_when(
      sim_spec_lci > spec_lower_boundary ~ 1,
      TRUE ~ 0
    ),
    sim_ppv_pass = case_when(
      sim_ppv_lci > ppv_lower_boundary ~ 1,
      TRUE ~ 0
    ),
    sim_npv_pass = case_when(
      sim_npv_lci > npv_lower_boundary ~ 1,
      TRUE ~ 0
    ),
    sim_sens_spec_pass = case_when(
      sim_sens_pass == 1 & sim_spec_pass == 1 ~ 1,
      TRUE ~ 0
    ),
    sim_global_pass = case_when(
      sim_sens_pass == 1 & sim_spec_pass == 1 & sim_ppv_pass == 1 & sim_npv_pass == 1 ~ 1,
      TRUE ~ 0
    ),
    sim_disease_prevalance = n_pos / n_total,
    sim_effective_prevalance = n_pos / n_effective
  )

return(outdf)
}
