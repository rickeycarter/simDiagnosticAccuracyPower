### Sample Size Estimation 

## General function is based on the idea there are two variables for primary classification that will be 
## grouped into a more general positive and negative class.  Indeterminate drop out of the calculations


## Load some common libraries
library(tidyverse)

### Set Parameters for the simulation


gen_sim_data <- function( n_sample_size = 2000,  # sample size to be worked from
                          n_sims = 15, ## Set number of simulations
est_sensitivity = .84,
est_specificity = .85,
sens_lower_boundary = .8,
spec_lower_boundary = .8,
ppv_lower_boundary = 0.4,
npv_lower_boundary = 0.9,
p_vec = c(.25, .10, .05, .05, .1, .02, .01, .07, .35),
pos_group = c(1,2,3,7),
neg_group = c(4,5,6,9)
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

sim_sens <- n_tp / n_pos
sim_sens_lci <- binom::binom.exact(n_tp, n_pos)$lower

sim_spec <- n_tn / n_neg
sim_spec_lci <- binom::binom.exact(n_tn, n_neg)$lower

sim_ppv <- n_tp / (n_tp + n_fp)
sim_ppv_lci <- binom::binom.exact(n_tp, n_tp+n_fp)$lower

sim_npv <- n_tn / (n_fn + n_tn)
sim_npv_lci <- binom::binom.exact(n_tn, n_fn+n_tn)$lower

## make some objects with references of the parameters

est_sensitivity_vec = rep(est_sensitivity, n_sims)
est_specificity_vec = rep(est_specificity, n_sims)
sens_lower_boundary_vec = rep(sens_lower_boundary, n_sims)
spec_lower_boundary_vec = rep(spec_lower_boundary, n_sims)
ppv_lower_boundary_vec = rep(ppv_lower_boundary, n_sims)
npv_lower_boundary_vec = rep(ppv_lower_boundary, n_sims)


## make a combined data frame with all results

outdf <- data.frame(est_sensitivity_vec,
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
    sim_disease_prevalance = n_pos / n_total,
    sim_effective_prevalance = n_pos / n_effective
  )

return(outdf)
}

summarize_power <- function(indf){
 summary_df <- indf %>%
   mutate(
         data_id = glue::glue("LCI Limits: sens {sens_lower_boundary_vec}, spec {spec_lower_boundary_vec}, ppv {ppv_lower_boundary_vec}, npv {npv_lower_boundary_vec}")
   ) %>%
   group_by(n_total,data_id, 
            est_sensitivity_vec,
                    est_specificity_vec,
                    sens_lower_boundary_vec,
                    spec_lower_boundary_vec,
                    ppv_lower_boundary_vec,
                    npv_lower_boundary_vec) %>%
   summarize(
     num_sims = n(),
     obs_sample_size = mean(n_total),
     obs_mean_npos = mean(n_pos),
     obs_mean_nneg = mean(n_neg),
     
     obs_mean_testpos = mean(n_tp + n_fp),
     obs_mean_testneg = mean(n_fn + n_tn),
     
     obs_avg_effective_sample_size = mean(n_effective),
     
     obs_sens_power = mean(sim_sens_pass),
     obs_sens_mean = mean(sim_sens),
     obs_sens_mean_lci = mean(sim_sens_lci),
     
     obs_spec_power = mean(sim_spec_pass),
     obs_spec_mean = mean(sim_spec),
     obs_spec_mean_lci = mean(sim_spec_lci),
     
     obs_ppv_power = mean(sim_ppv_pass),
     obs_ppv_mean = mean(sim_ppv),
     obs_ppv_mean_lci = mean(sim_ppv_lci),
     
     obs_npv_power = mean(sim_npv_pass),
     obs_npv_mean = mean(sim_npv),
     obs_npv_mean_lci = mean(sim_npv_lci)
        )
 p_npos <- ggplot(summary_df, aes(n_total, obs_mean_npos))+
   facet_grid(~data_id)+
   geom_point()+ 
   geom_line()
 p_nneg <- ggplot(summary_df , aes(n_total, obs_mean_nneg)) + 
   facet_grid(~data_id)+
   geom_point()+
   geom_line()
                  
 p_sens_power <-ggplot(summary_df, aes(obs_mean_npos, obs_sens_power)) + 
   facet_grid(~data_id)+
   geom_point()+
   geom_line() +
   coord_cartesian(ylim = c(0, 1.0)) +
   geom_hline(yintercept = .8, linetype="dashed") +
   geom_hline(yintercept = .9, linetype="dashed") 
 p_spec_power <-ggplot(summary_df, aes(obs_mean_nneg, obs_spec_power)) + 
   facet_grid(~data_id)+
   geom_point()+
   geom_line()+
   coord_cartesian(ylim = c(0, 1.0)) +
   geom_hline(yintercept = .8, linetype="dashed") +
   geom_hline(yintercept = .9, linetype="dashed") 
 p_ppv_power <-ggplot(summary_df, aes(n_total, obs_ppv_power)) + 
   facet_grid(~data_id)+
   geom_point()+
   geom_line()+
   coord_cartesian(ylim = c(0, 1.0)) +
   geom_hline(yintercept = .8, linetype="dashed") +
   geom_hline(yintercept = .9, linetype="dashed") 
 p_npv_power <-ggplot(summary_df, aes(n_total, obs_npv_power)) + 
   facet_grid(~data_id)+
   geom_point()+
   geom_line()+
   coord_cartesian(ylim = c(0, 1.0)) +
   geom_hline(yintercept = .8, linetype="dashed") +
   geom_hline(yintercept = .9, linetype="dashed") 
 p_combined <- cowplot::plot_grid(p_npos,
                                  p_nneg,
                                  p_sens_power, 
                                  p_spec_power,
                                  p_ppv_power,
                                  p_npv_power,nrow = 3, ncol=2)
 print(p_combined)
 return(summary_df)
}



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



## Calculation for the Study # 2

ex2_vector <- c(.25, .10, .05, .05, .1, .02, .01, .07, .35)
ex2_df <- gen_sim_data(n_sample_size = 500, n_sims = 2000, p_vec = ex2_vector) %>%
  bind_rows(gen_sim_data(n_sample_size = 750, n_sims = 2000, p_vec = ex2_vector)) %>%
  bind_rows(gen_sim_data(n_sample_size = 1000, n_sims = 2000, p_vec = ex2_vector)) %>%
  bind_rows(gen_sim_data(n_sample_size = 1250, n_sims = 2000, p_vec = ex2_vector)) %>%
  bind_rows(gen_sim_data(n_sample_size = 1500, n_sims = 2000, p_vec = ex2_vector)) %>%
  bind_rows(gen_sim_data(n_sample_size = 3000, n_sims = 2000, p_vec = ex2_vector)) %>%
  bind_rows(gen_sim_data(n_sample_size = 5000, n_sims = 2000, p_vec = ex2_vector))


ex2_power <- summarize_power(ex2_df)
