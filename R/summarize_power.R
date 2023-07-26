#' Summarize empirical power based on an input dataset of simulated experiments. Will return a list of results: plot of power curves (powerplot) and data frame of power results (powerresults)
#'
#' @param indf A data frame of simulated data generated from the gen_sim_data function
#' @param target_power Target power (decimal form) for reference line drawn on graphs. Defaults to 0.90. 
#' @export

summarize_power <- function(indf, 
                            target_power = 0.90){
 summary_df <- indf %>%
   mutate(
         data_id = glue::glue("{100*set_conf_level}% LCI Limits: sens {sens_lower_boundary_vec}, spec {spec_lower_boundary_vec}, ppv {ppv_lower_boundary_vec}, npv {npv_lower_boundary_vec}")
   ) %>%
   group_by(n_total,data_id,
            set_conf_level,
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
     
     #Derive test pos and test neg numbers
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
     obs_npv_mean_lci = mean(sim_npv_lci),
     
     obs_sens_spec_power = mean(sim_sens_spec_pass),
     obs_global_power = mean(sim_global_pass)
        )
 p_npos <- ggplot(summary_df, aes(n_total, obs_mean_npos))+
   facet_grid(~data_id)+
   geom_point()+ 
   geom_line() +
   labs(
     x="Total Number of Cases (records) Studied",
     y="Simulation Mean Number Disease Positive Cases"
     )
 
 p_nneg <- ggplot(summary_df , aes(n_total, obs_mean_nneg)) + 
   facet_grid(~data_id)+
   geom_point()+
   geom_line()+
   labs(
     x="Total Number of Cases (records) Studied",
     y="Simulation Mean Number Disease Negative Cases"
     )


 p_sens_power <-ggplot(summary_df, aes(obs_mean_npos, obs_sens_power)) + 
   facet_grid(~data_id)+
   geom_point()+
   geom_line() +
   coord_cartesian(ylim = c(0, 1.0)) +
   geom_hline(yintercept = target_power, linetype="dashed") +
   labs(
     x="Simulation Mean Number Disease Positive Cases",
     y="Empirical Power - Sensitivity"
     )

 p_spec_power <-ggplot(summary_df, aes(obs_mean_nneg, obs_spec_power)) + 
   facet_grid(~data_id)+
   geom_point()+
   geom_line()+
   coord_cartesian(ylim = c(0, 1.0)) +
   geom_hline(yintercept = target_power, linetype="dashed") +
   labs(
     x="Simulation Mean Number Negative Cases",
     y="Empirical Power - Specificity"
     )


 p_ppv_power <-ggplot(summary_df, aes(n_total, obs_ppv_power)) + 
   facet_grid(~data_id)+
   geom_point()+
   geom_line()+
   coord_cartesian(ylim = c(0, 1.0)) +
   geom_hline(yintercept = target_power, linetype="dashed") +
   labs(
     x="Total Number of Cases (records) Studied",
     y="Empirical Power - PPV"
     )


 p_npv_power <-ggplot(summary_df, aes(n_total, obs_npv_power)) + 
   facet_grid(~data_id)+
   geom_point()+
   geom_line()+
   coord_cartesian(ylim = c(0, 1.0)) +
   geom_hline(yintercept = target_power, linetype="dashed") +
   labs(
     x="Total Number of Cases (records) Studied",
     y="Empirical Power - NPV"
     )

 
 p_sens_spec_power <-ggplot(summary_df, aes(n_total, obs_sens_spec_power)) + 
   facet_grid(~data_id)+
   geom_point()+
   geom_line()+
   coord_cartesian(ylim = c(0, 1.0)) +
   geom_hline(yintercept = target_power, linetype="dashed") +
   labs(
     x="Total Number of Cases (records) Studied",
     y="Empirical Power - Joint Sens & Spec test"
     )


 p_global_power <-ggplot(summary_df, aes(n_total, obs_global_power)) + 
   facet_grid(~data_id)+
   geom_point()+
   geom_line()+
   coord_cartesian(ylim = c(0, 1.0)) +
  geom_hline(yintercept = target_power, linetype="dashed") +
   labs(
     x="Total Number of Cases (records) Studied",
     y="Empirical Power - Joint Sens, Spec, PPV, NPV test"
     )

 
 
 p_combined <- cowplot::plot_grid(p_npos,
                                  p_nneg,
                                  p_sens_power, 
                                  p_spec_power,
                                  p_ppv_power,
                                  p_npv_power,
                                  p_sens_spec_power,
                                  p_global_power,
                                  nrow = 4, ncol=2)
 outlist<-list(
   powerplot = p_combined,
   powerdata = summary_df
 )
 return(outlist)
}

