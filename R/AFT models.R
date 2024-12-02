# Accelerated failure time models

fitting_aft_1yr<-function(data, group){
  
  dataset<-data|> 
    filter(cohort==group)
  
  fit_aft_1yr <- aftreg(Surv(time_to_readmit_1yr, readmit_1yr) ~ sex +age_range + ethnicity + imd_decile + icb, 
                        data = data, id = der_pseudo_nhs_number, dist = "gompertz")
  
  
}