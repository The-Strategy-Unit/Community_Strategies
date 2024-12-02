
# Cox model

fit_cox_model<-function(data, group, time_to_event, event_type){

    dataset<-data|>
      filter(cohort== group )|>
      filter(!is.na(icb))|>
      rename(event={{event_type}})|>
      rename(time={{time_to_event}})|>
    mutate(ethnicity= relevel(ethnicity, ref = "White British"))|>
      mutate(icb=factor(icb))|>
    mutate(icb=relevel(icb, ref="QHM: NHS North East & North Cumbria ICB"))
  
  fit<-coxph(Surv(time, event) ~ sex +age_range + ethnicity + imd_decile + icb + cluster(der_pseudo_nhs_number), data=dataset)
  
  return(fit)
}




# Cox table and forest plot

plotting_cox_forestplot<-function(data, group, dependent_variable){
  
  icb_order<- data|>
    filter(!is.na(icb))|>
    filter(cohort==group)|>
    mutate(ethnicity= relevel(ethnicity, ref = "White British"))|>
    coxphmulti(dependent=dependent_variable, explanatory = c("sex","age_range", "ethnicity" , "imd_decile" ,"icb", "cluster(der_pseudo_nhs_number)"))|>
    fit2df()|>
    filter(str_starts(explanatory, "icb"))|>
    mutate(icb=sub("icb", "", explanatory))|>
    mutate(HR= substring(HR,1,4))|>
    mutate(HR=as.numeric(HR))|>
    arrange(HR)|>
    mutate(icb=factor(icb))
  
  
    data|>
    filter(!is.na(icb))|>
    filter(cohort==group)|>
    mutate(ethnicity= relevel(ethnicity, ref = "White British"))|>  
    mutate(icb=factor(icb, icb_order$icb))|>
    mutate(icb=relevel(icb, ref="QHM: NHS North East & North Cumbria ICB"))|>
      hr_plot(dependent=dependent_variable, 
              explanatory = c("sex","age_range", "ethnicity" , "imd_decile" ,"icb"),
              column_space = c(-1, 0, 0.6),
              plot_opts=list(geom_point(aes(size = Total, 
                                            fill=ifelse(as.numeric(HR)>1 & as.numeric(L95)>1,  "#ec6555", ifelse(as.numeric(HR)<1 & as.numeric(U95)<1, "#88b083", "black"))), shape=22)
                             ,  scale_fill_manual(values = c("#88b083",   "#ec6555","black" ))))
}





testing_ph_individual_variables<-function(fit){
  
  test.ph <- cox.zph(fit, transform="identity", term=FALSE)
  
  test.ph$table|>
    as.data.frame()|>
    mutate(p=round(p,4))|>
    mutate(chisq=round(chisq,4))|>
    mutate(sig=ifelse(p<0.05, "*", ""))

}

testing_ph_categories<-function(fit){
  
  test.ph2 <- cox.zph(fit, transform="identity", term=TRUE, singledf=TRUE)
  
  test.ph2$table|>
    as.data.frame()|>
    mutate(p=round(p,4))|>
    mutate(chisq=round(chisq,4))|>
    mutate(sig=ifelse(p<0.05, "*", "")) 
  
}

plotting_schoenfeld_residuals<-function(fit){
  
  test.ph2 <- cox.zph(fit, transform="identity", term=TRUE, singledf=TRUE)
  
  ggcoxzph(test.ph2, resid=FALSE)
  
}






