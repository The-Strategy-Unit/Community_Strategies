
# ICB table of number of readmissions within one year

icb_table_readmissions<-function(data, group){
  
  data_for_table<-ordering_icb_by_readmissions(data, group)
  
  data_for_table|>
    flextable() |>
    set_header_labels(icb="ICB",
                      total="No. at risk")|>
    align(part = "header", align = "center")|>
    align(j=1, align = "left")|>
    align(part = "body", align = "center")|>
    align(j=1, part="body", align="left")|>
    align(j=1, part="header", align="left")|>
    bg(bg = "#f9bf07", part = "header") |>
    bold(i = 1, bold = TRUE, part="header")|>
    fontsize(size = 10, part = "all")|>
    padding(padding = 1, part = "all", padding.top=NULL) |>
    autofit()|>
    htmltools_value(ft.align = "left")    
  
}

# ICB table of number of readmissions within one year

icb_table_deaths<-function(data, group){
  
  data_for_table<-ordering_icb_by_deaths(data, group)
  
  data_for_table|>
    flextable() |>
    set_header_labels(icb="ICB",
                      total="No. at risk")|>
    align(part = "header", align = "center")|>
    align(j=1, align = "left")|>
    align(part = "body", align = "center")|>
    align(j=1, part="body", align="left")|>
    align(j=1, part="header", align="left")|>
    bg(bg = "#f9bf07", part = "header") |>
    bold(i = 1, bold = TRUE, part="header")|>
    fontsize(size = 10, part = "all")|>
    padding(padding = 1, part = "all", padding.top=NULL) |>
    autofit()|>
    htmltools_value(ft.align = "left")    
  
}


# Table for cox results

# data=survival_data_first_readmission
# survival_time= "Surv(time_to_readmit_28days,readmit_28days)"
table_coxph_results<-function(data, group, survival_time ){

  table<-data|>
  filter(cohort==group)|>
  coxphmulti(dependent=survival_time, explanatory = c("sex","age_range", "ethnicity" , "imd_decile" ,"icb"))|>
  fit2df()

table|>
  mutate(pvalue=sub(".*p", "", HR))|>
  mutate(pvalue= substring(pvalue,2,6))|>
  mutate(pvalue=as.numeric(pvalue))|>
  mutate(sig=ifelse(pvalue<0.05, "*", ""))|>
  select(-pvalue)|>
  flextable() |>
  set_header_labels(sig="",
                    HR="Hazard Ratio (95% CI, p-value)")|>
  align(part = "header", align = "center")|>
  align(j=1, align = "left")|>
  align(part = "body", align = "center")|>
  align(j=1, part="body", align="left")|>
  align(j=1, part="header", align="left")|>
  bg(bg = "#f9bf07", part = "header") |>
  bold(i = 1, bold = TRUE, part="header")|>
  fontsize(size = 10, part = "all")|>
  padding(padding = 1, part = "all", padding.top=NULL) |>
  autofit()|>
  htmltools_value(ft.align = "left")    

}  
  
  
  
  
  
  
  
