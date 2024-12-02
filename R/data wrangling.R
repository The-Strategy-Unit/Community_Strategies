
# Read on RData file
read_Rdata_file<-function(data){
  
  e <- new.env(parent = emptyenv())
  load(data, envir = e)
  survival_data <- e[["survival_data"]]
  
  survival_data<-survival_data|>
    distinct()|>
    mutate(died_before_readmission_28days=ifelse(time_to_death>time_to_readmit_28days|
                                                   is.na(time_to_death), 0, 1))|>
    mutate(died_before_readmission_1yr=ifelse(time_to_death>time_to_readmit_1yr|
                                                is.na(time_to_death), 0, 1))|>
    filter(icb!="QNQ: NHS Frimley ICB") #remove Frimley as missing data
   
  return(survival_data)
  
 }


# Selecting only data from 2022-23
formatting_survival_data_2223<-function(data){
  
  survival_data_2223<-data|>
    filter(der_financial_year=="2022/23")
  
  return(survival_data_2223)
  
}


# Calculating mean and median number of readmissions

calculating_mean_median_admissions<-function(data){
  
mean_median_admissions<-data|>
  group_by(cohort, der_financial_year, der_pseudo_nhs_number)|>
  summarise(count=n())|>
  mutate(count=count-1)|>
  group_by(cohort, der_financial_year)|>
  summarise(mean=mean(count), 
            median=median(count),
            lower_quartile=quantile(count, probs = c(0.25)),
            upper_quartile=quantile(count, probs = c(0.75)),
            max=max(count),
            min=min(count))|>
  filter(der_financial_year=="2022/23")|>
  mutate(mean=round(mean,2))

}

# Remove duplicates so max of one readmission per patient

calculating_survival_data_first_readmission<-function(data){

  survival_data_first_readmission<-data|>
  slice_min(admission_date, by=(der_pseudo_nhs_number))|>
    mutate(ethnicity= relevel(ethnicity, ref = "White British"))
  
  }

# Ordering ICBs by readmissions
ordering_icb_by_readmissions<-function(data, group){

icb_order<-data|>
  filter(cohort== group )|>
  group_by(icb, readmit_1yr)|>
  summarise(Readmissions=n())|>
  mutate(Percentage=(Readmissions/sum(Readmissions))*100)|>
  group_by(icb)|>
  reframe(total=sum(Readmissions), readmit_1yr, Readmissions, Percentage)|>
  filter(readmit_1yr==1)|>
  mutate(Percentage=round(Percentage,1))|>
  arrange(Percentage)|>
  select(icb, Readmissions, total, Percentage)|>
  filter(!is.na(icb))|>
  mutate(icb=factor(icb))|>
  ungroup()

return(icb_order)
  
}

# Ordering ICBs by deaths
ordering_icb_by_deaths<-function(data, group){
  
  icb_order<-data|>
    filter(cohort== group )|>
    group_by(icb, mortality_1yr)|>
    summarise(Deaths=n())|>
    mutate(Percentage=(Deaths/sum(Deaths))*100)|>
    group_by(icb)|>
    reframe(total=sum(Deaths), mortality_1yr, Deaths, Percentage)|>
    filter(mortality_1yr==1)|>
    mutate(Percentage=round(Percentage,1))|>
    arrange(Percentage)|>
    select(icb, Deaths, total, Percentage)|>
    filter(!is.na(icb))|>
    mutate(icb=factor(icb))|>
    ungroup()
  
  return(icb_order)
  
}

formatting_deaths_data<-function(data){
  
  deaths_data<-data|>
    slice_min(admission_date, by=c(der_pseudo_nhs_number, der_financial_year, cohort))|> # select the time they first entered the cohort
    filter(time_to_death>0| is.na(time_to_death)) #remove those who are recorded as dying before admission! or at admission

    
  return(deaths_data)
  
  }


