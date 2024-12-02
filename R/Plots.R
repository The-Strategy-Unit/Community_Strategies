
# Barchart of number with a readmission by cohort

plot_readmission_barchart<-function(data, time_point, title){

data|>
  group_by(cohort, {{time_point}})|>
  summarise(count=n())|>
  mutate(Percentage=(count/sum(count))*100)|>
  filter({{time_point}}==1)|>
  ggplot(aes(x=cohort, y=Percentage))+
  geom_bar(stat="identity")+
  su_theme()+
  theme(legend.title=element_blank(),
        legend.text=element_text(size=14),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title=element_text(size=14))+
  scale_y_continuous(limits=c(0,60), expand=c(0,0))+
  labs(title= title)

}

# Line plot of proportion of readmissions over time

plot_readmissions_over_time<-function(data, time_point, title){
  
data|>
  group_by(cohort, der_financial_year, {{time_point}})|>
  summarise(count=n())|>
  mutate(Percentage=(count/sum(count))*100)|>
  filter({{time_point}}==1)|>
  ggplot(aes(x=der_financial_year, y=Percentage, group=cohort, color=cohort))+
  geom_line(linewidth=1.2)+
  su_theme()+
  theme(legend.title=element_blank(),
        legend.text=element_text(size=14),
        legend.position = c(0.3,0.80),
        axis.text.x=element_text(size=13),
        axis.text.y=element_text(size=14),
        axis.title=element_text(size=14),
        title=element_text(size=14))+
  scale_y_continuous(limits=c(0,100), expand=c(0,0))+
  labs(title= title, x=NULL )+
  scale_color_manual(values=c( "#f9bf07",   "#5881c1"  , "#ec6555" ))
  
}

# Distribution of number of readmissions

plot_numbers_of_readmissions<-function(data){

  data|>
  filter(der_financial_year=="2022/23")|>
  group_by(cohort, der_pseudo_nhs_number)|>
  summarise(count=n())|>
  mutate(count=count-1)|>
  ggplot(aes(x=count)) + 
  geom_histogram()+
  facet_wrap(~cohort)+
  su_theme()+
  theme(legend.title=element_blank(),
        legend.text=element_text(size=16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18),
        title=element_text(size=15))+
  scale_y_continuous(limits=c(0,NA), expand=c(0,0))+
  labs(title= "Number of readmissions per patient for 2022/23")
}


# Kaplan Meier plot

km_graph<-function(km_fit, col_name, readmission_death){
  
  names(km_fit$strata)<-gsub(col_name, "", names(km_fit$strata))  
  
  
  km_fit|>
    ggsurvfit(size=0.9)+
    add_pvalue(caption="log-rank {p.value}",  size=5)+
    add_confidence_interval()+
    labs(x = "Time (Days) ", y = "Probability", 
         title = paste0("Survival time to ", readmission_death), subtitle="") + 
    su_theme()+
    theme(legend.title=element_blank(),
          legend.text=element_text(size=12),
          axis.text=element_text(size=12),
          axis.title=element_text(size=13),
          axis.title.y = element_text(hjust=0.5),
          axis.title.x = element_text(hjust=0.5),
          title=element_text(size=13, hjust = 0, face="bold" ),
          plot.title.position = "plot")+
    scale_x_continuous(limits=c(0, 370), expand=c(0.08,0), breaks=c(0, 90, 180, 275, 365))+
    scale_y_continuous(limits=c(0.4, 1), expand=c(0,0))
  
}

km_graph2<-function(km_fit, col_name, readmission_death){
  
  names(km_fit$strata)<-gsub(col_name, "", names(km_fit$strata))  
  
  km_fit|>
    ggsurvfit(size=0.9)+
    add_pvalue(caption="log-rank {p.value}",  size=5)+
    add_confidence_interval()+
    labs(x = "Time (Days) ", y = "Probability", 
         title = paste0("Survival time to ", readmission_death), subtitle="") + 
    su_theme()+
    theme(legend.title=element_blank(),
          legend.text=element_text(size=12),
          axis.text=element_text(size=12),
          axis.title=element_text(size=13),
          axis.title.y = element_text(hjust=0.5),
          axis.title.x = element_text(hjust=0.5),
          title=element_text(size=13, hjust = 0, face="bold" ),
          plot.title.position = "plot")+
    scale_x_continuous(limits=c(0, 370), expand=c(0.08,0), breaks=c(0, 90, 180, 275, 365))+
    scale_y_continuous(limits=c(0.2, 1), expand=c(0,0))
  
}


# Comparing cohorts

plot_comparing_cohorts<-function(data){
  
  fit <- survfit(Surv(time_to_readmit_1yr, readmit_1yr) ~ cohort, data = data)
  
  dataframe<-deparse(substitute(data))

if (dataframe=="survival_data_2223_survived_1yr"){
  plot<-km_graph2(fit, "cohort=", "readmission" )
}

if (dataframe!="survival_data_2223_survived_1yr"){
  plot<-km_graph(fit, "cohort=", "readmission" )
}

plot+
  theme(axis.title.y = element_text(vjust=5, hjust=0.5))+
  scale_color_manual(values=c( "#f9bf07",   "#5881c1"  , "#ec6555" ))+
  scale_fill_manual(values=c( "#f9bf07",   "#5881c1"  , "#ec6555" ))+
  add_risktable(risktable_stats="{n.risk} ({cum.event})",
                stats_label= list(n.risk="Number at risk"),
                size=3.4,
                theme=theme_risktable_default(axis.text.y.size = 12,
                                              plot.title.size=14))

}

plot_comparing_cohorts_without_duplicates<-function(data){
  
  survival_data_first_readmission<-data|>
    group_by(der_pseudo_nhs_number)|>
    slice_min(admission_date)
  
 fit <- survfit(Surv(time_to_readmit_1yr, readmit_1yr) ~ cohort, data = survival_data_first_readmission)
  
  dataframe<-deparse(substitute(data))
  
  if (dataframe=="survival_data_2223_survived_1yr"){
    plot<-km_graph2(fit, "cohort=", "readmission" )
  }
  
  if (dataframe!="survival_data_2223_survived_1yr"){
    plot<-km_graph(fit, "cohort=", "readmission" )
  }

plot+
    theme(axis.title.y = element_text(vjust=5, hjust=0.5))+
    scale_color_manual(values=c( "#f9bf07",   "#5881c1"  , "#ec6555" ))+
    scale_fill_manual(values=c( "#f9bf07",   "#5881c1"  , "#ec6555" ))+
    add_risktable(risktable_stats="{n.risk} ({cum.event})",
                  stats_label= list(n.risk="Number at risk"),
                  size=3.4,
                  theme=theme_risktable_default(axis.text.y.size = 12,
                                                plot.title.size=14))
  
}

plot_comparing_cohorts_deaths<-function(data){
  
  fit <- survfit(Surv(time_to_death_1yr, mortality_1yr) ~ cohort, data = data)
  
  dataframe<-deparse(substitute(data))
  
  if (dataframe=="survival_data_2223_survived_1yr"){
    plot<-km_graph2(fit, "cohort=", "readmission" )
  }
  
  if (dataframe!="survival_data_2223_survived_1yr"){
    plot<-km_graph(fit, "cohort=", "readmission" )
  }
 
 plot+
    theme(axis.title.y = element_text(vjust=5, hjust=0.5))+
    scale_color_manual(values=c( "#f9bf07",   "#5881c1"  , "#ec6555" ))+
    scale_fill_manual(values=c( "#f9bf07",   "#5881c1"  , "#ec6555" ))+
    add_risktable(risktable_stats="{n.risk} ({cum.event})",
                  stats_label= list(n.risk="Number at risk"),
                  size=3.4,
                  theme=theme_risktable_default(axis.text.y.size = 12,
                                                plot.title.size=14))
  
}



kaplan_meier_plot_age<-function(data, group, time_to_event, event_type, readmission_death ){
  
  dataset<-data|>
    filter(cohort==group)|>
    rename(event={{event_type}})|>
    rename(time={{time_to_event}})

  
  fit <- survfit2(Surv(time, event) ~ age_range, data = dataset)
  
  dataframe<-deparse(substitute(data))
  
  if (dataframe=="survival_data_2223_survived_1yr"){
    plot<-km_graph2(fit, "cohort=", "readmission" )
  }
  
  if (dataframe!="survival_data_2223_survived_1yr"){
    plot<-km_graph(fit, "cohort=", "readmission" )
  }
 
 plot+
    scale_color_manual(values=c("#f9bf07", "#5881c1", "#ec6555", "#BCBAB8","black", "#88b083", "#901d10"))+
    scale_fill_manual(values=c("#f9bf07", "#5881c1", "#ec6555", "#BCBAB8","black" , "#88b083", "#901d10"))+
    labs(title=paste0("Survival time to ", readmission_death, " by age for the ",group, " cohort"))+
    add_risktable(risktable_stats="{n.risk} ({cum.event})",
                  stats_label= list(n.risk="Number at risk"),
                  size=3.3,
                  theme=theme_risktable_default(axis.text.y.size = 11,
                                                plot.title.size=14))
  
}

kaplan_meier_plot_sex<-function(data, group, time_to_event, event_type, readmission_death){
  
  dataset<-data|>
    filter(cohort==group)|>
    rename(event={{event_type}})|>
    rename(time={{time_to_event}})
  
  fit <- survfit2(Surv(time, event) ~ sex, data = dataset)
  
  dataframe<-deparse(substitute(data))
  
  if (dataframe=="survival_data_2223_survived_1yr"){
    plot<-km_graph2(fit, "cohort=", "readmission" )
  }
  
  if (dataframe!="survival_data_2223_survived_1yr"){
    plot<-km_graph(fit, "cohort=", "readmission" )
  }
  
  plot+
    scale_color_manual(values=c("#f9bf07", "#5881c1"))+
    scale_fill_manual(values=c("#f9bf07", "#5881c1"))+
    labs(title=paste0("Survival time to ", readmission_death, " by sex for the ",group, " cohort"))+
    add_risktable(risktable_stats="{n.risk} ({cum.event})",
                  stats_label= list(n.risk="Number at risk"),
                  size=3.3,
                  theme=theme_risktable_default(axis.text.y.size = 12,
                                                plot.title.size=14))
}


kaplan_meier_plot_ethnicity<-function(data, group, time_to_event, event_type, readmission_death){
  
  dataset<-data|>
    filter(cohort==group)|>
    rename(event={{event_type}})|>
    rename(time={{time_to_event}})
  
  fit <- survfit2(Surv(time, event) ~ ethnicity, data = dataset)
  
  dataframe<-deparse(substitute(data))
  
  if (dataframe=="survival_data_2223_survived_1yr"){
    plot<-km_graph2(fit, "cohort=", "readmission" )
  }
  
  if (dataframe!="survival_data_2223_survived_1yr"){
    plot<-km_graph(fit, "cohort=", "readmission" )
  }

plot+
    scale_color_manual(values=c("#f9bf07", "#5881c1", "#ec6555", "#BCBAB8","black" ,"#88b083", "#901d10" ))+
    scale_fill_manual(values=c("#f9bf07", "#5881c1", "#ec6555", "#BCBAB8","black", "#88b083", "#901d10" ))+
    theme(legend.text=element_text(size=10),
          legend.key.size = unit(0.5, 'cm'),
          #legend.position=c(0.12, 0.25),
          axis.title.y = element_text(vjust=5, hjust=0.5),
          title=element_text(hjust=-0.4))+
    labs(title=str_wrap(paste0("Survival time to ", readmission_death, " by ethnicity for the ",group, " cohort"),70))+
    add_risktable(risktable_stats="{n.risk} ({cum.event})",
                  stats_label= list(n.risk="Number at risk"),
                  size=3.3,
                  theme=theme_risktable_default(axis.text.y.size = 11,
                                                plot.title.size=14))
  
}

kaplan_meier_plot_icb<-function(data, group, time_to_event, event_type, readmission_death){
  
  dataset<-data|>
    filter(cohort==group)|>
    rename(event={{event_type}})|>
    rename(time={{time_to_event}})
  
  fit <- survfit2(Surv(time, event) ~icb, data = dataset)
  
  dataframe<-deparse(substitute(data))
  
  if (dataframe=="survival_data_2223_survived_1yr"){
    plot<-km_graph2(fit, "cohort=", "readmission" )
  }
  
  if (dataframe!="survival_data_2223_survived_1yr"){
    plot<-km_graph(fit, "cohort=", "readmission" )
  }
  

   plot +
     labs(title=paste0("Survival time to ", readmission_death, " by ICB for the ",group, " cohort"))+
   theme(legend.position = "none" )
  
}


kaplan_meier_plot_imd<-function(data, group, time_to_event, event_type, readmission_death){
  
  dataset<-data|>
    filter(cohort==group)|>
    rename(event={{event_type}})|>
    rename(time={{time_to_event}})
  
  fit <- survfit2(Surv(time, event) ~imd_decile, data = dataset)
  
  dataframe<-deparse(substitute(data))
  
  if (dataframe=="survival_data_2223_survived_1yr"){
    plot<-km_graph2(fit, "cohort=", "readmission" )
  }
  
  if (dataframe!="survival_data_2223_survived_1yr"){
    plot<-km_graph(fit, "cohort=", "readmission" )
  }
 
 plot+
    scale_fill_brewer(palette = "RdYlBu") +
    scale_color_brewer(palette = "RdYlBu")+
    labs(title=paste0("Survival time to ", readmission_death, " by IMD decile for the ",group, " cohort"))+
    add_risktable(risktable_stats="{n.risk} ({cum.event})",
                  stats_label= list(n.risk="Number at risk"),
                  size=3.5,
                  theme=theme_risktable_default(axis.text.y.size = 11,
                                                plot.title.size=14))
  
  
}

