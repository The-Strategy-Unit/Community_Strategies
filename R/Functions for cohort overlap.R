# Format cohort data
Formatting_cohort_overlap_data<-function(data){
  
  load(data)
  
  cohort_overlap_data_2324<-cohort_overlap_data_patients|>
    filter(der_financial_year=="2023/24")|>
    uncount(patients)|>
    filter(cohort!="00000000")|>
    rename(binary_cohort=cohort)|>
    ungroup()
    
  
  return(cohort_overlap_data_2324)
  
}

# Format spells and beddays data
Formatting_spells_beddays_data<-function(data){
  
  load(data)
  
  spells_beddays_data<-cohort_overlap_data_spells_beddays|>
    filter(der_financial_year=="2023/24")|>
    mutate(age_groups=case_when(age_range=="<60"| age_range=="60-64"~ "<65 yrs",
                                age_range=="65-69"| age_range=="70-74"~ "65-74 yrs",
                                age_range=="75-79"| age_range=="80-84"|
                                  age_range=="85-89"| age_range=="90+"~ "75+ yrs"))|>
    filter(cohort!="00000000")|>
    rename(binary_cohort=cohort)|>
    group_by(age_groups, sex, binary_cohort)|>
    summarise(Spells=sum(spells), Beddays=sum(beddays))|>
    ungroup()
  
  
  return(spells_beddays_data)
  
}

# Function to generate matrix  of values

table_of_overlap_values<-function(data){
  
upset_plot_data<-data

cohorts = colnames(upset_plot_data)[7:11]

upset_plot_data[cohorts] = upset_plot_data[cohorts] == 1

overlap_data<-upset_data(upset_plot_data, cohorts)

overlap_data<-overlap_data$presence|>
  mutate(age_groups=case_when(age_range=="<60"| age_range=="60-64"~ "<65 yrs",
                              age_range=="65-69"| age_range=="70-74"~ "65-74 yrs",
                              age_range=="75-79"| age_range=="80-84"|
                                age_range=="85-89"| age_range=="90+"~ "75+ yrs"))|>
  mutate(binary_cohort=stringr::str_extract(binary_cohort, "^.{5}"))|>
  group_by(intersection, age_groups, sex, group, binary_cohort)|>
  summarise(`Number of patients`=n())|>
  mutate(binary=ifelse(!is.na(`Number of patients`),1,0))|>
  spread(key=group, value=binary)%>%
  replace(is.na(.), 0)

return(overlap_data)

}

## Create data table

create_dt <- function(x) {
  
  DT::datatable(
    x
    , extensions = "Buttons"
    , rownames = FALSE
    , options = list(
      dom = "Blfrtip"
      , buttons = c("copy", "csv")
      , pageLength = 10
      , lengthMenu = list(
        c(10, 25, 50, -1)
        , c(10, 25, 50, "All"))))
}


# Summary barchart of percentage of overlap with different groups

plotting_barchart_summary_of_overlaps<-function(data, group, title){
  
  group_name<-deparse(substitute(group))
  
  total_number<-sum(data[[group_name]])

cohort_overlap_data_2324|>
  select(-`ACSC Acute`,-`ACSC Chronic`,-`ACSC Vaccine Preventable`  )|>
  filter({{group}}==1)|>
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))|>
  gather(key=cohort, value=number)|>
  mutate(total=total_number)|>
  mutate(percentage=round(((number/total))*100,1))|>
  arrange(desc(percentage))|>
  mutate(cohort=factor(cohort, unique(cohort)))|>
  ggplot(aes(x=cohort, y=number))+
  geom_bar(stat="identity")+
  su_theme()+
  theme(axis.text=element_text(size=10.5),
        axis.title.y=element_text(size=14))+
  labs(y="Number of Patients",
       x=NULL,
       title=title)+ 
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 7),
    drop = FALSE
  )+
  geom_text(aes(label=paste0(number, ' \n(',percentage, '%)'), vjust=-0.2))+
  scale_y_continuous(limits=c(0, total_number*1.2), expand=c(0,0))


}

# Summary barchart of percentage of overlap with different groups

plotting_barchart_summary_of_overlaps_individual_acsc<-function(data, group, title){
  
  group_name<-deparse(substitute(group))
  
  total_number<-sum(data[[group_name]])
  
  cohort_overlap_data_2324|>
    filter({{group}}==1)|>
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))|>
    gather(key=cohort, value=number)|>
    mutate(total=total_number)|>
    mutate(percentage=round(((number/total))*100,1))|>
    arrange(desc(percentage))|>
    mutate(cohort=factor(cohort, unique(cohort)))|>
    ggplot(aes(x=cohort, y=number))+
    geom_bar(stat="identity")+
    su_theme()+
    theme(axis.text=element_text(size=10.5),
          axis.title.y=element_text(size=14))+
    labs(y="Number of Patients",
         x=NULL,
         title=title)+ 
    scale_x_discrete(
      labels = function(x) str_wrap(x, width = 7),
      drop = FALSE
    )+
    geom_text(aes(label=paste0(number, ' \n(',percentage, '%)'), vjust=-0.2))+
    scale_y_continuous(limits=c(0, total_number*1.2), expand=c(0,0))
  
  
}


# Function to general venn diagrams

Plot_venn_diagram<-function(data, cohort1, cohort2, cohort3, cohort4, title){
  
  if(is.na(cohort4)){
    venn_data <- list(
      group1 = which(data[[cohort1]]=="1"),
      group2= which(data[[cohort2]]=="1"),
      group3 = which(data[[cohort3]]=="1")
    )
    names(venn_data) <- c(cohort1, cohort2, cohort3)   
    
    
  }
  else{
    venn_data <- list(
      group1 = which(data[[cohort1]]=="1"),
      group2 = which(data[[cohort2]]=="1"),
      group3 = which(data[[cohort3]]=="1"),
      group4= which(data[[cohort4]]=="1")
    )
    names(venn_data) <- c(cohort1, cohort2, cohort3, cohort4)  
    
  }
  
  ggVennDiagram(venn_data, label_alpha = 0, label_size = 3.4, label = "percent") +
    scale_fill_distiller(palette = "Spectral") +
    labs(title = str_wrap(title, 65))+
    scale_x_continuous(expand=c(0.1,0.1))+
    theme(legend.position="none",
          plot.title=element_text(face="bold", hjust = 0.5))
 
}

Plot_venn_diagram_5groups<-function(data, cohort1, cohort2, cohort3, cohort4, cohort5, title){

    venn_data <- list(
      group1 = which(data[[cohort1]]=="1"),
      group2 = which(data[[cohort2]]=="1"),
      group3 = which(data[[cohort3]]=="1"),
      group4= which(data[[cohort4]]=="1"),
      group5= which(data[[cohort5]]=="1")
    )
    names(venn_data) <- c(cohort1, cohort2, cohort3, cohort4, cohort5)  
    
  
  
  ggVennDiagram(venn_data, label_alpha = 0, label_size = 3.4,label = "percent",
                color =  c("group1"="#343739", "group2"= "#686f73" ,"group3"="#9d928a"  ,"group4"="black","group5"="#b2b7b9"),
                set_color = c("group1"="#343739" , "group2"= "#686f73" ,"group3"="#9d928a"  ,"group4"="black","group5"="#b2b7b9")) +
    scale_fill_distiller(palette = "Spectral") +
    labs(title = str_wrap(title, 75))+
    scale_x_continuous(expand=c(0.1,0.1))+
    theme(legend.position="none",
          plot.title=element_text(face="bold", hjust = 0.5))
}



# Function to generate upset plot with ACSC ALL

plot_upset_plot<-function(data,number_of_overlaps,y_axis  ){
  

upset_plot_data<-data|>
  select(-`ACSC Chronic`, -`ACSC Acute`, -`ACSC Vaccine Preventable`)

cohorts = colnames(upset_plot_data)[7:12]

upset_plot_data[cohorts] = upset_plot_data[cohorts] == 1

size = get_size_mode('exclusive_intersection')

ComplexUpset::upset(upset_plot_data, cohorts, name='Cohorts', 
                    width_ratio=0.1, n_intersections=number_of_overlaps,
                    set_sizes=FALSE,
                    keep_empty_groups=FALSE,
                    themes=(upset_modify_themes(
                      list(
                        'intersections_matrix'=theme(text=element_text(size=13),
                                                     axis.title.x=element_blank())
                      ))),
                    base_annotations = list(
                      'Intersection size'=(
                        intersection_size(
                          text_mapping=aes(
                            label=paste0(round(!!get_size_mode('exclusive_intersection')/nrow(upset_plot_data) * 100, 1), '%', '\n ', !!size )  ) , 
                          text=list(size=3),
                          bar_number_threshold = 1)+
                          theme_classic()+
                          theme(axis.title.y = element_text(size=14, vjust=-16),
                                axis.text.y=element_text(size=10),
                                axis.title.x=element_blank(),
                                axis.text=element_blank()))+
                          scale_y_continuous(limits=c(0,y_axis), expand=c(0,0)
                      )))
      }
 
# Function to generate upset plot with INDIVIDUAL ACSC

plot_upset_plot_individual_acsc<-function(data,number_of_overlaps,y_axis  ){
  
  
  upset_plot_data<-data
  
  cohorts = colnames(upset_plot_data)[7:14]
  
  upset_plot_data[cohorts] = upset_plot_data[cohorts] == 1
  
  size = get_size_mode('exclusive_intersection')
  
  ComplexUpset::upset(upset_plot_data, cohorts, name='Cohorts', 
                      width_ratio=0.1, n_intersections=number_of_overlaps,
                      set_sizes=FALSE,
                      keep_empty_groups=FALSE,
                      themes=(upset_modify_themes(
                        list(
                          'intersections_matrix'=theme(text=element_text(size=13),
                                                       axis.title.x=element_blank())
                        ))),
                      base_annotations = list(
                        'Intersection size'=(
                          intersection_size(
                            text_mapping=aes(
                              label=paste0(round(!!get_size_mode('exclusive_intersection')/nrow(upset_plot_data) * 100, 1), '%', '\n ', !!size )  ) , 
                            text=list(size=3),
                            bar_number_threshold = 1)+
                            theme_classic()+
                            theme(axis.title.y = element_text(size=14, vjust=-16),
                                  axis.text.y=element_text(size=10),
                                  axis.title.x=element_blank(),
                                  axis.text=element_blank()))+
                          scale_y_continuous(limits=c(0,y_axis), expand=c(0,0)
                          )))
}