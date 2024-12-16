# Format cohort data
Formatting_cohort_overlap_data<-function(data){
  
  load(data)
  
  cohort_overlap_data_2324<-cohort_overlap_data|>
    filter(der_financial_year=="2023/24")|>
    mutate(`ACSC All`=ifelse(amb_chronic==1|amb_acute==1| amb_vacc_prev==1, 1,0))|>
    rename(EOL=eol)|>
    rename(Frail=frail)|>
    rename(Falls=falls)|>
    rename(`Elderly Emergency`=elderly_emergency)|>
    rename(`Expanded EOL`= expanded_eol)|>
    rename(`ACSC Chronic`=amb_chronic)|>
   rename(`ACSC Acute`=amb_acute)|>
   rename(`ACSC Vaccine Preventable`=amb_vacc_prev)|>
    mutate(no_cohort= ifelse(`Elderly Emergency`==0 & Falls==0 & Frail==0 & `ACSC All`==0 &
    `Expanded EOL`==0 & EOL==0, 1,0))|>
      filter(no_cohort==0)|>
    select(-no_cohort)
  
  return(cohort_overlap_data_2324)
  
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
  
  ggVennDiagram(venn_data, label_alpha = 0, label_size = 3.4) +
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
    
  
  
  ggVennDiagram(venn_data, label_alpha = 0, label_size = 3.2,
                color =  c("group1"="#343739", "group2"= "#686f73" ,"group3"="#9d928a"  ,"group4"="black","group5"="#b2b7b9"),
                set_color = c("group1"="#343739" , "group2"= "#686f73" ,"group3"="#9d928a"  ,"group4"="black","group5"="#b2b7b9")) +
    scale_fill_distiller(palette = "Spectral") +
    labs(title = str_wrap(title, 75))+
    scale_x_continuous(expand=c(0.1,0.1))+
    theme(legend.position="none",
          plot.title=element_text(face="bold", hjust = 0.5))
}



# Function to generate upset plot

plot_upset_plot<-function(data,number_of_overlaps,y_axis  ){
  

upset_plot_data<-data

cohorts = colnames(upset_plot_data)[3:10]

upset_plot_data[cohorts] = upset_plot_data[cohorts] == 1
t(head(upset_plot_data[cohorts], 3))

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
                          theme(axis.title.y = element_text(size=14, vjust=-24),
                                axis.text.y=element_text(size=10),
                                axis.title.x=element_blank(),
                                axis.text=element_blank()))+
                          scale_y_continuous(limits=c(0,y_axis), expand=c(0,0)
                      )))
      }
 
