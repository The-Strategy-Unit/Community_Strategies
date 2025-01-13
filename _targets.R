# Load packages required to define the pipeline:
library(targets)


# Set target options:
tar_option_set(
  packages = c("dplyr",  "flextable", "lubridate", "tidyr", "stringr", "ggplot2", "StrategyUnitTheme", 
               "survival", "ggfortify", "survminer", "ggsurvfit", "RColorBrewer", "forcats", "plotly", "finalfit", 
               "cowplot", "eha", "ComplexUpset") 
)

#library(janitor)


# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Target list:
list(
  
  # Data formatting
 tar_target(
   survival_data,
    read_Rdata_file("Z:/Strategic Analytics/Projects 2024/NHSE Community Strategy/survival_data.RData")),
  
  tar_target(
    survival_data_2223,
    formatting_survival_data_2223(survival_data)),
  
  tar_target(
    survival_data_2223_survived_1yr,
    formatting_survival_data_2223(survival_data)|>
     filter(died_before_readmission_1yr!=1)),
  
  tar_target(
    mean_median_admissions,
    calculating_mean_median_admissions(survival_data)),
  
  tar_target(
    survival_data_first_readmission_2223,
    calculating_survival_data_first_readmission(survival_data_2223)),
  
  tar_target(
    survival_data_first_readmission_2223_survived_1yr,
    calculating_survival_data_first_readmission(survival_data_2223)|>
      filter(died_before_readmission_1yr!=1)),
  
  tar_target(
    survival_data_first_readmission_2223_survived_28days,
    calculating_survival_data_first_readmission(survival_data_2223)|>
      filter(died_before_readmission_28days!=1)),
  
  tar_target(
    icb_order_elderly_emergency,
    ordering_icb_by_readmissions(survival_data_first_readmission_2223, "elderly emergency")),
  tar_target(
    icb_order_falls,
    ordering_icb_by_readmissions(survival_data_first_readmission_2223, "falls")),
  tar_target(
    icb_order_frail,
    ordering_icb_by_readmissions(survival_data_first_readmission_2223, "frail")),
  
  tar_target(
    deaths_data,
   formatting_deaths_data(survival_data)),
  
  tar_target(
    deaths_data_2223,
    formatting_survival_data_2223(deaths_data)),
  
  tar_target(
    cohort_overlap_data_2324,
    Formatting_cohort_overlap_data("Z:/Strategic Analytics/Projects 2024/NHSE Community Strategy/cohort_overlap_data_patients.RData") ),
  
  tar_target(
    spells_beddays_data,
    Formatting_spells_beddays_data("Z:/Strategic Analytics/Projects 2024/NHSE Community Strategy/cohort_overlap_data_spells.RData")  ),
  
  # Matrix of overlap
  tar_target(
    table_of_overlap,
    table_of_overlap_values(cohort_overlap_data_2324)),
  
  # Plots
  tar_target(
    readmission_barchart_28days,
    plot_readmission_barchart(survival_data, readmit_28days, "Readmission within 28 days")),
    
  tar_target(
    readmission_barchart_1yr,
    plot_readmission_barchart(survival_data, readmit_1yr, "Readmission within 1 year")),
  
  tar_target(
    readmission_over_time_28days,
    plot_readmissions_over_time(survival_data, readmit_28days, "Readmission within 28 days")),
  
  tar_target(
    readmission_over_time_1yr,
    plot_readmissions_over_time(survival_data, readmit_1yr, "Readmission within 1 year")),
  
  tar_target(
    numbers_of_readmissions,
    plot_numbers_of_readmissions(survival_data)),
  
  tar_target(
    deaths_barchart_28days,
    plot_readmission_barchart(deaths_data, mortality_28days, "Deaths within 28 days")),
  
  tar_target(
    deaths_barchart_1yr,
    plot_readmission_barchart(deaths_data, mortality_1yr, "Deaths within 1 year")),
  
  tar_target(
    deaths_over_time_28days,
    plot_readmissions_over_time(deaths_data, mortality_28days, "Deaths within 28 days")),
  
  tar_target(
    deaths_over_time_1yr,
    plot_readmissions_over_time(deaths_data, mortality_1yr, "Deaths within 1 year")),
 
  
  # KM plots 
  
  tar_target(
    comparing_cohorts,
    plot_comparing_cohorts( survival_data_2223)),
  
  tar_target(
    comparing_cohorts_without_duplicates,
    plot_comparing_cohorts_without_duplicates( survival_data_2223)),
  
  tar_target(
    comparing_cohorts_survived_1yr,
    plot_comparing_cohorts( survival_data_2223_survived_1yr)),
  
  tar_target(
    comparing_cohorts_without_duplicates_survived_1yr,
    plot_comparing_cohorts_without_duplicates( survival_data_2223_survived_1yr)),
  
  tar_target(
    comparing_cohorts_deaths,
    plot_comparing_cohorts_deaths(deaths_data)),
  
  
  #age
  tar_target(
   km_age_elderly_emergency,
    kaplan_meier_plot_age( survival_data_2223, "elderly emergency", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_age_frail,
    kaplan_meier_plot_age(survival_data_2223,"frail", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_age_falls,
    kaplan_meier_plot_age(survival_data_2223,"falls", time_to_readmit_1yr, readmit_1yr, "readmission")),
  
  #sex
  tar_target(
    km_sex_elderly_emergency,
    kaplan_meier_plot_sex(survival_data_2223,"elderly emergency", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_sex_frail,
    kaplan_meier_plot_sex(survival_data_2223,"frail", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_sex_falls,
    kaplan_meier_plot_sex(survival_data_2223,"falls", time_to_readmit_1yr, readmit_1yr, "readmission")),
  
  #imd_decile
  tar_target(
    km_imd_elderly_emergency,
    kaplan_meier_plot_imd(survival_data_2223,"elderly emergency", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_imd_frail,
    kaplan_meier_plot_imd(survival_data_2223,"frail", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_imd_falls,
    kaplan_meier_plot_imd(survival_data_2223,"falls", time_to_readmit_1yr, readmit_1yr, "readmission")),
  
  #ethnicity
  tar_target(
    km_ethnicity_elderly_emergency,
    kaplan_meier_plot_ethnicity(survival_data_2223,"elderly emergency", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_ethnicity_frail,
    kaplan_meier_plot_ethnicity(survival_data_2223,"frail", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_ethnicity_falls,
    kaplan_meier_plot_ethnicity(survival_data_2223,"falls", time_to_readmit_1yr, readmit_1yr, "readmission")),
  
  #icb
  tar_target(
    km_icb_elderly_emergency,
    kaplan_meier_plot_icb(survival_data_2223,"elderly emergency", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_icb_frail,
    kaplan_meier_plot_icb(survival_data_2223,"frail", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_icb_falls,
    kaplan_meier_plot_icb(survival_data_2223,"falls", time_to_readmit_1yr, readmit_1yr, "readmission")),
  
  #age
  tar_target(
    km_age_elderly_emergency_survived_1yr,
    kaplan_meier_plot_age( survival_data_2223_survived_1yr, "elderly emergency", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_age_frail_survived_1yr,
    kaplan_meier_plot_age(survival_data_2223_survived_1yr,"frail", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_age_falls_survived_1yr,
    kaplan_meier_plot_age(survival_data_2223_survived_1yr,"falls", time_to_readmit_1yr, readmit_1yr, "readmission")),
  
  #sex
  tar_target(
    km_sex_elderly_emergency_survived_1yr,
    kaplan_meier_plot_sex(survival_data_2223_survived_1yr,"elderly emergency", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_sex_frail_survived_1yr,
    kaplan_meier_plot_sex(survival_data_2223_survived_1yr,"frail", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_sex_falls_survived_1yr,
    kaplan_meier_plot_sex(survival_data_2223_survived_1yr,"falls", time_to_readmit_1yr, readmit_1yr, "readmission")),
  
  #imd_decile
  tar_target(
    km_imd_elderly_emergency_survived_1yr,
    kaplan_meier_plot_imd(survival_data_2223_survived_1yr,"elderly emergency", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_imd_frail_survived_1yr,
    kaplan_meier_plot_imd(survival_data_2223_survived_1yr,"frail", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_imd_falls_survived_1yr,
    kaplan_meier_plot_imd(survival_data_2223_survived_1yr,"falls", time_to_readmit_1yr, readmit_1yr, "readmission")),
  
  #ethnicity
  tar_target(
    km_ethnicity_elderly_emergency_survived_1yr,
    kaplan_meier_plot_ethnicity(survival_data_2223_survived_1yr,"elderly emergency", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_ethnicity_frail_survived_1yr,
    kaplan_meier_plot_ethnicity(survival_data_2223_survived_1yr,"frail", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_ethnicity_falls_survived_1yr,
    kaplan_meier_plot_ethnicity(survival_data_2223_survived_1yr,"falls", time_to_readmit_1yr, readmit_1yr, "readmission")),
  
  #icb
  tar_target(
    km_icb_elderly_emergency_survived_1yr,
    kaplan_meier_plot_icb(survival_data_2223_survived_1yr,"elderly emergency", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_icb_frail_survived_1yr,
    kaplan_meier_plot_icb(survival_data_2223_survived_1yr,"frail", time_to_readmit_1yr, readmit_1yr, "readmission")),
  tar_target(
    km_icb_falls_survived_1yr,
    kaplan_meier_plot_icb(survival_data_2223_survived_1yr,"falls", time_to_readmit_1yr, readmit_1yr, "readmission")),
  
  
  #Deaths
  
  #age
  tar_target(
    km_age_elderly_emergency_deaths,
    kaplan_meier_plot_age(deaths_data_2223, "elderly emergency", time_to_death_1yr, mortality_1yr, "death")),
  tar_target(
    km_age_frail_deaths,
    kaplan_meier_plot_age(deaths_data_2223,"frail", time_to_death_1yr, mortality_1yr, "death")),
  tar_target(
    km_age_falls_deaths,
    kaplan_meier_plot_age(deaths_data_2223,"falls", time_to_death_1yr, mortality_1yr, "death")),
  
  #sex
  tar_target(
    km_sex_elderly_emergency_deaths,
    kaplan_meier_plot_sex(deaths_data_2223,"elderly emergency", time_to_death_1yr, mortality_1yr, "death")),
  tar_target(
    km_sex_frail_deaths,
    kaplan_meier_plot_sex(deaths_data_2223,"frail", time_to_death_1yr, mortality_1yr, "death")),
  tar_target(
    km_sex_falls_deaths,
    kaplan_meier_plot_sex(deaths_data_2223,"falls", time_to_death_1yr, mortality_1yr, "death")),
  
  #imd_decile
  tar_target(
    km_imd_elderly_emergency_deaths,
    kaplan_meier_plot_imd(deaths_data_2223,"elderly emergency", time_to_death_1yr, mortality_1yr, "death")),
  tar_target(
    km_imd_frail_deaths,
    kaplan_meier_plot_imd(deaths_data_2223,"frail", time_to_death_1yr, mortality_1yr, "death")),
  tar_target(
    km_imd_falls_deaths,
    kaplan_meier_plot_imd(deaths_data_2223,"falls", time_to_death_1yr, mortality_1yr, "death")),
  
  #ethnicity
  tar_target(
    km_ethnicity_elderly_emergency_deaths,
    kaplan_meier_plot_ethnicity(deaths_data_2223,"elderly emergency", time_to_death_1yr, mortality_1yr, "death")),
  tar_target(
    km_ethnicity_frail_deaths,
    kaplan_meier_plot_ethnicity(deaths_data_2223,"frail", time_to_death_1yr, mortality_1yr, "death")),
  tar_target(
    km_ethnicity_falls_deaths,
    kaplan_meier_plot_ethnicity(deaths_data_2223,"falls", time_to_death_1yr, mortality_1yr, "death")),
  
  #icb
  tar_target(
    km_icb_elderly_emergency_deaths,
    kaplan_meier_plot_icb(deaths_data_2223,"elderly emergency", time_to_death_1yr, mortality_1yr, "death")),
  tar_target(
    km_icb_frail_deaths,
    kaplan_meier_plot_icb(deaths_data_2223,"frail", time_to_death_1yr, mortality_1yr, "death")),
  tar_target(
    km_icb_falls_deaths,
    kaplan_meier_plot_icb(deaths_data_2223,"falls", time_to_death_1yr, mortality_1yr, "death")),
  
  #icb table
  tar_target(
    icb_table_elderly_emergency,
    icb_table_readmissions(survival_data_2223,"elderly emergency")),
  tar_target(
    icb_table_falls,
    icb_table_readmissions(survival_data_2223,"falls")),
  tar_target(
    icb_table_frail,
    icb_table_readmissions(survival_data_2223,"frail")),
  
  tar_target(
    icb_table_elderly_emergency_survived_1yr,
    icb_table_readmissions(survival_data_2223_survived_1yr,"elderly emergency")),
  tar_target(
    icb_table_falls_survived_1yr,
    icb_table_readmissions(survival_data_2223_survived_1yr,"falls")),
  tar_target(
    icb_table_frail_survived_1yr,
    icb_table_readmissions(survival_data_2223_survived_1yr,"frail")),
  
  tar_target(
    icb_table_deaths_elderly_emergency,
    icb_table_deaths(deaths_data_2223,"elderly emergency")),
  tar_target(
    icb_table_deaths_falls,
    icb_table_deaths(deaths_data_2223,"falls")),
  tar_target(
    icb_table_deaths_frail,
    icb_table_deaths(deaths_data_2223,"frail")),
  
    
  # Cox models- Readmissions
  tar_target(
    cox_model_28days_elderly_emergency,
    fit_cox_model(survival_data_first_readmission_2223, "elderly emergency", time_to_readmit_28days, readmit_28days)),
  tar_target(
    cox_model_28days_falls,
    fit_cox_model(survival_data_first_readmission_2223, "falls", time_to_readmit_28days, readmit_28days)),
  tar_target(
    cox_model_28days_frail,
    fit_cox_model(survival_data_first_readmission_2223, "frail", time_to_readmit_28days, readmit_28days)),
  
  tar_target(
    cox_model_1yr_elderly_emergency,
    fit_cox_model(survival_data_first_readmission_2223, "elderly emergency", time_to_readmit_1yr, readmit_1yr)),
  tar_target(
    cox_model_1yr_falls,
    fit_cox_model(survival_data_first_readmission_2223, "falls", time_to_readmit_1yr, readmit_1yr)),
  tar_target(
    cox_model_1yr_frail,
    fit_cox_model(survival_data_first_readmission_2223, "frail", time_to_readmit_1yr, readmit_1yr)),
  
  tar_target(
    cox_model_28days_elderly_emergency_survived_28days,
    fit_cox_model(survival_data_first_readmission_2223_survived_28days, "elderly emergency", time_to_readmit_28days, readmit_28days)),
  tar_target(
    cox_model_28days_falls_survived_28days,
    fit_cox_model(survival_data_first_readmission_2223_survived_28days, "falls", time_to_readmit_28days, readmit_28days)),
  tar_target(
    cox_model_28days_frail_survived_28days,
    fit_cox_model(survival_data_first_readmission_2223_survived_28days, "frail", time_to_readmit_28days, readmit_28days)),
  
  tar_target(
    cox_model_1yr_elderly_emergency_survived_1yr,
    fit_cox_model(survival_data_first_readmission_2223_survived_1yr, "elderly emergency", time_to_readmit_1yr, readmit_1yr)),
  tar_target(
    cox_model_1yr_falls_survived_1yr,
    fit_cox_model(survival_data_first_readmission_2223_survived_1yr, "falls", time_to_readmit_1yr, readmit_1yr)),
  tar_target(
    cox_model_1yr_frail_survived_1yr,
    fit_cox_model(survival_data_first_readmission_2223_survived_1yr, "frail", time_to_readmit_1yr, readmit_1yr)),
  
  
  # Cox forest plots and HRs
  tar_target(
    cox_forestplot_28days_elderly_emergency,
    plotting_cox_forestplot(survival_data_first_readmission_2223, "elderly emergency","Surv(time_to_readmit_28days,readmit_28days)" )),
  tar_target(
    cox_forestplot_28days_falls,
    plotting_cox_forestplot(survival_data_first_readmission_2223, "falls", "Surv(time_to_readmit_28days,readmit_28days)")),
  tar_target(
    cox_forestplot_28days_frail,
    plotting_cox_forestplot(survival_data_first_readmission_2223, "frail", "Surv(time_to_readmit_28days,readmit_28days)")),
  
  tar_target(
    cox_forestplot_1yr_elderly_emergency,
    plotting_cox_forestplot(survival_data_first_readmission_2223, "elderly emergency", "Surv(time_to_readmit_1yr,readmit_1yr)")),
  tar_target(
    cox_forestplot_1yr_falls,
    plotting_cox_forestplot(survival_data_first_readmission_2223, "falls", "Surv(time_to_readmit_1yr,readmit_1yr)")),
  tar_target(
    cox_forestplot_1yr_frail,
    plotting_cox_forestplot(survival_data_first_readmission_2223, "frail", "Surv(time_to_readmit_1yr,readmit_1yr)")),
  
  tar_target(
    cox_forestplot_28days_elderly_emergency_survived_28days,
    plotting_cox_forestplot(survival_data_first_readmission_2223_survived_28days, "elderly emergency","Surv(time_to_readmit_28days,readmit_28days)" )),
  tar_target(
    cox_forestplot_28days_falls_survived_28days,
    plotting_cox_forestplot(survival_data_first_readmission_2223_survived_28days, "falls", "Surv(time_to_readmit_28days,readmit_28days)")),
  tar_target(
    cox_forestplot_28days_frail_survived_28days,
    plotting_cox_forestplot(survival_data_first_readmission_2223_survived_28days, "frail", "Surv(time_to_readmit_28days,readmit_28days)")),
  
  tar_target(
    cox_forestplot_1yr_elderly_emergency_survived_1yr,
    plotting_cox_forestplot(survival_data_first_readmission_2223_survived_1yr, "elderly emergency", "Surv(time_to_readmit_1yr,readmit_1yr)")),
  tar_target(
    cox_forestplot_1yr_falls_survived_1yr,
    plotting_cox_forestplot(survival_data_first_readmission_2223_survived_1yr, "falls", "Surv(time_to_readmit_1yr,readmit_1yr)")),
  tar_target(
    cox_forestplot_1yr_frail_survived_1yr,
    plotting_cox_forestplot(survival_data_first_readmission_2223_survived_1yr, "frail", "Surv(time_to_readmit_1yr,readmit_1yr)")),
  
  #testing assumptions
  tar_target(
    ph_individual_variables_28days_elderly_emergency,
    testing_ph_individual_variables(cox_model_28days_elderly_emergency)),
  tar_target(
    ph_individual_variables_28days_falls,
    testing_ph_individual_variables(cox_model_28days_falls)),
  tar_target(
    ph_individual_variables_28days_frail,
    testing_ph_individual_variables(cox_model_28days_frail)),
  
  tar_target(
    ph_categories_28days_elderly_emergency,
    testing_ph_categories(cox_model_28days_elderly_emergency)),
  tar_target(
    ph_categories_28days_falls,
    testing_ph_categories(cox_model_28days_falls)),
  tar_target(
    ph_categories_28days_frail,
    testing_ph_categories(cox_model_28days_frail)),
  
  tar_target(
    schoenfeld_residuals_28days_elderly_emergency,
    plotting_schoenfeld_residuals(cox_model_28days_elderly_emergency)),
  tar_target(
    schoenfeld_residuals_28days_falls,
    plotting_schoenfeld_residuals(cox_model_28days_falls)),
  tar_target(
    schoenfeld_residuals_28days_frail,
    plotting_schoenfeld_residuals(cox_model_28days_frail)),
  
  tar_target(
    ph_individual_variables_1yr_elderly_emergency,
    testing_ph_individual_variables(cox_model_1yr_elderly_emergency)),
  tar_target(
    ph_individual_variables_1yr_falls,
    testing_ph_individual_variables(cox_model_1yr_falls)),
  tar_target(
    ph_individual_variables_1yr_frail,
    testing_ph_individual_variables(cox_model_1yr_frail)),
  
  tar_target(
    ph_categories_1yr_elderly_emergency,
    testing_ph_categories(cox_model_1yr_elderly_emergency)),
  tar_target(
    ph_categories_1yr_falls,
    testing_ph_categories(cox_model_1yr_falls)),
  tar_target(
    ph_categories_1yr_frail,
    testing_ph_categories(cox_model_1yr_frail)),
  
  tar_target(
    schoenfeld_residuals_1yr_elderly_emergency,
    plotting_schoenfeld_residuals(cox_model_1yr_elderly_emergency)),
  tar_target(
    schoenfeld_residuals_1yr_falls,
    plotting_schoenfeld_residuals(cox_model_1yr_falls)),
  tar_target(
    schoenfeld_residuals_1yr_frail,
    plotting_schoenfeld_residuals(cox_model_1yr_frail)),
  
  tar_target(
    ph_categories_28days_elderly_emergency_survived_28days,
    testing_ph_categories(cox_model_28days_elderly_emergency_survived_28days)),
  tar_target(
    ph_categories_28days_falls_survived_28days,
    testing_ph_categories(cox_model_28days_falls_survived_28days)),
  tar_target(
    ph_categories_28days_frail_survived_28days,
    testing_ph_categories(cox_model_28days_frail_survived_28days)),
  
  tar_target(
    schoenfeld_residuals_28days_elderly_emergency_survived_28days,
    plotting_schoenfeld_residuals(cox_model_28days_elderly_emergency_survived_28days)),
  tar_target(
    schoenfeld_residuals_28days_falls_survived_28days,
    plotting_schoenfeld_residuals(cox_model_28days_falls_survived_28days)),
  tar_target(
    schoenfeld_residuals_28days_frail_survived_28days,
    plotting_schoenfeld_residuals(cox_model_28days_frail_survived_28days)),
  
  tar_target(
    ph_categories_1yr_elderly_emergency_survived_1yr,
    testing_ph_categories(cox_model_1yr_elderly_emergency_survived_1yr)),
  tar_target(
    ph_categories_1yr_falls_survived_1yr,
    testing_ph_categories(cox_model_1yr_falls_survived_1yr)),
  tar_target(
    ph_categories_1yr_frail_survived_1yr,
    testing_ph_categories(cox_model_1yr_frail_survived_1yr)),
  
  tar_target(
    schoenfeld_residuals_1yr_elderly_emergency_survived_1yr,
    plotting_schoenfeld_residuals(cox_model_1yr_elderly_emergency_survived_1yr)),
  tar_target(
    schoenfeld_residuals_1yr_falls_survived_1yr,
    plotting_schoenfeld_residuals(cox_model_1yr_falls_survived_1yr)),
  tar_target(
    schoenfeld_residuals_1yr_frail_survived_1yr,
    plotting_schoenfeld_residuals(cox_model_1yr_frail_survived_1yr)),
  
  # Cox models- Deaths
  tar_target(
    cox_model_death_28days_elderly_emergency,
    fit_cox_model(deaths_data_2223, "elderly emergency", time_to_death_28days, mortality_28days)),
  tar_target(
    cox_model_death_28days_falls,
    fit_cox_model(deaths_data_2223, "falls", time_to_death_28days, mortality_28days)),
  tar_target(
    cox_model_death_28days_frail,
    fit_cox_model(deaths_data_2223, "frail", time_to_death_28days, mortality_28days)),
  
  tar_target(
    cox_model_death_1yr_elderly_emergency,
    fit_cox_model(deaths_data_2223, "elderly emergency", time_to_death_1yr, mortality_1yr)),
  tar_target(
    cox_model_death_1yr_falls,
    fit_cox_model(deaths_data_2223, "falls", time_to_death_1yr, mortality_1yr)),
  tar_target(
    cox_model_death_1yr_frail,
    fit_cox_model(deaths_data_2223, "frail", time_to_death_1yr, mortality_1yr)),
  
  
  tar_target(
    cox_forestplot_death_28days_elderly_emergency,
    plotting_cox_forestplot(deaths_data_2223, "elderly emergency","Surv(time_to_death_28days,mortality_28days)")),
  tar_target(
    cox_forestplot_death_28days_falls,
    plotting_cox_forestplot(deaths_data_2223, "falls", "Surv(time_to_death_28days,mortality_28days)")),
  tar_target(
    cox_forestplot_death_28days_frail,
    plotting_cox_forestplot(deaths_data_2223, "frail", "Surv(time_to_death_28days,mortality_28days)")),
  
  tar_target(
    cox_forestplot_death_1yr_elderly_emergency,
    plotting_cox_forestplot(deaths_data_2223, "elderly emergency","Surv(time_to_death_1yr,mortality_1yr)")),
  tar_target(
    cox_forestplot_death_1yr_falls,
    plotting_cox_forestplot(deaths_data_2223, "falls","Surv(time_to_death_1yr,mortality_1yr)")),
  tar_target(
    cox_forestplot_death_1yr_frail,
    plotting_cox_forestplot(deaths_data_2223, "frail", "Surv(time_to_death_1yr,mortality_1yr)")),
  
  #testing assumptions
  tar_target(
    ph_individual_variables_death_28days_elderly_emergency,
    testing_ph_individual_variables(cox_model_death_28days_elderly_emergency)),
  tar_target(
    ph_individual_variables_death_28days_falls,
    testing_ph_individual_variables(cox_model_death_28days_falls)),
  tar_target(
    ph_individual_variables_death_28days_frail,
    testing_ph_individual_variables(cox_model_death_28days_frail)),
  
  tar_target(
    ph_categories_death_28days_elderly_emergency,
    testing_ph_categories(cox_model_death_28days_elderly_emergency)),
  tar_target(
    ph_categories_death_28days_falls,
    testing_ph_categories(cox_model_death_28days_falls)),
  tar_target(
    ph_categories_death_28days_frail,
    testing_ph_categories(cox_model_death_28days_frail)),
  
  tar_target(
    schoenfeld_residuals_death_28days_elderly_emergency,
    plotting_schoenfeld_residuals(cox_model_death_28days_elderly_emergency)),
  tar_target(
    schoenfeld_residuals_death_28days_falls,
    plotting_schoenfeld_residuals(cox_model_death_28days_falls)),
  tar_target(
    schoenfeld_residuals_death_28days_frail,
    plotting_schoenfeld_residuals(cox_model_death_28days_frail)),
  
  tar_target(
    ph_individual_variables_death_1yr_elderly_emergency,
    testing_ph_individual_variables(cox_model_death_1yr_elderly_emergency)),
  tar_target(
    ph_individual_variables_death_1yr_falls,
    testing_ph_individual_variables(cox_model_death_1yr_falls)),
  tar_target(
    ph_individual_variables_death_1yr_frail,
    testing_ph_individual_variables(cox_model_death_1yr_frail)),
  
  tar_target(
    ph_categories_death_1yr_elderly_emergency,
    testing_ph_categories(cox_model_death_1yr_elderly_emergency)),
  tar_target(
    ph_categories_death_1yr_falls,
    testing_ph_categories(cox_model_death_1yr_falls)),
  tar_target(
    ph_categories_death_1yr_frail,
    testing_ph_categories(cox_model_death_1yr_frail)),
  
  tar_target(
    schoenfeld_residuals_death_1yr_elderly_emergency,
    plotting_schoenfeld_residuals(cox_model_death_1yr_elderly_emergency)),
  tar_target(
    schoenfeld_residuals_death_1yr_falls,
    plotting_schoenfeld_residuals(cox_model_death_1yr_falls)),
  tar_target(
    schoenfeld_residuals_death_1yr_frail,
    plotting_schoenfeld_residuals(cox_model_death_1yr_frail))
  
  
  #AFT models
 # tar_target(
 #   aft_1yr_elderly_emergency,
 #   fitting_aft_1yr(survival_data_first_readmission_2223, "elderly emergency")),
 # tar_target(
 #   aft_1yr_falls,
 #   fitting_aft_1yrs(survival_data_first_readmission_2223, "falls")),
  #tar_target(
  #  fitting_aft_1yr_frail,
  #  fitting_aft_1yr(survival_data_first_readmission_2223, "frail"))
  
)
  