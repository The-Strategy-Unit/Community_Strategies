
library(tidyverse)
library(DBI)
library(odbc)
library(dbplyr)
library(scales)
library(ggrepel)


# Establish connection with local SQL server ----
con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "MLCSU-BI-SQL",
  Database = "HESdata",
  Trusted_Connection = "True"
)


# Specify which strategies (mitigators) are relevant for community work ----

# Pull the strategy look up
strategy_lookups <- 
  tbl(
    con,
    in_schema("nhp_modelling_reference", "strategy_lookups")
    ) |>
  collect()

# Identity strategies relevant to community work
relevant_strategies <-
  c(
    "frail_elderly_high",
    "frail_elderly_intermediate",
    "eol_care_2_days",
    "eol_care_3_to_14_days",
    "emergency_elderly", # LoS reduction,
    "falls_related_admissions",
    "readmission_within_28_days",
    "zero_los_no_procedure_adult",
    "zero_los_no_procedure_child",
    "alcohol_partially_attributable_acute",
    "alcohol_partially_attributable_chronic",
    "alcohol_wholly_attributable",
    "intentional_self_harm",
    "obesity_related_admissions",
    "raid_ae",
    "raid_ip",
    "medically_unexplained_related_admissions",
    "medicines_related_admissions_explicit",
    "medicines_related_admissions_implicit_anti-diabetics",
    "medicines_related_admissions_implicit_benzodiasepines",
    "medicines_related_admissions_implicit_diurectics",
    "medicines_related_admissions_implicit_nsaids",
    "ambulatory_care_conditions_acute",
    "ambulatory_care_conditions_chronic",
    "ambulatory_care_conditions_vaccine_preventable",
    "smoking",
    "virtual_wards_activity_avoidance_ari",
    "virtual_wards_activity_avoidance_heart_failure",
    "virtual_wards_efficiencies_ari",
    "virtual_wards_efficiencies_heart_failure"
  )

# strategies table is every epikey (episode ID) and the strategy it is captured in
tb_strategies <- 
  tbl(con,
      in_schema("nhp_modelling", "strategies")
  )

community_strategies <- 
  tb_strategies |> 
  filter(strategy %in% relevant_strategies)


# Establish scale of raw data: ----
tb_inpatients <- 
  tbl(con,
      in_schema("nhp_modelling", "inpatients")
  )


#tb_hes_inpatients <- 
#  tbl(con,
#      in_schema("dbo", "tbInpatients")
#  )

#tb_inpatients_spell_id <-
#  tb_inpatients |> 
#  left_join(tb_hes_inpatients |> 
#              select(EPIKEY, SUSSPELLID), by = "EPIKEY")

# Count all the rows in the nhp-inpatient table: 
# 240,894,690 rows
tb_inpatients |> 
  count()

# Scale: 
# 282,217,029 rows
# + 41,322,339 rows for duplication (episode captured in multiple strategies) (+17.1%)
community_strategies |> 
  count()

# Explore ----
# To explore raw data within community strategies: inner join tb_inpatients with communities_strategies
comm_strat_inpatient <-
  tb_inpatients |>
  inner_join(community_strategies, "EPIKEY") 

# Count episodes per strategy
episodes_per_strategy <-
  comm_strat_inpatient |>
  count(strategy) |> 
  collect()

episodes_per_strategy |> 
  arrange(desc(n)) |> 
  mutate(prop = n/sum(n)*100)
  
  ggplot

# Count episodes per strategy and year
episodes_per_strategy_annual <-
  comm_strat_inpatient |>
  count(FYEAR, strategy) |> 
  collect()

# Visualise 
episodes_per_strategy_annual |> 
  mutate(label = 
           case_when(FYEAR == 202223 ~ strategy)
         ) |> 
  
  ggplot(aes(x = FYEAR, y = n, fill = strategy, group = n,
             label = label)) +
  geom_col() +
  geom_label_repel(
                   position = position_stack(vjust = 0.5),
                   min.segment.length = 0,
                   point.padding = 0.5,
                   angle = 45,
                   hjust = 0.5,
                   vjust = 0.5,
                   size = 3
                   ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Financial year",
       y = "Spells",
       title = "Inpatient activity by strategy",
       subtitle = "HES 2008/09 - 2022/23")




frail_elderly_high_icb_annual <-
  comm_strat_inpatient |> 
  filter(strategy %in% c("frail_elderly_high")) |> 
  count(FYEAR, icb22cdh) |> 
  collect()


frail_elderly_high_icb_annual |> 
  group_by(FYEAR) |> 
  summarise(n = sum(n)) |> 
  ggplot(aes(x = FYEAR, y = n)) +
  geom_line()


frail_elderly_high_icb_annual |> 
  drop_na(icb22cdh) |> 
  
  ggplot(aes(x = FYEAR, y = n)) +
  geom_line() +
  facet_wrap(~icb22cdh)





# Frail elderly admissions - grouped demographics
frailty_demograph_grouped <-
  comm_strat_inpatient |> 
  filter(strategy %in% c("frail_elderly_high")) |>
  filter(FYEAR >= 201516) |> 
  group_by(AGE, SEX, ETHNOS, LSOA11) |> 
  summarise(n = n()) |> 
  collect()











# Questions 
# 1. How do I could spells if the raw date is episodes (had planned on pulling in spell_id)
#  -- Last episode in spell taken at inpatient-table stage
# 2. How/when do I disconnect from SQL server connection?


