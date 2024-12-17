# Rank NHP mitigators by episodes and bed days

library(tidyverse)
library(DBI)
library(odbc)

# Establish connection with local SQL server 
con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "MLCSU-BI-SQL",
  Database = "HESdata",
  Trusted_Connection = "True"
)

# Pull the strategy look up
strategy_lookups <- 
  tbl(
    con,
    in_schema("nhp_modelling_reference", "strategy_lookups")
  ) |>
  collect()

# Strategies table is every epikey (episode ID) and the strategy it is captured in (double counting present)
tb_strategies <- 
  tbl(con,
      in_schema("nhp_modelling", "strategies")
  )

# Pull in inpatients table to get episode length
tb_inpatients <- 
  tbl(con,
      in_schema("nhp_modelling", "inpatients")
  )

# Join inpatient table to strategy table to 
inpatient_strat <-
  tb_inpatients |>
  inner_join(tb_strategies, "EPIKEY") 

# Count episodes and bed days per strategy
episodes_beddays_per_strategy <-
  inpatient_strat |> 
  group_by(strategy) |> 
  summarise(episodes = sum(sample_rate),
            bed_days = sum(SPELDUR * sample_rate)) |> 
  ungroup() |> 
  mutate(episode_prop = episodes/sum(episodes)*100) |> 
  select(strategy, episodes, episode_prop, bed_days) |> 
  arrange(desc(episodes)) |> 
  collect()

write_csv(episodes_beddays_per_strategy, "episodes_beddays_per_strategy.csv")

# Disconnect from SQL server
dbDisconnect(con)
