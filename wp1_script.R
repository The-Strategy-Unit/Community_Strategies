# WP1 - community services strategy

library(tidyverse)
library(janitor)
library(DT)
library(sf)
library(readxl)
library(patchwork)
library(GGally)
library(ggVennDiagram)
library(ggvenn)
library(plotly)
library(reshape2)
library(ggrepel)

# Project specifications ----

#    **Package 1: Aggregate population:**
#      
#      Aggregate activity (frail, emergency elderly admissions, end of life and falls)
#    
#    * Count of patients and activity (spells and bed days)
#    * Variation across country: totals and time series
#    * Patient characteristics: Demographic breakdown (age, sex, ethnicity and deprivation)
#    * ICB and provider comparisons
#    
#    ICB-specific outputs of the above
#    
#    **Package 2: Variation in activity by ICB:**
#      
#      * "Does this measure tell us whether the systems in place are good or bad at treating mitigable activity?" 
#    * Age and gender adjusted activity rates by ICB 
#    * Contextual population analysis of healthcare needs/disease prevalence - deprivation weighted
#    * Regression analysis - controlling for population disease prevalence 
#    * Link to CDSD (Jac CSDS repository)
#    
#    
#    **Package 3: Overlap:**
#      
#      Overlap in mitigation populations?  
#      
#      
#      **Package 4 - survival analysis:**
#      
#      Survival analysis 1: time to readmission 
#    
#    * what patient groups are likely to be readmitted post-acute care
#    
#      
#      Survival analysis 2: mortality
#    
#    * What services were people in contact with in the lead up to death (1 year)
#    * Might people have been better treated if not in the secondary sector
#    * Compare location of deaths


library(tidyverse)
library(janitor)
library(DT)
library(sf)
library(readxl)
library(patchwork)

# Functions/lookups ----
create_dt <- function(x) {
  
  DT::datatable(
    x
    , extensions = "Buttons"
    , options = list(
      dom = "Blfrtip"
      , buttons = c("copy", "csv")
      , lengthMenu = list(
        c(10, 25, 50, -1)
        , c(10, 25, 50, "All"))))
}


ethnicity_lookup <- tribble(
  ~Code, ~Description,
  "A", "White - British",
  "B", "White - Irish",
  "C", "White - Any other White background",
  "D", "Mixed - White and Black Caribbean",
  "E", "Mixed - White and Black African",
  "F", "Mixed - White and Asian",
  "G", "Mixed - Any other mixed background",
  "H", "Asian or Asian British - Indian",
  "J", "Asian or Asian British - Pakistani",
  "K", "Asian or Asian British - Bangladeshi",
  "L", "Asian or Asian British - Any other Asian background",
  "M", "Black or Black British - Caribbean",
  "N", "Black or Black British - African",
  "P", "Black or Black British - Any other Black background",
  "R", "Other Ethnic Groups - Chinese",
  "S", "Other Ethnic Groups - Any other ethnic group",
  "Z", "Not stated"
)

# Set SU theme ----
SU_colours <- c(
  `orange`                     = grDevices::rgb(248, 191, 7, maxColorValue = 255), # "#f9bf07",
  `charcoal`                   = grDevices::rgb(44, 40, 37, maxColorValue = 255), # "#2c2825",
  `slate`                      = grDevices::rgb(104, 111, 115, maxColorValue = 255), # "#686f73",
  `blue`                       = grDevices::rgb(88, 29, 193, maxColorValue = 255), # "#5881c1",
  `red`                        = grDevices::rgb(236, 101, 85, maxColorValue = 255), # "#ec6555",
  # additional accent colours from word doc template
  `yellow`                     = grDevices::rgb(252, 229, 155, maxColorValue = 255),
  `grey`                       = grDevices::rgb(163, 168, 172, maxColorValue = 255),
  `white`                      = grDevices::rgb(255, 255, 255, maxColorValue = 255),
  # light and dark ends from colour theme in word doc
  `light orange`               = grDevices::rgb(253, 242, 205, maxColorValue = 255),
  `dark orange`                = grDevices::rgb(124, 95, 3, maxColorValue = 255),
  `light charcoal`             = grDevices::rgb(235, 233, 231, maxColorValue = 255),
  `dark charcoal`              =        "#000000", # black
  `light slate`                = grDevices::rgb(224, 226, 227, maxColorValue = 255),
  `dark slate`                 = grDevices::rgb(51, 55, 57, maxColorValue = 255),
  `light blue`                 = grDevices::rgb(221, 229, 242, maxColorValue = 255),
  `dark blue`                  = grDevices::rgb(38, 61, 102, maxColorValue = 255),
  `light red`                  = grDevices::rgb(251, 224, 220, maxColorValue = 255),
  `dark red`                   = grDevices::rgb(144, 29, 16, maxColorValue = 255),
  `light yellow`               = grDevices::rgb(254, 249, 235, maxColorValue = 255),
  `dark yellow`                = grDevices::rgb(197, 152, 5, maxColorValue = 255),
  `light grey`                 = grDevices::rgb(236, 237, 238, maxColorValue = 255),
  `dark grey`                  = grDevices::rgb(79, 84, 88, maxColorValue = 255),
  `light white`                = grDevices::rgb(242, 242, 242, maxColorValue = 255),
  `dark white`                 = grDevices::rgb(127, 127, 127, maxColorValue = 255),
  `red2`                       = grDevices::rgb(215, 25, 28, maxColorValue = 255),
  `orange2`                    = grDevices::rgb(253, 174, 97, maxColorValue = 255),
  `yellow2`                    = grDevices::rgb(255, 255, 191, maxColorValue = 255),
  `green2`                     = grDevices::rgb(171, 221, 164, maxColorValue = 255),
  `blue2`                      = grDevices::rgb(43, 131, 186, maxColorValue = 255) # "#2b83ba"
)

SU_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols)) {
    return(SU_colours)
  }
  
  SU_colours[cols]
}

SU_palettes <- list(
  `main` = SU_cols("orange", "charcoal", "slate", "blue", "red"),
  `oranges` = SU_cols("light orange", "orange", "dark orange"),
  `slates` = SU_cols("light slate", "slate", "dark slate"),
  `mixed` = SU_cols("dark red", "orange", "yellow", "light blue", "slate"),
  `oj_coal` = SU_cols("yellow", "orange", "red", "dark red", "dark charcoal"),
  `oj_red` = SU_cols("yellow", "orange", "red", "dark red"),
  `white_oj_coal` = SU_cols("white", "yellow", "orange", "red", "dark red", "dark charcoal"), # added since shared
  `lyellow_oj_coal` = SU_cols("light yellow", "orange", "red", "dark red", "dark charcoal"), # added since shared
  `wy_oj_coal` = SU_cols("white", "light yellow", "yellow", "orange", "red", "dark red", "charcoal", "dark charcoal"),
  `red_coal` = SU_cols("red", "dark red", "charcoal", "dark charcoal"),
  `blue_yellow_red` = SU_cols("red2", "orange2", "yellow2", "green2", "blue2"),
  `red_yellow_blue` = SU_cols("blue2", "green2", "yellow2", "orange2", "red2")
)


SU_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- SU_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


scale_color_SU <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- SU_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("SU_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_SU <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- SU_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("SU_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

theme_SU <- function(base_size) {
  theme_minimal(
    # base_family = "Segoe UI",
    base_size = 12
  ) %+replace%
    theme(
      axis.title = element_text(size = 11, face = "bold", colour = SU_cols("charcoal")),
      plot.title = element_text(hjust = 0, face = "bold", size = 12, colour = SU_cols("charcoal"), margin = margin(b = 4, unit = "pt")),
      plot.subtitle = element_text(hjust = 0, face = "italic", size = 10, colour = SU_cols("charcoal"), margin = margin(b = 4, unit = "pt")),
      plot.caption = element_text(hjust = 0, face = "italic", size = 9, colour = SU_cols("slate"), margin = margin(b = 4, unit = "pt")),
      legend.text = element_text(size = 10, colour = SU_cols("charcoal")),
      legend.title = element_text(face = "bold", size = 11, colour = SU_cols("charcoal"), margin = margin(b = 4, unit = "pt"))
    )
}

theme_set(theme_SU())


# Import data and split into sub-cohorts ----

aggregate_data <- 
  read_csv("aggregate_data_recombined.csv") |> 
  mutate(flag_der_falls = 
           case_when(!is.na(flag_fall_imp_frac) ~ flag_fall_imp_frac,
                     TRUE ~ flag_fall_imp_tend)) |> 
  filter(der_activity_month <= 202408) # August 2024 last full month 

# Create dataset of core cohorts
aggregate_data_core_cohorts <-
  aggregate_data |> 
  filter(!(is.na(flag_frail) & is.na(flag_eol) & is.na(flag_elderly_emergency) & is.na(flag_der_falls)))

  
aggregate_data_frail <-
  aggregate_data %>% 
  drop_na(flag_frail) %>% 
  mutate(year  = lubridate::year(month))

aggregate_data_eol <-
  aggregate_data %>% 
  drop_na(flag_eol) %>% 
  mutate(year  = lubridate::year(month))

aggregate_data_falls <-
  aggregate_data %>% 
  drop_na(flag_der_falls) %>% 
  mutate(year  = lubridate::year(month))

#aggregate_data_falls <-
#  aggregate_data %>% 
#  filter(!is.na(flag_falls_exp) |
#           !is.na(flag_fall_imp_frac) | 
#           !is.na(flag_fall_imp_tend)) %>% 
#  mutate(year  = lubridate::year(month))

aggregate_data_elderly_emergency <-
  aggregate_data %>% 
  drop_na(flag_elderly_emergency) %>% 
  mutate(year  = lubridate::year(month))

aggregate_data_amb_chronic <-
  aggregate_data %>% 
  drop_na(amb_chronic) %>% 
  mutate(year  = lubridate::year(month))

aggregate_data_amb_acute <-
  aggregate_data %>% 
  drop_na(amb_acute) %>% 
  mutate(year  = lubridate::year(month))

aggregate_data_amb_vacc_prev <-
  aggregate_data %>% 
  drop_na(amb_vacc_prev) %>% 
  mutate(year  = lubridate::year(month))

aggregate_data_eol_1_year <-
  aggregate_data %>% 
  drop_na(death_location_type) %>% 
  mutate(year  = lubridate::year(month)) 

# Plot aggregate activity ----
aggregate_data_core_cohorts %>%  
  group_by(month) %>% 
  summarise(`1. Spells` = sum(spells),
            `2. Bed days` = sum(los_sum)) %>% 
  pivot_longer(cols = -month) %>% 
  
  ggplot(aes(x = month, y = value, colour = name)) +
  #geom_point() +
  geom_line(linewidth = 1) +
  facet_wrap(~name, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_SU() +
  theme(legend.position = "none",
        strip.background = element_rect(fill = NA, colour = "grey")) +
  labs(x = "Month",
       y = "Spells",
       title = "National trend in mitigable acute inpatient admissions",
       subtitle = "SUS Apr 2018 - Aug 2024")

# v2
aggregate_data_core_cohorts %>%  
  group_by(month) %>% 
  summarise(`1. Spells` = sum(spells),
            `2. Individuals` = sum(person_n),
            `3. Bed days` = sum(los_sum)) %>% 
  pivot_longer(cols = -month) %>% 
  
  ggplot(aes(x = month, y = value, colour = name)) +
  #geom_point() +
  geom_line(linewidth = 1) +
  facet_wrap(~name, scales = "free_y", nrow = (3)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_SU() +
  theme(legend.position = "none",
        strip.background = element_rect(fill = NA, colour = "grey")) +
  labs(x = "Month",
       y = "Spells",
       title = "National trend in mitigable acute inpatient admissions",
       subtitle = "SUS Apr 2018 - Aug 2024")




aggregate_data_core_cohorts %>%  
  mutate(year  = lubridate::year(month)) %>% 
  group_by(year) %>% 
  summarise(`1. Spells` = sum(spells),
            `2. Individuals` = sum(person_n),
            `3. Bed days` = sum(los_sum)
            ) %>%
  pivot_longer(cols = -year) %>% 
  pivot_wider(id_cols = year, names_from = name, values_from = value)
  

# By cohort
aggregate_data_frail %>% 
  group_by(month) %>% 
  summarise(frail_spells = sum(spells),
            frail_ind = sum(person_n),
            frail_bed_days = sum(los_sum)) %>% 
  left_join(
    aggregate_data_eol %>% 
      group_by(month) %>% 
      summarise(eol_short_spells = sum(spells),
                eol_short_ind = sum(person_n),
                eol_short_bed_days = sum(los_sum)),
    by = "month"
    ) %>%
  left_join(
    aggregate_data_falls %>% 
      group_by(month) %>% 
      summarise(falls_spells = sum(spells),
                falls_ind = sum(person_n),
                falls_bed_days = sum(los_sum)),
    by = "month"
  ) %>%
  left_join(
    aggregate_data_elderly_emergency %>% 
      group_by(month) %>% 
      summarise(emergency_elderly_spells = sum(spells),
                emergency_elderly_ind = sum(person_n),
                emergency_elderly_bed_days = sum(los_sum)),
    by = "month"
  ) %>%
  #left_join(
  #  aggregate_data_amb_acute %>% 
  #    group_by(month) %>% 
  #    summarise(amb_acute_spells = sum(spells),
  #              amb_acute_bed_days = sum(los_sum)),
  #  by = "month"
  #) %>%
  #left_join(
  #  aggregate_data_amb_chronic %>% 
  #    group_by(month) %>% 
  #    summarise(amb_chronic_spells = sum(spells),
  #              amb_chronic_bed_days = sum(los_sum)),
  #  by = "month"
  #) %>%
  #left_join(
  #  aggregate_data_amb_vacc_prev %>% 
  #    group_by(month) %>% 
  #    summarise(amb_vaccine_spells = sum(spells),
  #              amb_vaccine_bed_days = sum(los_sum)),
  #  by = "month"
  #) %>%
  #left_join(
  #  aggregate_data_eol_1_year %>% 
  #    group_by(month) %>% 
  #    summarise(eol_1_year_spells = sum(spells),
  #              eol_1_year_bed_days = sum(los_sum)),
  #  by = "month"
  #) %>%
  
  pivot_longer(cols = -month) %>% 
  filter(month != as.Date("2024-10-01")) |> 
  mutate(type = case_when(str_detect(name, "bed_days") ~ "3. Bed days",
                          str_detect(name, "ind") ~ "2. Individuals",
                          TRUE ~ "1. Spells"),
         Cohort = case_when(str_detect(name, "frail") ~ "1. Frail",
                            str_detect(name, "eol_short") ~ "3. End of life",
                            str_detect(name, "falls") ~ "4. Falls",
                            str_detect(name, "emergency_elderly") ~ "2. Emergency elderly",
                            str_detect(name, "amb_acute") ~ "5. Ambulatory - Acute",
                            str_detect(name, "amb_chronic") ~ "6. Ambulatory - Chronic",
                            str_detect(name, "amb_vacc") ~ "7. Ambulatory - Vaccine preventable",
                            str_detect(name, "eol_1_year") ~ "8. End of life - 1 year"
                            )
         ) %>% 
  
  ggplot(aes(x = month, y = value, colour = Cohort)) +
  #geom_point() +
  geom_line(linewidth = 1) +
  facet_wrap(~type, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_SU() +
  theme(axis.text.x = element_text(angle = 90),
        strip.background = element_rect(fill = NA, colour = "grey")) +
  labs(x = "Month",
       y = "Spells",
       title = "National trend in acute inpatient admissions",
       subtitle = "SUS Apr 2018 - Aug 2024")


# Free axis 
aggregate_data_frail %>% 
  group_by(month) %>% 
  summarise(frail_spells = sum(spells),
            frail_ind = sum(person_n),
            frail_bed_days = sum(los_sum)) %>% 
  left_join(
    aggregate_data_eol %>% 
      group_by(month) %>% 
      summarise(eol_short_spells = sum(spells),
                eol_short_ind = sum(person_n),
                eol_short_bed_days = sum(los_sum)),
    by = "month"
  ) %>%
  left_join(
    aggregate_data_falls %>% 
      group_by(month) %>% 
      summarise(falls_spells = sum(spells),
                falls_ind = sum(person_n),
                falls_bed_days = sum(los_sum)),
    by = "month"
  ) %>%
  left_join(
    aggregate_data_elderly_emergency %>% 
      group_by(month) %>% 
      summarise(emergency_elderly_spells = sum(spells),
                emergency_elderly_ind = sum(person_n),
                emergency_elderly_bed_days = sum(los_sum)),
    by = "month"
  ) %>%
  #left_join(
  #  aggregate_data_amb_acute %>% 
  #    group_by(month) %>% 
  #    summarise(amb_acute_spells = sum(spells),
  #              amb_acute_bed_days = sum(los_sum)),
  #  by = "month"
  #) %>%
  #left_join(
  #  aggregate_data_amb_chronic %>% 
  #    group_by(month) %>% 
  #    summarise(amb_chronic_spells = sum(spells),
  #              amb_chronic_bed_days = sum(los_sum)),
  #  by = "month"
  #) %>%
  #left_join(
  #  aggregate_data_amb_vacc_prev %>% 
  #    group_by(month) %>% 
  #    summarise(amb_vaccine_spells = sum(spells),
  #              amb_vaccine_bed_days = sum(los_sum)),
  #  by = "month"
  #) %>%
  #left_join(
  #  aggregate_data_eol_1_year %>% 
  #    group_by(month) %>% 
  #    summarise(eol_1_year_spells = sum(spells),
  #              eol_1_year_bed_days = sum(los_sum)),
  #  by = "month"
  #) %>%
  pivot_longer(cols = -month) %>% 
  mutate(type = case_when(str_detect(name, "bed_days") ~ "2. Bed days",
                          TRUE ~ "1. Spells"),
         Cohort = case_when(str_detect(name, "frail") ~ "1. Frail",
                            str_detect(name, "eol_short") ~ "3. End of life",
                            str_detect(name, "falls") ~ "4. Falls",
                            str_detect(name, "emergency_elderly") ~ "2. Emergency elderly",
                            str_detect(name, "amb_acute") ~ "5. Ambulatory - Acute",
                            str_detect(name, "amb_chronic") ~ "6. Ambulatory - Chronic",
                            str_detect(name, "amb_vacc") ~ "7. Ambulatory - Vaccine preventable",
                            str_detect(name, "eol_1_year") ~ "8. End of life - 1 year"
         )
  ) %>%  
  #mutate(Cohort = factor(Cohort, levels = "Frail", "Emergency elderly", "End of life", "Falls")) %>% 
  
  ggplot(aes(x = month, y = value, colour = type)) +
  #geom_point() +
  geom_line() +
  facet_wrap(Cohort~type, scales = "free", ncol = 2) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_SU() +
  theme(axis.text.x = element_text(angle = 90),
        strip.background = element_rect(fill = NA, colour = "grey"),
        legend.position = "none") +
  labs(x = "Month",
       y = "Spells",
       title = "National trend in acute inpatient admissions",
       subtitle = "SUS Apr 2018 - Aug 2024")


# Free axis - v2
aggregate_data_frail %>% 
  group_by(month) %>% 
  summarise(frail_spells = sum(spells),
            frail_ind = sum(person_n),
            frail_bed_days = sum(los_sum)) %>% 
  left_join(
    aggregate_data_eol %>% 
      group_by(month) %>% 
      summarise(eol_short_spells = sum(spells),
                eol_short_ind = sum(person_n),
                eol_short_bed_days = sum(los_sum)),
    by = "month"
  ) %>%
  left_join(
    aggregate_data_falls %>% 
      group_by(month) %>% 
      summarise(falls_spells = sum(spells),
                falls_ind = sum(person_n),
                falls_bed_days = sum(los_sum)),
    by = "month"
  ) %>%
  left_join(
    aggregate_data_elderly_emergency %>% 
      group_by(month) %>% 
      summarise(emergency_elderly_spells = sum(spells),
                emergency_elderly_ind = sum(person_n),
                emergency_elderly_bed_days = sum(los_sum)),
    by = "month"
  ) %>%
  pivot_longer(cols = -month) %>% 
  
  mutate(type = case_when(str_detect(name, paste(c("spells", "ind"), collapse = "|")) ~ "1. Spells & Individuals",
                          #str_detect(name, "bed_days") ~ "2. Bed days",
                          TRUE ~ "2. Bed days"),
         Cohort = case_when(str_detect(name, "frail") ~ "Frail",
                            str_detect(name, "eol") ~ "End of life",
                            str_detect(name, "falls") ~ "Falls",
                            str_detect(name, "emergency") ~ "Emergency elderly"),
         label = case_when(str_detect(name, "spells") ~ "1. Spells",
                           str_detect(name, "ind") ~ "2. Individuals",
                           str_detect(name, "bed_days") ~ "3. Bed days")
         ) %>% 
  #mutate(Cohort = factor(Cohort, levels = "Frail", "Emergency elderly", "End of life", "Falls")) %>% 
  
  ggplot(aes(x = month, y = value, colour = label, group = name)) +
  #geom_point() +
  geom_line(linewidth = 1) +
  facet_wrap(Cohort~type, scales = "free", ncol = 2) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_SU() +
  theme(axis.text.x = element_text(angle = 90),
        strip.background = element_rect(fill = NA, colour = "grey"),
        #legend.position = "none"
        ) +
  labs(x = "Month",
       y = "Spells",
       colour = "",
       title = "National trend in acute inpatient admissions",
       subtitle = "SUS Apr 2018 - Aug 2024")


# Table
aggregate_data_frail %>% 
  group_by(year) %>% 
  summarise(frail_spells = sum(spells),
            frail_ind = sum(person_n),
            frail_bed_days = sum(los_sum)) %>% 
  left_join(
    aggregate_data_eol %>% 
      group_by(year) %>% 
      summarise(eol_spells = sum(spells),
                eol_ind = sum(person_n),
                eol_bed_days = sum(los_sum)),
    by = "year"
  ) %>%
  left_join(
    aggregate_data_falls %>% 
      group_by(year) %>% 
      summarise(falls_spells = sum(spells),
                falls_ind = sum(person_n),
                falls_bed_days = sum(los_sum)),
    by = "year"
  ) %>%
  left_join(
    aggregate_data_elderly_emergency %>% 
      group_by(year) %>% 
      summarise(emergency_elderly_spells = sum(spells),
                emergency_elderly_ind = sum(person_n),
                emergency_elderly_bed_days = sum(los_sum)),
    by = "year"
  ) %>%
  pivot_longer(cols = -year) %>%
  mutate(value = scales::comma(value)) |> 
  pivot_wider(id_cols = year, names_from = name, values_from = value) |> 
  rename(
    `Frail - spells` = frail_spells,
    `Frail - individuals` = frail_ind,
    `Frail - bed days` = frail_bed_days,
    `EoL - spells` = eol_spells,
    `EoL - individuals` = eol_ind,
    `Eol - bed days` = eol_bed_days, 
    `Falls - spells` = falls_spells,
    `Falls - individuals` = falls_ind,
    `Falls - bed days` = falls_bed_days,
    `Emergency elderly - spells` = emergency_elderly_spells,
    `Emergency elderly - individuals` = emergency_elderly_ind,
    `Emergency elder - bed days` = emergency_elderly_bed_days
  )


## Map ----

# Read in icb lookup
icb_lookup <- 
  read_csv("mapping/S_ICB_ICB_to_NHSE_R.csv") %>% 
  clean_names() %>% 
  select(icb23cd, icb23cdh, icb23nm, nhser23nm) %>% 
  distinct()

# Import icb shp file from geoportal
icb_23_shp <- 
  st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>% 
  clean_names()

icb_23_shp %>% 
  select(icb23cd) %>% 
  left_join(
    aggregate_data %>%  
      mutate(year = year(month)) %>% 
      filter(year == 2023) %>% 
      group_by(icb_name_short) %>% 
      summarise(spells = sum(spells)) %>% 
      mutate(icb_code = str_extract(icb_name_short, "^[^:]+")) %>% 
      left_join(icb_lookup, by = c("icb_code" = "icb23cdh")),
    by = "icb23cd"
  ) %>% 
  
  # Plot chloropleth map 
  ggplot() +
  geom_sf(aes(fill = spells)) +
  scale_fill_gradient(low = "yellow", high = "red", 
                      name = "Admissions",
                      labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank()
  ) +
  labs(title = "Mitigable activity by ICB",
       subtitle = "SUS 2023 | Admission identified: Frail, emergency elderly, falls & end of life")

# ICB activity 
aggregate_data %>%  
  group_by(month, icb_name_short) %>% 
  summarise(spells = sum(spells)) %>% 
  filter(icb_name_short != "NULL") %>% 
  mutate(icb_name_short = str_extract(icb_name_short, "(?<=NHS).*")) %>% 
  mutate(icb_name_short = str_remove_all(icb_name_short, " ICB")) |> 
  
  ggplot(aes(x = month, y = spells, group = icb_name_short)) +
  #geom_point() +
  geom_line() +
  facet_wrap(~str_wrap(icb_name_short, 20), scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Month",
       y = "Spells",
       title = "ICB trends in mitigable acute inpatient admissions",
       subtitle = "SUS Apr 2018 - Aug 2024")

aggregate_data %>%  
  group_by(month, icb_name_short) %>% 
  summarise(spells = sum(spells)) %>% 
  filter(icb_name_short != "NULL") %>% 
  mutate(icb_name_short = str_extract(icb_name_short, "(?<=NHS).*")) %>% 
  
  ggplot(aes(x = month, y = spells)) +
  #geom_point() +
  geom_line(aes(group = icb_name_short), colour = "#2c2825") +
  geom_smooth(aes(group = 1), method = "loess", colour = "#f9bf07") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Month",
       y = "Spells",
       title = "ICB trends in mitigable acute inpatient admissions",
       subtitle = "SUS Apr 2018 - Aug 2024")


## Share of total activity ----
icb_spell_denominators <- 
  read_csv("icb_spell_denominators.csv") |>
  clean_names()

aggregate_data |> 
  group_by(der_activity_month) |> 
  summarise(cohort_spells = sum(spells)) |>  
  left_join(icb_spell_denominators |>
              group_by(der_activity_month) |> 
              summarise(denom_spells = sum(spells)), 
            by = c("der_activity_month")) |> 
  mutate(prop = round(cohort_spells/denom_spells * 100, 3)) |>
  mutate(month = 
           as.Date(
             paste0(str_sub(der_activity_month,1,4),
                    "-",
                    str_sub(der_activity_month,5,6),
                    "-01")
           )
  ) |> 
  
  ggplot(aes(x = month, y = prop)) +
  #geom_point() +
  geom_line() +
  geom_smooth(method = "loess", colour = "#f9bf07") +
  labs(y = "Cohort proportion",
       title = "Cohort spells as a proportion of total spells",
       subtitle = "NHS England admissions denominator | 2018-24")


spell_proportion <- function(data, label_input) {
  
  data |> 
    group_by(der_activity_month) |> 
    summarise(cohort_spells = sum(spells)) |>  
    left_join(icb_spell_denominators |> 
                group_by(der_activity_month) |> 
                summarise(denom_spells = sum(spells)), 
              by = c("der_activity_month")) |> 
    mutate(prop = round(cohort_spells/denom_spells * 100, 3)) |>
    mutate(month = 
             as.Date(
               paste0(str_sub(der_activity_month,1,4),
                      "-",
                      str_sub(der_activity_month,5,6),
                      "-01")
             )
    ) |> 
    mutate(id = label_input) |> 
    select(month, cohort_spells, denom_spells, prop, id)
}

spell_proportion(aggregate_data_elderly_emergency, "1. Emergency elderly") |> 
  union_all(spell_proportion(aggregate_data_frail, "2. Frail")) |> 
  union_all(spell_proportion(aggregate_data_falls, "3. Falls")) |>
  union_all(spell_proportion(aggregate_data_eol, "4. End of life")) |> 
  
  union_all(spell_proportion(aggregate_data_amb_acute, "5. Ambulatory - acute")) |> 
  union_all(spell_proportion(aggregate_data_amb_chronic, "6. Ambulatory - chronic")) |> 
  union_all(spell_proportion(aggregate_data_amb_vacc_prev, "7. Ambulatory - vaccine preventable")) |> 
  union_all(spell_proportion(aggregate_data_eol_1_year, "8. End of life - 1 year")) |> 
  
  ggplot(aes(x = month, y = prop, colour = id)) +
  geom_point() +
  geom_line() +
  facet_wrap(~id, scales = "free_y"
  ) +
  geom_smooth(method = "loess") +
  scale_color_SU() +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        axis.title.x = element_blank(),
        legend.position = "none"
  ) +
  labs(y = "Cohort proportion",
       colour = "Cohort:",
       title = "Spells as a proportion of total spells",
       subtitle = "NHS England | 2018-24")


# ICB boxplot with outliers
icb_spells_proportion_total <-
  aggregate_data |> 
  group_by(der_activity_month, icb_name_short) |> 
  summarise(cohort_spells = sum(spells)) |> 
  left_join(icb_spell_denominators, by = c("der_activity_month", "icb_name_short")) |> 
  mutate(prop = cohort_spells/spells * 100) |> 
  mutate(month = 
           as.Date(
             paste0(str_sub(der_activity_month,1,4),
                    "-",
                    str_sub(der_activity_month,5,6),
                    "-01")
           )
  ) |>
  mutate(icb_name_clean = str_sub(icb_name_short, 10,100)) |> 
  mutate(icb_name_clean = str_remove_all(icb_name_clean, " ICB"))

icb_spells_proportion_total |> 
  drop_na(icb_name_clean) |> 
  ggplot(aes(x = month, y = prop, group = month,)) +
  geom_boxplot(width = 5 ) +
  
  #stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_line(data = icb_spells_proportion_total |> 
              filter(icb_name_clean %in% c(#"Frimley",
                "Dorset",
                "North Central London",
                "Sussex",
                "Somerset",
                "North East London")),
            aes(colour = icb_name_clean, group = icb_name_clean), linewidth = 1) +
  scale_color_SU() +
  theme(axis.title.x = element_blank()) +
  labs(y = "Cohort proportion",
       colour = "Outlier ICB:",
       title = "Cohort admissions as a proportion of total activity",
       subtitle = "ICB 2018-24")


# Activity by ICB ----

## Age and sex adjusted population rate ----

icb_pop_2023 <- 
  read_excel("pop_estimates/sapehealthgeogstablefinal.xlsx", 
             sheet = "Mid-2022 ICB 2023", skip = 3) %>% 
  clean_names()

# Clean and group by ICB, sex and age-range
icb_pop_2023_sex_age_range <- 
  icb_pop_2023 %>%
  pivot_longer(cols = starts_with("f") | starts_with("m"), 
               names_to = "age_sex", 
               values_to = "population") %>%
  mutate(sex = as.numeric(ifelse(grepl("^m", age_sex), "1", "2")),
         age = as.numeric(sub("^[mf]", "", age_sex))) %>%
  mutate(age_range = 
           cut(age, 
               breaks = seq(0, 90, by = 5), 
               right = FALSE, 
               labels = paste(seq(0, 85, by = 5))
           )
  ) %>% 
  group_by(icb_2023_code, icb_2023_name, sex, age_range) %>% 
  summarise(population = sum(population, na.rm= TRUE)) %>%
  ungroup() %>% 
  mutate(icb_2023_name = str_replace_all(icb_2023_name, "Integrated Care Board", "ICB")) %>% 
  mutate(icb_name_short_trans = 
           case_when(
             icb_2023_name == "NHS Bath and North East Somerset, Swindon and Wiltshire ICB" ~ "NHS Bath & NE S'set, S'don & W ICB",
             icb_2023_name == "NHS Bedfordshire, Luton and Milton Keynes ICB" ~ "NHS Beds, Luton & Milton Keynes ICB",
             icb_2023_name == "NHS Birmingham and Solihull ICB" ~ "NHS Birmingham And Solihull ICB",
             icb_2023_name == "NHS Bristol, North Somerset and South Gloucestershire ICB" ~ "NHS Bristol, N S'set & S Gloucs ICB",
             icb_2023_name == "NHS Buckinghamshire, Oxfordshire and Berkshire West ICB" ~ "NHS Bucks, Oxford & Berkshire W ICB",
             icb_2023_name == "NHS Cambridgeshire and Peterborough ICB" ~ "NHS Cambs & Peterborough ICB",
             icb_2023_name == "NHS Cornwall and the Isles of Scilly ICB" ~ "NHS Cornwall & Isles Of Scilly ICB",
             icb_2023_name == "NHS Herefordshire and Worcestershire ICB" ~ "NHS Herefords & Worcestershire ICB",
             icb_2023_name == "NHS Hertfordshire and West Essex ICB" ~ "NHS Hertfordshire & West Essex ICB",
             icb_2023_name == "NHS Lancashire and South Cumbria ICB" ~ "NHS Lancashire & South Cumbria ICB",
             icb_2023_name == "NHS Leicester, Leicestershire and Rutland ICB" ~ "NHS Leics, Leic'shire & Rutland ICB",
             icb_2023_name == "NHS North East and North Cumbria ICB" ~ "NHS North East & North Cumbria ICB",
             icb_2023_name == "NHS Nottingham and Nottinghamshire ICB" ~ "NHS Nott'ham & Nottinghamshire ICB",
             icb_2023_name == "NHS Shropshire, Telford and Wrekin ICB" ~ "NHS Shrops, Telford & Wrekin ICB",
             icb_2023_name == "NHS Staffordshire and Stoke-on-Trent ICB" ~ "NHS Staffordshire & Stoke-On-Tr ICB",
             icb_2023_name == "NHS Suffolk and North East Essex ICB" ~ "NHS Suffolk & North East Essex ICB",
             TRUE ~ icb_2023_name
           ))


# Apply standardisation to cohort sub-groups

standardisation_function <- function(data_input) {
  
  crude_spell_rate <-
    data_input %>%  
    mutate(year = year(month),
           age_range = as.character(age_range)) %>% 
    group_by(year, icb_name_short, sex, age_range) %>% 
    summarise(spells = sum(spells)) %>%
    mutate(icb_code = str_extract(icb_name_short, "^[^:]+")) %>% 
    left_join(icb_lookup, by = c("icb_code" = "icb23cdh")) %>% 
    left_join(icb_pop_2023_sex_age_range %>% 
                mutate(age_range = as.character(age_range)),
              by = c("icb23cd" = "icb_2023_code", 
                     "sex", "age_range"), keep = F) %>%
    mutate(spell_rate = spells/population)
  
  # Define a standard population (e.g., the total population in the dataset)
  standard_population <- 
    icb_pop_2023_sex_age_range %>%
    group_by(age_range, sex) %>%
    summarise(population = sum(population))
  
  # Merge with the standard population and calculate adjusted rate
  adjusted_rate_icb <-
    crude_spell_rate %>%
    left_join(standard_population, by = c("age_range", "sex"), suffix = c("", "_standard")) %>% 
    drop_na(spell_rate) %>% 
    group_by(icb23cd, icb_name_short, year) %>%
    summarise(adjusted_spell_rate = sum(spell_rate * population_standard) / sum(population_standard) * 1000)
  
  adjusted_rate_icb
  
}

adjusted_rate_sub_cohorts <-
  standardisation_function(aggregate_data) %>% 
  mutate(id = "rate") %>% 
  union_all(standardisation_function(aggregate_data_frail) %>% 
              mutate(id = "frail_rate")) %>% 
  union_all(standardisation_function(aggregate_data_falls) %>% 
              mutate(id = "falls_rate")) %>%
  union_all(standardisation_function(aggregate_data_eol) %>% 
              mutate(id = "eol_rate")) %>% 
  union_all(standardisation_function(aggregate_data_elderly_emergency) %>% 
              mutate(id = "elderly_emergency_rate")) %>% 
  ungroup() %>% 
  mutate(id_clean = case_when(id == "frail_rate" ~ "A. Frail",
                              id == "falls_rate" ~ "C. Falls",
                              id == "eol_rate" ~ "D. End of life",
                              id == "elderly_emergency_rate" ~ "B. Emergency elderly",
                              id == "rate" ~ "E. Combined cohort"))

crude_spell_rate <-
  aggregate_data %>%  
  mutate(year = year(month),
         age_range = as.character(age_range)) %>% 
  group_by(year, icb_name_short, sex, age_range) %>% 
  summarise(spells = sum(spells)) %>%
  mutate(icb_code = str_extract(icb_name_short, "^[^:]+")) %>% 
  left_join(icb_lookup, by = c("icb_code" = "icb23cdh")) %>% 
  left_join(icb_pop_2023_sex_age_range %>% 
              mutate(age_range = as.character(age_range)),
            by = c("icb23cd" = "icb_2023_code", 
                   "sex", "age_range"), keep = F) %>%
  mutate(spell_rate = spells/population)

plot_function <- function(cohort, label) {
  
  icb_23_shp %>%
    select(icb23cd) %>%
    left_join(adjusted_rate_sub_cohorts %>%  
                filter(id == cohort,
                       year == 2023) %>% 
                mutate(quintile = cut(adjusted_spell_rate, 
                                      breaks = quantile(adjusted_spell_rate, probs = seq(0, 1, 0.2), na.rm = TRUE), 
                                      include.lowest = TRUE, 
                                      labels = FALSE)),
              by = "icb23cd") %>%
    
    ggplot() +
    geom_sf(aes(fill = factor(quintile))) +
    scale_fill_brewer(palette = "YlOrRd", 
                      name = paste0(label, ": Quintiles"),
                      labels = c("1st", "2nd", "3rd", "4th", "5th")) +
    theme(panel.grid = element_blank(),
          axis.text = element_blank()) 
}

frail_map <- plot_function("frail_rate", "A. Frail")
falls_map <- plot_function("falls_rate", "C. Falls")
eol_map <- plot_function("eol_rate", "D. EoL")
elderly_emergency_map <- plot_function("elderly_emergency_rate", "B. Elderly emergency")

# Plot patchwork map
frail_map +
  elderly_emergency_map +
  falls_map +
  eol_map +
  
  plot_annotation(
    title = 'Mitigable activity by ICB - admission rate',
    subtitle = 'SUS 2023 | Admission identified: Frail, emergency elderly, falls & end of life',
    caption = 'Note: 1st quintile represents the lowest 20% admission rates'
  )


## Line chart - per ICB
standardisation_function(aggregate_data) %>% 
  filter(year != 2024) %>% 
  ggplot(aes(x = year, y = adjusted_spell_rate, group = icb_name_short)) +
  geom_line() +
  labs(x = "Year",
       y = "Adjusted admission rate",
       title = "Age and sex adjusted admission rate by ICB",
       subtitle = "Admissions per 1,000 population | 2018-23")

## Box plot
standardisation_function(aggregate_data) %>% 
  filter(year != 2024) %>% 
  ggplot(aes(x = year, y = adjusted_spell_rate, group = year)) +
  geom_point() +
  geom_boxplot() +
  labs(x = "Year",
       y = "Adjusted admission rate",
       title = "Age and sex adjusted admission rate by ICB",
       subtitle = "Admissions per 1,000 population | 2018-23")

## Boxplot per sub-cohort
adjusted_rate_sub_cohorts %>% 
  filter(year != 2024,
         id != "rate") %>% 
  ggplot(aes(x = year, y = adjusted_spell_rate, group = year, fill = id_clean)) +
  geom_point() +
  geom_boxplot() +
  facet_wrap(~id_clean, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.background = element_rect(fill = NA, colour = "grey")) +
  labs(y = "Adjusted admission rate",
       title = "Mitigable activity - Admission rate per 1,000 population by sub-cohort",
       subtitle = "SUS 2018-24")


adjusted_rate_sub_cohorts |> 
  filter(year == 2023) |> 
  select(-icb23cd, -year, -id) |> 
  filter(id_clean != "E. Combined cohort") |> 
  group_by(id_clean) |> 
  mutate(quintile = 
           case_when(adjusted_spell_rate >= quantile(adjusted_spell_rate, 0.8)~ "5",
                     adjusted_spell_rate <= quantile(adjusted_spell_rate, 0.2)~ "1",
                     TRUE ~ "2-4")) |>
  mutate(facet = "") |>
  mutate(icb_name_clean = str_sub(icb_name_short, 10,100)) |> 
  mutate(icb_name_clean = str_remove_all(icb_name_clean, " ICB"))|> 
  mutate(icb_name_clean = fct_reorder(icb_name_clean, adjusted_spell_rate)) |>
  
  ggplot(aes(y = icb_name_clean, x = adjusted_spell_rate, fill = quintile)) +
  geom_col() +
  facet_grid(facet~id_clean, scales = "free_x") +
  scale_fill_SU() +
  theme(strip.background.x = element_rect(fill = NA, colour = "grey")) +
  labs(x = "Adjusted admission rate",
       y = "ICB",
       fill = "Quintile:",
       title = "Admission rate by ICB and sub-cohort",
       subtitle = "Age and sex adjusted rate per 1,000 population | SUS admissions 2023")

## Funnel plots ----

# Create function to draw funell plots
funnel_plot_function <- function(cohort, subtitle_text) {
  
  data <-
    adjusted_rate_sub_cohorts |> 
    filter(id_clean == {{cohort}}, 
           year == 2023) |> 
    left_join(icb_pop_2023_sex_age_range |> 
                group_by(icb_2023_code) |> 
                summarise(population = sum(population)),
              by = c("icb23cd" = "icb_2023_code")
              ) 
  
  mean_rate <- mean(data$adjusted_spell_rate)
  sd_rate <- sd(data$adjusted_spell_rate)
  
  # Create the funnel plot
  data |>
    mutate(std_from_mean = (adjusted_spell_rate - mean_rate) / sd_rate) |> 
    mutate(fill_text = 
             case_when(std_from_mean >= 1 ~ "1. Above 1 SD",
                       std_from_mean <= -1 ~ "3. Below 1 SD",
                       TRUE ~ "2. Within 1 SD")) |>
    mutate(icb_name_clean = str_sub(icb_name_short, 10,100)) |> 
    mutate(icb_name_clean = str_remove_all(icb_name_clean, " ICB"))|> 
    mutate(label = case_when(fill_text != "2. Within 1 SD" ~ icb_name_clean)) |> 
    
    ggplot(aes(x = population, y = adjusted_spell_rate, 
               colour = fill_text, 
               alpha = fill_text
    )) +
    geom_point(size = 4) +
    geom_label_repel(aes(label = label), size = 3, show.legend = FALSE) +
    geom_hline(yintercept = mean_rate, color = "blue", linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = mean_rate + sd_rate, color = "red", linetype = "dotted", linewidth = 1) +
    geom_hline(yintercept = mean_rate - sd_rate, color = "red", linetype = "dotted", linewidth = 1) +
    scale_x_continuous(labels = scales::comma) +
    scale_alpha_manual(values = c(1,0.2,1)) +
    scale_color_SU() +
    labs(x = "Population",
         y = "Adjusted admission rate per 1,000",
         title = "Variation in admission rates by ICB and underlying population",
         subtitle = paste0("Subcohort: ", subtitle_text, " | 2023"),
         colour = "",
         alpha = ""
    ) 
  }

funnel_plot_function("A. Frail", "Frail")
funnel_plot_function("B. Emergency elderly", "Emergency elderly")
funnel_plot_function("C. Falls", "Falls")
funnel_plot_function("D. End of life", "End of life")


# 2x2 plots to compare admission rates in 2 cohorts on 1 graph

compare_2_2_plots <- function(cohort_1, cohort_2, cohort_1_axis, cohort_2_axis) {
  
  data_cohort_1 <-
    adjusted_rate_sub_cohorts |> 
    filter(id_clean == {{cohort_1}}, 
           year == 2023) 
  
  mean_rate_cohort_1 <- mean(data_cohort_1$adjusted_spell_rate)
  sd_rate_cohort_1 <- sd(data_cohort_1$adjusted_spell_rate)
  
  data_cohort_2 <-
    adjusted_rate_sub_cohorts |> 
    filter(id_clean == {{cohort_2}}, 
           year == 2023) 
  
  mean_rate_cohort_2 <- mean(data_cohort_2$adjusted_spell_rate)
  sd_rate_cohort_2 <- sd(data_cohort_2$adjusted_spell_rate)
  
  # Plot
  plot <-
    data_cohort_1 |> 
    mutate(std_from_mean_cohort_1 = (adjusted_spell_rate - mean_rate_cohort_1) / sd_rate_cohort_1) |> 
    select(1:2, adjusted_spell_rate, std_from_mean_cohort_1) |> 
    rename(cohort_1_adj_rate = adjusted_spell_rate) |> 
    left_join(data_cohort_2 |> 
                mutate(std_from_mean_cohort_2 = (adjusted_spell_rate - mean_rate_cohort_2) / sd_rate_cohort_2) |> 
                select(1:2, adjusted_spell_rate, std_from_mean_cohort_2) |> 
                rename(cohort_2_adj_rate = adjusted_spell_rate),
              by = c("icb23cd", "icb_name_short")) |> 
    rename(ICB = icb_name_short) |> 
    
    mutate(fill_text = 
             case_when(
               (std_from_mean_cohort_1 <= 1 & std_from_mean_cohort_1 >= -1) &
                 (std_from_mean_cohort_2 <= 1 & std_from_mean_cohort_2 >= -1) ~ "A. Within 1 SD",
               (std_from_mean_cohort_1 > 1 & std_from_mean_cohort_2 > 1) ~ "B. Above 1 SD",
               (std_from_mean_cohort_1 < -1 & std_from_mean_cohort_2 < -1) ~ "C. Below 1 SD",
               TRUE ~ "D. +/- 1 SD on one axis"
             )) |> 
    
    ggplot(aes(x = cohort_1_adj_rate, y = cohort_2_adj_rate, 
               colour = fill_text, alpha = fill_text, 
               label =  ICB
               )) +
    geom_point(size = 3) +
    
    # Cohort 1 lines
    geom_vline(xintercept = mean_rate_cohort_1, color = "blue", linetype = "dashed", linewidth = 0.3) +
    geom_vline(xintercept = mean_rate_cohort_1 + sd_rate_cohort_1, color = "red", linetype = "dotted", linewidth = 0.25) +
    geom_vline(xintercept = mean_rate_cohort_1 - sd_rate_cohort_1, color = "red", linetype = "dotted", linewidth = 0.25) +
    
    # Cohort 2 line
    geom_hline(yintercept = mean_rate_cohort_2, color = "blue", linetype = "dashed", linewidth = 0.3) +
    geom_hline(yintercept = mean_rate_cohort_2 + sd_rate_cohort_2, color = "red", linetype = "dotted", linewidth = 0.25) +
    geom_hline(yintercept = mean_rate_cohort_2 - sd_rate_cohort_2, color = "red", linetype = "dotted", linewidth = 0.25) +
    
    scale_x_continuous(labels = scales::comma) +
    scale_alpha_manual(values = c(0.3, 1, 1, 1)) + # Only shade out those within 1 SD on both axis
    scale_color_SU() +
    theme_SU() +
    labs(x = paste0("Admission rate: ", cohort_1_axis),
         y = paste0("Admission rate: ", cohort_2_axis),
         colour = "",
         alpha = ""
         )
  
  #plot
  
  ggplotly(plot, tooltip = "label") |>
    layout(
      title = list(
        text = "<b>Variation in admission rates by ICB</b><br>",
        x = 0.05,  # Center the title
        y = 0.95,
        xanchor = "left",
        font = list(size = 14)  # Title font size
      ),
      annotations = list(
        list(
          text = "<i>Comparison of adjusted admission rates by sub-cohort</i>",
          x = 0,  # Center the title
          y = 0.98,
          xanchor = "left",
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 12)  # Subtitle font size
          )
        )
      )
}

compare_2_2_plots("A. Frail", "B. Emergency elderly", "Frail", "Emergency elderly")
compare_2_2_plots("A. Frail", "C. Falls", "Frail", "Falls")
compare_2_2_plots("A. Frail", "D. End of life", "Frail", "End of life")
compare_2_2_plots("B. Emergency elderly", "C. Falls", "Emergency elderly", "Falls")
compare_2_2_plots("B. Emergency elderly", "D. End of life", "Emergency elderly", "End of life")
compare_2_2_plots("C. Falls", "D. End of life", "Falls", "End of life")


# Overlap in mitigators ----
### Plot 4d geom_point positions for all 4x subcohorts

library(GGally)

# Create pairs plot
compare_admission_rates <-
  adjusted_rate_sub_cohorts |> 
  filter(year == 2023) |> 
  select(-icb23cd, -year, -id) |> 
  pivot_wider(id_cols = icb_name_short,
              names_from = id_clean,
              values_from = adjusted_spell_rate) |> 
  select(icb_name_short,`A. Frail`, `B. Emergency elderly`, `C. Falls`, `D. End of life`) 


compare_admission_rates |> 
  ggpairs(columns = 2:5, 
          #title = "Admission Rates per ICB for 4 Sub-Cohorts",
          upper = list(continuous = wrap("cor", size = 3)),
          lower = list(continuous = wrap("points", alpha = 0.6, size = 1.5)),
          diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  #theme(strip.background = element_rect(fill = NA, colour = "grey")) +
  StrategyUnitTheme::su_theme() +
  labs(title = "Admission Rates per ICB for mitigable sub-cohorts",
       subtitle = "SUS adjusted admission rate per 1,000 | 2023",
       x = "Admission rate")


thresholds <- sapply(compare_admission_rates[, 2:5], function(x) quantile(x, 0.9))

# Comparative bar plot
adjusted_rate_sub_cohorts |> 
  filter(year == 2023) |> 
  select(-icb23cd, -year, -id) |> 
  filter(id_clean != "E. Combined cohort") |> 
  group_by(id_clean) |> 
  mutate(quintile = 
           case_when(adjusted_spell_rate >= quantile(adjusted_spell_rate, 0.8)~ "Highest quintile",
                     TRUE ~ "Quintile 1-4")) |>
  mutate(facet = "") |>
  mutate(icb_name_clean = str_sub(icb_name_short, 10,100)) |> 
  mutate(icb_name_clean = str_remove_all(icb_name_clean, " ICB"))|> 
  mutate(icb_name_clean = fct_reorder(icb_name_clean, adjusted_spell_rate)) |>
  
  ggplot(aes(y = icb_name_clean, x = adjusted_spell_rate, fill = quintile)) +
  geom_col() +
  facet_grid(facet~id_clean, scales = "free_x") +
  scale_fill_SU() +
  theme(strip.background.x = element_rect(fill = NA, colour = "grey")) +
  labs(x = "Adjusted admission rate",
       y = "ICB",
       fill = "Quintile:",
       title = "Admission rate by ICB and sub-cohort",
       subtitle = "Age and sex adjusted rate per 1,000 population | SUS admissions 2023")


## Venn diagram
overlap_data <-
  aggregate_data %>%
  filter(year(month) == 2023) |> 
  mutate(flag_falls = 
           case_when(
             !is.na(flag_falls_exp) ~ flag_falls_exp,
             !is.na(flag_fall_imp_frac) ~ flag_fall_imp_frac,
             !is.na(flag_fall_imp_tend) ~ flag_fall_imp_tend,
             TRUE ~ NA
           )) |> 
  group_by(flag_frail, flag_elderly_emergency, flag_eol, flag_falls) |> 
  summarise(spells = sum(spells)) |> 
  mutate(
    flag_frail = !is.na(flag_frail),
    flag_eol = !is.na(flag_eol),
    flag_falls = !is.na(flag_falls),
    flag_elderly_emergency = !is.na(flag_elderly_emergency)
  ) # Convert flags to logical values

# Calculate the number of spells for each combination
overlap_combinations <- 
  overlap_data %>%
  group_by(flag_frail, flag_elderly_emergency, flag_eol, flag_falls) %>%
  summarise(spells = sum(spells), .groups = 'drop')

expanded_data <- 
  overlap_combinations %>%
  rowwise() %>%
  do(data.frame(flag_frail = rep(.$flag_frail, .$spells),
                flag_eol = rep(.$flag_eol, .$spells),
                flag_falls = rep(.$flag_falls, .$spells),
                flag_elderly_emergency = rep(.$flag_elderly_emergency, .$spells)
  ))

# Convert to matrix
expanded_matrix <- as.matrix(expanded_data)

# Create a list for Venn diagram
venn_data <- list(
  Frail = which(expanded_matrix[, "flag_frail"]),
  EOL = which(expanded_matrix[, "flag_eol"]),
  Falls = which(expanded_matrix[, "flag_falls"]),
  `Emergency elderly` = which(expanded_matrix[, "flag_elderly_emergency"])
)


# Create Venn diagram
ggVennDiagram(venn_data) +
  scale_fill_gradient(low = "white", high = "#f9bf07") +
  theme_minimal() +
  labs(title = "Venn Diagram of Patient Cohorts Sized by Spells")


ggvenn(venn_data)


## Heat map
# Summarize the data
summary_data <- 
  expanded_data %>%
  group_by(flag_frail, flag_elderly_emergency, flag_eol, flag_falls) %>%
  summarise(count = n(), .groups = 'drop')

# Convert logical values to character for better readability in the heatmap
summary_data <- summary_data %>%
  mutate(
    flag_frail = ifelse(flag_frail, "Frail", "Not Frail"),
    flag_eol = ifelse(flag_eol, "EOL", "Not EOL"),
    flag_falls = ifelse(flag_falls, "Falls", "Not Falls"),
    flag_elderly_emergency = ifelse(flag_elderly_emergency, "Elderly emergency", "Not elderly emergency")
  )

# Create the heatmap
ggplot(summary_data, aes(x = flag_frail, y = flag_eol, fill = count)) +
  geom_tile() +
  facet_wrap(~ flag_falls) +
  scale_fill_gradient(low = "white", high = "#f9bf07") +
  labs(title = "Heatmap of Spells by Cohorts",
       x = "Frail",
       y = "EOL",
       fill = "Number of Spells") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Patient characteristics ----

## Age and gender structure

aggregate_data_frail %>% 
  filter(year == 2023,
         age_range < 100) %>% 
  group_by(age_range, sex) %>% 
  summarise(pts = sum(person_n)) %>%
  mutate(prop = pts/sum(pts)*100) %>% 
  select(-pts) %>% 
  pivot_wider(id_cols = age_range,
              names_from = sex, 
              values_from = prop) %>%
  pivot_longer(-age_range) %>%
  mutate(name = 
           case_when(name == 1 ~ "Male",
                     TRUE ~ "Female")) %>%
  mutate(age_range_text = paste(age_range, "-", age_range+4)) %>% 
  
  ggplot(aes(x = value, y = age_range_text, fill = name)) +
  geom_col(position = "stack") +
  geom_vline(xintercept = 50) +
  scale_fill_SU() +
  labs(x = "Percent",
       y = "Age range",
       fill = "Sex",
       title = "Gender distribution in frail patient cohort by age range",
       subtitle = "SUS 2023")

# Gender distribution structure by ICB
aggregate_data_frail %>% 
  filter(year == 2023,
         age_range < 100) %>% 
  group_by(icb_name_short, sex) %>% 
  summarise(pts = sum(person_n)) %>%
  mutate(prop = pts/sum(pts)*100) %>% 
  select(-pts) %>% 
  pivot_wider(id_cols = icb_name_short,
              names_from = sex, 
              values_from = prop) %>%
  arrange(`2`) |> 
  ungroup() |> 
  mutate(rn = row_number()) |> 
  pivot_longer(-c(icb_name_short, rn)) %>%
  mutate(name = 
           case_when(name == 1 ~ "Male",
                     TRUE ~ "Female")) %>%
  drop_na(icb_name_short) %>%
  mutate(icb_name_clean = str_sub(icb_name_short, 10,100)) |> 
  mutate(icb_name_clean = str_remove_all(icb_name_clean, " ICB"))|> 
  
  ggplot(aes(x = value, y = reorder(icb_name_clean, rn), fill = name)) +
  geom_col(position = "stack") +
  geom_vline(xintercept = 50) +
  scale_fill_SU() +
  labs(x = "Percent",
       y = "ICB",
       fill = "Sex",
       title = "Gender distribution in frail patient cohort by ICB",
       subtitle = "SUS 2023")

# Age structure by ICB
age_icb_frail <-
  aggregate_data_frail %>% 
  filter(year == 2023,
         age_range < 100) %>% 
  group_by(icb_name_short, age_range) %>% 
  summarise(pts = sum(person_n)) %>%
  mutate(prop = pts/sum(pts)*100) %>% 
  select(-pts) %>% 
  pivot_wider(id_cols = icb_name_short,
              names_from = age_range, 
              values_from = prop) %>%
  pivot_longer(-icb_name_short) %>%
  drop_na(icb_name_short) %>%
  mutate(icb_name_clean = str_sub(icb_name_short, 10,100)) |> 
  mutate(icb_name_clean = str_remove_all(icb_name_clean, " ICB"))


age_icb_frail |> 
  left_join(age_icb_frail|>
              ungroup() |>
              filter(name == 90) |>
              arrange(value) |>
              mutate(rn = row_number()) |> 
              select(icb_name_clean, rn),
            by = "icb_name_clean"
            ) |> 
  mutate(name = factor(name, levels = c(95, 90, 85, 80, 75, 70))) |> 
  
  ggplot(aes(x = value, y = reorder(icb_name_clean, rn), fill = name)) +
  geom_col(position = "stack") +
  geom_vline(xintercept = 50) +
  scale_fill_SU() +
  #guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Percent",
       y = "ICB",
       fill = "Age-range",
       title = "Age range distribution in frail patient cohort by ICB",
       subtitle = "SUS 2023")


## Ethnicity
aggregate_data_frail %>% 
  filter(year == 2023,
         age_range < 100) %>% 
  mutate(ethnic_group = str_sub(ethnic_group, 1,1)) %>% 
  group_by(ethnic_group, sex) %>% 
  summarise(pts = sum(person_n)) %>%
  mutate(prop = pts/sum(pts)*100) %>% 
  #select(-pts) |> 
  left_join(ethnicity_lookup, by = c("ethnic_group" = "Code")) %>% 
  drop_na(Description) %>% 
  select(-pts, -ethnic_group) %>% 
  pivot_wider(id_cols = Description,
              names_from = sex, 
              values_from = prop) %>%
  pivot_longer(-Description) %>%
  mutate(name = 
           case_when(name == 1 ~ "Male",
                     TRUE ~ "Female")) %>%
  #mutate(age_range_text = paste(age_range, "-", age_range+4)) %>% 
  
  ggplot(aes(x = value, y = Description, fill = name)) +
  geom_col(position = "stack") +
  geom_vline(xintercept = 50) +
  scale_fill_SU() +
  labs(x = "Percent",
       y = "Ethnicity",
       fill = "Sex",
       title = "Gender distribution in frail patient cohort by ethnicity",
       subtitle = "SUS 2023")


## Ethnicity and age
aggregate_data_frail %>% 
  filter(year == 2023,
         age_range < 100) %>% 
  mutate(ethnic_group = str_sub(ethnic_group, 1,1)) %>% 
  group_by(ethnic_group, age_range) %>% 
  summarise(spells = sum(spells)) %>% 
  left_join(ethnicity_lookup, by = c("ethnic_group" = "Code")) %>% 
  mutate(prop = spells/sum(spells)*100) %>% 
  ungroup() %>% 
  drop_na(Description) %>% 
  select(-spells, -ethnic_group) %>%
  mutate(age_range = paste(age_range, "-", age_range+4)) %>%
  pivot_wider(id_cols = Description,
              names_from = age_range, 
              values_from = prop) %>%
  pivot_longer(-Description) %>%
  #mutate(name = case_when(name == 1 ~ "Male", TRUE ~ "Female")) %>%
  
  ggplot(aes(x = value, y = Description, fill = name)) +
  geom_col(position = "stack") +
  scale_fill_SU() +
  labs(x = "Percent",
       y = "Ethnicity",
       fill = "Age range",
       title = "Age distribution in frail patient cohort by ethnicity",
       subtitle = "SUS 2023")


# Patchwork: age-gender and ethnicity-deprivation

patchwork_function <- function(data, title_text) {
  
  # Plot 1
  plot_1 <-
    data %>% 
    filter(year == 2023,
           age_range < 100) %>% 
    group_by(age_range, sex) %>% 
    summarise(pts = sum(person_n)) |> 
    mutate(pts_2 = 
             case_when(sex == 1 ~ 0-pts,
                       TRUE ~ pts)) |> 
    mutate(sex = 
             case_when(sex == 1 ~ "Male",
                       TRUE ~ "Female")) %>%
    mutate(age_range_text = paste(age_range, "-", age_range+4)) |> 
    
    ggplot(aes(x = pts_2, y = age_range_text, fill = sex)) +
    geom_col() +
    geom_vline(xintercept = 0) +
    scale_fill_SU() +
    scale_x_continuous(labels = function(x) scales::comma(abs(x))) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(hjust = 0.5)
          ) +
    labs(x = "Patients",
         y = "Age-range",
         fill = "Sex:",
         subtitle = "Age and sex distribution",
         #subtitle = "2023"
         )
  
  
  # Plot 2
  ethnicity_imd <-
    data |> 
    filter(year == 2023) |>
    mutate(ethnic_group = str_sub(ethnic_group, 1,1)) %>% 
    left_join(ethnicity_lookup, by = c("ethnic_group" = "Code")) |> 
    group_by(Description, imd_decile) |> 
    summarise(person_n = sum(person_n)) |> 
    ungroup()
  
  # Reshape the data for the heatmap
  heatmap_data <- dcast(ethnicity_imd, Description  ~ imd_decile, value.var = "person_n")
  
  # Convert the data to a matrix
  heatmap_matrix <- as.matrix(heatmap_data[,-1])
  rownames(heatmap_matrix) <- heatmap_data$ethnic_group
  
  plot_2 <-
    ethnicity_imd |> 
    group_by(Description) |> 
    drop_na(Description) |> 
    mutate(prop = person_n/sum(person_n) * 100) |> 
    
    ggplot(aes(x = factor(imd_decile), y = Description, fill = prop)) +
    geom_tile(alpha = 0.9) +
    scale_fill_gradient(low = "#686f73", high = "#f9bf07") +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(hjust = 0.5)
          ) +
    labs(x = "IMD Decile",
         y = "Ethnic Group",
         subtitle = "IMD distribution in patient ethnicity groups",
         #subtitle = "Frail cohort 2023",
         fill = "Proportion (%)"
    ) 
  
  plot_1 + plot_2 +
    plot_annotation(title = paste0("Sub-cohort demographics - ", title_text),
                    subtitle = "Mitigable SUS admissions | 2023")
  
  
}


patchwork_function(aggregate_data_elderly_emergency, "Emergency elderly patients")
patchwork_function(aggregate_data_frail, "Frail patients")
patchwork_function(aggregate_data_falls, "Falls patients")
patchwork_function(aggregate_data_eol, "End-of-life patients")


# Top diagnoses by cohort

read_diagnosis_list <- function(cohort) {

  data <- 
    read_csv(paste0("top_diagnoses/", cohort, ".csv")) |> 
    clean_names() |> 
    select(4,2) |> 
    rename(Admissions = 2) |> 
    arrange(desc(Admissions)) |> 
    mutate(Proportion = round(Admissions/sum(Admissions)*100, 2)) |> 
    rename(`Primary diagnosis` = icd10_l4_desc) |> 
    head(10) |> 
    mutate(Admissions = scales::comma(Admissions))
  
  data
}

read_diagnosis_list("frail") 
read_diagnosis_list("emergency_elderly")
read_diagnosis_list("falls")
read_diagnosis_list("eol")
read_diagnosis_list("amb_chronic")
read_diagnosis_list("amb_acute")
read_diagnosis_list("amb_vacc_prev")
read_diagnosis_list("eol_broad")


# Top diagnoses by cohort

read_procedures_list <- function(cohort) {
  
  data <- 
    read_csv(paste0("top_procedures/", cohort, ".csv")) |> 
    clean_names() |> 
    select(4,2) |> 
    rename(Admissions = 2) |> 
    arrange(desc(Admissions)) |> 
    mutate(Proportion = round(Admissions/sum(Admissions)*100, 2)) |> 
    rename(`Primary procedure` = opcs_l4_desc) |> 
    head(10)
  
  data
}

read_procedures_list("frail")
read_procedures_list("emergency_elderly")
read_procedures_list("falls")
read_procedures_list("eol")
read_procedures_list("amb_chronic")
read_procedures_list("amb_acute")
read_procedures_list("amb_vacc_prev")
read_procedures_list("eol_broad")

