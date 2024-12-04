library(tidyverse)

# Read in split data and write combined csv ----

# Initialize an empty list to store the data frames
data_list <- list()

# Loop through each part and read the CSV files into the list
for (i in 1:10) {
  file_name <- paste0("split_data/aggregate_data_part_", i, ".csv")
  data_list[[i]] <- read.csv(file_name)
}

# Combine all data frames into one
aggregate_data_recombined <- bind_rows(data_list)

# Save the recombined data frame to a new CSV file
write.csv(aggregate_data_recombined, "aggregate_data_recombined.csv", row.names = FALSE)