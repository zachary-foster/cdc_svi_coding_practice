# Imports
library(readr)
library(purrr)
library(dplyr)

# Read data
svi_data <- read_csv("data/cdc_svi_oregon_county_subset.csv")

# Identify metric of interest
metrics <- c("E_PCI", "EP_AGE65", "EP_AGE17", "EP_DISABL", "EP_SNGPNT", "EP_NOVEH")
high_is_bad <- c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
svi_data[metrics]

# Standardize metrics for comparison
standardize <- function(input, flip = FALSE) {
  output <- rank(input) / length(input)
  if (flip) {
    output <- 1 - output
  }
  return(output)
}
standardized_data <- map2_dfc(svi_data[metrics], ! high_is_bad, standardize)
standardized_data <- bind_cols(svi_data["COUNTY"], standardized_data)

# Consolidate metrics into one
standardized_data$summary <- rowSums(standardized_data[metrics])

# Sort results by vulnerability
standardized_data <- standardized_data[order(standardized_data$summary), ]

# Save the results
write_csv(standardized_data, file = "county_rankings.csv")
