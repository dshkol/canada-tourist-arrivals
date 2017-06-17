library(tidyverse)

# Define some helper vectors for importing and reformating data
headers <- c("Market", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul","Aug", "Sep", "Oct", "Nov", "Dec", "Total")

regions <- c("AFRICA", "ASIA", "CARIBBEAN", "CENTRAL & OTHER NORTH AMERICA", "EUROPE", "OCEANIA", "SOUTH AMERICA", "United States")

# location of Arrivals excel file
excel_file <- "Arrivals at Frontiers of Tourists from Abroad (by month).xls"

# vector of available years as characters
# currently data extends to 1990 and data is current to March 2017, but let's use full years only for this 
years <- as.character(1990:2016)

# function to load, parse, and clean up that sheet
format_arrivals_sheet <- function(excel_file, sheet_year) {
  temp_sheet <- read_excel(excel_file, sheet = sheet_year, col_names = FALSE, skip = 4, trim_ws = TRUE)
  colnames(temp_sheet) <- headers
  
  temp_sheet <- temp_sheet %>% drop_na() %>% 
    mutate(Region = ifelse(Market %in% regions, Market, NA), Year = sheet_year) %>%
    fill(Region) %>%
    filter(!Market %in% regions | Market == "United States")
  
  temp_sheet$Region <- gsub("United States", "UNITED STATES", temp_sheet$Region)
  return(temp_sheet)
}

# Apply this function to every element of our year sheet name vector and return a df
arrivals <- map2_df(excel_file, years, format_arrivals_sheet)

# Let's worry about only annual data for now:
arrivals_annual <- arrivals %>% select(Region, Market, Year, Total)
arrivals_annual$Destination <- "Canada"
arrivals_annual$Total <- ifelse(arrivals_annual$Total == 0, 1, arrivals_annual$Total)

# excluding US
overseas_annual <- arrivals_annual %>% filter(!Market == "United States")

