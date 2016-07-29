library(readr)
library(dplyr)
library(tidyr)
library(shiny)
library(flexdashboard)
library(RColorBrewer)


# Grab our data
arrivals <- read_csv("canada-tourist-arrivals.csv")
arrivals$country <- enc2native(arrivals$country)

# Select data

sel_country <- c("France","Germany","Australia")
num_sel_country <- length(sel_country)


# DataTable
# library(DT)
# datatable(arrivals[,1:4])

# Time Series Data
library(xts)
library(dygraphs)

arrivals$ym <- paste0("20",arrivals$year,"-",arrivals$month)
arrivals_ts <- arrivals[,c(1,4,5)]

# Apply selected countries

arrivals_ts <- arrivals_ts[arrivals_ts$country %in% sel_country,]

# Reshape data into a wide format for dygraphs
arrivals_ts <- arrivals_ts %>%
  select(country, ym, arrivals) %>%
  spread(key = country, value = arrivals) %>%
  mutate(ym = as.yearmon(ym))

arrivals_xt <- xts(arrivals_ts[,-1], order.by =arrivals_ts$ym)

dygraph(arrivals_xt, main = "Tourist Arrivals by Country") %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dyAxis("y", label = "Arrivals") %>%
  dyLegend(width = 250 + (num_sel_country-1)*100) %>%
  dyOptions(colors = brewer.pal(num_selected, "Set1"),
            includeZero = TRUE,
            axisLineColor = "grey") %>%
  dyRangeSelector()



# Year Over Year performance


