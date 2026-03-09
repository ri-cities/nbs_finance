# Load packages
library(leaflet)
library(plotly)
library(DT)
library(readxl)
library(data.table)
library(markdown)
library(rnaturalearth)
library(rnaturalearthdata)
library(munsell)


# Import data  ####

# Set working directory for the app only if the app should be run locally
# setwd("/Users/alina/R_Projects/Dashboards/NaturePositiveCRTFIs")

# Path to Excel file
# file_path <- file.path(getwd(), "Financing_NBS_for_climate_resilience_data_dashboard.xlsx")
file_path <- "Financing_NBS_for_climate_resilience_data_dashboard.xlsx"

# Get all sheet names
sheet_names <- excel_sheets(file_path)

# "List of content" "Bibliography" "Inventory short" "Inventory long" "data_for_analysis_projects" "data_for_analysis_acad" "data_grey_for_saving"

# Import all sheets into a named list of data frames
data_list <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
})

# Assign sheet names as list names
names(data_list) <- sheet_names

inventory_short <- data_list$`Inventory short`
data_combined_p <- data_list$data_for_analysis_projects

# The following sheets included in the excel file were not used in the app: 
#   Bibliography <- data_list$Bibliography
#   inventory_long <- data_list$`Inventory long`
#   data_for_analysis_acad <- data_list$data_for_analysis_acad
#   data_for_analysis_grey <- data_list$data_for_analysis_grey


# Run ecosystems script
source("ecosystems_sunburst.R")
# source(file.path(getwd(), "ecosystems_sunburst.R"))
# source(file.path(here::here(), "NaturePositiveCRTFIs/ecosystems.R"))
# knitr::knit(file.path(getwd(), "ecosystems_sunburst.Rmd")) # Run separate script

# Run instruments script
source("instruments_sunburst.R")
# source(file.path(getwd(), "instruments_sunburst.R"))

# Run instruments script
# source(file.path(getwd(), "econ_appraisal.R"))

