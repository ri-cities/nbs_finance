# install & load packages
packages <- c('leaflet', 'plotly', 'DT', 'readxl', 'data.table')

## Function to install packages only if they are not already installed
install_if_not_installed <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}

## Call the function with your list of packages
install_if_not_installed(packages)


# Import data  ####

# Set working directory for the app
setwd("/Users/alina/R_Projects/Dashboards/NaturePositiveCRTFIs")

# Path to Excel file
# file_path <- "/Users/alina/R_Projects/Dashboards/NaturePositiveCRTFIs/Financing_NBS_for_climate_resilience_data_dashboard.xlsx"
file_path <- file.path(getwd(), "Financing_NBS_for_climate_resilience_data_dashboard.xlsx")

# Get all sheet names
sheet_names <- excel_sheets(file_path)

# "List of content" "Bibliography" "Inventory short" "Inventory long" "data_for_analysis_projects" "data_for_analysis_acad" "data_grey_for_saving"

# Import all sheets into a named list of data frames
data_list <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet)
})

# Assign sheet names as list names
names(data_list) <- sheet_names

#   Bibliography <- data_list$Bibliography
inventory_short <- data_list$`Inventory short`
#   inventory_long <- data_list$`Inventory long`
data_combined_p <- data_list$data_for_analysis_projects
#   data_for_analysis_acad <- data_list$data_for_analysis_acad
#   data_for_analysis_grey <- data_list$data_for_analysis_grey


# run ecosystems script
source(file.path(getwd(), "ecosystems_sunburst.R"))
# source(file.path(here::here(), "NaturePositiveCRTFIs/ecosystems.R"))
# knitr::knit(file.path(getwd(), "ecosystems_sunburst.Rmd")) # Run separate script

# run instruments script
source(file.path(getwd(), "instruments_sunburst.R"))

# run instruments script
# source(file.path(getwd(), "econ_appraisal.R"))

