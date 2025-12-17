# LOAD PACKAGES  ####
# install & load packages
# packages <- c('shiny', 'leaflet', 'DT', 'readxl', 'data.table', 
#               'ggplot2', 'stats', 'graphics') # used in plotly installation
# lapply(packages, library, character.only = TRUE)

library(shiny)
library(leaflet)
library(DT)
library(data.table)
library(ggplot2)  # used in plotly installation
library(stats)    # used in plotly installation
library(graphics) # used in plotly installation
library(plotly)
library(readxl)
library(geodata)
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(markdown)
library(rnaturalearth)

## Function to install packages only if they are not already installed
# install_if_not_installed <- function(packages) {
#   for (package in packages) {
#     if (!require(package, character.only = TRUE, quietly = TRUE)) {
#       install.packages(package, dependencies = TRUE)
#       library(package, character.only = TRUE)
#     }
#   }
# }
# 
# ## Call the function with your list of packages
# install_if_not_installed(packages)


# IMPORT DATA  ####

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
# source(file.path(getwd(), "ecosystems_sunburst.R"))
# source(file.path(here::here(), "NaturePositiveCRTFIs/ecosystems.R"))
knitr::knit(file.path(getwd(), "ecosystems_sunburst.Rmd")) # Run separate script

# run instruments script
# source(file.path(getwd(), "instruments_sunburst.R"))

# run instruments script
# source(file.path(getwd(), "econ_appraisal.R"))

# SUNBURST PLOTS ####
# Add Fig. description: 
# Title: Ecosystem types by ecosystem category. 
# Text: The panel illustrates the distribution of ecosystem types (outer circle), categorized by broader ecosystem categories (inner circle), that benefit from a nature-positive climate risk transfer and financing instrument (CRTFI) identified in the review. 
# Each mentioned nature-positive financial instrument and the ecosystem that it supports are counted. Multiple counts per publication are possible. Additionally, the same project may be counted multiple times if it appears in multiple publications. Sample size: projects in academic publications (n = 104) and non-academic publications (n = 209). 

# # 3. Ecosystems ####
# # ecosystem_sunburst.R 
# # Useful information on formatting of sunburst in plotly: https://plotly.com/r/reference/sunburst/#sunburst-insidetextorientation 
# # Write function to transform data into a sunburst-ready format
as.sunburstDF <- function(DF, value_column = NULL, add_root = FALSE, drop_na_nodes = TRUE) {
  colNamesDF <- names(DF)

  # Check if input is a data.table, if not, convert the input dataframe to a data.table
  if (is.data.table(DF)) {
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }

  if (add_root) {
    DT[, root := "total"]
  }

  colNamesDT <- names(DT) #   # Save column names of the data.table DT: "ecos_group"  "ecos_all"    "count"       "perc_of_all"
  hierarchy_columns <- setdiff(colNamesDT, value_column) #   # Determine which columns define the hierarchy (exclude the value column if provided)

  # Adjust the column order if a value column is specified
  if (!is.null(value_column) && !add_root) {
    setnames(DT, value_column, "values", skip_absent = TRUE)
    setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
  } else if (!is.null(value_column) && add_root) {
    setnames(DT, value_column, "values", skip_absent = TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
  }

  hierarchyList <- list()
  for (i in seq_along(hierarchy_columns)) {
    current_columns <- colNamesDT[1:i]

    if (is.null(value_column)) {
      currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by = current_columns, .SDcols = "values"]
    }

    setnames(currentDT, length(current_columns), "labels")
    currentDT[, depth := length(current_columns) - 1]
    hierarchyList[[i]] <- currentDT
  }

  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)

  if (drop_na_nodes) {
    hierarchyDT <- na.omit(hierarchyDT, cols = "labels")
    parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", "depth", value_column))
    hierarchyDT[, parents := apply(.SD, 1, function(x) {
      ifelse(all(is.na(x)), NA_character_, paste(x[!is.na(x)], collapse = " - "))
    }), .SDcols = parent_columns]
  } else {
    parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
    hierarchyDT[, parents := apply(.SD, 1, function(x) {
      ifelse(x["depth"] == "0", NA_character_, paste(x[seq(2, as.integer(x["depth"]) + 1)], collapse = " - "))
    }), .SDcols = parent_columns]
  }

  hierarchyDT[, ids := apply(.SD, 1, function(x) {
    paste(c(if (is.na(x["parents"])) NULL else x["parents"], x["labels"]), collapse = " - ")
  }), .SDcols = c("parents", "labels")]

  hierarchyDT[, union(setdiff(names(hierarchyDT), c("labels", "values", "parents", "ids")), "depth") := NULL]
  return(hierarchyDT)
}
# 
# # To test the code in function step by step, run subsequent code chunk to create data_ecos, then define the inputs of the function (DT, value_column, add_root)
# # DT <- data_ecos 
# # value_column <-  "count"
# # add_root <-  TRUE
# # 
# # remove(DT, value_column, add_root)
# 
# # {r, prepare the data_ecos for sunburst_ecos_..., part 1}
# # 1a. Create a separate object for preprocessing (original data remains untouched)
data_combined_p_ecos_long <- data_combined_p %>%
  select(ecos_group, ecos_all) %>%           # Filter relevant columns
  mutate(across(everything(), ~ strsplit(as.character(.), ";\\s*"))) %>% # Split multi-value cells
  tidyr:::unnest(cols = everything())                                    # Expand into separate rows

# 1b. Group the data at the most detailed level and count occurrences
data_ecos <- data_combined_p_ecos_long %>%
  group_by(ecos_group, ecos_all) %>%
  summarise(count = dplyr::n(), .groups = "drop") # count occurrences at the most detailed level

# 1c. Update variable names to shorten unspecified ecosystems
data_ecos$ecos_group <- gsub("Coastal/marine", "Coastal/ marine", data_ecos$ecos_group, fixed = TRUE) # "fixed = TRUE" ensures that the pattern is matched literally, rather than as a regular expression
data_ecos$ecos_all <- gsub("Unspecified coastal and marine ecosystems", "Unspecified", data_ecos$ecos_all)
data_ecos$ecos_all <- gsub("Ecosystem not specified", "Unclear", data_ecos$ecos_all)
data_ecos$ecos_all <- gsub("Sandy shores and dunes", "Sandy shores & dunes", data_ecos$ecos_all)
data_ecos$ecos_all <- gsub("Rivers and floodplains", "Rivers & floodplains", data_ecos$ecos_all)
data_ecos$ecos_all <- gsub("Urban / semi-urban", "Urban/ semi-urban", data_ecos$ecos_all, fixed = TRUE)


# 1d. Percentage
## [do not use] OPTION A: Add percentage of count of each ecos value of total count
# data_ecos$perc_of_all <-
#   round(data_ecos$count/sum(data_ecos$count)*100, 2)

# ## [use] OPTION B: Remove percentage of count of each ecos value of total count
data_ecos$perc_of_all <- NULL
# 
# 
# ```{r, prepare data for sunburst_ecos_..., part 2, add custom colors}
# Generate Sunburst Data
sunburstDF <- as.sunburstDF(data_ecos, value_column = "count", add_root = TRUE)

# Calculate percentage for all rows excluding those where parents is NA
sunburstDF[!is.na(parents), percentage := round((values / sum(values, na.rm = TRUE)) * 100, 2)]
# sum(sunburstDF$percentage, na.rm=TRUE) # 100 [checking step]

# Add hoverinfo
sunburstDF[!is.na(parents), hoverinfo := paste0(labels, "<br>", values #, " (", percentage, "%)"
)]

# Remove "total" from circle in the middle
sunburstDF[1, labels := "Ecosystems"]

# Add line breaks
## Define a function to wrap text based on max_length determined by percentage
wrap_labels <- function(text, percentage) {
  max_length <- ifelse(percentage >= 8, 6,
                       ifelse(percentage >= 3 & percentage < 8, 7,
                              ifelse(percentage >= 2.99 & percentage < 3, 6, # inland wetlands (2.99)
                                     ifelse(percentage >= 2 & percentage < 2.99, 13,
                                            ifelse(percentage >= 1.3 & percentage < 2, 18,
                                                   25))))) # else, max_length is 23
  # Wrap text
  sapply(seq_along(text), function(i) {
    paste(strwrap(text[i], width = max_length[i]), collapse = "\n")
  })
}

## Apply the wrapping function to rows 6 to 26
sunburstDF[2:26, labels := wrap_labels(labels, percentage)]

# Define the custom colors
custom_colors_ecos <-c("white", "#87B8B8",  "#9A6324", "#586F30", "#A99D8C")


# 4. Financial instruments ####
# Add Fig. description:
# Title: Nature-positive climate risk transfer and financing instruments (CRTFI)
# Text: Nature-positive CRTFI by instrument category. The panel presents the distribution of nature-positive CRTFI (outer circle), categorized by instrument type (inner circle). The share of each instrument in the outer circle reflects its prevalence in the instrument category.
# Multiple counts of the same project may occur if a project is described in multiple publications. Sample size: projects in academic publications (n = 104) and non-academic publications (n = 209).
# instruments_sunburst.R
# r, prepare the data_finance for sunburst_finance, incl all finance1 model categories except academic, part 1
# Create treemap using ggplot and treemapify for geom_treemap

# 1a. Create a separate object for preprocessing (original data remains untouched)
data_combined_p_finance_model_long <- data_combined_p %>%
  select(finance_model1, finance_model2, finance_model3) %>%           # Filter relevant columns
  mutate(across(everything(), ~ strsplit(as.character(.), ";\\s*"))) %>% # Split multi-value cells
  tidyr:::unnest(cols = everything()) %>%                                      # Expand into separate rows
  filter(finance_model3 != "not specified") %>%                           # Remove rows where finance_model3 is "not specified"
  filter(finance_model1 != "Academic links of NBS and insurance") # Select rows matching specific values of finance_model1

# 1b. Combine three instruments in one single category (finance_model3)
data_combined_p_finance_model_long$finance_model3 <-
  gsub("Insurance payouts for community asset-building programs", "Insurance payouts used to fund NBS", data_combined_p_finance_model_long$finance_model3, fixed = TRUE)
data_combined_p_finance_model_long$finance_model3 <-
  gsub("Property insurance coverage enhancements", "Insurance payouts used to fund NBS", data_combined_p_finance_model_long$finance_model3, fixed = TRUE)


# 1c. Group the data at the most detailed level and count occurrences
data_finance <- data_combined_p_finance_model_long %>%
  group_by(finance_model1, finance_model2, finance_model3) %>%
  summarise(count = dplyr::n(), .groups = "drop") # count occurrences at the most detailed level

# 1d.Add percentage of count of each finance_model3 value of total count
data_finance$perc_of_all <-
  round(data_finance$count/sum(data_finance$count)*100, 3)

# 1e. Remove the finance_model2 column
data_finance_13 <- data_finance %>%
  select(-finance_model2)

# 1e. Update variable names
data_finance_13$finance_model1 <- gsub("Debt instruments", "Debt", data_finance_13$finance_model1)
data_finance_13$finance_model1 <- gsub("Credit enhancement instruments", "Credit enhancement", data_finance_13$finance_model1)
data_finance_13$finance_model1 <- gsub("Performance-based instruments", "Performance-based", data_finance_13$finance_model1)
data_finance_13$finance_model1 <- gsub("Risk transfer instruments", "Risk transfer", data_finance_13$finance_model1)
data_finance_13$finance_model1 <- gsub("Agri-environmental schemes", "Agri-environmental", data_finance_13$finance_model1)
data_finance_13$finance_model1 <- gsub("Equity instruments: Actively managed funds", "Equity: Funds", data_finance_13$finance_model1)
data_finance_13$finance_model1 <- gsub("Equity: Funds", "Equity: Managed funds", data_finance_13$finance_model1)
data_finance_13$finance_model1 <- gsub("Market instruments: Resilience and carbon credits", "Market: Resilience credits", data_finance_13$finance_model1)
# data_finance_13$finance_model1 <- gsub("Intersection of debt & risk-transfer instruments", "Debt & risk-transfer", data_finance_13$finance_model1, fixed = TRUE)
# data_finance_13$finance_model1 <- gsub("Other instruments", "Other", data_finance_13$finance_model1)


data_finance_13$finance_model3 <- gsub("Asset-backed security for NBS project", "Asset-backed security", data_finance_13$finance_model3, fixed = TRUE)
data_finance_13$finance_model3 <- gsub("Micro-loan / credit line for NBS implementation or loan with nature conditions", "Micro-loan / credit line", data_finance_13$finance_model3, fixed = TRUE)
data_finance_13$finance_model3 <- gsub("Catastrophe bond with nature-based risk reduction conditions", "Catastrophe bond with nature condition", data_finance_13$finance_model3, fixed = TRUE)
data_finance_13$finance_model3 <- gsub("Agricultural fallowing agreement for temporary land conversion", "Fallowing agreement", data_finance_13$finance_model3, fixed = TRUE)
data_finance_13$finance_model3 <- gsub("Tax benefits for conservation agriculture", "Tax benefits", data_finance_13$finance_model3, fixed = TRUE)
data_finance_13$finance_model3 <- gsub("Catastrophe wrapper for debt instrument that funds NBS", "Catastrophe wrapper for debt instrument", data_finance_13$finance_model3, fixed = TRUE)
data_finance_13$finance_model3 <- gsub("Guarantee / fund for green bond defaulting and political risk insurance for green bond", "Guarantee / fund / political risk insurance for green bond defaulting", data_finance_13$finance_model3, fixed = TRUE)
# data_finance_13$finance_model3 <- gsub("Fund/ political risk insurance for green bond defaulting", "Guarantee / fund / political risk insurance for green bond defaulting", data_finance_13$finance_model3, fixed = TRUE) # not used
data_finance_13$finance_model3 <- gsub("Disaster risk reduction fund financed by a share of insurance premiums", "Disaster risk reduction fund financed by insurance", data_finance_13$finance_model3, fixed = TRUE)
data_finance_13$finance_model3 <- gsub("Construction all risks insurance for an NBS project", "Construction all risks insurance for NBS project", data_finance_13$finance_model3, fixed = TRUE)
data_finance_13$finance_model3 <- gsub("Upstream-downstream compensation fund / water fund", "Water/ Compensation fund", data_finance_13$finance_model3, fixed = TRUE)
data_finance_13$finance_model3 <- gsub("Forest carbon offset buffer pool to protect NBS and carbon credits against risks", "Forest carbon offset buffer pool", data_finance_13$finance_model3, fixed = TRUE)
data_finance_13$finance_model3 <- gsub("Insurance for dual-benefit resilience and carbon credits", "Insurance for resilience and carbon credits", data_finance_13$finance_model3, fixed = TRUE)
data_finance_13$finance_model3 <- gsub("PES in combination with other instruments", "PES combined with other instruments", data_finance_13$finance_model3, fixed = TRUE)
data_finance_13$finance_model3 <- gsub("Bundling agricultural insurance and NBS", "Bundling agricultural insurance & NBS", data_finance_13$finance_model3, fixed = TRUE)
data_finance_13$finance_model3 <- gsub("Insurance payouts for community asset-building programs", "Insurance payouts for NBS", data_finance_13$finance_model3, fixed = TRUE)
data_finance_13$finance_model3 <- gsub("NBS as a prerequisite for insurance subsidies / public funding", "NBS as a prerequisite for subsidies / assistance", data_finance_13$finance_model3, fixed = TRUE)
# data_finance_13$finance_model3 <- gsub("Catastrophe fund or bond for liabilities linked to prescribed fires", "Catastrophe fund/ bond for liabilities linked to prescribed fires", data_finance_13$finance_model3, fixed = TRUE)



# For correct line breaks
# data_finance_13$finance_model3 <- gsub("Nature-positive resilience insurance", "Nature-positive resilience insurance", data_finance_13$finance_model3, fixed = TRUE)


# 1f. Percentage
## OPTION A: Add percentage of count of each ecos value of total count
# data_finance_13$perc_of_all <-
#   round(data_finance_13$count/sum(data_finance_13$count)*100, 2)
#
# ## OPTION B: Remove percentage of count of each ecos value of total count
data_finance_13$perc_of_all <- NULL



# r, [should be skipped] write function as.sunburstDF for sunburst without white circle in the middle
# This function is to create dataframe without an inner circle for total value
# If this function is used, the custom_color needs to be adjusted: remove the first vector item ("white").

# Write function to transform data into a sunburst-ready format
as.sunburstDF <- function(DF, value_column = NULL, add_root = FALSE, drop_na_nodes = TRUE) {
  colNamesDF <- names(DF)

  if (is.data.table(DF)) {
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }

  if (add_root) {
    DT[, root := "total"]
  }

  colNamesDT <- names(DT)
  hierarchy_columns <- setdiff(colNamesDT, value_column)

  if (!is.null(value_column) && !add_root) {
    setnames(DT, value_column, "values", skip_absent = TRUE)
    setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
  } else if (!is.null(value_column) && add_root) {
    setnames(DT, value_column, "values", skip_absent = TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
  }

  hierarchyList <- list()
  for (i in seq_along(hierarchy_columns)) {
    current_columns <- colNamesDT[1:i]

    if (is.null(value_column)) {
      currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by = current_columns, .SDcols = "values"]
    }

    setnames(currentDT, length(current_columns), "labels")
    currentDT[, depth := length(current_columns) - 1]
    hierarchyList[[i]] <- currentDT
  }

  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)

  if (drop_na_nodes) {
    hierarchyDT <- na.omit(hierarchyDT, cols = "labels")
    parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", "depth", value_column))
    hierarchyDT[, parents := apply(.SD, 1, function(x) {
      ifelse(all(is.na(x)), NA_character_, paste(x[!is.na(x)], collapse = " - "))
    }), .SDcols = parent_columns]
  } else {
    parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
    hierarchyDT[, parents := apply(.SD, 1, function(x) {
      ifelse(x["depth"] == "0", NA_character_, paste(x[seq(2, as.integer(x["depth"]) + 1)], collapse = " - "))
    }), .SDcols = parent_columns]
  }

  hierarchyDT[, ids := apply(.SD, 1, function(x) {
    paste(c(if (is.na(x["parents"])) NULL else x["parents"], x["labels"]), collapse = " - ")
  }), .SDcols = c("parents", "labels")]

  hierarchyDT[, union(setdiff(names(hierarchyDT), c("labels", "values", "parents", "ids")), "depth") := NULL]
  return(hierarchyDT)
}

# r, prepare data for sunburst_finance_..., part 2
# Generate sunburst data with percentage as outer circle
## function as.sunburstDF() is defined in ecosystems section
sunburstDF_inst <- as.sunburstDF(data_finance_13, value_column = "count", add_root = TRUE)

# Calculate percentages for hovertext
# sunburstDF_inst[, percentage := round((values / sum(values)) * 100, 2)]

# Calculate percentage for all rows excluding those where parents is NA
sunburstDF_inst[!is.na(parents), percentage := round((values / sum(values, na.rm = TRUE)) * 100, 2)]
# sum(sunburstDF_inst$percentage, na.rm=TRUE) # 100 [checking step]

# Add hoverinfo
sunburstDF_inst[!is.na(parents), hoverinfo := paste0(labels, "<br>", values #, " (", percentage, "%)"
)]

# Remove "total" from circle in the middle
sunburstDF_inst[1, labels := "Climate risk \ntransfer & financing \ninstruments"]

# Add line breaks
## Define a function to wrap text based on max_length determined by percentage
wrap_labels <- function(text, percentage) {
  max_length <- ifelse(percentage >= 8, 6,
                       ifelse(percentage >= 3 & percentage < 8, 7,
                              ifelse(percentage >= 2 & percentage < 3, 17,
                                     ifelse(percentage >= 1 & percentage < 2, 23,
                                            40)))) # else, max_length is 23
  # Wrap text
  sapply(seq_along(text), function(i) {
    paste(strwrap(text[i], width = max_length[i]), collapse = "\n")
  })
}

## Apply the wrapping function to rows 6 to 26
sunburstDF_inst[8:40, labels := wrap_labels(labels, percentage)]

## For correct line breaks
sunburstDF_inst$labels <- gsub("Resilience bond", "Resilience \nbond", sunburstDF_inst$labels, fixed = TRUE)
sunburstDF_inst$labels <- gsub("Debt-for-nature\nswap", "Debt-for- \nnature swap", sunburstDF_inst$labels, fixed = TRUE)
sunburstDF_inst$labels <- gsub("Environmental impact\nbond", "Environmental \nimpact bond", sunburstDF_inst$labels, fixed = TRUE)
sunburstDF_inst$labels <- gsub("Partial or full credit guarantee for\nNBS project",
                               "Partial or full credit guarantee \nfor NBS project",
                               sunburstDF_inst$labels, fixed = FALSE)
sunburstDF_inst$labels <- gsub("Construction all risks insurance for\nNBS project",
                               "Construction all risks insurance \nfor NBS project",
                               sunburstDF_inst$labels, fixed = FALSE)




# Define the custom colors
custom_colors_instruments <- c("white", "#9BB9D1","#89023E" ,"#DB7F67","#BC9CB0", "#99582A", "#A3BBAD", "#0F7173") # with 7 categories

