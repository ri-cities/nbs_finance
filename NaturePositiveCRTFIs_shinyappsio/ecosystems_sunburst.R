library(plotly)

## 3. Sunburst
#Useful information on formatting of sunburst in plotly: https://plotly.com/r/reference/sunburst/#sunburst-insidetextorientation 
#  ```{r, write function as.sunburstDF to transform data to format ready to plot sunburst}
# Write function to transform data into a sunburst-ready format
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

# To test the code in function step by step, run subsequent code chunk to create data_ecos, then define the inputs of the function (DT, value_column, add_root)
# DT <- data_ecos 
# value_column <-  "count"
# add_root <-  TRUE
# 
# remove(DT, value_column, add_root)

# {r, prepare the data_ecos for sunburst_ecos_..., part 1}
# 1a. Create a separate object for preprocessing (original data remains untouched)
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
## OPTION A: Add percentage of count of each ecos value of total count
# data_ecos$perc_of_all <-
#   round(data_ecos$count/sum(data_ecos$count)*100, 2)

# ## OPTION B: Remove percentage of count of each ecos value of total count
data_ecos$perc_of_all <- NULL


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


# [can be skipped] Ensure the correct number of colors for the chart. If there are less than 4 categories, the rest will be repeated.
# num_colors <- length(custom_colors)
# if (nrow(sunburstDF) < num_colors) {
#   custom_colors <- rep(custom_colors, length.out = nrow(sunburstDF))
# }


### a.i percentage (ecos_group & ecos_all)
#```{r, create sunburst_ecos_perc}
# Create sunburst_ecos_count with percentage as label
# sunburst_ecos_perc <- 
#   plot_ly(
#     data = sunburstDF,
#     ids = ~ids,
#     labels = ~labels,
#     parents = ~parents,
#     values = ~values,
#     type = 'sunburst',
#     branchvalues = 'total',
#     hoverinfo = 'text',
#     textinfo = 'label+percent parent', 
#     hovertext = ~hoverinfo, 
#     insidetextorientation = "radial",  # Adjust text orientation
#     # insidetextorientation = ~ifelse(1:nrow(sunburstDF) %in% 6:26, "radial", "horizontal"),  # Adjust text orientation
#     marker = list(
#       colors = custom_colors  # Apply the custom color scale
#     )
#   )
# 
# sunburst_ecos_perc


#```{r, save sunburst_ecos_colored_perc as htmlwidget}
# Save the sunburst chart with custom colors
# htmlwidgets::saveWidget(
#   as_widget(
#     plot_ly(
#       data = sunburstDF,
#       ids = ~ids,
#       labels = ~labels,
#       parents = ~parents,
#       values = ~values,
#       type = 'sunburst',
#       branchvalues = 'total',
#       hoverinfo = 'text',
#       textinfo = 'label+percent parent', 
#       hovertext = ~hoverinfo, 
#       insidetextorientation = "radial",  # Adjust text orientation
#       # insidetextorientation = ~ifelse(1:nrow(sunburstDF) %in% 6:26, "radial", "horizontal"),  # Adjust text orientation
#       marker = list(
#         colors = custom_colors  # Apply the custom color scale
#       )
#     )
#   ),
#   file = file.path(getwd(), "plots", "white", "evidence", "plot_ecos_sunburst", "sunburst_ecos_perc_of_allecos.html"), 
#   selfcontained = TRUE # save the HTML as a single self-contained file (with external resources base64 encoded), else it is saved as file with external resources placed in an adjacent directory
# )

