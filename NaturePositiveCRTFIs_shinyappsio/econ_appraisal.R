# BENEFITS
## 1. Ecosystem services
# ```{r, create data_combined_p_ES for plotting}
# The variable data_combined_p$ES_all has been created above in data cleaning section. 

# 1a. Create data_combined_p_ES
data_combined_p_ES <- data_combined_p %>%
  mutate(ES_split = str_split(ES_all, ";\\s*")) %>% 
  tidyr:::unnest(ES_split) %>% 
  count(ES_split, source) %>%
  rename(ES_all = ES_split,
         count = n)

# 1b. Rename cells NA to be "No co-benefits mentioned"
data_combined_p_ES <- data_combined_p_ES %>%
  mutate(ES_all = if_else(is.na(ES_all), "No co-benefits mentioned", ES_all))

# 2. Add columns 
# 2a. Calculate source_count as the total count per source (academic and non-academic)
data_combined_p_ES <- data_combined_p_ES %>%
  group_by(source) %>%
  mutate(source_count = sum(count)) %>%  # Total count grouped by source (academic vs non-academic)
  ungroup()

# Note: source_count of academic and non-academic sources does not add up to nrow(data_combined_p) = 313, because some projects mention multiple ES, therefore the number is higher (347+204 = 374)

# 2b.part1 Add percentage of evidence by ES_all (publications in the case of acad literature + projects in the case of grey literature) that mentions this ES
data_combined_p_ES <- data_combined_p_ES %>%
  group_by(ES_all) %>%
  mutate(
    count_by_ES_total = sum(count),  # Sum counts across sources for each ES_all
    # Given that ES_all can contain each ES_all only once in one cell, the next line can be calculated directly (without needed ..._single)
    perc_of_allprojects_by_ES = round(count_by_ES_total / nrow(data_combined_p) * 100, 3) 
  ) %>%
  ungroup()  # Remove grouping for further operations

## Note: perc_of_allprojects_by_ES does not sum up to 100% because several projects mention multiple ES
sum(data_combined_p_ES$perc_of_allprojects_by_ES)/2 # 175.2395

# 2b.part2 Ensure that perc_of_allprojects_by_ES is only shown for "non-academic" rows, and NA for "academic"
## some ES exist only for non-academic, no ES exist only for academic
data_combined_p_ES <- data_combined_p_ES %>%
  mutate(perc_of_allprojects_by_ES = if_else(source == "academic", NA_real_, perc_of_allprojects_by_ES))

# 2c. Add perc_of_allES_by_ES column 
total_count <- sum(data_combined_p_ES$count)
data_combined_p_ES$perc_of_allES_by_ES <- data_combined_p_ES$count_by_ES_total / total_count*100  # Calculate total percentage for each ES
remove(total_count)

## Note: Sum of perc_of_allES_by_ES for unique ES adds up to 100%
data_combined_p_ES %>%
  distinct(ES_all, .keep_all = TRUE) %>%                # Filter unique rows by ES_all
  summarise(total_perc = sum(perc_of_allES_by_ES)) %>%  # Sum perc_of_allES_by_ES
  pull(total_perc)                                          # Extract the value


# 2d. Add percentage that an ES represents of all ES found (only one ES per project can exist) 
## Statements: "___% of all ES found in the evidence are provisioning services" = "provisioning services accounts for ___ % of all ecosystem services"
data_combined_p_ES <- data_combined_p_ES %>%
  mutate(
    perc_of_allES_by_source = 
      round(count / source_count * 100, 3))


# 2e. Add percentage that an ES represents of all projects (within same source group) 
## Statements: "___% of projects found in the evidence consider reefs" = "reefs are mentioned in ____ of the identified projects/sources of evidence"
data_combined_p_ES <- data_combined_p_ES %>%
  mutate(
    perc_of_allprojects_by_source = case_when(
      source == "academic" ~ round(count / nrow(data_acad_p) * 100, 3),
      source == "non-academic" ~ round(count / nrow(data_grey_p) * 100, 3),
      TRUE ~ NA_real_  # Handle any unexpected cases (optional)
    )
  )

## Note: Does not sum up to 100% because several projects mention multiple ES 
sum(data_combined_p_ES$perc_of_allprojects_by_source)/2 # 159.024


# 3. Ensure correct order of rows for plotting
## 3a. Determine the order of ecosystem services based on the count for "non-academic"
## Non-academic is selected because some ES only exist in non-academic data, but not in academic data. 
## All ES in academic data also exist in non-academic data
order_ES_all <- data_combined_p_ES %>%
  filter(source == "non-academic") %>%
  arrange(count_by_ES_total) %>%
  pull(ES_all)

## 3b. Move "No co-benefits mentioned" to the first position in the order_ES_all vector
order_ES_all <- c("No co-benefits mentioned", setdiff(order_ES_all, "No co-benefits mentioned"))

## 3c. Convert to a factor for ordered plotting
data_combined_p_ES <- data_combined_p_ES %>%
  mutate(ES_all = factor(ES_all, levels = order_ES_all))

## 3d. Update source as a factor for ordered plotting
data_combined_p_ES <- data_combined_p_ES %>%
  mutate(source = factor(source, levels = rev(c("academic", "non-academic"))))


# 4. Remove objects
remove(order_ES_all)
```

### a. Percentage
```{r, create plot_ES_all_perc_of_allES_long}
# Plot with horizontal bars in long format (bars grouped on x-axis)
for (format in names(plot_format)) {
  plot_ES_all_perc_long <- 
    data_combined_p_ES %>%
    ggplot(aes(x = ES_all, y = perc_of_allES_by_source, fill = source)) +
    geom_bar(stat = "identity", 
             position = position_dodge2(width = 0.8, preserve = "single"),  # Ensure that bars without academic publications have the same width
             width = 0.8
             # color = "white"
    ) +  # 
    scale_x_discrete(expand = expansion(add = c(0.6, 0))) +  # Increase space between categories (x-axis)
    scale_y_continuous(expand = c(0.01, 0), limits = c(0, 25)) +  # No padding on x-axis
    labs(
      title = "Ecosystem services",
      subtitle = "All mentioned co-benefits are counted. Multiple counts per publication are possible.", 
      x = "",
      y = "Percentage of all co-benefits",
      fill = "Database"
    )+
    scale_fill_manual(values = c("academic" = green1, "non-academic" = "lightsalmon1")) +  # Custom colors
    plot_format[[format]]$theme +  # Use the specified theme
    guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the order of legend items
    theme(panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(), # Remove minor grid lines+
          plot.title = element_text(hjust = 0, size = 12, margin = margin(b = 5)),  # Reduced bottom margin
          plot.subtitle = element_text(hjust = 0, size = 10, margin = margin(t = 2, b = 10)),  # Adjust margins
          plot.title.position = "plot",  # Align title to the leftmost edge of the plot
          axis.text.y = element_text(size = 9, margin = margin(r = 5))  # Adjust axis spacing
    )+
    coord_flip() 
  
  # Show plot
  print(plot_ES_all_perc_long)
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_ES_all_perc_of_allES_long.png"),
         plot = plot_ES_all_perc_long, width = 7, height = 4, dpi = 300)
}

# Remove  objects
remove(plot_ES_all_perc_long)
```

```{r, create plot_ES_all_perc_of_allprojects_long}
# Plot with horizontal bars in long format (bars grouped on x-axis)
for (format in names(plot_format)) {
  plot_ES_all_perc_long <- 
    data_combined_p_ES %>%
    ggplot(aes(x = ES_all, y = perc_of_allprojects_by_source, fill = source)) +
    geom_bar(stat = "identity", 
             position = position_dodge2(width = 0.8, preserve = "single"),  # Ensure that bars without academic publications have the same width
             width = 0.8
             # color = "white"
    ) +  # 
    scale_x_discrete(expand = expansion(add = c(0.6, 0))) +  # Increase space between categories (x-axis)
    scale_y_continuous(expand = c(0.01, 0), limits = c(0, 50)) +  # No padding on x-axis
    labs(
      title = "Ecosystem services",
      subtitle = "All mentioned co-benefits are counted. Multiple counts are possible in non-academic publications.", 
      x = "",
      y = "Percentage of projects that consider the co-benefit",
      fill = "Database"
    )+
    scale_fill_manual(values = c("academic" = green1, "non-academic" = "lightsalmon1")) +  # Custom colors
    plot_format[[format]]$theme +  # Use the specified theme
    guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the order of legend items
    theme(panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(), # Remove minor grid lines+
          plot.title = element_text(hjust = 0, size = 12, margin = margin(b = 5)),  # Reduced bottom margin
          plot.subtitle = element_text(hjust = 0, size = 10, margin = margin(t = 2, b = 10)),  # Adjust margins
          plot.title.position = "plot",  # Align title to the leftmost edge of the plot
          axis.text.y = element_text(size = 9, margin = margin(r = 5))  # Adjust axis spacing
    )+
    coord_flip() 
  
  # Show plot
  print(plot_ES_all_perc_long)
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_ES_all_perc_of_allprojects_long.png"),
         plot = plot_ES_all_perc_long, width = 7, height = 4, dpi = 300)
}

# Remove  objects
remove(plot_ES_all_perc_long)
```

### b. Count
```{r, create plot_ES_all_count_perc_of_allprojects_by_ES}
for (format in names(plot_format)) {
  plot_ES_all_count_perc <- 
    ggplot(data_combined_p_ES, 
           aes(x = count, y = ES_all, fill = source)) +
    geom_bar(stat = "identity", position = "stack", color = "white", width=1) +  # Use identity for precomputed counts and white lines
    scale_x_continuous(expand = c(0.01, 0), limits = c(0, 138), breaks=seq(0, 138, by=25)) +  # No padding on x-axis
    labs(    title = "Ecosystem services",
             subtitle = "All mentioned co-benefits are counted. Multiple counts are possible in non-academic publications.", 
             x = "Count", y = "") +
    scale_fill_manual(values = c("academic" = green1, "non-academic" = "lightsalmon1")) +  # Custom colors
    plot_format[[format]]$theme +  # Use the specified theme
    guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the order of legend items
    scale_y_discrete(labels = function(x) str_wrap(x, width = 70)) +  # Wrap x-axis labels after 25 characters
    # Increase space between y-axis ticks and labels
    theme(panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          axis.text.y = element_text(size=9), 
          plot.title = element_text(hjust = 0, size = 12, margin = margin(b = 5)),  # Reduced bottom margin
          plot.subtitle = element_text(hjust = 0, size = 10, margin = margin(t = 2, b = 10)),  # Adjust margins
          plot.title.position = "plot"  # Align title to the leftmost edge of the plot
    )+
    geom_hline(yintercept = 0, color = "black") +  # Add a horizontal line at y = 0
    guides(fill = guide_legend(title = "Database", reverse = TRUE))+  # Adjust legend title; Reverse the order of legend items
    
    # Add percentage at position x+2 (right of the bar)
    geom_text(aes(x= count_by_ES_total+2, label = scales::percent(perc_of_allprojects_by_ES/100, accuracy = 0.1)), 
              hjust=0,  # Position the label at the height of the bar
              color = "black", size = 3)  # Label formatting
  
  
  # Show plot
  print(plot_ES_all_count_perc)
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_ES_all_count_perc_of_allprojects_by_ES.png"),
         plot = plot_ES_all_count_perc, width = 8, height = 3, dpi = 300)
}

# Remove the plot
remove(plot_ES_all_count_perc, data_combined_p_ES)
```

```{r, [not used] create plot_ES_all_count_long}
# 3. Plot with horizontal bars in long format (bars grouped on x-axis)
for (format in names(plot_format)) {
  plot_ES_all_count_long <- 
    data_combined_p_ES %>%
    ggplot(aes(x = ES_all, y = count, fill = source)) +
    geom_bar(stat = "identity", position = "dodge", color = "white") +  # Use identity for precomputed counts and white lines
    scale_x_discrete(expand = expansion(add = c(0.6, 0))) +  # Increase space between categories (x-axis)
    scale_y_continuous(expand = c(0.01, 0), limits = c(0, 80)) +  # No padding on x-axis
    labs(
      title = "Ecosystem services",
      subtitle = "All mentioned co-benefits are counted. Multiple counts per publication are possible.", 
      x = "",
      y = "Count",
      fill = "Database"
    )+
    scale_fill_manual(values = c("academic" = green1, "non-academic" = "lightsalmon1")) +  # Custom colors
    plot_format[[format]]$theme +  # Use the specified theme
    guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the order of legend items
    theme(panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(), # Remove minor grid lines+
          plot.title = element_text(hjust = 0, size = 12, margin = margin(b = 5)),  # Reduced bottom margin
          plot.subtitle = element_text(hjust = 0, size = 10, margin = margin(t = 2, b = 10)),  # Adjust margins
          plot.title.position = "plot"  # Align title to the leftmost edge of the plot
    )+
    coord_flip() 
  
  # Show plot
  print(plot_ES_all_count_long)
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_ES_all_count_long.png"),
         plot = plot_ES_all_count_long, width = 7, height = 4, dpi = 300)
}

# Remove  objects
remove(plot_ES_all_count_long)
```

```{r, [not used] create plot_ES_all_count}
for (format in names(plot_format)) {
  plot_ES_all_count <- 
    ggplot(data_combined_p_ES, 
           aes(x = count, y = ES_all, fill = source)) +
    geom_bar(stat = "identity", position = "stack", color = "white", width=1) +  # Use identity for precomputed counts and white lines
    scale_x_continuous(expand = c(0.01, 0), limits = c(0, 150), breaks=seq(0,150, by=25)) +  # No padding on x-axis
    labs(title = "Ecosystem services",
         subtitle = "All mentioned co-benefits are counted. Multiple counts per publication are possible.", 
         x = "Count", y = "") +
    scale_fill_manual(values = c("academic" = green1, "non-academic" = "lightsalmon1")) +  # Custom colors
    plot_format[[format]]$theme +  # Use the specified theme
    guides(fill = guide_legend(reverse = TRUE)) +  # Reverse the order of legend items
    scale_y_discrete(labels = function(x) str_wrap(x, width = 70)) +  # Wrap x-axis labels after 25 characters
    # Increase space between y-axis ticks and labels
    theme(panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          axis.text.y = element_text(size=9), 
          plot.title = element_text(hjust = 0, size = 12, margin = margin(b = 5)),  # Reduced bottom margin
          plot.subtitle = element_text(hjust = 0, size = 10, margin = margin(t = 2, b = 10)),  # Adjust margins
          plot.title.position = "plot"  # Align title to the leftmost edge of the plot
    )+
    geom_hline(yintercept = 0, color = "black") +  # Add a horizontal line at y = 0
    guides(fill = guide_legend(title = "Database", reverse = TRUE))  # Adjust legend title; Reverse the order of legend items
  
  # Show plot
  print(plot_ES_all_count)
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_ES_all_count.png"),
         plot = plot_ES_all_count , width = 8, height = 3, dpi = 300)
}

# Remove the plot
remove(plot_ES_all_count)
```


## 2. Quantification of hazard regulation 
### 2.1 Comparison of acad and non-acad publications
```{r, prepare dataframe for plot_hazard_regulation}
# Check unique values of author gender
# unique(data_combined$author_gender)

# 1. Add source_label column
data_combined_p_hazard_regulation <- data_combined_p %>%
  select(hazard_regulation, source) %>%
  mutate(source_label = case_when(
    source == "academic" ~ "Projects in academic publications",
    source == "non-academic" ~ "Projects in non-academic publications",
    TRUE ~ NA_character_  # Add a default case if there are other source values
  )
  )

# 2a. Group and calculate counts and percentages
data_combined_p_hazard_regulation <- data_combined_p_hazard_regulation %>%
  group_by(source_label, source, hazard_regulation) %>%
  summarise(hazard_regulation_count = n(), .groups = "drop") %>%
  group_by(source_label) %>%
  mutate(total_count = sum(hazard_regulation_count),
         percentage = (hazard_regulation_count / total_count) * 100) %>%
  ungroup()

# 2b. Create the labels for percentages > 20%
data_combined_p_hazard_regulation <- data_combined_p_hazard_regulation %>%
  mutate(label = ifelse(percentage > 9, hazard_regulation_count, NA))

# 3. Ensure `hazard_regulation` is a factor with the desired levels
## 3a. Create the vector of categories ordered 
order_hazard_regulation <- c("monetary valuation", "mixed quantitative & monetary valuation", "mixed qualitative & monetary valuation", "quantitative description", "mixed qualitative & quantitative description", "qualitative description", "brief mention", "no description")

# Decided to order by degree of quantification rather than by count
# order_hazard_regulation <- names(sort(table(data_combined_p_hazard_regulation$count), decreasing = FALSE))


## 3b. Convert hazard_regulation to factor
data_combined_p_hazard_regulation <- data_combined_p_hazard_regulation %>%
  mutate(hazard_regulation = factor(hazard_regulation, levels = 
                                      order_hazard_regulation))

# 4. Add columns cum_perc and x_axis_bar_center
data_combined_p_hazard_regulation <- data_combined_p_hazard_regulation %>%
  group_by(source_label) %>%
  arrange(desc(hazard_regulation), .by_group = TRUE) %>%  # Reverse the order of hazard_regulation
  mutate(cum_perc = cumsum(percentage),
         x_axis_bar_center = cum_perc - 0.5 * percentage) %>%
  ungroup()


# 5. Update source_label format to include total count
data_combined_p_hazard_regulation <- data_combined_p_hazard_regulation %>%
  mutate(source_label = paste0(source_label, "\n (n = ", total_count, ")"))

# 6. Add hazard_regulation_label with wrapped text of hazard_regulation
data_combined_p_hazard_regulation <- data_combined_p_hazard_regulation %>%
  mutate(hazard_regulation_label = str_wrap(hazard_regulation, width = 20))

# 7. Add source_label_short column 
## 7a. Create text for source_label_short
data_combined_p_hazard_regulation <- data_combined_p_hazard_regulation %>%
  mutate(source_label_short = case_when(
    source == "academic" ~ "Academic publications",
    source == "non-academic" ~ "Non-academic publications",
    TRUE ~ NA_character_  # Add a default case if there are other source values
  )
  )

## 7b. Add total count to source_label_short text
data_combined_p_hazard_regulation <- data_combined_p_hazard_regulation %>%
  mutate(source_label_short = paste0(source_label_short, "\n (n = ", total_count, ")"))


# 8. Check levels
levels(data_combined_p_hazard_regulation$hazard_regulation)

# 9. Define colors
## 9.1 Define color vector
# custom_colors8 <- rev(c("#63957A","#8ABD6F","#9BC19F", "turquoise4"  ,"#6AB9CB","#467986","deepskyblue3","skyblue3"))
# custom_colors8 <- c("#7C967F","#9BC19F", "#C7DAC9","paleturquoise3","#6AB9CB","#467986","deepskyblue3","skyblue3")
custom_colors8 <- c("#a3b5a5","#9BC19F", "#C7DAC9","paleturquoise3","slategray2", "lightskyblue3","#6AB9CB","deepskyblue3","skyblue3")

# custom_colors6_plus2 <- rev(c("#9BC19F","slategray2","#C3DAC3","lightskyblue3","#C1DDAD", "#A0CB88","#9BCBD1", "lightgrey", "darkgrey"))


## 9.2 Define the color mapping based on the unique `geographic_scale` levels
color_mapping <- setNames(custom_colors8, c("no description", 
                                            "brief mention", 
                                            "qualitative description", 
                                            "mixed qualitative & quantitative description", 
                                            "quantitative description", 
                                            "mixed quantitative & monetary valuation", 
                                            "monetary valuation", 
                                            "mixed qualitative & monetary valuation"))


# 10. Remove objects
remove(order_hazard_regulation, custom_colors8)
```

```{r, create plot_hazard_regulation_v1}
# Create stacked bar plot
for (format in names(plot_format)) {
  plot_hazard_regulation <- 
    ggplot(data_combined_p_hazard_regulation, 
           aes(x = source_label, y = percentage, fill = hazard_regulation)) +
    geom_bar(stat = "identity", position = "stack") +
    # coord_flip() +  # Horizontal orientation
    scale_y_continuous(labels = function(x) paste0(round(x, 0), "%")) +  # Round percentages to integers and add % sign
    scale_fill_manual(values = color_mapping) +  
    guides(fill = "none")+ # don't show fill guides
    ggtitle("Quantification of hazard regulating services of NBS") +  
    plot_format[[format]]$theme +  # Use the specified theme
    theme(
      plot.title.position = "plot", # Position title in the plot area
      plot.title = element_text(hjust = 0.025, size = 12), 
      axis.text.x = element_text(size = 8),  
      axis.text.y = element_text(size = 8, margin = margin(r = 5)),  # Adjust axis spacing
      axis.title.x = element_text(size = 9)
    ) +  # Align title to the left with reasonable hjust value
    labs(x = "", y = "", fill = "Gender")+
    # Add text labels only for percentage > 9
    geom_text(aes(label = ifelse(percentage > 7, paste0(hazard_regulation, " (",round(percentage, 0), "%)"), NA)), 
              position = position_stack(vjust = 0.5), na.rm = TRUE, 
              size = 2.1, color = "black")  
  
  
  plot_hazard_regulation
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_hazard_regulation_v1.png"), 
         plot = plot_hazard_regulation, width = 6, height = 3, dpi = 300)
}
```

```{r, create plot_hazard_regulation_v2}
for (format in names(plot_format)) {
  
  # Plot with labels inside & outside of stacked bars 
  plot_hazard_regulation <- ggplot(data_combined_p_hazard_regulation, 
                                   aes(x = source_label, y = percentage, fill = hazard_regulation)) +
    
    # Add geom_text_repel with numeric 'source_label_num' for nudge_x adjustment
    geom_text_repel(
      data = subset(data_combined_p_hazard_regulation, 
                    source_label == "Projects in non-academic publications\n (n = 209)" & percentage <= 5.5),
      aes(x = as.numeric(factor(source_label))+1.45,  # Using numeric 'source_label_num' for label position
          y = rev(x_axis_bar_center),
          label = rev(paste0(hazard_regulation_label, " (", round(percentage, 0), "%)"))),
      size = 2.2,
      nudge_x = 0.5,  # Adjust the horizontal nudging
      hjust = 0,      # Align labels to the left
      direction = "y",
      segment.size = .3,
      segment.alpha = .3,
      segment.linetype = "dotted",
      box.padding = 0.3,
      max.overlaps = 5,
      force_pull = TRUE,
      min.segment.length = 0.5
    ) +
    
    # Bar plot
    geom_bar(stat = "identity", position = "stack") +
    
    # Customize axes and labels
    scale_y_continuous(labels = function(x) paste0(round(x, 0), "%")) +  
    scale_x_discrete() +  # Use discrete scale for 'source_label'
    scale_fill_manual(values = color_mapping) +  
    guides(fill = "none")+ # don't show fill guides (legend)
    ggtitle("Quantification of hazard regulating services of NBS") +  
    labs(x = "", y = "")+
    plot_format[[format]]$theme +  
    theme(
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.025, size = 12), 
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8, margin = margin(r = 5)),
      axis.title.x = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    ) + 
    # Add labels for percentage > 5.5
    geom_text(aes(
      label = ifelse(percentage > 5.5, paste0(hazard_regulation, " (",round(percentage, 0), "%)"), NA)), 
      position = position_stack(vjust = 0.5), na.rm = TRUE, 
      size = 2.2, color = "black")
  
  # Show the plot
  plot_hazard_regulation
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_hazard_regulation_v2.png"), 
         plot = plot_hazard_regulation, width = 8, height = 4, dpi = 300)
}
```

```{r, create plot_hazard_regulation_v3}
# Create plot
for (format in names(plot_format)) {
  
  # Plot with labels inside & outside of stacked bars 
  plot_hazard_regulation <- ggplot(data_combined_p_hazard_regulation, 
                                   aes(x = source_label_short, y = percentage, fill = hazard_regulation)) +
    
    # Bar plot
    geom_bar(stat = "identity", position = "stack") +
    
    # Customize axes and labels
    scale_y_continuous(labels = function(x) paste0(round(x, 0), "%")) +  
    scale_x_discrete() +  # Use discrete scale for 'source_label_short'
    scale_fill_manual(values = color_mapping
    ) +  # Reverse color order in legend
    # guides(fill = hazard_regulation)+ # don't show fill guides (legend)
    ggtitle("Quantification of hazard regulating services of NBS") +  
    labs(x = "", y = "", fill="Evaluation of hazard regulation services")+
    guides(
      fill = guide_legend(
        override.aes = list(label = "", size = 3), # Control the fill aesthetics
        reverse = FALSE, 
        order = 1, 
        keywidth = 0.8, # Set the width of the legend keys
        keyheight = 0.8 # Set the height of the legend keys
      ))+
    plot_format[[format]]$theme +  
    theme(
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.025, size = 12), 
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8, margin = margin(r = 5)),
      axis.title.x = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    ) +
    # Add labels for percentage > 5
    geom_text(aes(
      label = ifelse(percentage > 3.8, paste0(round(percentage, 0), "%"), NA)), 
      position = position_stack(vjust = 0.5), na.rm = TRUE, 
      size = 2.2, color = "black")
  
  # Show the plot
  plot_hazard_regulation
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_hazard_regulation_v3.png"), 
         plot = plot_hazard_regulation, width = 8, height = 4, dpi = 300)
}
```

```{r, [not used] create plot_hazard_regulation_v4}
# Create stacked bar plot
for (format in names(plot_format)) {
  plot_hazard_regulation <- 
    ggplot(data_combined_p_hazard_regulation, 
           aes(x = reorder(source_label, -total_count), y = percentage, fill = hazard_regulation)) +
    # Add geom_text_repel for labels outside the bar, only for Academic publications
    geom_text_repel(
      data = subset(data_combined_p_hazard_regulation, 
                    source_label == "Projects in academic publications\n (n = 104)"),
      aes(x = source_label, 
          y = rev(x_axis_bar_center), 
          label = rev(hazard_regulation_label)), 
      size = 2, 
      # position = position_stack(vjust = 1),           # specify stacked position to follow bar layer's default position
      nudge_x = 2,  # Nudge labels slightly outside the stack
      direction = "y", 
      xlim = c(NA, NA),
      segment.size = .5,
      segment.alpha = .3,
      segment.linetype = "dotted",
      box.padding = 0.4,  # Control the space around the label
      max.overlaps = 5,  # Prevent too many overlapping labels
      force_pull = TRUE,  # Pull the labels inside the plot if space allows
      min.segment.length = 0.5  # Adjust the minimum segment length to control label position
    )+
    geom_bar(stat = "identity", position = "stack") +
    # Add text labels only for percentage > 9
    geom_text(aes(label = ifelse(percentage > 9, paste(round(percentage, 0), "%"), NA)), 
              position = position_stack(vjust = 0.5), na.rm = TRUE, 
              size = 3, color = "black") +  
    coord_flip() +  # Horizontal orientation
    scale_y_continuous(labels = function(x) paste0(round(x, 0), "%")) +  # Round percentages to integers and add % sign
    scale_fill_manual(values = color_mapping, 
                      breaks = rev(levels(data_combined2$hazard_regulation))) +  # Reverse color order in legend
    ggtitle("Quantification of hazard regulating services of NBS") +  
    plot_format[[format]]$theme +  # Use the specified theme
    theme(
      plot.title.position = "plot", # Position title in the plot area
      plot.title = element_text(hjust = 0.025, size = 12)) +  # Align title to the left with reasonable hjust value
    labs(x = "", y = "", fill = "Description of hazard regulation")
  
  plot_hazard_regulation
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_hazard_regulation_v4.png"), 
         plot = plot_hazard_regulation, width = 6, height = 5, dpi = 300)
}
```

### 2.2 Joint reporting for acad and non-acad publications
```{r, plot_hazard_regulation_onebar}
library(ggplot2)
library(dplyr)
library(ggrepel)
library(stringr)

# 1. Aggregate across sources
data_hazard_regulation <- data_combined_p %>%
  count(hazard_regulation) %>%
  mutate(percentage = (n / sum(n)) * 100)

## 2.3 Add ES_label_short column 
data_hazard_regulation <- data_hazard_regulation %>%
  mutate(hazard_regulation = case_when(
    hazard_regulation == "no description" ~ "No description",
    hazard_regulation == "brief mention" ~ "Brief mention",
    hazard_regulation == "qualitative description" ~ "Qualitative",
    hazard_regulation == "quantitative description" ~ "Quantitative",
    hazard_regulation == "monetary valuation" ~ "Monetary valuation",
    hazard_regulation == "mixed qualitative & monetary valuation"~ "Qualitative & monetary valuation",
    hazard_regulation == "mixed qualitative & quantitative description"~ "Qualitative & quantitative",
    hazard_regulation == "mixed quantitative & monetary valuation" ~ "Quantitative & monetary valuation",
    TRUE ~ NA_character_  # Add a default case if there are other ES values
  )
  )

# 2. Define ordered levels for hazard_regulation
order_hazard_regulation <- c(
  "Monetary valuation", 
  "Quantitative & monetary valuation", 
  "Qualitative & monetary valuation", 
  "Quantitative", 
  "Qualitative & quantitative", 
  "Qualitative", 
  "Brief mention", 
  "No description"
)

# 3. Convert hazard_regulation to factor with the defined levels
data_hazard_regulation <- data_hazard_regulation %>%
  mutate(hazard_regulation = factor(hazard_regulation, levels = order_hazard_regulation))

# 4. Compute cumulative percentage and label placement
data_hazard_regulation <- data_hazard_regulation %>%
  arrange(desc(hazard_regulation)) %>%
  mutate(cum_perc = cumsum(percentage),
         x_axis_bar_center = cum_perc - 0.5 * percentage,
         label = ifelse(percentage > 6, paste0(round(percentage, 0), "%"), NA),
         hazard_regulation_label = str_wrap(hazard_regulation, width = 20))

# 5. Define custom colors
custom_colors <- c(
  # "#a3b5a5", "#9BC19F", "#C7DAC9", "slategray2","#6AB9CB","deepskyblue3", "lightskyblue3", "skyblue3" # "deepskyblue3", "slategray2" "paleturquoise3"
  # "#a3b5a5", "#9BC19F", "#C7DAC9", "slategray2","lightskyblue3", "skyblue3", "#3191C1", "#2064AE"
  "lightgrey", "#9BC19F", "#C7DAC9", "skyblue3","#A3C4D7", "#5AA7CD","slategray2", "#4C83BE" 
  
) 

# 6. Define color mapping
color_mapping_hazard <- setNames(custom_colors, rev(levels(data_hazard_regulation$hazard_regulation)))

# 7. Create the plot
for (format in names(plot_format)) {
  
  plot_hazard_regulation_onebar <- ggplot(data_hazard_regulation, 
                                          aes(x = "Hazard regulation", y = percentage, fill = hazard_regulation)) +
    geom_bar(stat = "identity", position = "stack", width = 0.8) +  # Reduce bar width
    
    # Add text labels for percentages > 9%
    geom_text(aes(label = label), 
              position = position_stack(vjust = 0.5), na.rm = TRUE, 
              size = 2.5, color = "black") +
    
    # Flip coordinates for horizontal bars
    coord_flip() +  
    
    # Customize scales and labels
    
    scale_y_continuous(limits = c(0, 101),     # Set y-axis limits from 0 to 100% 
                       expand = c(0, 0),       # with no expansion so that the axis line stops exactly at 100%
                       labels = function(x) paste0(round(x, 0), "%")) +  
    scale_x_discrete(expand = expansion(mult = c(0.2, 0.2))) +  # Adjust spacing around x-axis
    scale_fill_manual(values = color_mapping_hazard) +
    
    # Title and theme
    ggtitle("Quantification of hazard regulating services of NBS") +
    plot_format[[format]]$theme +  # Use the specified theme
    theme(
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.025, size = 12), 
      axis.text.x = element_text(size = 8), # Increase bottom margin (left side after flipping)
      axis.text.y = element_text(size = 8, margin = margin(l=70, r = 5)), # l=70 to move y-axis to the right for alignment in panel figure
      axis.title.x = element_text(size = 9),
      axis.title.y = element_blank(),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 7), 
      legend.position = "bottom",
      legend.justification = "left", 
      legend.box.margin = margin(l = -135),  # Shift the legend box 20 units to the left
      legend.title.position = "top", 
      plot.margin = margin(t = 5, r = 15, b = 5, l = 5)
    ) +
    
    # Customize legend
    guides(
      fill = guide_legend(
        override.aes = list(label = "", size = 3),
        reverse = TRUE, 
        order = 1, 
        nrow = 2, 
        keywidth = 0.8, 
        keyheight = 0.8
      )
    ) +
    
    # Update x-axis label
    labs(x = "", y = "", fill = "Description of hazard regulation")
  
  
  # Print the plot
  print(plot_hazard_regulation_onebar)
  
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_hazard_regulation_onebar_height2.png"), 
         plot = plot_hazard_regulation_onebar, width = 8, height = 2.3, dpi = 300)
}
```

```{r}
# library(ggplot2)
# library(cowplot)

# Standardizing theme settings to ensure alignment
custom_theme <- theme(
  plot.margin = margin(t = 8, r = 20 , b = 8, l = 0), # Adjust left margin
  axis.title.y = element_blank(),  
  
  plot.title = element_text(hjust = 0.025, size = 13), 
  axis.text.x = element_text(size = 12), # Increase bottom margin (left side after flipping)
  axis.text.y = element_text(size = 12, margin = margin(l=70, r = 5)), # l=70 to move y-axis to the right for alignment in panel figure
  axis.title.x = element_text(size = 10),
  #axis.title.y = element_blank(),
  legend.title = element_text(size = 11),
  legend.text = element_text(size = 10), 
  
  axis.ticks.y = element_line(),
  panel.grid.major.y = element_blank(), 
  panel.grid.minor.y = element_blank()
)

# Apply theme and flip axes with coord_flip(clip = "off") to avoid clipping
plot_a <- plot_hazard_regulation_onebar + custom_theme + coord_flip(clip = "off") + geom_bar(stat = "identity", position = "stack", width = 0.5)+
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +  # Adjust spacing around x-axis
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), na.rm = TRUE, 
            size = 4, color = "black") +
  guides(
    fill = guide_legend(
      override.aes = 
        list(label = ""#, 
             # size = 4
        ),
      reverse = TRUE, 
      order = 1, 
      nrow = 2, 
      keywidth = 1, 
      keyheight = 1
    )
  )

plot_b <- plot_ES_quanti_v2 + custom_theme + coord_flip(clip = "off") + geom_bar(stat = "identity", position = "stack", width = 0.5)+
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +  # Adjust spacing around x-axis
  geom_text(aes(
    label = ifelse(percentage >= 5, paste0(round(percentage, 0), "%"), NA)), 
    position = position_stack(vjust = 0.5), na.rm = TRUE, 
    size = 4, color = "black")+
  guides(
    fill = guide_legend(
      override.aes = 
        list(label = ""#, 
             # size = 4
        ),
      reverse = TRUE, 
      order = 1, 
      nrow = 1, 
      position = "bottom", 
      keywidth = 1, # Set the width of the legend keys
      keyheight = 1 # Set the height of the legend keys
    ))


plot_c <- plot_outcome + custom_theme + coord_flip(clip = "off") +
  geom_bar(stat = "identity", position = "stack", width = 0.5)+
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +  # Adjust spacing around x-axis
  geom_text(aes(
    label = ifelse(percentage >= 5, paste0(round(percentage, 0), "%"), NA)), 
    position = position_stack(vjust = 0.5), na.rm = TRUE, 
    size = 4, color = "black")+
  guides(
    fill = guide_legend(
      override.aes = list(label = ""#, 
                          # size = 4
      ),
      reverse = TRUE, 
      order = 1, 
      nrow = 1, 
      position = "bottom", 
      keywidth = 1, # Set the width of the legend keys
      keyheight = 1 # Set the height of the legend keys
    )) 


# Define relative heights based on bar counts
panel_plot <- plot_grid(
  plot_a, plot_b, plot_c,
  ncol = 1, align = "v", axis = "l", 
  rel_heights = c(2.2, 6.2, 2.7)  # Adjusted for different bar counts
)

# Save and display
ggsave(file.path(getwd(), "plots/white/evidence/Extended Data Fig. 4_600dpi_12x18.png"), 
       panel_plot, width = 12, height = 18, dpi = 600)

# ggsave(file.path(getwd(), "plots/Figures in paper/Extended Data/Extended Data Fig. 4_600dpi_10x15.png"), 
#        panel_plot, width = 10, height = 15, dpi = 600)
```


## 3. Probabilistic
### 3.1 Comparison of acad & non-acad publications 
```{r, prepare data_combined_p_probabilistic for plot_probabilistic}
# Print frequency table
unique(data_combined_p$probabilistic)

# 1. Add source_label column
data_combined_p_probabilistic <- data_combined_p %>%
  select(probabilistic, source) %>%
  mutate(source_label = case_when(
    source == "academic" ~ "Projects in academic publications",
    source == "non-academic" ~ "Projects in non-academic publications",
    TRUE ~ NA_character_  # Add a default case if there are other source values
  )
  )

# 2.2 Group and calculate counts and percentages
data_combined_p_probabilistic <- data_combined_p_probabilistic %>%
  group_by(source_label, probabilistic) %>%
  summarise(probabilistic_count = n(), .groups = "drop") %>%
  group_by(source_label) %>%
  mutate(total_count = sum(probabilistic_count),
         percentage = (probabilistic_count / total_count) * 100) %>%
  ungroup()

# 2.2 Create the labels for percentages > 7%
data_combined_p_probabilistic <- data_combined_p_probabilistic %>%
  mutate(label = ifelse(percentage > 7, probabilistic_count, NA))

# 3. Ensure `probabilistic` is a factor with the desired levels
## 3a. Create the vector of categories ordered 
order_probabilistic <- rev(c("Yes", "No", "Unknown", "Hazard regulation not quantified"))

## 3b. Convert probabilistic to factor
data_combined_p_probabilistic <- data_combined_p_probabilistic %>%
  mutate(probabilistic = factor(probabilistic, levels = 
                                  order_probabilistic))

# 4. Add columns cum_perc and x_axis_bar_center
# data_combined_p_probabilistic <- data_combined_p_probabilistic %>%
#     group_by(source_label) %>%
#     arrange(desc(probabilistic), .by_group = TRUE) %>%  # Reverse the order of probabilistic
#     mutate(cum_perc = cumsum(percentage),
#             x_axis_bar_center = cum_perc - 0.5 * percentage) %>%
#   ungroup()


# 5. Update source_label format to include total count
data_combined_p_probabilistic <- data_combined_p_probabilistic %>%
  mutate(source_label = paste0(source_label, "\n (n = ", total_count, ")"))

# 6. Add probabilistic_label with wrapped text of probabilistic
# data_combined_p_probabilistic <- data_combined_p_probabilistic %>%
#   mutate(probabilistic_label = str_wrap(probabilistic, width = 20))

# 7. Check levels
# levels(data_combined_p_probabilistic$probabilistic)

# Remove objects
remove(order_probabilistic)

# Define colors 
# custom_colors3_plus1 <- rev(c("#63957A","#8ABD6F","#9BC19F", "grey"))
custom_colors2_plus2 <- c("grey","#E1D1C1","skyblue3","#9BC19F")
```

```{r, create plot_probabilistic}
# Create stacked bar plot
for (format in names(plot_format)) {
  plot_probabilistic <- 
    ggplot(data_combined_p_probabilistic, 
           aes(x = source_label, y = percentage, fill = probabilistic)) +
    geom_bar(stat = "identity", position = "stack") +
    # coord_flip() +  # Horizontal orientation
    scale_y_continuous(labels = function(x) paste0(round(x, 0), "%")) +  # Round percentages to integers and add % sign
    scale_fill_manual(values = custom_colors2_plus2, 
                      breaks = rev(levels(data_combined2$probabilistic))) +  # Reverse color order in legend
    ggtitle("Probabilistic analysis used to assess the NBS' hazard regulation benefits") +  # Add title
    plot_format[[format]]$theme +  # Use the specified theme
    theme(
      plot.title.position = "plot", # Position title in the plot area
      plot.title = element_text(hjust = 0.025, size = 12), 
      axis.text.x = element_text(size = 8),  
      axis.text.y = element_text(size = 8, margin = margin(r = 5)),  # Adjust axis spacing
      axis.title.x = element_text(size = 9)
    ) +  # Align title to the left with reasonable hjust value
    labs(x = "", y = "", fill = "Gender")+
    # Add text labels only for percentage > 6
    geom_text(aes(label = ifelse(percentage > 6, paste0(probabilistic, " (",round(percentage, 0), "%)"), NA)), 
              position = position_stack(vjust = 0.5), na.rm = TRUE, 
              size = 2.2, color = "black")  
  
  
  plot_probabilistic
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_probabilistic.png"), 
         plot = plot_probabilistic, width = 6, height = 3, dpi = 300)
}
```


### 3.2 Joint reporting for acad & non-acad publications
```{r, prepare data_combined_p_probabilistic for plot_probabilistic_onebar}
# Print frequency table
unique(data_combined_p$probabilistic)

# 1. Group and calculate counts and percentages
data_combined_p_probabilistic <- data_combined_p %>%
  select(probabilistic) %>%
  group_by(probabilistic) %>%
  summarise(probabilistic_count = n(), .groups = "drop") %>%
  mutate(total_count = sum(probabilistic_count),
         percentage = (probabilistic_count / total_count) * 100) %>%
  ungroup()

# 2. Create the labels for percentages > 7%
data_combined_p_probabilistic <- data_combined_p_probabilistic %>%
  mutate(label = ifelse(percentage > 7, probabilistic_count, NA))

# 3. Ensure `probabilistic` is a factor with the desired levels
## 3a. Create the vector of categories ordered 
order_probabilistic <- rev(c("Yes", "No", "Unknown", "Hazard regulation not quantified"))

## 3b. Convert probabilistic to factor
data_combined_p_probabilistic <- data_combined_p_probabilistic %>%
  mutate(probabilistic = factor(probabilistic, levels = 
                                  order_probabilistic))

# 5. Update source_label format to include total count
data_combined_p_probabilistic <- data_combined_p_probabilistic %>%
  mutate(source_label = paste0("Projects in all publications", "\n (n = ", max(total_count), ")"))

# 6. Add probabilistic_label with wrapped text of probabilistic
# data_combined_p_probabilistic <- data_combined_p_probabilistic %>%
#   mutate(probabilistic_label = str_wrap(probabilistic, width = 20))

# 7. Check levels
# levels(data_combined_p_probabilistic$probabilistic)

# Remove objects
remove(order_probabilistic)

# Define colors 
# custom_colors3_plus1 <- rev(c("#63957A","#8ABD6F","#9BC19F", "grey"))
custom_colors2_plus2 <- c("grey","#E1D1C1","skyblue3","#9BC19F")
```

```{r, create plot_probabilistic}
# Create stacked bar plot
for (format in names(plot_format)) {
  plot_probabilistic_onebar <- 
    ggplot(data_combined_p_probabilistic, 
           aes(x = source_label, y = percentage, fill = probabilistic)) +
    geom_bar(stat = "identity", position = "stack", width=0.5) +
    # coord_flip() +  # Horizontal orientation
    scale_x_discrete(expand = c(0.14, 0.14)) +  # Reduce x-axis padding
    scale_y_continuous(
      limits = c(0, 101),
      expand = c(0, 0),
      labels = function(x) paste0(round(x, 0), "%")) +  # Round percentages to integers and add % sign
    scale_fill_manual(values = custom_colors2_plus2, 
                      breaks = levels(data_combined_p_probabilistic$probabilistic)) +  # Reverse color order in legend
    # ggtitle("Probabilistic analysis used to assess the NBS' hazard regulation benefits") +  # Add title
    plot_format[[format]]$theme +  # Use the specified theme
    theme(
      plot.title.position = "plot", # Position title in the plot area
      plot.title = element_text(hjust = 0.025, size = 12), 
      axis.ticks.x = element_blank(), 
      axis.text.x = element_text(size = 8),  
      axis.text.y = element_text(size = 8, margin = margin(r = 5)),  # Adjust axis spacing
      axis.title.x = element_text(size = 9), 
      legend.position = "none"
    ) +  # Align title to the left with reasonable hjust value
    labs(x = "", y = "", fill = "")+
    # Add text labels only for percentage > 6
    geom_text(aes(label = ifelse(percentage > 6, paste0(probabilistic, " (",round(percentage, 0), "%)"), NA)), 
              position = position_stack(vjust = 0.5), na.rm = TRUE, 
              size = 2.5, color = "black")  
  
  plot_probabilistic_onebar
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_probabilistic_onebar.png"), 
         plot = plot_probabilistic_onebar, width = 5.5, height = 3, dpi = 600)
}
```


## 4. Uncertainty
```{r, create data_combined_p_uncertainty for plotting}
# Values: "disaster risk reduction; climate change mitigation; climate change adaptation; economic and social development; water security; environmental degradation and biodiversity loss"; "human health"; food security" 
# unique(data_combined_p$uncertainty)

# 1. Create data_combined_p_uncertainty
data_combined_p_uncertainty <- data_combined_p %>%
  mutate(uncertainty_split = str_split(uncertainty, ";\\s*")) %>% 
  filter(!is.na(uncertainty)) %>%  # Filter rows where uncertainty is not NA
  unnest(uncertainty_split) %>% 
  count(uncertainty_split, source) %>%
  rename(uncertainty = uncertainty_split,
         count = n)

# 2. Add columns
# 2a. Calculate source_count as the total count per source (academic and non-academic)
data_combined_p_uncertainty <- data_combined_p_uncertainty %>%
  group_by(source) %>%
  mutate(source_count = sum(count)) %>%  # Total count grouped by source (academic vs non-academic)
  ungroup()

# Note: source_count of academic and non-academic sources does not add up to nrow(data_combined_p) = 313, because some projects mention multiple uncertainty, therefore the number is higher (331+481 = 812)

# 2b.part1 Add percentage of evidence by uncertainty (publications in the case of acad literature + projects in the case of grey literature) that mentions the uncertainty factor
data_combined_p_uncertainty <- data_combined_p_uncertainty %>%
  group_by(uncertainty) %>%
  mutate(
    count_by_uncertainty_total = sum(count),  # Sum counts across sources for each uncertainty
    # Given that uncertainty can contain each uncertainty only once in one cell, the next line can be calculated directly (without needed ..._single)
    perc_of_allprojects_by_uncertainty = round(count_by_uncertainty_total / nrow(data_combined_p) * 100, 3) 
  ) %>%
  ungroup()  # Remove grouping for further operations

## Note: perc_of_allprojects_by_uncertainty does not sum up to 100% because not all projects mention uncertainty (some mention multiple uncertainty factors)
sum(data_combined_p_uncertainty$perc_of_allprojects_by_uncertainty)/2 # 45.8435

# 2b.part2 Ensure that perc_of_allprojects_by_uncertainty is only shown for "non-academic" rows, and NA for "academic"
# data_combined_p_uncertainty <- data_combined_p_uncertainty %>%
#    mutate(perc_of_allprojects_by_uncertainty = if_else(source == "academic", NA_real_, perc_of_allprojects_by_uncertainty))

######## 

# 2c. Add perc_of_alluncertainty_by_uncertainty column 
total_count <- sum(data_combined_p_uncertainty$count)
data_combined_p_uncertainty$perc_of_alluncertainty_by_uncertainty <-
  data_combined_p_uncertainty$count_by_uncertainty_total / total_count*100  # Calculate total percentage for each uncertainty source
remove(total_count)

## Note: Sum of perc_of_alluncertainty_by_uncertainty for unique uncertainty adds up to 100%
data_combined_p_uncertainty %>%
  distinct(uncertainty, .keep_all = TRUE) %>%                # Filter unique rows by uncertainty
  summarise(total_perc = sum(perc_of_alluncertainty_by_uncertainty)) %>%  # Sum perc_of_alluncertainty_by_uncertainty
  pull(total_perc)                                          # Extract the value


# 2d. Add percentage that a uncertainty represents of all uncertainty found (only one uncertainty per project can exist) 
## Statements: "___% of all uncertainty found in the evidence are provisioning services" = "provisioning services accounts for ___ % of all ecosystem services"
data_combined_p_uncertainty <- data_combined_p_uncertainty %>%
  mutate(
    perc_of_alluncertainty_by_source = 
      round(count / source_count * 100, 3))


# 2e. Add percentage that an uncertainty factor represents of all projects (within same source group) 
## Statements: "___% of projects found in the evidence consider climate scenarios as source of uncertainty" = "climate scenarios are mentioned in ____ of the identified projects/sources of evidence as source of uncertainty"
data_combined_p_uncertainty <- data_combined_p_uncertainty %>%
  mutate(
    perc_of_allprojects_by_source = case_when(
      source == "academic" ~ round(count / nrow(data_acad_p) * 100, 3),
      source == "non-academic" ~ round(count / nrow(data_grey_p) * 100, 3),
      TRUE ~ NA_real_  # Handle any unexpected cases (optional)
    )
  )

######## 

# 2f. Add percentage that an uncertainty factor represents of all projects (within same source group) 
## Statements: "___% of projects that quantify hazard regulation benefits of the NBS found in the evidence consider climate scenarios as source of uncertainty" = "climate scenarios are mentioned in ____ of the identified projects/sources of evidence as source of uncertainty"
## 2f.i Get count of project that quantify the NBS' hazard regulation benefits for each source
count_haz_reg_quantified_acad <- data_combined_p %>%
  select(hazard_regulation, source) %>%
  filter(
    !hazard_regulation %in% c("no description", "brief mention", "qualitative description") &  # 'not in' 
      source == "academic"
  ) %>%
  nrow() # 46

count_haz_reg_quantified_grey <- data_combined_p %>%
  select(hazard_regulation, source) %>%
  filter(
    !hazard_regulation %in% c("no description", "brief mention", "qualitative description") &  # 'not in' 
      source == "non-academic"
  ) %>%
  nrow() # 50

## 2f.ii Add column perc_of_projects_with_haz_reg_quantified_by_source
data_combined_p_uncertainty <- data_combined_p_uncertainty %>%
  mutate(
    perc_of_projects_with_haz_reg_quantified_by_source = case_when(
      source == "academic" ~ round(count / count_haz_reg_quantified_acad * 100, 3),
      source == "non-academic" ~ round(count / count_haz_reg_quantified_grey * 100, 3),
      TRUE ~ NA_real_  # Handle any unexpected cases (optional)
    )
  )

## Note: Does not sum up to 100% because several projects mention multiple societal uncertainty 
sum(data_combined_p_uncertainty$perc_of_allprojects_by_source)/2 # 54.733

## 2g. Add column perc_of_projects_with_haz_reg_quantified_by_uncertainty
data_combined_p_uncertainty <- data_combined_p_uncertainty %>%
  mutate(
    perc_of_projects_with_haz_reg_quantified_by_uncertainty = 
      round(count_by_uncertainty_total / sum(count_haz_reg_quantified_acad,count_haz_reg_quantified_grey) * 100, 3)
  )

## 2h. Add column source_label
data_combined_p_uncertainty <- data_combined_p_uncertainty %>%
  mutate(
    source_label = case_when(
      source == "academic" ~ paste0(source, " (n=", count_haz_reg_quantified_acad, ")"),
      source == "non-academic" ~ paste0(source, " (n=", count_haz_reg_quantified_grey, ")"),
      TRUE ~ NA_character_
    )
  )

# 3. Ensure correct order for plotting
## 3a. Determine the order of uncertainty based on the count for "academic"
## Some uncertainty facotrs exists only in academic publications. 
order_uncertainty <- data_combined_p_uncertainty %>%
  filter(source == "academic") %>%
  arrange(count_by_uncertainty_total) %>%
  pull(uncertainty)

## 3b. Update uncertainty as a factor for ordered plotting
data_combined_p_uncertainty <- data_combined_p_uncertainty %>%
  mutate(uncertainty = factor(uncertainty, levels = order_uncertainty))

## 3c. Update source as a factor for ordered plotting
data_combined_p_uncertainty <- data_combined_p_uncertainty %>%
  mutate(source = factor(source, levels = rev(c("academic", "non-academic"))))

## 3d. Update source_label as a factor for ordered plotting
data_combined_p_uncertainty <- data_combined_p_uncertainty %>%
  mutate(source_label = factor(source_label, levels = rev(c("academic (n=46)", "non-academic (n=50)"))))


# Remove objects
remove(order_uncertainty)
```

### a. Percentage
```{r, create plot_uncertainty_perc_of_allprojects_long}
# Plot with horizontal bars in long format (bars grouped on x-axis)
for (format in names(plot_format)) {
  plot_uncertainty_perc_long <- 
    data_combined_p_uncertainty %>%
    ggplot(aes(x = uncertainty, y = perc_of_projects_with_haz_reg_quantified_by_source, fill = source_label)) +
    geom_bar(stat = "identity", 
             position = position_dodge2(width = 0.8, preserve = "single"),  # Ensure that bars without academic publications have the same width
             width = 0.8
             # color = "white"
    ) +  # 
    scale_x_discrete(expand = expansion(add = c(0.6, 0))) +  # Increase space between categories (x-axis)
    scale_y_continuous(expand = c(0.01, 0), limits = c(0, 83), breaks = seq(0, 83, by=20)) +  # No padding on x-axis
    labs(
      # title = "Sources of uncertainty in the valuation of the NBS' hazard regulating services",
      # subtitle = "Only projects that quantify the hazard regulating benefits of the NBS are considered.\nMultiple counts per publication are possible.",
      x = "",
      y = "Percentage of projects",
      fill = "Database"
    )+
    scale_fill_manual(values = c("academic (n=46)" = green1, 
                                 "non-academic (n=50)" = "lightsalmon1")) + 
    plot_format[[format]]$theme +  # Use the specified theme
    guides(fill = guide_legend(
      override.aes = list(label = "", size = 2), # Control the fill aesthetics
      reverse = TRUE, 
      order = 1, 
      keywidth = 0.5, # Set the width of the legend keys
      keyheight = 0.5 # Set the height of the legend keys
    )) +  # Reverse the order of legend items
    theme(panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(), # Remove minor grid lines+
          plot.title = element_text(hjust = 0, size = 12, margin = margin(b = 5)),  # Reduced bottom margin
          plot.subtitle = element_text(hjust = 0, size = 10, margin = margin(t = 2, b = 10)),  # Adjust margins
          plot.title.position = "plot",  # Align title to the leftmost edge of the plot
          axis.text.x = element_text(size = 9),  
          axis.text.y = element_text(size = 9, margin = margin(r = 5)),  # Adjust axis spacing
          axis.title.x = element_text(size = 9), 
          legend.position = "none" # remove legend
          # legend.text = element_text(size = 8), # Set guide label text size
          # legend.title = element_text(size = 9), # Set guide title text size
          # legend.box.margin = margin(10, 10, 10, 10), 
          # legend.background = element_rect(color = "black", linewidth = 0.2, fill = "transparent"),  
          # legend.position = c(0.98, 0.05),  # Place the legend in the lower-right corner (inside the plot)
          # legend.justification = c(1, 0)
          # legend.justification = c("right", "bottom")  # Adjust alignment to the bottom-right corner
    )+
    coord_flip() 
  
  # Show plot
  print(plot_uncertainty_perc_long)
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format,
                   "evidence/plot_uncertainty_perc_of_projects_with_haz_reg_quantified_long.png"),
         plot = plot_uncertainty_perc_long, width = 7, height = 3.5, dpi = 600) # set height to 4 when including title
}

# Remove  objects
remove(plot_uncertainty_perc_long)
```

### b. Count
```{r, create plot_uncertainty_count_perc_of_allprojects_by_uncertainty}
for (format in names(plot_format)) {
  plot_uncertainty_count_perc <- 
    ggplot(data_combined_p_uncertainty, 
           aes(x = count, y = uncertainty, fill = source_label)) +
    geom_bar(stat = "identity", position = "stack", color = "white", width=1) +  # Use identity for precomputed counts and white lines
    scale_x_continuous(expand = c(0.01, 0), limits = c(0, 81), breaks=seq(0, 81, by=20)) +  # No padding on x-axis
    labs(
      # title = "Sources of uncertainty \nin the valuation of the NBS' hazard regulating services",
      # subtitle = "All mentioned uncertainty factors are counted. Multiple counts per publication are possible.", 
      x= paste0("Count of projects (n=", 
                sum(count_haz_reg_quantified_acad+count_haz_reg_quantified_grey), ")"),
      y = "") +
    # scale_fill_manual(values = c("academic" = green1, "non-academic" = "lightsalmon1")) +  # Custom colors
    scale_fill_manual(values = c("academic (n=46)" = green1, 
                                 "non-academic (n=50)" = "lightsalmon1")) + 
    plot_format[[format]]$theme +  # Use the specified theme
    scale_y_discrete(labels = function(x) str_wrap(x, width = 70)) +  # Wrap x-axis labels after 25 characters
    # Increase space between y-axis ticks and labels
    guides(fill = guide_legend(
      override.aes = list(label = "", size = 1), # Control the fill aesthetics
      reverse = TRUE, 
      order = 1, 
      keywidth = 0.2, # Set the width of the legend keys
      keyheight = 0.2 # Set the height of the legend keys
    )) +  # Reverse the order of legend items
    theme(panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          plot.title = element_text(hjust = 0, size = 12, margin = margin(b = 5)),  # Reduced bottom margin
          plot.subtitle = element_text(hjust = 0, size = 10, margin = margin(t = 2, b = 10)),  # Adjust margins
          plot.title.position = "plot",  # Align title to the leftmost edge of the plot
          axis.text.x = element_text(size = 9),  
          axis.text.y = element_text(size = 9, margin = margin(r = 5)),  # Adjust axis spacing
          axis.title.x = element_text(size = 9), 
          axis.title.y = element_text(size = 9), 
          legend.text = element_text(size = 8), # Set guide label text size
          legend.title = element_text(size = 9), # Set guide title text size
          legend.box.margin = margin(10, 10, 10, 10), 
          legend.background = element_rect(color = "black", linewidth = 0.2, fill = "transparent"),  
          legend.position = c(0.98, 0.05),  # Place the legend in the lower-right corner (inside the plot)
          legend.justification = c(1, 0)
          # legend.justification = c("right", "bottom")  # Adjust alignment to the bottom-right corner
    )+
    geom_hline(yintercept = 0, color = "black") +  # Add a horizontal line at y = 0
    guides(fill = guide_legend(title = "Database", reverse = TRUE))+  # Adjust legend title; Reverse the order of legend items
    
    # Add percentage at position x+0.5 (right of the bar)
    geom_text(data = subset(data_combined_p_uncertainty, source == "academic"),  # Subset where source is "academic"
              aes(x = count_by_uncertainty_total + 0.5, 
                  label = scales::percent(perc_of_projects_with_haz_reg_quantified_by_uncertainty / 100, accuracy = 1)), 
              hjust = 0,  # Position the label at the height of the bar
              color = "black", 
              size = 3  # Label formatting
    )
  
  # Showaxis.title.y = # Show plot
  print(plot_uncertainty_count_perc)
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_uncertainty_count_perc_of_projects_with_haz_reg_quantified_by_uncertainty.png"),
         plot = plot_uncertainty_count_perc, width = 7, height = 3, dpi = 600)
}

# Remove the plot
remove(plot_uncertainty_count_perc)
# remove(data_combined_p_uncertainty)
```


## 5. Disaggregation 
```{r, data_combined_p_disaggregation}
unique(data_combined_p$disaggregation)

# "Are benefits disaggregated by socio-economic characteristics?"
```

```{r, create data_combined_p_disaggregation for plotting}
# Values: "disaster risk reduction; climate change mitigation; climate change adaptation; economic and social development; water security; environmental degradation and biodiversity loss"; "human health"; food security" 
# unique(data_combined_p$disaggregation)

# 1. Create data_combined_p_disaggregation
data_combined_p_disaggregation <- data_combined_p %>%
  mutate(disaggregation_split = str_split(disaggregation, ";\\s*")) %>% 
  filter(!is.na(disaggregation)) %>%  # Filter rows where disaggregation is not NA
  unnest(disaggregation_split) %>% 
  count(disaggregation_split, source) %>%
  rename(disaggregation = disaggregation_split,
         count = n)

# 2. Add columns
# 2a. Calculate source_count as the total count per source (academic and non-academic)
data_combined_p_disaggregation <- data_combined_p_disaggregation %>%
  group_by(source) %>%
  mutate(source_count = sum(count)) %>%  # Total count grouped by source (academic vs non-academic)
  ungroup()

# Note: source_count of academic and non-academic sources does not add up to nrow(data_combined_p) = 313, because some projects mention multiple disaggregation, therefore the number is higher (331+481 = 812)

# 2b.part1 Add percentage of evidence by disaggregation (publications in the case of acad literature + projects in the case of grey literature) that mentions the disaggregation factor
data_combined_p_disaggregation <- data_combined_p_disaggregation %>%
  group_by(disaggregation) %>%
  mutate(
    count_by_disaggregation_total = sum(count),  # Sum counts across sources for each disaggregation
    # Given that disaggregation can contain each disaggregation only once in one cell, the next line can be calculated directly (without needed ..._single)
    perc_of_allprojects_by_disaggregation = round(count_by_disaggregation_total / nrow(data_combined_p) * 100, 3) 
  ) %>%
  ungroup()  # Remove grouping for further operations

## Note: perc_of_allprojects_by_disaggregation does not sum up to 100% because not all projects mention disaggregation (some mention multiple and others no disaggregation factors)
sum(data_combined_p_disaggregation$perc_of_allprojects_by_disaggregation)/2 

# 2b.part2 Ensure that perc_of_allprojects_by_disaggregation is only shown for "non-academic" rows, and NA for "academic"
# data_combined_p_disaggregation <- data_combined_p_disaggregation %>%
#    mutate(perc_of_allprojects_by_disaggregation = if_else(source == "academic", NA_real_, perc_of_allprojects_by_disaggregation))

######## 

# 2c. Add perc_of_alldisaggregation_by_disaggregation column 
total_count <- sum(data_combined_p_disaggregation$count)
data_combined_p_disaggregation$perc_of_alldisaggregation_by_disaggregation <-
  data_combined_p_disaggregation$count_by_disaggregation_total / total_count*100  # Calculate total percentage for each disaggregation source
remove(total_count)

## Note: Sum of perc_of_alldisaggregation_by_disaggregation for unique disaggregation adds up to 100%
data_combined_p_disaggregation %>%
  distinct(disaggregation, .keep_all = TRUE) %>%                # Filter unique rows by disaggregation
  summarise(total_perc = sum(perc_of_alldisaggregation_by_disaggregation)) %>%  # Sum perc_of_alldisaggregation_by_disaggregation
  pull(total_perc)                                          # Extract the value


# 2d. Add percentage that a disaggregation represents of all disaggregation found (only one disaggregation per project can exist) 
## Statements: "___% of all disaggregation found in the evidence are provisioning services" = "provisioning services accounts for ___ % of all ecosystem services"
data_combined_p_disaggregation <- data_combined_p_disaggregation %>%
  mutate(
    perc_of_alldisaggregation_by_source = 
      round(count / source_count * 100, 3))


# 2e. Add percentage that an disaggregation factor represents of all projects (within same source group) 
## Statements: "___% of projects found in the evidence disaggregate benefits by income" 
data_combined_p_disaggregation <- data_combined_p_disaggregation %>%
  mutate(
    perc_of_allprojects_by_source = case_when(
      source == "academic" ~ round(count / nrow(data_acad_p) * 100, 3),
      source == "non-academic" ~ round(count / nrow(data_grey_p) * 100, 3),
      TRUE ~ NA_real_  # Handle any unexpected cases (optional)
    )
  )

######## 

# 2f. Add percentage that an disaggregation factor represents of all projects (within same source group) 
## Statements: "___% of projects that quantify hazard regulation benefits of the NBS found in the evidence disaggregate benefits by income" =
## 2f.i Get count of project that quantify the NBS' hazard regulation benefits for each source
count_haz_reg_quantified_acad <- data_combined_p %>%
  select(hazard_regulation, source) %>%
  filter(
    !hazard_regulation %in% c("no description", "brief mention", "qualitative description") &  # 'not in' 
      source == "academic"
  ) %>%
  nrow() # 46

count_haz_reg_quantified_grey <- data_combined_p %>%
  select(hazard_regulation, source) %>%
  filter(
    !hazard_regulation %in% c("no description", "brief mention", "qualitative description") &  # 'not in' 
      source == "non-academic"
  ) %>%
  nrow() # 50

## 2f.ii Add column perc_of_projects_with_haz_reg_quantified_by_source
data_combined_p_disaggregation <- data_combined_p_disaggregation %>%
  mutate(
    perc_of_projects_with_haz_reg_quantified_by_source = case_when(
      source == "academic" ~ round(count / count_haz_reg_quantified_acad * 100, 3),
      source == "non-academic" ~ round(count / count_haz_reg_quantified_grey * 100, 3),
      TRUE ~ NA_real_  # Handle any unexpected cases (optional)
    )
  )

## Note: Does not sum up to 100% because several projects mention multiple societal disaggregation 
sum(data_combined_p_disaggregation$perc_of_allprojects_by_source)/2 # 55.6955

## 2g. Add column perc_of_projects_with_haz_reg_quantified_by_disaggregation
data_combined_p_disaggregation <- data_combined_p_disaggregation %>%
  mutate(
    perc_of_projects_with_haz_reg_quantified_by_disaggregation = 
      round(count_by_disaggregation_total / sum(count_haz_reg_quantified_acad,count_haz_reg_quantified_grey) * 100, 3)
  )


## 2h. Add column source_label
data_combined_p_disaggregation <- data_combined_p_disaggregation %>%
  mutate(
    source_label = case_when(
      source == "academic" ~ paste0(source, " (n=", count_haz_reg_quantified_acad, ")"),
      source == "non-academic" ~ paste0(source, " (n=", count_haz_reg_quantified_grey, ")"),
      TRUE ~ NA_character_
    )
  )


# 3. Ensure correct order for plotting
## 3a. Determine the order of disaggregation based on the count for "academic"
## Some disaggregation factors exists only in academic publications. 
## Arrange by count_by_disaggregation_total and then by reversed alphabetical order for disaggregation
order_disaggregation <- data_combined_p_disaggregation %>%
  filter(source == "academic") %>%
  arrange(count_by_disaggregation_total, desc(disaggregation)) %>% 
  pull(disaggregation)


## 3b. Update disaggregation as a factor for ordered plotting
data_combined_p_disaggregation <- data_combined_p_disaggregation %>%
  mutate(disaggregation = factor(disaggregation, levels = order_disaggregation))

## 3c. Update source as a factor for ordered plotting
data_combined_p_disaggregation <- data_combined_p_disaggregation %>%
  mutate(source = factor(source, levels = rev(c("academic", "non-academic"
  ))))

## 3d. Update source_label as a factor for ordered plotting
data_combined_p_disaggregation <- data_combined_p_disaggregation %>%
  mutate(source_label = factor(source_label, levels = rev(c(
    paste0("academic (n=", count_haz_reg_quantified_acad, ")"), 
    paste0("non-academic (n=", count_haz_reg_quantified_grey, ")") 
  ))))

# Remove objects
remove(order_disaggregation)
```

### a. Percentage
```{r, create plot_disaggregation_perc_of_allprojects_long}
# Plot with horizontal bars in long format (bars grouped on x-axis)
for (format in names(plot_format)) {
  plot_disaggregation_perc_long <- 
    data_combined_p_disaggregation %>%
    ggplot(aes(x = disaggregation, y = perc_of_projects_with_haz_reg_quantified_by_source, fill = source_label)) +
    geom_bar(stat = "identity", 
             position = position_dodge2(width = 0.8, preserve = "single"),  # Ensure that bars without academic publications have the same width
             width = 0.8
             # color = "white"
    ) +  # 
    scale_x_discrete(expand = expansion(add = c(0.6, 0))) +  # Increase space between categories (x-axis)
    scale_y_continuous(expand = c(0.01, 0), limits = c(0, 90), breaks = seq(0, 90, by=10)) +  # No padding on x-axis
    labs(
      # title = "Disaggregation of benefits \nin the valuation of the NBS' hazard regulating services",
      # subtitle = "Only projects that quantify the hazard regulating benefits of the NBS are considered.\nMultiple counts per publication are possible.",
      x = "",
      y = "Percentage of projects",
      fill = "Database"
    )+
    scale_fill_manual(values = c("academic (n=46)" = green1, 
                                 "non-academic (n=50)" = "lightsalmon1")) +  # Custom colors
    plot_format[[format]]$theme +  # Use the specified theme
    guides(fill = guide_legend(
      override.aes = list(label = "", size = 2), # Control the fill aesthetics
      reverse = TRUE, 
      order = 1, 
      keywidth = 0.5, # Set the width of the legend keys
      keyheight = 0.5 # Set the height of the legend keys
    )) +  # Reverse the order of legend items
    theme(panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(), # Remove minor grid lines+
          plot.title = element_text(hjust = 0, size = 12, margin = margin(b = 5)),  # Reduced bottom margin
          plot.subtitle = element_text(hjust = 0, size = 10, margin = margin(t = 2, b = 10)),  # Adjust margins
          plot.title.position = "plot",  # Align title to the leftmost edge of the plot
          axis.text.x = element_text(size = 9),  
          axis.text.y = element_text(size = 9, margin = margin(r = 5)),  # Adjust axis spacing
          axis.title.x = element_text(size = 9), 
          legend.position = "none" # remove legend
          #   legend.text = element_text(size = 8), # Set guide label text size
          #   legend.title = element_text(size = 9), # Set guide title text size
          #   legend.box.margin = margin(10, 10, 10, 10), 
          #   legend.background = element_rect(color = "black", linewidth = 0.2, fill = "transparent"),  
          # # legend.position = c(0.89, 0.10),  # Place the legend in the lower-right corner (inside the plot)
          #   legend.justification = c("right", "bottom")  # Adjust alignment to the bottom-right corner
    )+
    coord_flip() 
  
  # Show plot
  print(plot_disaggregation_perc_long)
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format,
                   "evidence/plot_disaggregation_perc_of_projects_with_haz_reg_quantified_long.png"),
         plot = plot_disaggregation_perc_long, width = 7, height = 4, dpi = 600)
}
```

### b. Count
```{r, create plot_disaggregation_count_perc_of_allprojects_by_disaggregation}
for (format in names(plot_format)) {
  plot_disaggregation_count_perc <- 
    ggplot(data_combined_p_disaggregation, 
           aes(x = count, y = disaggregation, fill = source_label)) +
    geom_bar(stat = "identity", position = "stack", color = "white", width=1) +  # Use identity for precomputed counts and white lines
    scale_x_continuous(expand = c(0.01, 0), limits = c(0, 60), breaks=seq(0, 60, by=10)) +  # No padding on x-axis
    labs(
      # title = "Disaggregation of benefits in the valuation of the NBS' hazard regulating services",
      # subtitle = "Only projects that quantify the hazard regulating benefits of the NBS are considered.\nMultiple counts per publication are possible.",
      x= paste0("Count of projects (n=", 
                sum(count_haz_reg_quantified_acad+count_haz_reg_quantified_grey), ")"),
      y = "") +
    # scale_fill_manual(values = c("academic" = green1, "non-academic" = "lightsalmon1")) +  # Custom colors
    scale_fill_manual(values = c("academic (n=46)" = green1, 
                                 "non-academic (n=50)" = "lightsalmon1")) +  # Custom colors
    
    plot_format[[format]]$theme +  # Use the specified theme
    scale_y_discrete(labels = function(x) str_wrap(x, width = 70)) +  # Wrap x-axis labels after 25 characters
    # Increase space between y-axis ticks and labels
    guides(fill = guide_legend(
      override.aes = list(label = "", size = 2), # Control the fill aesthetics
      reverse = TRUE, 
      order = 1, 
      keywidth = 0.5, # Set the width of the legend keys
      keyheight = 0.5 # Set the height of the legend keys
    )) +  # Reverse the order of legend items
    theme(panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          plot.title = element_text(hjust = 0, size = 12, margin = margin(b = 5)),  # Reduced bottom margin
          plot.subtitle = element_text(hjust = 0, size = 10, margin = margin(t = 2, b = 10)),  # Adjust margins
          plot.title.position = "plot",  # Align title to the leftmost edge of the plot
          axis.text.x = element_text(size = 9),  
          axis.text.y = element_text(size = 9, margin = margin(r = 5)),  # Adjust axis spacing
          axis.title.x = element_text(size = 9), 
          
          legend.text = element_text(size = 8), # Set guide label text size
          legend.title = element_text(size = 9), # Set guide title text size
          legend.box.margin = margin(10, 10, 10, 10), 
          legend.background = element_rect(color = "black", linewidth = 0.2, fill = "transparent"),  
          legend.position = c(0.98, 0.05),  # Place the legend in the lower-right corner (inside the plot)
          legend.justification = c(1, 0)
          # legend.justification = c("right", "bottom")  # Adjust alignment to the bottom-right corner, remove legend.position & legend.justification
          # legend.position = "none" # remove legend 
    )+
    geom_hline(yintercept = 0, color = "black") +  # Add a horizontal line at y = 0
    guides(fill = guide_legend(title = "Database", reverse = TRUE))+  # Adjust legend title; Reverse the order of legend items
    
    # Add percentage at position x+1 (right of the bar)
    geom_text(data = subset(data_combined_p_disaggregation, source == "academic"),  # Subset where source is "academic"
              aes(x = count_by_disaggregation_total + 1, 
                  label = scales::percent(perc_of_projects_with_haz_reg_quantified_by_disaggregation / 100, accuracy = 1)), 
              hjust = 0,  # Position the label at the height of the bar
              color = "black", 
              size = 3  # Label formatting
    )
  
  # Show plot
  print(plot_disaggregation_count_perc)
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_disaggregation_count_perc_of_projects_with_haz_reg_quantified_by_disaggregation.png"),
         plot = plot_disaggregation_count_perc, width = 8, height = 3, dpi = 600)
}

# Remove the plot
remove(plot_disaggregation_count_perc)
# remove(data_combined_p_disaggregation)
```

# ########################
# ECONOMIC APPRAISAL 
## 1. Economic appraisal
### 1a. Comparison of acad & non-acad publications
```{r, prepare data_combined_p_econ_appraisal for plot_econ_appraisal}
# Print frequency table
unique(data_combined_p$econ_appraisal)

# 1.1 Create data_combined_p_econ_appraisal & add source_label column
data_combined_p_econ_appraisal <- data_combined_p %>%
  select(econ_appraisal, source) %>%
  mutate(source_label = case_when(
    source == "academic" ~ "Projects in academic publications",
    source == "non-academic" ~ "Projects in non-academic publications",
    TRUE ~ NA_character_  # Add a default case if there are other source values
  )
  )

## 1.2 Summarize rare values in group "Econometric / pricing analysis"
data_combined_p_econ_appraisal <- data_combined_p_econ_appraisal %>%
  mutate(econ_appraisal = case_when(
    econ_appraisal == "Black/Scholes option pricing model" ~ "Econometric / pricing analysis",
    econ_appraisal == "Econometric (statistical) model to estimate the effect of farm programs on land use" ~ "Econometric / pricing analysis",
    TRUE ~ econ_appraisal  # Retain original value for all other cases
  ))

# 2.1 Group and calculate counts and percentages
data_combined_p_econ_appraisal <- data_combined_p_econ_appraisal %>%
  group_by(source_label, source, econ_appraisal) %>%
  summarise(econ_appraisal_count = n(), .groups = "drop") %>%
  group_by(source_label) %>%
  mutate(total_count = sum(econ_appraisal_count),
         percentage = (econ_appraisal_count / total_count) * 100) %>%
  ungroup()

# 2.2 Create the labels for percentages >= 8%
data_combined_p_econ_appraisal <- data_combined_p_econ_appraisal %>%
  mutate(label = ifelse(percentage >= 8, econ_appraisal_count, NA))

unique(data_combined_p_econ_appraisal$econ_appraisal)

# 3. Ensure `econ_appraisal` is a factor with the desired levels
## 3a. Create the vector of categories ordered 
order_econ_appraisal <- c(
  "Benefit appraisal", "Cost-based assessment", "Econometric / pricing analysis", "Multi-criteria analysis", "Economic appraisal conducted, details not reported", "Cost-effectiveness analysis", "Cost-benefit analysis", "Ecosystem service monetary valuation", "No economic appraisal reported")

## 3b. Convert econ_appraisal to factor
data_combined_p_econ_appraisal <- data_combined_p_econ_appraisal %>%
  mutate(econ_appraisal = factor(econ_appraisal, levels = 
                                   order_econ_appraisal))

# 4. Add columns cum_perc and x_axis_bar_center
data_combined_p_econ_appraisal <- data_combined_p_econ_appraisal %>%
  group_by(source_label) %>%
  arrange(desc(econ_appraisal), .by_group = TRUE) %>%  # Reverse the order of econ_appraisal
  mutate(cum_perc = cumsum(percentage),
         x_axis_bar_center = cum_perc - 0.5 * percentage) %>%
  ungroup()


# 5. Update source_label format to include total count
data_combined_p_econ_appraisal <- data_combined_p_econ_appraisal %>%
  mutate(source_label = paste0(source_label, "\n (n = ", total_count, ")"))

# 6. Add econ_appraisal_label with wrapped text of econ_appraisal
data_combined_p_econ_appraisal <- data_combined_p_econ_appraisal %>%
  mutate(econ_appraisal_label = str_wrap(econ_appraisal, width = 30))

# data_combined_p_econ_appraisal$econ_appraisal_label <- data_combined_p_econ_appraisal$econ_appraisal


# data_combined_p_econ_appraisal$econ_appraisal_label <- recode(
#   data_combined_p_econ_appraisal$econ_appraisal_label,
#   "Economic appraisal\nconducted, details\nnot reported" = "Economic appraisal conducted, \ndetails not reported"
# )


# 7. Check levels
levels(data_combined_p_econ_appraisal$econ_appraisal)

# 8. Define colors 
length(order_econ_appraisal) # 9 colors are needed
# custom_colors3_plus1 <- rev(c("#63957A","#8ABD6F","#9BC19F", "grey"))
custom_colors8_plus1 <- rev(c("lightgrey", "#9BC19F","slategray2","#C3DAC3","lightskyblue3","#C1DDAD", "#c7e1e4", "#A0CB88","#9BCBD1"))

color_mapping <- setNames(custom_colors8_plus1, order_econ_appraisal)
length(order_econ_appraisal)

# Remove objects
remove(order_econ_appraisal)
```

```{r, create plot_econ_appraisal_v2}
for (format in names(plot_format)) {
  
  # Plot with labels inside & outside of stacked bars 
  plot_econ_appraisal <- ggplot(data_combined_p_econ_appraisal, 
                                aes(x = source_label, y = percentage, fill = econ_appraisal)) +
    
    # Add geom_text_repel with numeric 'source_label_num' for nudge_x adjustment
    geom_text_repel(
      data = subset(data_combined_p_econ_appraisal, 
                    source_label == "Projects in non-academic publications\n (n = 209)" & percentage <= 5.5),
      aes(x = as.numeric(factor(source_label))+1.45,  # Using numeric 'source_label_num' for label position
          y = rev(x_axis_bar_center),
          label = rev(paste0(econ_appraisal_label, " (", round(percentage, 0), "%)"))),
      size = 2.2,
      nudge_x = 0.5,  # Adjust the horizontal nudging
      hjust = 0,      # Align labels to the left
      direction = "y",
      segment.size = .3,
      segment.alpha = .3,
      segment.linetype = "dotted",
      box.padding = 0.4,
      max.overlaps = 5,
      force_pull = TRUE,
      min.segment.length = 0.5
    ) +
    
    # Bar plot
    geom_bar(stat = "identity", position = "stack") +
    
    # Customize axes and labels
    scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +  
    scale_x_discrete() +  # Use discrete scale for 'source_label'
    scale_fill_manual(values = color_mapping) +  
    guides(fill = "none")+ # don't show fill guides (legend)
    ggtitle("Economic appraisal") +
    labs(x = "", y = "")+
    plot_format[[format]]$theme +  
    theme(
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.025, size = 12), 
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8, margin = margin(r = 5)),
      axis.title.x = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    ) + 
    # Add labels for percentage > 5.5
    geom_text(aes(
      label = ifelse(percentage > 3.5, paste0(econ_appraisal, " (",round(percentage, 0), "%)"), NA)), 
      position = position_stack(vjust = 0.5), na.rm = TRUE, 
      size = 2.2, color = "black")
  
  # Show the plot
  plot_econ_appraisal
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_econ_appraisal_v2.png"), 
         plot = plot_econ_appraisal, width = 9, height = 4, dpi = 300)
}
```

```{r, create plot_econ_appraisal_v3}
# 1. Add source_label_short column
## 1a. Create text for source_label_short
data_combined_p_econ_appraisal <- data_combined_p_econ_appraisal %>%
  mutate(source_label_short = case_when(
    source == "academic" ~ "Academic publications",
    source == "non-academic" ~ "Non-academic publications",
    TRUE ~ NA_character_  # Add a default case if there are other source values
  )
  )

## 1b. Add total count to source_label_short text
data_combined_p_econ_appraisal <- data_combined_p_econ_appraisal %>%
  mutate(source_label_short = paste0(source_label_short, "\n (n = ", total_count, ")"))

# Create plot
for (format in names(plot_format)) {
  
  # Plot with labels inside & outside of stacked bars 
  plot_econ_appraisal <- ggplot(data_combined_p_econ_appraisal, 
                                aes(x = source_label_short, y = percentage, fill = econ_appraisal)) +
    
    # Bar plot
    geom_bar(stat = "identity", position = "stack") +
    
    # Customize axes and labels
    scale_y_continuous(labels = function(x) paste0(round(x, 0), "%")) +  
    scale_x_discrete() +  # Use discrete scale for 'source_label_short'
    scale_fill_manual(values = color_mapping
    ) +  # Reverse color order in legend
    # guides(fill = econ_appraisal)+ # don't show fill guides (legend)
    ggtitle("Economic appraisal") +
    labs(x = "", y = "", fill="Economic appraisal")+
    guides(
      fill = guide_legend(
        override.aes = list(label = "", size = 3), # Control the fill aesthetics
        reverse = FALSE, 
        order = 1, 
        keywidth = 0.8, # Set the width of the legend keys
        keyheight = 0.8 # Set the height of the legend keys
      ))+
    plot_format[[format]]$theme +  
    theme(
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.025, size = 12), 
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8, margin = margin(r = 5)),
      axis.title.x = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    ) +
    # Add labels for percentage > 5
    geom_text(aes(
      label = ifelse(percentage > 3.8, paste0(round(percentage, 0), "%"), NA)), 
      position = position_stack(vjust = 0.5), na.rm = TRUE, 
      size = 2.2, color = "black")
  
  # Show the plot
  plot_econ_appraisal
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_econ_appraisal_v3.png"), 
         plot = plot_econ_appraisal, width = 8, height = 4, dpi = 300)
}
```

### 1b. Joint reporting for acad & non-acad publications
```{r, plot_econ_appraisal}
library(tidyverse)

# Summarize data without 'source' distinction
data_combined_p_econ_appraisal <- data_combined_p %>%
  select(econ_appraisal) %>%
  mutate(econ_appraisal = case_when(
    econ_appraisal == "Black/Scholes option pricing model" ~ "Econometric / pricing analysis",
    econ_appraisal == "Econometric (statistical) model to estimate the effect of farm programs on land use" ~ "Econometric / pricing analysis",
    TRUE ~ econ_appraisal
  )) %>%
  group_by(econ_appraisal) %>%
  summarise(econ_appraisal_count = n(), .groups = "drop") %>%
  mutate(total_count = sum(econ_appraisal_count),
         percentage = (econ_appraisal_count / total_count) * 100) %>%
  arrange(desc(econ_appraisal))

# Define category order
order_econ_appraisal <- c(
  "Benefit appraisal", "Cost-based assessment", "Econometric / pricing analysis", 
  "Multi-criteria analysis", "Economic appraisal conducted, details not reported", 
  "Cost-effectiveness analysis", "Cost-benefit analysis", 
  "Ecosystem service monetary valuation", "No economic appraisal reported"
)

# Convert to factor
data_combined_p_econ_appraisal <- data_combined_p_econ_appraisal %>%
  mutate(econ_appraisal = factor(econ_appraisal, levels = order_econ_appraisal))

# Define colors
custom_colors8_plus1 <- rev(c("lightgrey", "#9BC19F","slategray2","#C3DAC3",
                              "lightskyblue3","#C1DDAD", "#c7e1e4", "#A0CB88","#9BCBD1"))
color_mapping <- setNames(custom_colors8_plus1, order_econ_appraisal)

# Create the plot
plot_econ_appraisal <- ggplot(data_combined_p_econ_appraisal, 
                              aes(x = "", y = percentage, fill = econ_appraisal)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_y_continuous(labels = function(x) paste0(round(x, 0), "%")) +  
  scale_fill_manual(values = color_mapping) +
  ggtitle("Economic Appraisal") +
  labs(x = "", y = "", fill="Economic Appraisal") +
  plot_format[[format]]$theme +  
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, size = 12), 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 8, margin = margin(r = 5)),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  geom_text(aes(
    label = ifelse(percentage > 3.8, paste0(round(percentage, 0), "%"), NA)), 
    position = position_stack(vjust = 0.5), na.rm = TRUE, 
    size = 3, color = "black")

# Show the plot
print(plot_econ_appraisal)

# Save the plot
ggsave("plot_econ_appraisal.png", plot = plot_econ_appraisal, width = 6, height = 4, dpi = 600)

```


## 2. Economic appraisal (details)
### 2a. Comparison of acad & non-acad publications
```{r, prepare data_combined_p_econ_appraisal2 for plot_econ_appraisal2}
# Print frequency table
unique(data_combined_p$econ_appraisal)

# 1.1 Create data_combined_p_econ_appraisal2 & add source_label column
data_combined_p_econ_appraisal2 <- data_combined_p %>%
  select(econ_appraisal, source) %>%
  filter(econ_appraisal != "No economic appraisal reported",) %>%
  mutate(source_label = case_when(
    source == "academic" ~ "Projects in academic publications",
    source == "non-academic" ~ "Projects in non-academic publications",
    TRUE ~ NA_character_  # Add a default case if there are other source values
  )
  )

## 1.2 Summarize rare values in group "Econometric / pricing analysis"
data_combined_p_econ_appraisal2 <- data_combined_p_econ_appraisal2 %>%
  mutate(econ_appraisal = case_when(
    econ_appraisal == "Black/Scholes option pricing model" ~ "Econometric / pricing analysis",
    econ_appraisal == "Econometric (statistical) model to estimate the effect of farm programs on land use" ~ "Econometric / pricing analysis",
    TRUE ~ econ_appraisal  # Retain original value for all other cases
  ))

# 2.1 Group and calculate counts and percentages
data_combined_p_econ_appraisal2 <- data_combined_p_econ_appraisal2 %>%
  group_by(source_label, source, econ_appraisal) %>%
  summarise(econ_appraisal_count = n(), .groups = "drop") %>%
  group_by(source_label) %>%
  mutate(total_count = sum(econ_appraisal_count),
         percentage = (econ_appraisal_count / total_count) * 100) %>%
  ungroup()

# 2.2 Create the labels for percentages >= 8%
data_combined_p_econ_appraisal2 <- data_combined_p_econ_appraisal2 %>%
  mutate(label = ifelse(percentage >= 8, econ_appraisal_count, NA))

unique(data_combined_p_econ_appraisal2$econ_appraisal)

# 3. Ensure `econ_appraisal` is a factor with the desired levels
## 3a. Create the vector of categories ordered 
order_econ_appraisal <- c(
  "Benefit appraisal", "Cost-based assessment", "Econometric / pricing analysis", "Multi-criteria analysis", "Economic appraisal conducted, details not reported", "Cost-effectiveness analysis", "Cost-benefit analysis", "Ecosystem service monetary valuation")

## 3b. Convert econ_appraisal to factor
data_combined_p_econ_appraisal2 <- data_combined_p_econ_appraisal2 %>%
  mutate(econ_appraisal = factor(econ_appraisal, levels = 
                                   order_econ_appraisal))

# 4. Add columns cum_perc and x_axis_bar_center
data_combined_p_econ_appraisal2 <- data_combined_p_econ_appraisal2 %>%
  group_by(source_label) %>%
  arrange(desc(econ_appraisal), .by_group = TRUE) %>%  # Reverse the order of econ_appraisal
  mutate(cum_perc = cumsum(percentage),
         x_axis_bar_center = cum_perc - 0.5 * percentage) %>%
  ungroup()


# 5. Update source_label format to include total count
data_combined_p_econ_appraisal2 <- data_combined_p_econ_appraisal2 %>%
  mutate(source_label = paste0(source_label, "\n (n = ", total_count, ")"))

# 6. Add econ_appraisal_label with wrapped text of econ_appraisal
data_combined_p_econ_appraisal2 <- data_combined_p_econ_appraisal2 %>%
  mutate(econ_appraisal_label = str_wrap(econ_appraisal, width = 30))

# data_combined_p_econ_appraisal2$econ_appraisal_label <- data_combined_p_econ_appraisal2$econ_appraisal

# data_combined_p_econ_appraisal2$econ_appraisal_label <- recode(
#   data_combined_p_econ_appraisal2$econ_appraisal_label,
#   "Economic appraisal\nconducted, details\nnot reported" = "Economic appraisal conducted, \ndetails not reported"
# )


# 7. Check levels
levels(data_combined_p_econ_appraisal2$econ_appraisal)

# 8. Define colors 
length(order_econ_appraisal) # 9 colors are needed
# custom_colors3_plus1 <- rev(c("#63957A","#8ABD6F","#9BC19F", "grey"))
custom_colors8 <- rev(c("#9BC19F","slategray2","#C3DAC3","#c7e1e4","#c3cfbf", "lightskyblue3", "#A0CB88","#9BCBD1"))

color_mapping <- setNames(custom_colors8, order_econ_appraisal)
length(order_econ_appraisal)

# Remove objects
remove(order_econ_appraisal)
```

```{r, create plot_econ_appraisal2_v2}
for (format in names(plot_format)) {
  
  # Plot with labels inside & outside of stacked bars 
  plot_econ_appraisal <- ggplot(data_combined_p_econ_appraisal2, 
                                aes(x = source_label, y = percentage, fill = econ_appraisal)) +
    
    # Add geom_text_repel with numeric 'source_label_num' for nudge_x adjustment
    geom_text_repel(
      data = subset(data_combined_p_econ_appraisal2, 
                    source == "non-academic" & percentage <= 3.5),
      aes(x = as.numeric(factor(source_label))+1.45,  # Using numeric 'source_label_num' for label position
          y = rev(x_axis_bar_center),
          label = rev(paste0(econ_appraisal_label, " (", round(percentage, 0), "%)"))),
      size = 2.2,
      nudge_x = 0.5,  # Adjust the horizontal nudging
      hjust = 0,      # Align labels to the left
      direction = "y",
      segment.size = .3,
      segment.alpha = .3,
      segment.linetype = "dotted",
      box.padding = 0.4,
      max.overlaps = 5,
      force_pull = TRUE,
      min.segment.length = 0.5
    ) +
    
    # Bar plot
    geom_bar(stat = "identity", position = "stack") +
    
    # Customize axes and labels
    scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +  
    scale_x_discrete() +  # Use discrete scale for 'source_label'
    scale_fill_manual(values = color_mapping) +  
    guides(fill = "none")+ # don't show fill guides (legend)
    ggtitle("Economic appraisal") +
    labs(x = "", y = "")+
    plot_format[[format]]$theme +  
    theme(
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.025, size = 12), 
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8, margin = margin(r = 5)),
      axis.title.x = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    ) + 
    # Add labels for percentage > 5.5
    geom_text(aes(
      label = ifelse(percentage > 3.5, paste0(econ_appraisal, " (",round(percentage, 1), "%)"), NA)), 
      position = position_stack(vjust = 0.5), na.rm = TRUE, 
      size = 2, color = "black")
  
  # Show the plot
  plot_econ_appraisal
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_econ_appraisal2_v2.png"), 
         plot = plot_econ_appraisal, width = 9, height = 4, dpi = 300)
}
```

```{r, create plot_econ_appraisal2_v3}
# 1. Add source_label_short column
## 1a. Create text for source_label_short
data_combined_p_econ_appraisal2 <- data_combined_p_econ_appraisal2 %>%
  mutate(source_label_short = case_when(
    source == "academic" ~ "Academic publications",
    source == "non-academic" ~ "Non-academic publications",
    TRUE ~ NA_character_  # Add a default case if there are other source values
  )
  )

## 1b. Add total count to source_label_short text
data_combined_p_econ_appraisal2 <- data_combined_p_econ_appraisal2 %>%
  mutate(source_label_short = paste0(source_label_short, "\n (n = ", total_count, ")"))

# Create plot
for (format in names(plot_format)) {
  
  # Plot with labels inside & outside of stacked bars 
  plot_econ_appraisal <- ggplot(data_combined_p_econ_appraisal2, 
                                aes(x = source_label_short, y = percentage, fill = econ_appraisal)) +
    
    # Bar plot
    geom_bar(stat = "identity", position = "stack") +
    
    # Customize axes and labels
    scale_y_continuous(labels = function(x) paste0(round(x, 0), "%")) +  
    scale_x_discrete() +  # Use discrete scale for 'source_label_short'
    scale_fill_manual(values = color_mapping
    ) +  # Reverse color order in legend
    # guides(fill = econ_appraisal)+ # don't show fill guides (legend)
    ggtitle("Economic appraisal") +
    labs(x = "", y = "", fill="Economic appraisal")+
    guides(
      fill = guide_legend(
        override.aes = list(label = "", size = 3), # Control the fill aesthetics
        reverse = FALSE, 
        order = 1, 
        keywidth = 0.8, # Set the width of the legend keys
        keyheight = 0.8 # Set the height of the legend keys
      ))+
    plot_format[[format]]$theme +  
    theme(
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.025, size = 12), 
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8, margin = margin(r = 5)),
      axis.title.x = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    ) +
    # Add labels for percentage > 5
    geom_text(aes(
      label = ifelse(percentage > 3.8, paste0(econ_appraisal_count, " (", round(percentage, 1), "%)"), NA)), 
      position = position_stack(vjust = 0.5), na.rm = TRUE, 
      size = 2, color = "black")
  
  # Show the plot
  plot_econ_appraisal
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_econ_appraisal2_v3.png"), 
         plot = plot_econ_appraisal, width = 8, height = 4, dpi = 300)
}
```

### 2b. Joint reporting for acad & non-acad publications
```{r, plot_econ_appraisal3}
# Summarize data without 'source' distinction
data_combined_p_econ_appraisal <- data_combined_p %>%
  select(econ_appraisal) %>%
  mutate(econ_appraisal = case_when(
    econ_appraisal == "Black/Scholes option pricing model" ~ "Econometric / pricing analysis",
    econ_appraisal == "Econometric (statistical) model to estimate the effect of farm programs on land use" ~ "Econometric / pricing analysis",
    TRUE ~ econ_appraisal
  )) %>%
  group_by(econ_appraisal) %>%
  summarise(econ_appraisal_count = n(), .groups = "drop") %>%
  mutate(total_count = sum(econ_appraisal_count),
         percentage = (econ_appraisal_count / total_count) * 100) %>%
  arrange(desc(percentage))

# Create a second dataset excluding "No economic appraisal reported"
data_combined_p_econ_appraisal_filtered <- data_combined_p_econ_appraisal %>%
  filter(econ_appraisal != "No economic appraisal reported") %>%
  mutate(total_count = sum(econ_appraisal_count),
         percentage = (econ_appraisal_count / total_count) * 100) %>%
  arrange(desc(percentage))

# Define category order
order_econ_appraisal <- c(
  "Benefit appraisal", "Cost-based assessment", "Econometric / pricing analysis", 
  "Multi-criteria analysis", "Economic appraisal conducted, details not reported", 
  "Cost-effectiveness analysis", "Cost-benefit analysis", 
  "Ecosystem service monetary valuation", "No economic appraisal reported"
)

# Convert to factor
data_combined_p_econ_appraisal <- data_combined_p_econ_appraisal %>%
  mutate(econ_appraisal = factor(econ_appraisal, levels = order_econ_appraisal))

data_combined_p_econ_appraisal_filtered <- data_combined_p_econ_appraisal_filtered %>%
  mutate(econ_appraisal = factor(econ_appraisal, levels = order_econ_appraisal))

# Define colors
custom_colors8_plus1 <- rev(c("lightgrey", "#9BC19F","slategray2","#C3DAC3",
                              "lightskyblue3","#C1DDAD", "#c7e1e4", "#A0CB88","#9BCBD1"))
color_mapping <- setNames(custom_colors8_plus1, order_econ_appraisal)

# Create plot
for (format in names(plot_format)) {
  
  # Create the plot
  plot_econ_appraisal <- ggplot() +
    geom_bar(data = data_combined_p_econ_appraisal, 
             aes(x = "All projects\n(n = 313)", y = percentage, fill = econ_appraisal),
             stat = "identity", position = "stack", width = 0.5) +
    geom_bar(data = data_combined_p_econ_appraisal_filtered, 
             aes(x = "Projects with\neconomic appraisal\n(n=75)", y = percentage, fill = econ_appraisal),
             stat = "identity", position = "stack", width = 0.5) +
    scale_y_continuous(
      limits=c(0,101), 
      expand=c(0,0),
      labels = function(x) paste0(round(x, 0), "%")) +  
    scale_fill_manual(values = color_mapping) +
    # ggtitle("Economic appraisal") +
    labs(x = "", y = "", fill="Economic appraisal") +
    plot_format[[format]]$theme +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.025, size = 12), 
      axis.text.x = element_text(size = 8, margin = margin(t = 5)),
      axis.ticks.x = element_blank(),
      axis.title.x = element_text(size = 9),
      axis.text.y = element_text(size = 8, margin = margin(r = 5)),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    ) +
    geom_text(data = data_combined_p_econ_appraisal, aes(
      x = "All projects\n(n = 313)", y = cumsum(percentage) - (percentage / 2), 
      label = ifelse(percentage > 4, paste0(round(percentage, 0), "%"), NA)), 
      na.rm = TRUE, size = 3, color = "black") +
    geom_text(data = data_combined_p_econ_appraisal_filtered, aes(
      x = "Projects with\neconomic appraisal\n(n=75)", y = cumsum(percentage) - (percentage / 2), 
      label = ifelse(percentage > 4, paste0(round(percentage, 0), "%"), NA)), 
      na.rm = TRUE, size = 3, color = "black")
  
  plot_econ_appraisal
  
  # Save the plot
  ggsave(file.path(getwd(), "plots", format, "evidence/plot_econ_appraisal3.png"), 
         plot = plot_econ_appraisal, width = 8, height = 3.7, dpi = 600)
}
```


