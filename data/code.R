# List of required packages
required_packages <- c(
  "here",
  "tidyverse",
  "dplyr",
  "ggplot2",
  "transformr",
  "plotly",
  "htmlwidgets",
  "readr"
)

# Run the required packages for this visualisation - check if it's installed
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load all packages 
lapply(required_packages, library, character.only = TRUE)

# Load data 
sexuality_dat <- read.csv(here("data/sexual orientation table.xlsx - 1a.csv"))

# Replace empty cells values with "NA"
sexuality_dat <- read.csv("data/sexual orientation table.xlsx - 1a.csv", na.strings = c("", "NA"))

# Load data using relative path 
here("sexual orientation table.xlsx - 1a.csv")
View(sexuality_dat)
head(sexuality_dat)

# Remove blank rows 
new_sex_dat <- sexuality_dat %>% na.omit()

# Remove years 2012 & 2013 as they don't have any data (this is due to an error in the original data set which the author retracted)
# Also remove unneeded columns 
clean_sex_dat <- new_sex_dat %>%
  rename(Sexuality_data = "Table.1a..Sexual.Identity.by.Region.and.Country..United.Kingdom..2012.to.2022..thousands.") %>%
  filter(!if_any(everything(), ~ grepl("\\[x\\]", .))) %>%
  select(c("Sexuality_data", X, X.1, X.4, X.7, X.10, X.13, X.16, X.19, X.22, X.25, X.28, X.31, X.34, X.37, X.40)) 
print(clean_sex_dat)

# Remove notes "[note 15]" from year column
clean_sex_dat2 <- as.data.frame(
  lapply(clean_sex_dat, function(x) gsub("\\[note 15\\]", "", x))
)

# Rename columns - couldn't find a better way to do it, a bit tedious 
clean_sex_dat3 <- clean_sex_dat2 %>% 
  rename(
    "Sexual orientation" = "Sexuality_data",
    "Year" = "X",
    "North East est." = "X.1",
    "North West est." = "X.4",
    "Yorkshire & Humber est." = "X.7",
    "East Midlands est." = "X.10",
    "West Midlands est." = "X.13",
    "East est." = "X.16",
    "London est." = "X.19",
    "South East est." = "X.22",
    "South West est." = "X.25",
    "England est." = "X.28",
    "Wales est." = "X.31",
    "Scotland est." = "X.34",
    "N. Ireland est." = "X.37",
    "UK est." = "X.40"
  ) %>%
  filter(`Sexual orientation` != "Sexual Orientation") %>%
  filter(`Sexual orientation` != "Heterosexual or straight")

# Checking the minimum and maximum years (should be 2014-2022)
range(clean_sex_dat3$Year, na.rm = TRUE)

# Structure of the data 
summary(clean_sex_dat3)

# Convert factors to numeric - long process that the internet told me to do to get all my data as numerical rather than character
# Identify non-numeric columns, excluding "Sexual orientation"
cols_to_convert <- sapply(names(clean_sex_dat3), function(col) {
  col != "Sexual orientation" && !is.numeric(clean_sex_dat3[[col]])
})

# Convert the specified columns to numeric, keeping "Sexual orientation" column unchanged
clean_sex_dat3[, cols_to_convert] <- lapply(clean_sex_dat3[, cols_to_convert, drop = FALSE], function(x) as.numeric(gsub(",", "", x)))

# Identify columns that still have non-numeric data (excluding "Sexual orientation")
non_numeric_cols <- colnames(clean_sex_dat3)[sapply(clean_sex_dat3, function(x) 
  any(is.na(as.numeric(gsub(",", "", as.character(x))))))]
print(non_numeric_cols)

# Filter problematic rows
problematic_rows <- clean_sex_dat3 %>%
  filter(is.na(as.numeric(gsub(",", "", `North East est.`))))
print(problematic_rows)

# Remove columns that are completely NA
clean_sex_dat3 <- clean_sex_dat3 %>%
  select(where(~ !all(is.na(.))))

# Minimum and maximum values for all columns
clean_sex_dat3 %>%
  summarise(across(where(is.numeric), list(min = ~min(., na.rm = TRUE), max = ~max(., na.rm = TRUE))))

# Barplot
# Total UK estimates excluded 
# Had to match columns with 'est'and create new columns for 'Region' and 'Estimates'
long_reg_data <- clean_sex_dat3 %>%
  pivot_longer(
    cols = matches("est\\."), 
    names_to = "Region",       
    values_to = "Estimate"   
  ) %>%
  filter(Region != "UK est.") %>%  # Exclude "UK est."
  mutate(Estimate = as.numeric(gsub(",", "", Estimate))) 
head(long_reg_data)

barplot_dat <- long_reg_data %>%
  filter(!Region == "England est.")

# the plot 
ggplot(barplot_dat, aes(x = Year, y = Estimate, fill = `Sexual orientation`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Region, scales = "fixed") +
  labs(
    title = "Queer Sexual Orientation by Region Over Time",
    x = "Year",
    y = "Population Estimate (in thousands)",
    fill = "Sexual Orientation"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Bar plot 2
# Reshape the data and filter for just UK est. to produce BARCHART
long_data <- clean_sex_dat3 %>%
  pivot_longer(
    cols = -c(`Sexual orientation`, Year),  
    names_to = "Region",
    values_to = "Estimate"
  )
uk_data <- long_data %>%
  filter(Region %in% c("Sexual orientation", "UK est."))

# Clean the estimate column (convert to numeric and handle NA)
uk_data <- uk_data %>%
  mutate(
    Estimate = as.numeric(gsub(",", "", Estimate)),  # Remove commas and convert to numeric
    Estimate = ifelse(is.na(Estimate), 0, Estimate) # Replace NA with 0
  )

# Adjust the plot to produce barchart 
imp_barchart <- ggplot(uk_data, aes(x = factor(Year), y = Estimate, fill = `Sexual orientation`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "UK Estimates of Queer Sexuality 2014-2022",
       x = "Year",
       y = "Population Estimate (in thousands)",
       fill = "Sexual Orientation") +
  scale_y_continuous(
    limits = c(0, max(uk_data$Estimate, na.rm = TRUE)),  # Automatically adjust y-axis
    breaks = seq(0, max(uk_data$Estimate, na.rm = TRUE), by = 100)
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7))
print(imp_barchart)

# Filter UK-specific data for the line graph
uk_line_data <- long_data %>%
  filter(Region == "UK est.") %>%  # Focus on "UK est."
  mutate(
    Estimate = as.numeric(gsub(",", "", Estimate))  # Ensure Estimate is numeric
  )

# Line graph
# Create the line graph
line_graph <- ggplot(uk_line_data, aes(x = Year, y = Estimate, color = `Sexual orientation`, group = `Sexual orientation`)) +
  geom_line(size = 1) +  # Use lines to connect points
  geom_point(size = 2) +  # Add points for emphasis
  labs(
    title = "Trends in Sexual Orientation in the UK (2014-2022)",
    x = "Year",
    y = "Population Estimate (in thousands)",
    color = "Sexual Orientation"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.position = "right"
  )

# Print the line graph
print(line_graph)

# Save the line plot
ggsave(
  filename = "plots/multi_line_plot.png",
  plot = line_graph,
  width = 12,
  height = 8,
  dpi = 300
)

# More detailed line plot
# Create totals by region 
totals_by_region <- clean_sex_dat3 %>%
  pivot_longer(
    cols = matches("est\\."), 
    names_to = "Region",
    values_to = "Estimate"
  ) %>%
  filter(!is.na(Estimate)) %>% # Exclude NA values
  mutate(
    Estimate = as.numeric(gsub(",", "", Estimate)), # Convert to numeric
    Year = as.numeric(Year) # Ensure Year is numeric
  ) %>%
  group_by(Region, Year) %>%
  summarise(Total = sum(Estimate, na.rm = TRUE), .groups = "drop")
# Build the plot
line_plot <- totals_by_region %>%
  filter(Region != "England est.") %>% # remove as all the sub-regions make up this value
  filter(Region != "UK est.") %>% 
  ggplot(aes(x = Year, y = Total, color = Region, group = Region)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(2014, 2022, by = 1)
  ) +
  labs(
    title = "Trends in Population of Queer Sexuality by UK Region (2014-2022)",
    x = "Year",
    y = "Population Estimate (thousands)",
    color = "Region"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
line_plot 

# Save the line plot
ggsave(
  filename = "plots/line_plot_uk_queer_population.png",
  plot = line_plot,
  width = 12,
  height = 8,
  dpi = 300
)

# Complex line plot
# Prepare the data for visualization
long_sex_data <- clean_sex_dat3 %>%
  pivot_longer(
    cols = matches("est\\."),
    names_to = "Region",
    values_to = "Estimate"
  ) %>%
  filter(!is.na(Estimate) & Region != "UK est." & Region != "England est.") %>%
  mutate(
    Estimate = as.numeric(Estimate),
    Year = as.numeric(Year)
  )
# Line plot with facets for each Sexual Orientation
complex_line_plot <- ggplot(long_sex_data, aes(x = Year, y = Estimate, color = Region, group = interaction(Region, `Sexual orientation`))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2014, 2022, by = 1)) +
  labs(
    title = "Trends in Queer Sexual Orientation by Region (2014-2022)",
    x = "Year",
    y = "Population Estimate (thousands)",
    color = "Region"
  ) +
  facet_wrap(~ `Sexual orientation`, scales = "free_y") +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Print the plot
print(complex_line_plot)

# Save the line plot
ggsave(
  filename = "plots/complex_line_plot.png",  
  plot = complex_line_plot,                        
  width = 12, # Width of the image in inches
  height = 8, # Height of the image in inches
  dpi = 300 # Resolution (dots per inch)
)

# Interactive plot 
# Ensure England and UK estimates are excluded 
totals_by_region <- totals_by_region %>%
  filter(!Region %in% c("England est.", "UK est."))

# Adjust the colours of the plot 
dynamic_colors <- colorRampPalette(c("blue", "green", "red", "purple"))(
  length(unique(totals_by_region$Region))
)
interactive_line_plot <- plot_ly(
  data = totals_by_region,
  x = ~Year,
  y = ~Total,
  color = ~Region,
  type = 'scatter',
  mode = 'lines+markers',
  text = ~paste(
    "Region: ", Region,
    "<br>Year: ", Year,
    "<br>Population: ", Total, "k"
  ),
  hoverinfo = 'text',
  colors = dynamic_colors  # Use dynamically generated colors
) %>%
  layout(
    title = "Trends in Population of Queer Sexual Orientation by Region (2014-2022)",
    xaxis = list(title = "Year", tickvals = seq(2014, 2022, 1)),
    yaxis = list(title = "Population Estimate (thousands)"),
    legend = list(orientation = "h", x = 0.1, y = -0.2)
  )


# Improved interactive plot 
# The interactive plot
interactive_line_plot

# Save the interactive plot as an HTML file
htmlwidgets::saveWidget(
  widget = interactive_line_plot,
  file = "plots/interactive_line_plot.html",
  selfcontained = TRUE                  
)

# Ensure data is consistent 
long_data <- long_data %>%
  mutate(`Sexual orientation` = str_trim(`Sexual orientation`))
# Aggregate data to calculate total population and breakdown 
aggregated_data <- long_data %>%
  group_by(Region, Year) %>%
  filter(Region != "England est.") %>%
  filter(Region != "UK est.") %>%
  summarise(
    Total = sum(Estimate, na.rm = TRUE),
    GayLesbian = sum(Estimate[`Sexual orientation` == "Gay or lesbian"], na.rm = TRUE),
    Bisexual = sum(Estimate[`Sexual orientation` == "Bisexual"], na.rm = TRUE),
    Other = sum(Estimate[`Sexual orientation` == "Other"], na.rm = TRUE),
    DontKnowRefuse = sum(Estimate[`Sexual orientation` == "Don't know or refuse"], na.rm = TRUE),
  ) %>%
  mutate(
    Breakdown = paste(
      "Gay/Lesbian: ", GayLesbian, "k",
      "<br>Bisexual: ", Bisexual, "k",
      "<br>Other: ", Other, "k",
      "<br>Don't Know/Refuse: ", DontKnowRefuse, "k"
    )
  ) %>%
  ungroup()

# Create the interactive plot
# Adjust the colours of the plot 
dynamic_colors <- colorRampPalette(c("blue", "green", "red", "purple"))(
  length(unique(aggregated_data$Region))
)
# The plot
comp_interactive_line_plot <- plot_ly(
  data = aggregated_data,
  x = ~Year,
  y = ~Total,  # Total population for y-axis
  color = ~Region,
  type = 'scatter',
  mode = 'lines+markers',
  text = ~paste(
    "Region: ", Region,
    "<br>Year: ", Year,
    "<br>Total Population: ", Total, "k",
    "<br>", Breakdown
  ),
  hoverinfo = 'text',
  colors = dynamic_colors
) %>%
  layout(
    title = "Trends in Population of Queer Sexual Orientation by Region (2014-2022)",
    xaxis = list(title = "Year", tickvals = seq(2014, 2022, 1)),
    yaxis = list(title = "Population Estimate (thousands)"),
    legend = list(orientation = "h", x = 0.1, y = -0.2)
  )

# Display the interactive plot
comp_interactive_line_plot

# Save the interactive plot as an HTML file
htmlwidgets::saveWidget(
  widget = comp_interactive_line_plot,
  file = "plots/comp_interactive_line_plot.html",
  selfcontained = TRUE
)

