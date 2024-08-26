
#Data Visulization 
library(ggplot2)
library(reshape2)
library(dplyr)
library(maps)
library(tidyr)
library(dplyr)
library(ggplot2)
library(bbplot)
library(lubridate)
library(readr)






bbc_style <- function() {
  font <- "Helvetica"
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(family = font, size = 15, face = "bold", color = "#222222", hjust = 0.5),
    plot.subtitle = ggplot2::element_text(family = font, size = 18, margin = ggplot2::margin(9, 0, 9, 0)),
    plot.caption = ggplot2::element_blank(),
    
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family = font, size = 10, color = "#222222", hjust = 0.70),
    
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family = font, size = 11, color = "#222222"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = 20, hjust = 0)
  )
}

#Loading Dataset
library(readr)
df <- read_csv("cleandataset/migrants_clean.csv")


str(df)

# Dealing qith missing values
# Get the number of rows and columns
num_rows <- nrow(df)
num_cols <- ncol(df)
cat("Number of rows:", num_rows, "\n")
cat("Number of columns:", num_cols, "\n")


# Calculate missing values for each column
missing_values <- sapply(df, function(x) sum(is.na(x)))
print(missing_values)



#1). Heatmap of Correlation Matrix

# Filter numeric columns
numeric_df <- df %>% select_if(is.numeric)

# Compute correlation matrix
correlation_matrix <- cor(numeric_df, use = "complete.obs")

# Melt the correlation matrix for plotting
melted_correlation <- melt(correlation_matrix)

# Customize appearance
heatmap_plot <- ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(plot.title = ggplot2::element_text( size = 22, face = "bold", color = "#222222"),
        axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1,face="bold"),
        axis.text.y = element_text(face="bold",size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.justification = c(1, 0),
        
        legend.direction = "vertical") +
  labs(title = "Correlation Heatmap",
       x = "",
       y = "")

# Print the plot
print(heatmap_plot)


#map


#2) Which regions have the highest number of migrant deaths and disappearances, and how do these numbers vary over time?"
# How do these numbers vary over time, particularly in the most affected regions?

#==>
# Splitting 'Coordinates' column into 'latitude' and 'longitude'
df <- separate(df, Coordinates, into = c("latitude", "longitude"), sep = ", ", convert = TRUE)

# Convert latitude and longitude to numeric
df$latitude <- as.numeric(df$latitude)
df$longitude <- as.numeric(df$longitude)

# Download world map data
world_map <- map_data("world")

# Creating the scatterplot on a world map
ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(long, lat, map_id = region),
           color = "green", fill = "lightyellow") +
  
  geom_point(data = df,
             aes(x = longitude, y = latitude, 
                 color = `Total Number of Dead and Missing`, 
                 size = `Total Number of Dead and Missing`)) +
  
  # Title and axis labels
  labs(
    title = "Migration Incidents on World Map",
    x = "Longitude",
    y = "Latitude",
    color = "Total Number of Dead and Missing",
    size = "Total Number of Dead and Missing"
  ) +
  
  scale_color_gradient(low = "blue", high = "red") +
  
  bbc_style() +
  theme(
    text = element_text(family = "noto"),
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# Create a date column for easier grouping by year
df$date <- as.Date(paste0(df$`Incident Year`, "-01-01"))

# 1. Grouping by region to find total missing migrants
region_data <- df %>%
  group_by(`Region of Incident`) %>%
  summarise(total_missing = sum(`Total Number of Dead and Missing`, na.rm = TRUE)) %>%
  arrange(desc(total_missing))

# Assuming region_data is your data frame with the columns 'Region of Incident' and 'total_missing'
# Create the bar chart
ggplot(region_data, aes(x = reorder(`Region of Incident`, total_missing), y = total_missing)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +  # Flip coordinates for better readability
  geom_text(aes(label = total_missing), hjust = -0.09, color = "black") +  # Add text labels on bars
  labs(
    title = "Regions with the Highest Number of Missing Migrants",
    x = NULL,  # Remove x-axis label
    y = "Total Missing Migrants"
  ) +
  bbc_style() +  # Apply BBC style
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.text.x = element_blank(),   # Remove x-axis text
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(face = "bold",margin = margin(r = -25))  # Make y-axis text bold
    
  )


# Filtering for the top 5 most affected regions
top_regions <- region_data %>%
  top_n(5) %>%
  pull(`Region of Incident`)

# Filtering the main dataset to include only the top regions
df_top_regions <- df %>% 
  filter(`Region of Incident` %in% top_regions)

# Calculating yearly trends for each of the top regions
region_trends <- df_top_regions %>%
  group_by(year = `Incident Year`, `Region of Incident`) %>%
  summarize(total_missing = sum(`Total Number of Dead and Missing`, na.rm = TRUE))

# Plotting the trends over time
ggplot(region_trends, aes(x = year, y = total_missing, color = `Region of Incident`)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Trends in Missing Migrants for the Most Affected Regions",
       x = "Year", y = "Total Missing", color = "Region") +
  bbc_style()



# 2. What insights can we gain from analyzing the total number of missing migrants each year?
total_missing_per_year <- df %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%  # Convert date to numeric year
  group_by(year) %>%
  summarize(total_missing = sum(`Total Number of Dead and Missing`, na.rm = TRUE))

# Plotting total number of missing migrants over time
ggplot(total_missing_per_year, aes(x = year, y = total_missing)) +
  geom_line(color = "green") +
  geom_point() +
  geom_text(aes(label = total_missing), vjust = -0.5, size = 3) + 
  labs(title = "Total Number of Missing Migrants Over Time",
       x = "Year", y = "Total Missing") +
  bbc_style()

#3.What is the trend in the number of survivors over the years?
# Group by Incident year and calculate the sum of Number of Survivors
survivors_trend <- df %>%
  group_by(`Incident Year`) %>%
  summarize(total_survivors = sum(`Number of Survivors`, na.rm = TRUE))

# Plotting the trend of survivors
ggplot(survivors_trend, aes(x = `Incident Year`, y = total_survivors)) +
  geom_line(color = "green") +
  geom_point() +
  geom_text(aes(label = total_survivors), vjust = -0.5, size = 3) +
  labs(
    title = "Trend in the Number of Survivors Over the Years",
    x = "Year",
    y = "Total Survivors"
  ) +
  bbc_style()

#4. What is the gender ratio of missing migrants across different regions
# Group by Region of Incident and calculate the sum of Number of Females and Males
gender_ratio <- df %>%
  group_by(`Region of Incident`) %>%
  summarize(total_females = sum(`Number of Females`, na.rm = TRUE),
            total_males = sum(`Number of Males`, na.rm = TRUE)) %>%
  gather(key = "Gender", value = "Count", total_females, total_males)

ggplot2::ggplot(gender_ratio, aes(x = `Region of Incident`, y = Count, fill = Gender)) +
  ggplot2::geom_bar(stat = "identity", position = position_dodge(width = 0.8)) + 
  ggplot2::geom_text(aes(label = Count, y = Count, group = Gender), 
                     position = position_dodge(width = 0.8), 
                     vjust = -0.7, hjust = 0.7, size = 2.5, color = "black") + 
  ggplot2::labs(
    title = "Gender Ratio of Missing Migrants Across Different Regions",
    x = "Region",
    y = "Count",
    fill = "Gender" 
  ) +
  bbc_style() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_fill_discrete(labels = c("Total Females", "Total Males")) 

#5, How does the number of missing migrants vary across different locations of death? 
#Are certain locations significantly associated with higher numbers of missing migrants?
library(dplyr)
library(ggplot2)
library(stringr)

# Filter the top 8 locations of death
top_locations <- df %>%
  group_by(`Location of Death`) %>%
  summarise(total_missing = sum(`Total Number of Dead and Missing`)) %>%
  top_n(8, total_missing)

# Function to wrap text to one line for each bar
wrap_text <- function(text, width = 40) {
  str_wrap(text, width = width)
}

# Apply the function to mutate the dataframe
top_locations <- top_locations %>%
  mutate(`Location of Death` = sapply(`Location of Death`, wrap_text))

# Plotting the impact of location of death for the top 8 locations
ggplot2::ggplot(top_locations, aes(x = reorder(`Location of Death`, total_missing), y = total_missing)) +
  ggplot2::geom_bar(stat = "identity", fill = "darkred") +
  ggplot2::geom_text(aes(label = total_missing), hjust = 1.3, color = "white", size = 5) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Top 8 Impact of Location of Death on Number of Missing Migrants",
    x = "Location of Death",
    y = "Total Number of Dead and Missing"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    panel.grid.major = ggplot2::element_blank(),    
    panel.grid.minor = ggplot2::element_blank(),    
    axis.title.x = ggplot2::element_blank(),        
    axis.text.x = ggplot2::element_blank(),         
    axis.ticks.x = ggplot2::element_blank(),        
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = -15), color = "black"), 
    axis.title.y = ggplot2::element_blank(),        # Remove y-axis title
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    # Add space between the y axis and the bars
    axis.text.y.margin = margin(r = -1110) # Adjust the 'r' value as needed
  )


#6) "Which migration route has the highest incidence of missing persons, and how does it compare to others?"
# Group by Migration route


route_comparison <- df %>%
  group_by(`Migration Route`) %>%
  summarize(total_missing = sum(`Total Number of Dead and Missing`, na.rm = TRUE)) %>%
  filter(!is.na(`Migration Route`))


library(ggplot2)
library(ggthemes)

# Calculate total missing per route
route_comparison <- df %>%
  group_by(`Migration Route`) %>%
  summarize(total_missing = sum(`Total Number of Dead and Missing`, na.rm = TRUE)) %>%
  filter(!is.na(`Migration Route`))

# Sort routes by total missing in descending order
route_comparison <- route_comparison[order(route_comparison$total_missing, decreasing = TRUE), ]
# Create the first bar chart for the top 8 routes
ggplot2::ggplot(route_comparison[1:8, ], 
                aes(x = reorder(stringr::str_wrap(`Migration Route`, width = 20), total_missing), 
                    y = total_missing)) +
  ggplot2::geom_bar(stat = "identity", fill = "lightgreen", width = 0.8) + # Adjust width for spacing
  ggplot2::geom_text(aes(label = total_missing), hjust = 1.3, color = "black", face="bold") +
  ggplot2::coord_flip() +
  ggplot2::labs(title = "Top Migration Routes by Number of Missing Persons",
                x = "Route", y = "Total Missing") +
  bbc_style() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(), 
    panel.grid.minor = ggplot2::element_blank(), 
    axis.title.x = ggplot2::element_blank(), 
    axis.text.x = ggplot2::element_blank(),  
    axis.ticks.x = ggplot2::element_blank(),
    text = ggplot2::element_text(family = "noto"),
    plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
    axis.text.y = ggplot2::element_text(face = "bold",margin = ggplot2::margin(r = -7))
  )

library(ggplot2)
library(stringr)

ggplot2::ggplot(route_comparison[9:nrow(route_comparison), ], 
                aes(x = reorder(stringr::str_wrap(`Migration Route`, width = 20), total_missing), 
                    y = total_missing)) +
  ggplot2::geom_bar(stat = "identity", fill = "lightgreen", width = 0.7) +
  ggplot2::geom_text(aes(label = total_missing), hjust = -0.1, color = "black", size = 3) + 
  ggplot2::coord_flip() +
  ggplot2::labs(title = "More Migration Routes by Number of Missing Persons",
                x = "Route", y = "Total Missing") +
  bbc_style() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(), 
    panel.grid.minor = ggplot2::element_blank(), 
    axis.title.x = ggplot2::element_blank(), 
    axis.text.x = ggplot2::element_blank(),  
    axis.ticks.x = ggplot2::element_blank(),
    text = ggplot2::element_text(family = "noto"),
    plot.title = ggplot2::element_text(size = 15, hjust = 0.5),
    axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = -8), size=8, face = "bold")
  ) 


#7 Top 10 countries of origin by number of migrant deaths
library(ggplot2)
library(dplyr)
library(ggthemes)
library(tidyr)
library(stringr)


# Split multiple countries into separate columns
df_separated <- df %>%
  separate(`Country of Origin`, into = paste0("Country", 1:10), sep = ",", fill = "right")

# Exclude Unknown/Mixed
df_filtered <- df_separated %>%
  filter(!(`Country1` %in% c("Unknown", "Mixed")) & 
           !(`Country2` %in% c("Unknown", "Mixed")) &
           !(`Country3` %in% c("Unknown", "Mixed")) & 
           !(`Country4` %in% c("Unknown", "Mixed")) &
           !(`Country5` %in% c("Unknown", "Mixed")) & 
           !(`Country6` %in% c("Unknown", "Mixed")) &
           !(`Country7` %in% c("Unknown", "Mixed")) & 
           !(`Country8` %in% c("Unknown", "Mixed")) & 
           !(`Country9` %in% c("Unknown", "Mixed")) & 
           !(`Country10` %in% c("Unknown", "Mixed")))

# Calculate top 10 countries of origin by number of deaths 
top_countries <- df_filtered %>% 
  gather(key = "Country_Index", value = "Country", Country1:Country10, na.rm = TRUE) %>%
  group_by(Country) %>% 
  summarise(total_deaths = sum(`Number of Dead`, na.rm = TRUE)) %>% 
  arrange(desc(total_deaths)) %>% 
  head(10)

ggplot2::ggplot(top_countries, aes(x = reorder(Country, total_deaths), y = total_deaths)) +
  ggplot2::geom_bar(stat = "identity", fill = "darkred") +
  ggplot2::geom_text(aes(label = total_deaths), hjust = 2, color = "white",) +
  ggplot2::coord_flip() +
  ggplot2::labs(title = "Top 10 Countries of Origin by Number of Migrant Deaths",
                x = "Country of Origin",
                y = "Total Deaths") +
  bbc_style() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(), 
    panel.grid.minor = ggplot2::element_blank(), # Remove minor grid lines
    axis.title.x = ggplot2::element_blank(), # Remove x-axis title
    axis.text.x = ggplot2::element_blank(),  # Remove x-axis text
    axis.ticks.x = ggplot2::element_blank(),
    text = ggplot2::element_text(family = "noto"),
    plot.title = ggplot2::element_text(size = 15, hjust = 0.5),
    axis.text.y = ggplot2::element_text(face = "bold",margin = ggplot2::margin(r = -15)) # Make y-axis text bold
  )


