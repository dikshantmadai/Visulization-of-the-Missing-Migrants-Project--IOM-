library(tidyverse)
library(readr)
migrants <- read_csv("unclean_dataset/Global Missing Migrants Dataset.csv")
View(migrants)
str(migrants)


mis_values <- colSums(is.na(migrants))
print(mis_values)
View(migrants)


#Region of origin and country of origin have different number of missing values. Maybe we can fill in the NaN values in region of origin by country of origin.
origin_missing <- migrants[rowSums(is.na(migrants[, c("Country of Origin", "Region of Origin")])) > 0, ]
head(origin_missing, 30)


#I checked the corresponding region and country of origin. Now, found out that some of the countries are there but no region. So, if we know the country we can fill the region of origin with corresponding region instead of NaN values. First I want to check the unique values for region of origin column
unique_regions <- unique(migrants$`Region of Origin`)
unique_regions



head(migrants)

library(dplyr)

# Mapping dictionary for filling NaN values
region_mapping <- c(
  "Chad" = "Middle Africa",
  "Sudan" = "Northern Africa",
  "Egypt" = "Northern Africa",
  "Syrian Arab Republic" = "Western Asia",
  "Nigeria" = "Western Africa",
  "Mauritania" = "Western Africa",
  "Unknown" = "Unknown",
  'Bangladesh' = 'Southern Asia',
  'Afghanistan,Iraq,Syrian Arab Republic' = 'Mixed'
)

# Assuming 'migrants' is a data frame
migrants <- migrants %>%
  mutate(
    `Region of Origin` = case_when(
      is.na(`Region of Origin`) & `Country of Origin` %in% names(region_mapping) ~ region_mapping[`Country of Origin`], 
      TRUE ~ `Region of Origin`
    )
  ) 

head(migrants, 10)


mis_values <- colSums(is.na(migrants))
print(mis_values)




#We still have some missing values here. Let's check them again.
head(migrants)

# Identify rows with missing values in 'Region of Origin' or 'Country of Origin'
origin2 <- is.na(migrants$`Region of Origin`) | is.na(migrants$`Country of Origin`)

# Subset the data frame to include only those rows with missing values
origin_missing2 <- migrants[origin2, ]

# Display the first few rows of the resulting data frame
head(origin_missing2)




#NaN in Country of origin I can fill in with Uknown

migrants$`Country of Origin`[is.na(migrants$`Country of Origin`)] <- "Uknown"
head(migrants)




mis_values <- colSums(is.na(migrants))
print(mis_values)


#****Now Let's check number of dead column.#


head(migrants)
dim(migrants)
str(migrants)
mis_values <- colSums(is.na(migrants))
print(mis_values)
unique(migrants$`Number of Dead`)

#After checking dataset I think we can use 0 for NaN in number of dead column since it is the number of confirmed death of migrants therefore we may assume NaN means 0

migrants$`Number of Dead`[is.na(migrants$`Number of Dead`)] <- 0

# Convert columns to appropriate data types (similar to infer_objects)
migrants <- lapply(migrants, function(x) {
  if (is.character(x)) {
    as.factor(x)
  } else if (is.numeric(x)) {
    as.numeric(x)
  } else if (is.logical(x)) {
    as.logical(x)
  } else {
    x
  }
})

migrants <- data.frame(migrants) 
mis_values <- colSums(is.na(migrants))
print(mis_values)




unique(migrants$`Migration.route`)


#Now let's check Migration route variable. There seem to be a great amount of NaN values.


origin_missing <- migrants[is.na(migrants$`Migration.route`), ]
head(origin_missing, 30)



unique(migrants$`Migration.route`)

#It seems that there is nothing much we can do here so I will just fill in NaN values with Uknown.


# 1. Add "Uknown" to the factor levels:
levels(migrants$Migration.route) <- c(levels(migrants$Migration.route), "Uknown")

# 2. Replace NA values with "Uknown":
migrants$Migration.route[is.na(migrants$Migration.route)] <- "Uknown"

# 3. Display the first few rows:
head(migrants)

migrants <- data.frame(migrants) 
mis_values <- colSums(is.na(migrants))
print(mis_values)



#I will check Coordinates.
origin_missing <- migrants[is.na(migrants$Coordinates), ]
head(origin_missing, 30)

# 1. Add "Uknown" to the factor levels:
levels(migrants$Coordinates) <- c(levels(migrants$Coordinates), "Uknown")

# 2. Replace NA values with "Uknown":
migrants$Coordinates[is.na(migrants$Coordinates)] <- "Uknown" 

# 3. Display the first few rows:
head(migrants)

migrants <- data.frame(migrants) 
mis_values <- colSums(is.na(migrants))
print(mis_values)




#I will check UNSD.Geographical.Grouping.
migrants$UNSD.Geographical.Grouping <- factor(migrants$UNSD.Geographical.Grouping, 
                                              levels = c(levels(migrants$UNSD.Geographical.Grouping), 
                                                         "North America"))
migrants$UNSD.Geographical.Grouping[is.na(migrants$UNSD.Geographical.Grouping)] <- "North America"


#Checking coordinates with UNSD showed that the region is Northern America so I fill NA with Northern America.


migrants <- data.frame(migrants) 
mis_values <- colSums(is.na(migrants))
print(mis_values)

origin_missing <- migrants[is.na(migrants$Information.Source), ]
head(origin_missing, 30)

migrants <- data.frame(migrants) 
mis_values <- colSums(is.na(migrants))
print(mis_values)


info_source_counts <- migrants %>%
  group_by(`Information.Source`) %>%  # Group by the column (assuming it exists)
  summarise(count = n()) %>%           # Count occurrences for each group
  arrange(desc(count))                # Arrange by count (descending)
print(info_source_counts)              # Print the results

# Check if "Unknown" already exists as a level
if (!("Unknown" %in% levels(migrants$Information.Source))) {
  # Add "Unknown" as a level if it's not there
  migrants$Information.Source <- factor(migrants$Information.Source, levels = c(levels(migrants$Information.Source), "Unknown"))
}

# Use logical indexing to assign "Unknown" to missing values
migrants$Information.Source[is.na(migrants$Information.Source)] <- "Unknown"

migrants <- data.frame(migrants) 
mis_values <- colSums(is.na(migrants))
print(mis_values)





#Now we can deal with duplicates
#c.Identify duplicate rows
duplicate_rows <- migrants[duplicated(migrants), ]

# Print duplicate rows
head(duplicate_rows)

#Check for duplicated rows
duplicated_rows <- sum(duplicated(migrants))

# Print the total number of duplicated rows
print(paste("Total duplicated rows:", duplicated_rows))
# Remove duplicate rows
df <- distinct(migrants)
#After removing the duplicated rows again Check for duplicated rows 
duplicated_rows <- sum(duplicated(df))

# Print the total number of duplicated rows
print(paste("Total duplicated rows:", duplicated_rows))

# Display the number of rows and columns
dims <- dim(df)
cat("Number of rows:", dims[1], "\n")
cat("Number of columns:", dims[2], "\n")



# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Assuming 'df' is your dataframe

# Select numerical columns
numerical_cols <- select_if(migrants, is.numeric)

# Create a list to store plots
plots <- list()

# Loop through each numerical column and create box plot
for(col in colnames(numerical_cols)) {
  plot <- ggplot(migrants, aes(x = "", y = !!sym(col))) +
    geom_boxplot() +
    labs(title = paste("Box Plot of", col), x = "", y = col)
  
  plots[[col]] <- plot
}

# Combine all box plots
grid.arrange(grobs = plots, ncol = 3, width = 2000, height = 15)



library(dplyr)

names(migrants)[names(migrants)=="Incident.year"] <- "Incident.Year"
names(migrants)[names(migrants)=="Migration.route"] <- "Migration.Route"
names(migrants)[names(migrants)=="Location.of.death"] <- "Location.of.Death"


str(migrants)
unique(migrants$Number.of.Dead)
migrants$`Number.of.Dead` <- as.integer(migrants$`Number.of.Dead`)
typeof(migrants$Number.of.Dead)

negative_values <- subset(migrants, 
                          migrants$`Minimum.Estimated.Number.of.Missing` < 0 |
                            migrants$`Total.Number.of.Dead.and.Missing` < 0 |
                            migrants$`Number.of.Survivors` < 0)
head(negative_values, 10)





negative_estimate_rows <- migrants[migrants$`Minimum.Estimated.Number.of.Missing` < 0, ]
head(negative_estimate_rows, 10)


#So we have 4 rows with negative values in minimum estimated number of missing. Let's convert them to positives.
migrants$`Minimum.Estimated.Number.of.Missing` <- abs(migrants$`Minimum.Estimated.Number.of.Missing`)
unique(migrants$Minimum.Estimated.Number.of.Missing)



#Now that we got rid of negative values in this column let's check the others again.
unique(migrants$Total.Number.of.Dead.and.Missing)


unique(migrants$Number.of.Survivors)

#There are not negative values in total number of dead and missing but there are missing values in number of survivors. Let's check it

negative_estimate_rows <- migrants[migrants$`Number.of.Survivors` < 0, ]
head(negative_estimate_rows, 10)

#I think I got where the problem comes from. We get number of survivors by subtracting total number of dead and missing from estimate and sometimes real number is bigger than estimate so we got - 1 etc. I think it makes sense then to simply convert them to 0 when negative since negative probably means nobody survived.


migrants$`Number.of.Survivors`[migrants$`Number.of.Survivors` < 0] <- 0
unique(migrants$Number.of.Survivors)
unique(migrants$Number.of.Survivors)
unique(migrants$Number.of.Survivors)
unique_counts <- table(migrants$Incident.Year)

# Print the results
print(unique_counts)


# Calculate missing values for each column
missing_values <- sapply(df, function(x) sum(is.na(x)))
print(missing_values)
