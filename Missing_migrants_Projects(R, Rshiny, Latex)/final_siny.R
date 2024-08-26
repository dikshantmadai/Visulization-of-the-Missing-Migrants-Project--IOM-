library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)
library(maps)
library(tidyr)
library(lubridate)
library(readr)
library(plotly)

# Read the CSV file 
df <- read_csv("migrants_clean.csv")


bbc_style <- function() {
  font <- "Helvetica"
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(family = font, size = 22, face = "bold", color = "#222222", hjust=0.5),
    plot.subtitle = ggplot2::element_text(family = font, size = 18, margin = ggplot2::margin(9, 0, 9, 0)),
    plot.caption = ggplot2::element_blank(),
    
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family = font, size = 10, color = "#222222", hjust = 0.70),
    
    axis.title = ggplot2::element_blank(),  # Removed duplicate definition
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

# Function to insert newline characters after every 6 words
insert_newlines <- function(text, words_per_line = 6) {
  words <- unlist(strsplit(text, " "))
  lines <- lapply(seq(1, length(words), words_per_line), function(i) {
    paste(words[i:min(i + words_per_line - 1, length(words))], collapse = " ")
  })
  return(paste(lines, collapse = "\n"))
}

# Data Preparation
df <- separate(df, Coordinates, into = c("latitude", "longitude"), sep = ", ", convert = TRUE)
df$latitude <- as.numeric(df$latitude)
df$longitude <- as.numeric(df$longitude)
df$date <- as.Date(paste0(df$`Incident Year`, "-01-01"))
df$`Location of Death` <- sapply(df$`Location of Death`, insert_newlines)

# Split multiple countries into separate columns
df_separated <- df %>%
  separate(`Country of Origin`, into = paste0("Country", 1:10), sep = ",", fill = "right")
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


# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Migration Incident Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabset == 'worldmap'",
        sliderInput("year_filter", "Filter by Year:",
                    min = min(df$`Incident Year`),
                    max = max(df$`Incident Year`),
                    value = c(min(df$`Incident Year`), max(df$`Incident Year`)))
      ),
      conditionalPanel(
        condition = "input.tabset == 'region_trend'",
        selectInput("region_filter", "Select Region:", 
                    choices = unique(df$`Region of Incident`))
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabset",
                  tabPanel("Correlation Heatmap", value = "heatmap", plotlyOutput("heatmap")),
                  tabPanel("World Map with Incidents", value = "worldmap", plotlyOutput("worldmap")),
                  tabPanel("Regions with Highest Missing Migrants", value = "region_bar", plotlyOutput("region_bar")),
                  tabPanel("Missing Migrants Trend by Region", value = "region_trend", plotlyOutput("region_trend")),
                  tabPanel("Total Missing Migrants Over Time", value = "total_missing_trend", plotlyOutput("total_missing_trend")),
                  tabPanel("Survivors Trend Over Time", value = "survivors_trend", plotlyOutput("survivors_trend")),
                  tabPanel("Gender Ratio of Missing Migrants", value = "gender_ratio", plotlyOutput("gender_ratio")),
                  tabPanel("Impact of Location of Death", value = "location_impact", plotlyOutput("location_impact")),
                  tabPanel("Migration Routes by Missing Persons", value = "migration_routes", plotlyOutput("migration_routes")),
                  tabPanel("Top Countries of Origin by Deaths", value = "country_origin", plotlyOutput("country_origin"))
      )
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  
  # Filter data based on user selections
  filtered_df <- reactive({
    df %>%
      filter(`Incident Year` >= input$year_filter[1] & `Incident Year` <= input$year_filter[2])
  })
  
  
  output$heatmap <- renderPlotly({
    numeric_df <- df %>% select_if(is.numeric)
    correlation_matrix <- cor(numeric_df, use = "complete.obs")
    melted_correlation <- melt(correlation_matrix)
    
    # Create the plotly plot with specified width and height
    plot_ly(width = 1000, height = 700) %>%
      add_trace(
        data = melted_correlation,
        x = ~Var1,
        y = ~Var2,
        z = ~value,
        type = "heatmap",
        colorbar = list(title = "Correlation"),
        showscale = TRUE
      ) %>%
      layout(
        title = "Correlation Heatmap",
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        font = list(size = 12, family = "Arial, sans-serif"),
        margin = list(l = 150, r = 50, b = 50, t = 50),  # Adjust margins as needed
        plot_bgcolor = "rgba(0, 0, 0, 0)"  # Set plot background color to transparent
      ) %>%
      config(displayModeBar = FALSE)  # Optionally hide the mode bar if needed
  })
  
  # World Map with Incidents
  output$worldmap <- renderPlotly({
    world <- map_data("world")
    
    map_plot <- ggplot() +
      geom_polygon(data = world, aes(x = long, y = lat, group = group), color = "green", fill = "lightyellow", size = 0.1, alpha=0.7) +
      geom_point(data = filtered_df(), aes(x = longitude, y = latitude, size = `Total Number of Dead and Missing`), color = "red", alpha = 0.3) +
      labs(title = "Incident Locations and Casualties",
           caption = "Source: Incident Data") +
      scale_color_gradient(low = "blue", high = "red") +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, hjust = 0.5))
    
    ggplotly(map_plot)
  })
  
  # Regions with Highest Missing Migrants
  output$region_bar <- renderPlotly({
    region_data <- df %>%
      group_by(`Region of Incident`) %>%
      summarise(total_missing = sum(`Total Number of Dead and Missing`, na.rm = TRUE)) %>%
      arrange(desc(total_missing))
    ggplotly(
      ggplot(region_data, aes(x = reorder(`Region of Incident`, total_missing), y = total_missing)) +
        geom_bar(stat = "identity", fill = "darkred") +
        coord_flip() + 
        geom_text(aes(label = total_missing), hjust = 0.5, vjust = -0.2, color = "pink") +
        labs(title = "Regions with the Highest Number of Missing Migrants",
             x = NULL, 
             y = "Total Missing Migrants") +
        bbc_style() + 
        theme(
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),  
          axis.title.x = element_blank(),  
          axis.text.x = element_blank(),   
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(face = "bold",margin = margin(r = -25))  
        )
    )
  })
  
  # Missing Migrants Trend by Region
  output$region_trend <- renderPlotly({
    region_trends <- df %>%
      filter(`Region of Incident` == input$region_filter) %>%
      group_by(year = `Incident Year`) %>%
      summarize(total_missing = sum(`Total Number of Dead and Missing`, na.rm = TRUE))
    ggplotly(
      ggplot(region_trends, aes(x = year, y = total_missing)) +
        geom_line(color = "green", size = 1) +
        geom_point(color = "green") +
        geom_text(aes(label = total_missing), vjust = -0.5,hjust = -0.5, size = 3) +
        labs(title = paste("Trend in Missing Migrants for", input$region_filter),
             x = "Year", y = "Total Missing") +
        bbc_style()
    )
  })
  
  # Total Missing Migrants Over Time
  output$total_missing_trend <- renderPlotly({
    total_missing_per_year <- df %>%
      group_by(year = year(date)) %>%
      summarize(total_missing = sum(`Total Number of Dead and Missing`, na.rm = TRUE))
    ggplotly(
      ggplot(total_missing_per_year, aes(x = year, y = total_missing)) +
        geom_line(color = "green") +
        geom_point(color = "green") +
        geom_text(aes(label = total_missing), vjust = -0.5, size = 3) +  
        labs(title = "Total Number of Missing Migrants Over Time",
             x = "Year", y = "Total Missing") +
        bbc_style()
    )
  })
  
  # Survivors Trend Over Time
  output$survivors_trend <- renderPlotly({
    survivors_trend <- df %>%
      group_by(`Incident Year`) %>%
      summarize(total_survivors = sum(`Number of Survivors`, na.rm = TRUE))
    ggplotly(
      ggplot(survivors_trend, aes(x = `Incident Year`, y = total_survivors)) +
        geom_line(color = "green") +
        geom_point(color = "green") +
        geom_text(aes(label = total_survivors), vjust = -0.5, size = 3) +
        labs(
          title = "Trend in the Number of Survivors Over the Years",
          x = "Year",
          y = "Total Survivors"
        ) +
        bbc_style()
    )
  })
  
  # Gender Ratio of Missing Migrants
  output$gender_ratio <- renderPlotly({
    gender_ratio <- df %>%
      group_by(`Region of Incident`) %>%
      summarize(total_females = sum(`Number of Females`, na.rm = TRUE),
                total_males = sum(`Number of Males`, na.rm = TRUE)) %>%
      gather(key = "Gender", value = "Count", total_females, total_males)
    
    ggplotly(
      ggplot(gender_ratio, aes(x = `Region of Incident`, y = Count, fill = Gender)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(
          title = "Gender Ratio of Missing Migrants Across Different Regions",
          x = "Region",
          y = "Count",
          fill = ""  # Empty fill label
        ) +
        scale_fill_manual(values = c("pink", "skyblue"),
                          name = "Gender",  # Legend title
                          labels = c("Total Female", "Total Male")) +  # Legend labels
        bbc_style() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    )
  })
  
  # Impact of Location of Death
  output$location_impact <- renderPlotly({
    top_locations <- df %>%
      group_by(`Location of Death`) %>%
      summarise(total_missing = sum(`Total Number of Dead and Missing`)) %>%
      top_n(8, total_missing)
    ggplotly(
      ggplot(top_locations, aes(x = reorder(`Location of Death`, total_missing), y = total_missing)) +
        geom_bar(stat = "identity", fill = "blue") +
        geom_text(aes(label = total_missing), hjust = 1.3, color = "white") + # Adjust hjust for better positioning
        coord_flip() +
        labs(
          title = "Top 8 Impact of Location of Death on Number of Missing Migrants",
          x = "Location of Death",
          y = "Total Number of Dead and Missing"
        ) +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.major = element_blank(),     
          panel.grid.minor = element_blank(),     
          axis.title.x = element_blank(),         
          axis.text.x = element_blank(),          
          axis.ticks.x = element_blank(),         
          axis.text.y = element_text(face = "bold", margin = margin(r = -25), color = "black"),  
          axis.title.y = element_blank(),         
          panel.background = element_rect(fill = "white", color = NA)  
        )
    )
  })
  
  # Migration Routes by Missing Persons
  output$migration_routes <- renderPlotly({
    route_comparison <- df %>%
      group_by(`Migration Route`) %>%
      summarize(total_missing = sum(`Total Number of Dead and Missing`, na.rm = TRUE)) %>%
      filter(!is.na(`Migration Route`))
    route_comparison <- route_comparison[order(route_comparison$total_missing, decreasing = TRUE), ]
    
    ggplotly(
      ggplot(route_comparison[1:8, ], aes(x = reorder(`Migration Route`, total_missing), y = total_missing)) +
        geom_bar(stat = "identity", fill = "purple") +
        geom_text(aes(label = total_missing), hjust = -0.2, color = "white") + # Adjust hjust for better positioning
        coord_flip() +
        labs(title = "Top 8 Migration Routes by Number of Missing Persons",
             x = "Route", y = "Total Missing") +
        bbc_style() +
        theme(
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),  
          axis.title.x = element_blank(),  
          axis.text.x = element_blank(),   
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(face = "bold", margin = margin(r = -25))
        )
    )
  })
  
  # Top Countries of Origin by Deaths
  output$country_origin <- renderPlotly({
    top_countries <- df_filtered %>% 
      gather(key = "Country_Index", value = "Country", Country1:Country10, na.rm = TRUE) %>%
      group_by(Country) %>% 
      summarise(total_deaths = sum(`Number of Dead`, na.rm = TRUE)) %>% 
      arrange(desc(total_deaths)) %>% 
      head(10)
    ggplotly(
      ggplot(top_countries, aes(x = reorder(Country, total_deaths), y = total_deaths)) +
        geom_bar(stat = "identity", fill = "darkred") +
        geom_text(aes(label = total_deaths), color = "white", hjust = 0.5, vjust = 1.5)
      + # Adjust hjust for better positioning
        coord_flip() +
        labs(title = "Top 10 Countries of Origin by Number of Migrant Deaths",
             x = "Country of Origin",
             y = "Total Deaths") +
        bbc_style() +
        theme(
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),  
          axis.title.x = element_blank(),  
          axis.text.x = element_blank(),   
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(face = "bold",margin = margin(r = -25))  
        )
    )
  })
}


# Run the Shiny app
shinyApp(ui = ui, server = server) 