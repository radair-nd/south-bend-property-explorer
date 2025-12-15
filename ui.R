netrface
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Load needed libraries
library(tidyverse)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(sf)
library(lubridate)
library(bslib)
library(viridis)
library(plotly)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Load and clean data
abandon <- st_read("Abandoned_Property_Parcels/Abandoned_Property_Parcels.shp")
abandon$Date_of_Ou <- as.Date(abandon$Date_of_Ou)
abandon <- abandon %>% filter(!is.na(Date_of_Ou), !is.na(Outcome_St))
abandon_sf <- abandon
parks <- read_csv("Parks_Locations_and_Features.csv")
school_sf <- st_read("South_Bend_School_Zones/South_Bend_School_Zones.shp")
mgt_data <- read_csv("mgt_gip_data.csv")

parks_sf <- parks %>%
  filter(!is.na(Lat), !is.na(Lon)) %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = FALSE)
# Amenity columns (all except identifiers/coords)
amenity_cols <- setdiff(names(parks_sf),
                        c("Park_Name","Park_Type","Zip_Code","Address","Lat","Lon","geometry"))
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# User Inerface
ui <- fluidPage(
  theme = bs_theme(bootswatch = "cyborg"), # adding in dark theme for contrast due to amount of white space in charts
  titlePanel(
    div(
      "South Bend Abandon Property Outcome Explorer",
      style = "font-size: 24px; font-weight: bold;"
    )
  ),
  # Adding separate tabs to ensure functions like four separate pages 
  tabsetPanel(
    id = "tabs",
    # Tab 1 GIP Model Dashboard Tab (Andrew Benedum)
    tabPanel("GIP Model",
             fluidPage(
               fluidRow(
                 column(width = 3,
                        helpText("Hedgeye-style GIP quadrant visualization"),
                        
                        # Year filter
                        sliderInput(
                          inputId = "year_filter",
                          label = "Select Year Range:",
                          min = 2015,
                          max = 2018,
                          value = c(2015, 2018),
                          step = 1,
                          sep = ""
                        ),
                        
                        # Quarter range selector
                        sliderInput(
                          inputId = "quarter_range",
                          label = "Select Quarter Range:",
                          min = 1,
                          max = 44,
                          value = c(1, 44),
                          step = 1
                        )
                 ),
                 column(width = 9,
                        fluidRow(
                          column(width = 6,
                                 h6("S&P 500 Price", align = "center"),
                                 plotOutput("gip_plot", height = 300)
                          ),
                          column(width = 6,
                                 h6("Quarterly Returns", align = "center"),
                                 plotOutput("quarterly_returns_plot", height = 300)
                          ),
                        ),
                        fluidRow(
                          column(width = 6,
                                 h6("Quadrant Distribution", align = "center"),
                                 plotOutput("quadrant_distribution_plot", height = 300)
                          ),
                          column(width = 6,
                                 h6("Avg Return by Quadrant", align = "center"),
                                 plotOutput("avg_return_by_quadrant", height = 300)
                          )
                        )
                 )
               )
             )
    ),
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # Tab 2 General (Robert Adair)
    tabPanel("General",
             fluidPage(
               fluidRow(
                 column(width = 3,
                        selectInput("outcome", "Select Outcome:",
                                    choices = sort(unique(abandon_sf$Outcome_St)),
                                    selected = "Demolished"),
                        selectInput("zip", "Select ZIP Code:",
                                    choices = sort(unique(abandon_sf$Zip_Code)),
                                    selected = "46616",
                                    multiple = TRUE),
                        sliderInput("yearRange", "Select Year Range:",
                                    min = year(min(abandon_sf$Date_of_Ou, na.rm = TRUE)),
                                    max = year(max(abandon_sf$Date_of_Ou, na.rm = TRUE)),
                                    value = c(2015, 2018), step = 1, sep = "")
                 ),
                 column(width = 9,
                        fluidRow(
                          column(width = 6,
                                 h4("Summary Table"),
                                 div(style = "height:300px; overflow-y:auto;",
                                     tableOutput("summaryTable")
                                 )
                          ),
                          column(width = 6,
                                 h4("Timeline of Property Outcomes"),
                                 plotOutput("outcomePlot", height = 300)
                          )
                        ),
                        
                        fluidRow(
                          column(width = 6,
                                 h4("Structure Types Involved"),
                                 plotlyOutput("structurePlot", height = 300)
                          ),
                          column(width = 6,
                                 h4("Map of Selected Properties"),
                                 leafletOutput("mapPlot", height = 300)
                         )
                        )
                       )
                      )
                     )
    ),
   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # Tab 3 Parks and Amenities (Nicole Hernandez)
    tabPanel("Parks and Amenities",
             fluidPage(
               fluidRow(
                 column(width = 3,
                        selectInput("zipGeo", "ZIP Code (parks & properties):",
                                    choices = sort(unique(c(abandon_sf$Zip_Code, parks_sf$Zip_Code))),
                                    multiple = TRUE),
                        checkboxGroupInput(
                          inputId  = "parkType",
                          label    = "Select Park Type:",
                          choices  = sort(unique(parks$Park_Type)),
                          selected = unique(parks$Park_Type), 
                          inline   = FALSE                     
                        )
                      ),
                 
                 column(width = 9,
                        fluidRow(
                          column(width = 6,
                                 h6("Amenities Frequency"),
                                 plotOutput("amenityFreq", height = 300)
                          ),
                          column(width = 6,
                                 h6("Park Type"),
                                 plotOutput("parkTypePlot", height = 300)
                          )
                        ),
                        
                        fluidRow(
                          column(width = 6,
                                 h6("Distribution of Amenity Richness"),
                                 plotOutput("amenityRichness", height = 300)
                          ),
                          column(width = 6,
                                 h6("Counts of parks and properties"),
                                 leafletOutput("geoMap", height = 300)
                       )
                      )
                     )
                    )
                   )
    ),
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # # Tab 4 Properties and School Districts (Nicole Ho)
    tabPanel("Properties and School Districts",
             fluidPage(
               fluidRow(
                 column(width = 3,
                        selectInput("school_type", "School Type", choices = c("All", "Elementary", "Middle", "High")),
                        selectInput("zip_filter", "ZIP Code", choices = sort(unique(abandon_sf$Zip_Code)), multiple = TRUE),
                        checkboxInput("show_abandoned", "Show Abandoned Properties", TRUE)
                 ),
                 
                 column(width = 9,
                        fluidRow(
                          column(width = 6,
                                 h6("Properties in School Districts Map"),
                                 leafletOutput("school_map", height = 300)
                          ),
                          column(width = 6,
                                 h6("Property Density Chart"),
                                 plotlyOutput("abandon_density_chart", height = 300)
                          )
                        ),
                        
                        fluidRow(
                          column(width = 12,
                                 h6("Property Outcomes by District"),
                                 plotlyOutput("outcomes_by_district", height = 300)
                          )
                         )
                        )
                       )
                      )
                     )
                    )
                   )
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Server 
server <- function(input, output, session) {
  # Tab 1 <<<<<<<<<<<<<
  # Load the GIP data
  mgt_data <- read_csv("mgt_gip_data.csv")
  
  # Prepare data once
  mgt_gip_data <- mgt_data %>%
    mutate(
      quarter_index = as.numeric(factor(quarter, levels = unique(quarter))),
      year = as.numeric(substr(quarter, 1, 4)),
      quadrant_name = case_when(
        quadrant == 1 ~ "Q1: Goldilocks",
        quadrant == 2 ~ "Q2: Reflation",
        quadrant == 3 ~ "Q3: Stagflation",
        quadrant == 4 ~ "Q4: Deflation"
      )
    )
  
  quad_colors <- c(
    "Q1: Goldilocks"  = "#90EE90",
    "Q2: Reflation"   = "#FFFF99",
    "Q3: Stagflation" = "#FFB6C1",
    "Q4: Deflation"   = "#ADD8E6"
  )
  
  # Reactive filtered data based on year and quarter range
  filtered_gip_data <- reactive({
    mgt_gip_data %>%
      filter(
        year >= input$year_filter[1],
        year <= input$year_filter[2],
        quarter_index >= input$quarter_range[1],
        quarter_index <= input$quarter_range[2]
      )
  })
  
  # Main GIP Plot
  output$gip_plot <- renderPlot({
    
    ggplot(filtered_gip_data(), aes(x = quarter, y = sp500_price)) +
      
      # Background shading
      geom_rect(
        aes(
          xmin = quarter_index - 0.5,
          xmax = quarter_index + 0.5,
          ymin = -Inf,
          ymax = Inf,
          fill = quadrant_name
        ),
        alpha = 0.4
      ) +
      
      # Price line
      geom_line(aes(group = 1), linewidth = 1.2, color = "black") +
      geom_point(size = 2, color = "black") +
      
      scale_fill_manual(values = quad_colors, name = "Economic Quadrant") +
      
      labs(
        title = "S&P 500 Price Across Economic Quadrants",
        subtitle = "Color-coded by GIP Model quadrant",
        x = "Quarter",
        y = "S&P 500 Price",
        caption = "Data: FRED (GDP, CPI, S&P 500)"
      ) +
      
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        legend.position = "bottom"
      )
    
  })
  
  # Quarterly Returns Chart
  output$quarterly_returns_plot <- renderPlot({
    
    filtered_gip_data() %>%
      filter(!is.na(quarterly_return)) %>%
      ggplot(aes(x = quarter, y = quarterly_return, fill = quadrant_name)) +
      geom_col(color = "black", size = 0.3) +
      scale_fill_manual(values = quad_colors, name = "Economic Quadrant") +
      labs(
        title = "Quarterly Returns by Economic Quadrant",
        x = "Quarter",
        y = "Quarterly Return (%)",
        caption = "Positive returns indicate market gains"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        legend.position = "bottom"
      )
    
  })
  
  # Quadrant Distribution Chart
  output$quadrant_distribution_plot <- renderPlot({
    
    filtered_gip_data() %>%
      count(quadrant_name) %>%
      ggplot(aes(x = reorder(quadrant_name, -n), y = n, fill = quadrant_name)) +
      geom_col(color = "black", size = 0.5) +
      scale_fill_manual(values = quad_colors, guide = "none") +
      labs(
        title = "Quadrant Distribution in Selected Period",
        x = "Economic Quadrant",
        y = "Number of Quarters",
        caption = "Shows frequency of each economic state"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    
  })
  
  # Average Returns by Quadrant
  output$avg_return_by_quadrant <- renderPlot({
    
    filtered_gip_data() %>%
      filter(!is.na(quarterly_return)) %>%
      group_by(quadrant_name) %>%
      summarise(avg_return = mean(quarterly_return, na.rm = TRUE), .groups = 'drop') %>%
      ggplot(aes(x = reorder(quadrant_name, avg_return), y = avg_return, fill = quadrant_name)) +
      geom_col(color = "black", size = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      scale_fill_manual(values = quad_colors, guide = "none") +
      labs(
        title = "Average Quarterly Return by Quadrant",
        x = "Economic Quadrant",
        y = "Average Return (%)",
        caption = "Higher values indicate better market performance"
      ) +
      coord_flip() +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none"
      )
    
  }) 
  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  # Tab 2
  # Page 2 filters
  filtered <- reactive({
    abandon_sf %>%
      filter(
        Outcome_St == input$outcome,
        Zip_Code %in% input$zip,
        year(Date_of_Ou) >= input$yearRange[1],
        year(Date_of_Ou) <= input$yearRange[2]
      )

  })
  # Summary table
  output$summaryTable <- renderTable({
    st_drop_geometry(filtered()) %>%
      count(Program_De, Code_Enfor) %>%
      rename(`Program Description` = Program_De,
             `Code Enforcement Status` = Code_Enfor,
             Count = n)
  })
  
# Timeline line plot
  output$outcomePlot <- renderPlot({
    filtered() %>%
      count(Date_of_Ou) %>%
      ggplot(aes(x = Date_of_Ou, y = n)) +
      geom_line(color = "#FFAA01", size = 0.9) +
      geom_point() +
      theme_minimal() +
      labs(title = paste("Timeline of", input$outcome, "Properties"),
           x = "Date", y = "Count")
  })
  # Structure type histogram using plotly to make interactive
  output$structurePlot <- renderPlotly({
    p <- filtered() %>%
      count(Structures) %>%
      ggplot(aes(x = reorder(Structures, n), y = n)) +
      geom_bar(stat = "identity", fill = "#0074cc") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Types of Structures Involved",
           x = "Structure Type", y = "Count")
    
    ggplotly(p)
  })
  # Map of properties to show reference of where they are for general reference. More detail will be added in later tabs
  output$mapPlot <- renderLeaflet({
    leaflet(data = filtered()) %>%
      addTiles() %>%
      addPolygons(
        popup = ~paste(Address_Nu, Street_Nam, Suffix),
        color = "#01018d", weight = 2, fillOpacity = 0.5
      )
  })
  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  # Tab 3
  # Filters
  props_geo <- reactive({
    dat <- abandon_sf
    if (!is.null(input$zipGeo) && length(input$zipGeo) > 0) {
      dat <- dat %>% filter(Zip_Code %in% input$zipGeo)
    }
    dat
  })
  
  parks_geo <- reactive({
    dat <- parks_sf
    # ZIP filter
    if (!is.null(input$zipGeo) && length(input$zipGeo) > 0) {
      dat <- dat %>% filter(Zip_Code %in% input$zipGeo)
    }
    # Park type filter
    if (!is.null(input$parkType) && length(input$parkType) > 0) {
      dat <- dat %>% filter(Park_Type %in% input$parkType)
    }
    
    dat
  })
  
# Setting up color palette that sensitive to color blindness
  pal <- colorFactor(
    palette = magma(length(parks$Park_Type)), 
    domain  = parks$Park_Type
  )
  # Map of abandoned properties and parks
  output$geoMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      # Abandoned parcels layer
      addPolygons(
        data        = props_geo(),
        color       = "navy",
        weight      = 2,
        fillOpacity = 0.3,
        group       = "Abandoned Parcels",
        popup       = ~paste0("<b>", Address_Nu, " ", Street_Nam, Suffix, "</b>")
      ) %>%
      # Parks layer with fillColor mapped by Park_Type
      addCircleMarkers(
        data        = parks_geo(),
        lng         = ~Lon,
        lat         = ~Lat,
        radius      = 4,
        color       = "black",           # marker border
        weight      = 1,
        fillColor   = ~pal(Park_Type),   # fill mapped to park type
        fillOpacity = 0.8,
        popup       = ~paste0("<b>", Park_Name, "</b><br/>", Park_Type),
        group       = "Parks"
      ) %>%
      # Layers control
      addLayersControl(
        overlayGroups = c("Abandoned Parcels", "Parks"),
        options       = layersControlOptions(collapsed = FALSE)
      ) %>%
      # Legend for Park_Type
      addLegend(
        position = "bottomleft",
        pal      = pal,
        values   = parks_geo()$Park_Type,
        title    = "Park Type"
      ) %>%
      # measurement tool
      addMeasure(
        position            = "topright",
        primaryLengthUnit   = "meters",
        secondaryLengthUnit = "miles",
        activeColor         = "red",
        completedColor      = "green"
      )
  })

  # Amenity frequency horizontal bar chart
  output$amenityFreq <- renderPlot({
    prks <- parks_geo()
    df <- prks %>%
      st_drop_geometry() %>%
      summarise(across(all_of(amenity_cols), ~ sum(. == 1, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "Amenity", values_to = "Count") %>%
      filter(Count > 0)
    
    ggplot(df, aes(x = reorder(Amenity, Count), y = Count)) +
      geom_col(fill = "#2323FF") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Frequency of Amenities in Filtered Parks",
           x = "Amenity", y = "Number of Parks")
  })
  
  # Park type breakdown pie chart
  output$parkTypePlot <- renderPlot({
    prks <- parks_geo()
    df <- prks %>%
      st_drop_geometry() %>%
      count(Park_Type)

    ggplot(df, aes(x = "", y = n, fill = Park_Type)) +
      geom_col(width = 1) +
      coord_polar("y") +
      scale_fill_viridis_d(option = "turbo") +
      theme_void() +
      labs(title = "Park Type Breakdown") +
      theme(legend.position = "right")
  })

  # Amenity richness histogram
  output$amenityRichness <- renderPlot({
    prks <- parks_geo()
    df <- prks %>%
      st_drop_geometry() %>%
      mutate(AmenityCount = rowSums(select(., all_of(amenity_cols)) == 1, na.rm = TRUE))
    
    ggplot(df, aes(x = AmenityCount)) +
      geom_histogram(binwidth = 1, fill = "#2323FF", color = "black") +
      theme_minimal() +
      labs(title = "Distribution of Amenity Richness per Park",
           x = "Number of Amenities", y = "Count of Parks")
  })
  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  # Tab 4
  # Filtered abandoned properties
  filtered_abandoned <- reactive({
    data <- abandon_sf
    if (!is.null(input$zip_filter) && length(input$zip_filter) > 0) {
      data <- data %>% filter(as.character(Zip_Code) %in% as.character(input$zip_filter))
    }
    data
  })
  
  # Convert abandoned polygons to points of center of shape for mapping
  abandon_centroids <- reactive({
    filtered_abandoned() %>%
      st_centroid() %>%
      mutate(
        lon = st_coordinates(.)[,1],
        lat = st_coordinates(.)[,2]
      )
  })
  
  # Filtered schools by type and zipcode
  filtered_schools <- reactive({
    data <- school_sf
    
    # Filter by school type
    if (input$school_type != "All") {
      data <- data %>% filter(Type == input$school_type)
    }
    
    # Filter by zipcode 
    if (!is.null(input$zip_filter) && length(input$zip_filter) > 0) {
      # Get the spatial union of abandoned properties in selected zipcodes
      zip_bounds <- filtered_abandoned() %>% st_union()
      # Only keep school districts that intersect with those zipcodes
      data <- data[st_intersects(data, zip_bounds, sparse = FALSE),]
    }
    
    data
  })

  # Spatial join abandoned properties within school district polygons to get a zip code semi match
  abandoned_in_district <- reactive({
    st_join(filtered_abandoned(), filtered_schools(), join = st_intersects)
  })
  
  # Map of school districts shapes with abandon properties points
  output$school_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = filtered_schools(),
        color = "blue", 
        weight = 2, 
        fillOpacity = 0.3,
        popup = ~paste(SchoolName, "<br>", Type)
      ) %>%
      {
        if (isTRUE(input$show_abandoned)) {
          addCircleMarkers(
            .,
            data = abandon_centroids(),
            lng = ~lon, 
            lat = ~lat,
            radius = 4, 
            color = "orange",
            popup = ~paste0(Address_Nu, " ", Direction, " ", Street_Nam)
          )
        } else .
      }
  })
  
  output$abandon_density_chart <- renderPlotly({
    abandoned_in_district() %>%
      st_drop_geometry() %>%
      count(SchoolName) %>%
      mutate(
        SchoolAbbrev = substr(SchoolName, 1, 20)  # Abbreviating the first 20 characters for space management
      ) %>%
      plot_ly(
        x = ~SchoolAbbrev,
        y = ~n,
        type = "bar",
        name = "Abandoned Properties",
        marker = list(color = "#0074cc"),
        hovertemplate = "<b>%{customdata}</b><br>Count: %{y}<extra></extra>",
        customdata = ~SchoolName
      ) %>%
      layout(
        xaxis = list(title = "", tickangle = -45, tickfont = list(size = 8)),
        yaxis = list(title = "Count"),
        margin = list(l = 40, r = 20, b = 100, t = 30),
        height = 300
      )
  })
  # Property outcomes by school district interactive stacked bar chart
  output$outcomes_by_district <- renderPlotly({
    abandoned_in_district() %>%
      st_drop_geometry() %>%
      count(SchoolName, Outcome_St) %>%
      plot_ly(
        x = ~SchoolName,
        y = ~n,
        color = ~Outcome_St,
        type = "bar",
        hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y}<extra></extra>"
      ) %>%
      layout(
        xaxis = list(title = "School District", tickangle = -45),
        yaxis = list(title = "Count"),
        barmode = "stack",
        margin = list(b = 120),
        height = 350,
        legend = list(orientation = "v", x = 1.02, y = 1)
      )
  })
}
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Running the shiny app
# Specifying height to ensure full screen output when rendering
shinyApp(ui = ui, server = server, options = list(height = 1080))
