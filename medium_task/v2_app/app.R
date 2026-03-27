library(shiny)
library(leaflet)
library(tidyverse)
library(ecotourism)
library(shinythemes)
library(shinyWidgets)

# load all four organism datasets
data(glowworms)
data(gouldian_finch)
data(manta_rays)
data(orchids)

# combine all into one dataframe
all_organisms <- bind_rows(
  glowworms |> mutate(organism = "Glowworm"),
  gouldian_finch |> mutate(organism = "Gouldian Finch"),
  manta_rays |> mutate(organism = "Manta Ray"),
  orchids |> mutate(organism = "Orchid")
) |>
  filter(!is.na(obs_lat), !is.na(obs_lon))  # remove missing coordinates

# colour palette for each organism
organism_colours <- c(
  "Glowworm" = "gold",        
  "Gouldian Finch" = "darkorange",  
  "Manta Ray" = "blue",       
  "Orchid" = "purple"           
)

funfacts <- c(
  "Glowworms use bioluminescence to attract prey.",
  "Gouldian Finches are nicknamed the 'rainbow bird' for their vibrant plumage.",
  "Manta Rays can have wingspans over 7 meters.",
  "Some orchids mimic insects to attract pollinators.",
  "Australia is home to more than 800 bird species, many found nowhere else.",
    "Kangaroos and emus cannot walk backward — that’s why they’re on Australia’s coat of arms.",
    "Over 80% of Australia’s mammals, reptiles, and frogs are found nowhere else in the world.",
    "The platypus is one of the only mammals that lays eggs.",
    "Koalas sleep up to 18–20 hours a day due to their low-energy eucalyptus diet.",
    "Wombats have cube-shaped poop, which helps mark territory without rolling away.",
    "There are more than one million feral camels roaming the Australian outback.",
    "The Great Barrier Reef is home to over 1,500 species of fish and 400 types of coral.",
    "Echidnas and platypuses are the only monotremes (egg-laying mammals) on Earth.",
    "Australia is home to the longest fence in the world — the Dingo Fence, stretching over 5,600 km.",
    "Cassowaries are considered one of the most dangerous birds due to their powerful legs and sharp claws."
  
)


data(weather)
data(top_stations)

organism_weather <- top_stations |>
  left_join(weather, by = "ws_id") |>
  mutate(organism = case_when(
    organism == "orchids"        ~ "Orchid",
    organism == "gouldian_finch" ~ "Gouldian Finch",
    organism == "manta_rays"     ~ "Manta Ray",
    organism == "glowworms"      ~ "Glowworm"
  ))

# UI and server so shiny app loads
ui <- fluidPage(
  theme = shinytheme("flatly"),  # clean light theme as base
  
  tags$head(
    tags$style(HTML("
    
    body {
      background-color: #FAF7F2;
      font-family: 'Georgia', serif;
      color: #2C3E50;
    }
    
    .title-bar {
      background: linear-gradient(135deg, #2C3E50 0%, #3D5166 100%);
      color: #FAF7F2;
      padding: 22px 30px;
      margin-bottom: 0px;
      text-align: center;
      letter-spacing: 1px;
    }
    .title-bar h2 {
      font-size: 28px;
      font-weight: bold;
      margin-bottom: 4px;
      color: #FAF7F2;
    }
    .title-bar p {
      opacity: 0.7;
      font-size: 13px;
      margin: 0;
      letter-spacing: 2px;
      text-transform: uppercase;
    }
    
    .nav-tabs {
      background-color: #2C3E50;
      border: none;
      padding: 0 20px;
    }
    .nav-tabs > li > a {
      color: #FAF7F2 !important;
      opacity: 0.6;
      border: none !important;
      border-radius: 0 !important;
      padding: 12px 20px;
      font-size: 13px;
      letter-spacing: 1px;
      text-transform: uppercase;
    }
    .nav-tabs > li.active > a {
      background-color: transparent !important;
      border-bottom: 3px solid #8B7355 !important;
      opacity: 1 !important;
      color: #FAF7F2 !important;
    }
    .nav-tabs > li > a:hover {
      background-color: rgba(255,255,255,0.1) !important;
      opacity: 1 !important;
    }
    .tab-content {
      background-color: #FAF7F2;
      padding: 20px;
    }
    
    .main-panel-bg {
      background-color: #F0ECE6;  
      padding: 20px;
      border-radius: 8px;
    }

    
    .sidebar {
      background: #2C3E50;
      padding: 20px;
      border-radius: 8px;
      color: #FAF7F2;
      margin-top: -34px;
    }
    .sidebar label {
      color: #FAF7F2 !important;
      font-size: 12px;
      text-transform: uppercase;
      letter-spacing: 1px;
    }
    .section-title {
      color: #2C3E50;
      font-weight: bold;
      padding-bottom: 5px;
      margin-top: 0px;
      margin-bottom: 0px;
      font-size: 13px;
      text-transform: uppercase;
      letter-spacing: 1px;
    }
    
    .section-title-main {
      color: #2C3E50;        
      font-weight: bold;
      margin-top: 0;
      margin-bottom: 12px;   
      font-size: 15px;       
      text-transform: uppercase;
      letter-spacing: 1px;
      test-align: center;
}
    
    .stat-card {
      background: white;
      border-top: 3px solid #8B7355;
      padding: 14px 16px;
      border-radius: 8px;
      margin: 8px 4px;
      box-shadow: 0 2px 8px rgba(44,62,80,0.08);
    }
    .stat-number {
      font-size: 20px;
      font-weight: bold;
      color: #2C3E50;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    .stat-label {
      font-size: 10px;
      color: #8B7355;
      text-transform: uppercase;
      letter-spacing: 1.5px;
      margin-top: 2px;
    }
    
    .chart-card {
      background: white;
      padding: 18px;
      border-radius: 8px;
      box-shadow: 0 2px 8px rgba(44,62,80,0.08);
      margin-top: -10px;
      margin-bottom: 40px;
    }
    .chart-title {
      color: #2C3E50;
      font-size: 13px;
      font-weight: bold;
      text-transform: uppercase;
      letter-spacing: 1px;
      border-bottom: 1px solid #E8E0D4;
      padding-bottom: 8px;
      margin-bottom: 12px;
    }
    
    .btn-success {
      background-color: #8B7355 !important;
      border-color: #8B7355 !important;
      color: white !important;
    }
    .btn-success:hover {
      background-color: #2C3E50 !important;
      border-color: #2C3E50 !important;
    }
    
    .section-separator {
      height: 3px;
      background-color: #8B7355;
      margin-top: -10px;
      margin-bottom: 15px;
      border-radius: 2px;   
    }
    .sidebar-separator {
      height: 2px;
      background-color: #8B7355;  
      margin: 12px 0;             
      border-radius: 1px;
      opacity: 0.7;         
    }

    
    .trip-card {
      background: white;
      border-left: 4px solid #8B7355;
      padding: 16px;
      border-radius: 8px;
      margin-bottom: 12px;
      box-shadow: 0 2px 8px rgba(44,62,80,0.08);
    }
    
    ::-webkit-scrollbar { width: 6px; }
    ::-webkit-scrollbar-track { background: #FAF7F2; }
    ::-webkit-scrollbar-thumb { background: #8B7355; border-radius: 3px; }
    
  "))
  ),
  
  # title bar
div(class = "title-bar",
  h2("Australian Wildlife Explorer"),
  p("Ecotourism Sightings · Weather · Tourism Analysis")
),

  #different tabs
  tabsetPanel(id = "main_tabs",
              tabPanel("Explorer",
                       div(style = "padding: 16px 0 8px 0;",

                       ),
  # main layout
  sidebarLayout(
    sidebarPanel( width=3,
      div(class = "sidebar",
          h4(class = "section-title", "Filters"),
          
          # organism dropdown
          pickerInput(
            inputId = "organism",
            label = "Select organism",
            choices = c("All", "Glowworm", 
                        "Gouldian Finch", 
                        "Manta Ray", "Orchid"),
            selected = "All",
            options = list(style = "btn-success")  # green button
          ),
          
          # year slider
          sliderInput(
            inputId = "year_range",
            label = "Filter by year",
            min = 2014,
            max = 2024,
            value = c(2014, 2024),
            sep = ""
          ),
          
          hr(),
          h4("Map View"),
          div(style = "margin-bottom:10px;",
              actionButton("view_individual", "Individual Markers")
          ),
          div(style = "margin-bottom:10px;",
              actionButton("view_clustered", "Clustered Markers")
          )
        ),
      uiOutput("funfact_block")
    ),
    
    mainPanel( width=9,
      leafletOutput("map", height = "450px"),
      br(),
      fluidRow(
        column(2, div(class = "stat-card",
                      div(class = "stat-number", textOutput("stat_total")),
                      div(class = "stat-label", "Total Sightings")
        )),
        column(2 , div(class = "stat-card",
                      div(class = "stat-number", textOutput("stat_species")),
                      div(class = "stat-label", "Species")
        )),
        column(2, div(class = "stat-card",
                      div(class = "stat-number", textOutput("stat_years")),
                      div(class = "stat-label", "Year Range")
        )),
        column(4, div(class = "stat-card",
                      div(class = "stat-number", textOutput("stat_state")),
                      div(class = "stat-label", "Top State")
        )),
        column(2, div(class = "stat-card",
                      div(class = "stat-number", textOutput("stat_month")),
                      div(class = "stat-label", "Peak Month")
        ))
      )
    )
  )
  ),                    
  tabPanel("Weather Analysis",
           sidebarLayout(
             sidebarPanel(width = 3,
                          div(class = "sidebar", style = "margin-top:20px;",
                              h4(class = "section-title", "Filters"),
                              pickerInput("w_organism", "Filter by organism",
                                          choices = c("All", "Glowworm", "Gouldian Finch", "Manta Ray", "Orchid"),
                                          selected = "All",
                                          options = list(style = "btn-success")
                              ),
                              sliderInput("w_year", "Year range",
                                          min = 2014, max = 2024,
                                          value = c(2014, 2024), sep = ""
                              )
                          ),
                          div(style="margin-top:20px;",
                              h5(tags$b("Legend")),
                              tags$ul(
                                tags$li(span(style="color:#4a7c59; font-weight:bold;", "— Temperature line")),
                                tags$li(span(style="color:#a8d5b5; font-weight:bold;", "■ Clear days")),
                                tags$li(span(style="color:#5c8fa0; font-weight:bold;", "■ Rainy days"))
                              )
                          ),
                          uiOutput("weather_fact")
             ),
             
             mainPanel(width = 9,
                       div(class = "main-panel-bg",
                           h4(class = "section-title-main", "Seasonal Weather Patterns"),
                           fluidRow(
                             column(6,
                                    div(class = "chart-card",
                                        h5(class = "chart-title", "Monthly Temperature Range"),
                                        plotOutput("temp_chart", height = "260px"),
                                        div(style = "text-align:center; margin-top:8px; font-size:15px; color:#555;",
                                            "Shows average monthly temperature range across selected years.")
                                    )
                             ),
                             column(6,
                                    div(class = "chart-card",
                                        h5(class = "chart-title", "Rainy vs Clear Days by Month"),
                                        plotOutput("rain_chart", height = "260px"),
                                        div(style = "text-align:center; margin-top:8px; font-size:15px; color:#555;",
                                            "Compares rainy vs clear days per month.")
                                    )
                             )
                           ),
                           # separator bar here
                           div(class = "section-separator"),
                           
                           h4(class = "section-title-main", "Organism–Temperature Relationship"),
                           div(class = "chart-card",
                               h5(class = "chart-title", "Average Temperature by Month and Organism"),
                               plotOutput("temp_heatmap", height = "260px"),
                               div(style = "text-align:center; margin-top:8px; font-size:15px; color:#555;",
                                   "Highlights how organisms experience temperature variation.")
                           )
                       )
             )
             
             
             
           )
  ),
  tabPanel("Trip Planner",
           sidebarLayout(
             sidebarPanel(width = 3,
                          div(class = "sidebar", style = "margin-top:20px;",
                              h4(class = "section-title", "Plan Your Trip"),
                              
                              pickerInput("trip_filter", "Choose a filter",
                                          choices = c("Organism", "Place", "Month", "Time of Day"),
                                          selected = "Organism",
                                          options = list(style = "btn-success")),
                              
                              uiOutput("trip_filter_choice"),
                              
                              actionButton("generate_plan", "Generate My Trip Plan",
                                           style = "width:100%; margin-top:15px; font-weight:bold;")
                          )
                          ,
                          uiOutput("trip_funfact")
             ),
             
             mainPanel(width = 9,
                       div(class = "main-panel-bg",
                           h4(class = "section-title-main", "Your Personalised Trip Guide"),
                           uiOutput("trip_results")
                       )
                       
             )
           )
  )
  
,
  tabPanel("Trends",
           sidebarPanel(width = 3,
                        div(class = "sidebar", style = "margin-top:20px;",
                            
                            h4(class = "section-title", "Filters"),
                            
                            # organism dropdown
                            pickerInput("t_organism", "Filter by organism",
                                        choices = c("All", "Glowworm", "Gouldian Finch", "Manta Ray", "Orchid"),
                                        selected = "All",
                                        options = list(style = "btn-success")
                            ),
                            
                            # year slider
                            sliderInput("t_year", "Year range",
                                        min = 2014, max = 2024,
                                        value = c(2014, 2024), sep = ""
                            )
                        ),
                        uiOutput("trend_fact")
           ),
           mainPanel(width = 9,
                     div(class = "main-panel-bg",
                         
                         # Section 1: Sightings Trends (Yearly + Monthly together)
                         h4(class = "section-title-main", "Sightings Trends"),
                         fluidRow(
                           column(6,
                                  div(class = "chart-card",
                                      h5(class = "chart-title", "Sightings Over the Years"),
                                      plotOutput("year_chart", height = "260px"),
                                      div(style = "text-align:center; margin-top:8px; font-size:15px; color:#555;",
                                          "Shows overall wildlife sightings trend across selected years.")
                                  )
                           ),
                           column(6,
                                  div(class = "chart-card",
                                      h5(class = "chart-title", "Monthly Trend"),
                                      plotOutput("month_chart", height = "260px"),
                                      div(style = "text-align:center; margin-top:8px; font-size:15px; color:#555;",
                                          "Highlights peak months for wildlife sightings.")
                                  )
                           )
                         ),
                         
                         # Separator bar
                         div(class = "section-separator"),
                         
                         # Section 2: Regional & Organism Insights
                         h4(class = "section-title-main", "Regional and Organism Insights"),
                         fluidRow(
                           column(6,
                                  div(class = "chart-card",
                                      h5(class = "chart-title", "Sightings by State"),
                                      plotOutput("state_chart", height = "260px"),
                                      div(style = "text-align:center; margin-top:8px; font-size:15px; color:#555;",
                                          "Shows distribution of sightings across Australian states.")
                                  )
                           ),
                           column(6,
                                  div(class = "chart-card",
                                      h5(class = "chart-title", "Organism Share Over the Years"),
                                      plotOutput("stack_chart", height = "260px"),
                                      div(style = "text-align:center; margin-top:8px; font-size:15px; color:#555;",
                                          "Compares how different organisms contribute to sightings over time.")
                                  )
                           )
                         )
                     )
           )
           
           
  )
  )
)

server <- function(input, output) {
  
  map_view <- reactiveVal("individual")
  
  observeEvent(input$view_individual, {
    map_view("individual")
  })
  
  observeEvent(input$view_clustered, {
    map_view("clustered")
  })
  
  #reactive- renders automatically when dropdown changes
  filtered_data <- reactive({
    
    d <- all_organisms |>
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
    
    # only filter by organism if not "All"
    if (input$organism != "All") {
      d <- d |> filter(organism == input$organism)
    }
    
    d
  })

  weather_data <- reactive({
    d <- organism_weather |>
      filter(year >= input$w_year[1], year <= input$w_year[2])
    if (input$w_organism != "All") d <- d |> filter(organism == input$w_organism)
    d
  })
  
  trip_plan <- eventReactive(input$generate_plan, {
    d <- all_organisms
    
    # Apply only the chosen filter
    if (input$trip_filter == "Organism") {
      d <- d |> filter(organism == input$p_organism)
    } else if (input$trip_filter == "Place") {
      d <- d |> filter(obs_state == input$p_state)
    } else if (input$trip_filter == "Month") {
      d <- d |> filter(month == as.integer(input$p_month))
    } else if (input$trip_filter == "Time of Day") {
      hour_ranges <- list(
        "Morning (6-11)"    = 6:10,
        "Midday (11-15)"    = 11:14,
        "Afternoon (15-18)" = 15:17,
        "Evening (18-22)"   = 18:21,
        "Night (22-6)"      = c(22:23, 0:5)
      )
      hrs <- hour_ranges[[input$p_time]]
      d <- d |> filter(hour %in% hrs)
    }
    
    # If too few results, return NULL
    if (nrow(d) < 5) return(NULL)
    
    # Calculate recommendations
    top_regions <- d |>
      filter(!is.na(obs_state)) |>
      count(obs_state) |>
      arrange(desc(n)) |>
      slice_head(n = 3)
    
    peak_month <- d |>
      count(month) |>
      slice_max(n, n = 1, with_ties = FALSE) |>
      pull(month)
    
    peak_hour <- d |>
      filter(!is.na(hour)) |>
      count(hour) |>
      slice_max(n, n = 1, with_ties = FALSE) |>
      pull(hour)
    
    list(
      total       = nrow(d),
      top_regions = top_regions,
      peak_month  = peak_month,
      peak_hour   = peak_hour
    )
  })
  
  trends_data <- reactive({
    d <- all_organisms |>
      filter(year >= input$t_year[1], year <= input$t_year[2])
    if (input$t_organism != "All") d <- d |> filter(organism == input$t_organism)
    d
  })
  
  
  output$funfact_block <- renderUI({
    fact <- if (input$organism == "All") {
      funfacts[5]
    } else {
      switch(input$organism,
             "Glowworm"       = funfacts[1],
             "Gouldian Finch" = funfacts[2],
             "Manta Ray"      = funfacts[3],
             "Orchid"         = funfacts[4],
             funfacts[5]      # fallback
      )
    }
    
    div(
      style = "background:#fffbea; border-left:4px solid #f39c12;
             padding:14px; border-radius:8px; margin-top:20px;",
      h5(tags$b("Wildlife Fun Fact !")),
      tags$i(fact)
    )
  })
  
  output$weather_fact <- renderUI({
    facts <- c(
      "Australia’s wettest month is usually January.",
      "The Outback can swing from 5°C nights to 40°C days.",
      "Rainfall varies dramatically between tropical north and arid interior.",
      "Cyclones in northern Australia typically occur between November and April."
    )
    
    div(
      style = "background:#eef9ff; border-left:4px solid #5c8fa0;
             padding:14px; border-radius:8px; margin-top:20px;",
      h5(tags$b("Weather Insight")),
      tags$i(sample(facts, 1))
    )
  })
  
  output$trend_fact <- renderUI({
    facts <- c(
      "Western Australia consistently leads in wildlife sightings.",
      "September is the busiest month for ecotourism sightings.",
      "Glowworms peak in cooler months, while manta rays peak in summer.",
      "Sightings have steadily grown since 2014, reflecting ecotourism growth."
    )
    div(
      style = "background:#fffbea; border-left:4px solid #f39c12;
             padding:14px; border-radius:8px; margin-top:20px;",
      h5(tags$b("Trend Insight")),
      tags$i(sample(facts, 1))
    )
  })
  
  
  output$temp_chart <- renderPlot({
    weather_data() |>
      filter(!is.nan(temp), !is.nan(min), !is.nan(max)) |>
      group_by(month) |>
      summarise(
        avg_temp = mean(temp, na.rm = TRUE),
        avg_min  = mean(min,  na.rm = TRUE),
        avg_max  = mean(max,  na.rm = TRUE),
        .groups  = "drop"
      ) |>
      ggplot(aes(x = factor(month, levels = 1:12, labels = month.abb))) +
      geom_ribbon(aes(ymin = avg_min, ymax = avg_max, group = 1),
                  fill = "#4a7c59", alpha = 0.15) +
      geom_line(aes(y = avg_temp, group = 1),
                colour = "#4a7c59", linewidth = 1.3) +
      geom_point(aes(y = avg_temp),
                 colour = "#4a7c59", size = 3,
                 fill = "white", shape = 21, stroke = 2) +
      labs(x = NULL, y = "Temperature (°C)") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })
  
  output$rain_chart <- renderPlot({
    weather_data() |>
      filter(!is.na(rainy)) |>
      mutate(condition = ifelse(rainy == 1, "Rainy", "Clear")) |>
      count(month, condition) |>
      mutate(month_lbl = factor(month, levels = 1:12, labels = month.abb)) |>
      ggplot(aes(x = month_lbl, y = n, fill = condition)) +
      geom_col(position = "stack") +
      scale_fill_manual(
        values = c("Clear" = "#a8d5b5", "Rainy" = "#5c8fa0"),
        name = NULL
      ) +
      labs(x = NULL, y = "Days") +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank()
      )
  })
  
  output$temp_heatmap <- renderPlot({
    organism_weather |>
      filter(!is.nan(temp)) |>
      group_by(organism, month) |>
      summarise(avg_temp = mean(temp, na.rm = TRUE), .groups = "drop") |>
      mutate(month_lbl = factor(month, levels = 1:12, labels = month.abb)) |>
      ggplot(aes(x = month_lbl, y = organism, fill = avg_temp)) +
      geom_tile(colour = "white", linewidth = 0.5) +
      scale_fill_gradient(low = "#c8e6f5", high = "#c0392b",
                          name = "Avg Temp (°C)") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid = element_blank(),
        legend.position = "right"
      )
  })
  
  output$stack_chart <- renderPlot({
    all_organisms |>
      filter(year >= input$t_year[1], year <= input$t_year[2]) |>
      count(year, organism) |>
      ggplot(aes(x = year, y = n, fill = organism)) +
      geom_area(position = "stack", alpha = 0.85) +
      scale_fill_manual(values = organism_colours, name = NULL) +
      labs(x = NULL, y = "Sightings") +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank()
      )
  })
  
  output$stat_total <- renderText({
    format(nrow(filtered_data()), big.mark = ",")
  })
  
  output$stat_species <- renderText({
    as.character(length(unique(filtered_data()$organism)))
  })
  
  output$stat_years <- renderText({
    yrs <- range(filtered_data()$year, na.rm = TRUE)
    paste0(yrs[1], "–", yrs[2])
  })
  
  output$stat_state <- renderText({
    filtered_data() |>
      filter(!is.na(obs_state)) |>
      count(obs_state) |>
      slice_max(n, n = 1, with_ties = FALSE) |>
      pull(obs_state)
  })
  
  output$stat_month <- renderText({
    m <- filtered_data() |>
      count(month) |>
      slice_max(n, n = 1, with_ties = FALSE) |>
      pull(month)
    month.name[m]
  })
  
  # render leaflet map
  output$map <- renderLeaflet({
    
    # base map — only runs ONCE on startup
    leaflet() |>
      addTiles() |>                        # base map tiles
      setView(
        lng = 134.0,            
        lat = -28.0,  
        zoom = 4 # zoom out enough to see all Aus
      )
  })
  
  output$month_chart <- renderPlot({
    
    # pick colour — use teal if All selected
    bar_colour <- if (input$t_organism == "All") "#2E8B57" else unname(organism_colours[input$t_organism])
    
    trends_data() |>
      count(month) |>
      ggplot(aes(
        x = factor(month, levels = 1:12, labels = month.abb),
        y = n,
        fill = n
      )) +
      geom_col(show.legend = FALSE) +
      scale_fill_gradient(
        low = "lightyellow",
        high = bar_colour        # uses organism colour or teal for All
      ) +
      labs(
        x = NULL,
        y = "Sightings"
      ) +
      theme_minimal(base_size = 11)
  })
  
  output$year_chart <- renderPlot({
    
    trends_data() |>
      count(year) |>
      ggplot(aes(x = year, y = n)) +
      geom_line(colour = "#4a7c59", linewidth = 1) +   # green line
      geom_point(colour = "#4a7c59", size = 2.5) +     # dots on each year
      geom_area(fill = "#4a7c59", alpha = 0.15) +      # shaded area below
      labs(x = NULL, y = "Sightings") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })
  
  output$state_chart <- renderPlot({
    
    trends_data() |>
      filter(!is.na(obs_state)) |>
      count(obs_state) |>              # count sightings per state
      arrange(desc(n)) |>              # sort highest first
      ggplot(aes(
        x = reorder(obs_state, n),     # reorder bars by count
        y = n,
        fill = obs_state               # different colour per state
      )) +
      geom_col(show.legend = FALSE) +  # bar chart, no legend needed
      coord_flip() +                   # horizontal bars, easier to read
      scale_fill_viridis_d() +         # nice colour palette
      labs(
        x = NULL,
        y = "Number of sightings"
      ) +
      theme_minimal(base_size = 13)
  })
  output$trip_filter_choice <- renderUI({
    switch(input$trip_filter,
           "Organism" = pickerInput("p_organism", "Select organism",
                                    choices = c("Glowworm", "Gouldian Finch", "Manta Ray", "Orchid"),
                                    selected = "Glowworm"),
           "Place" = pickerInput("p_state", "Select state",
                                 choices = sort(unique(na.omit(all_organisms$obs_state))),
                                 selected = "Queensland"),
           "Month" = pickerInput("p_month", "Select month",
                                 choices = setNames(1:12, month.name),
                                 selected = 8),
           "Time of Day" = pickerInput("p_time", "Select time",
                                       choices = c("Morning (6-11)", "Midday (11-15)",
                                                   "Afternoon (15-18)", "Evening (18-22)", "Night (22-6)"),
                                       selected = "Morning (6-11)")
    )
  })
  
  output$trip_results <- renderUI({
    plan <- trip_plan()
    
    if (is.null(plan)) {
      return(div(
        style = "background:#e8f5e9; border-left:4px solid #4a7c59;
             padding:16px; border-radius:8px; margin-bottom:14px;",
        h3("General Australia Wildlife Guide"),
        p("Australia offers diverse ecotourism experiences across all states."), 
        br(),
        h4(tags$b(("Best months to visit:" ))),
        p("September–November for spring blooms, or May–August for cooler wildlife activity."),
        br(),
        h4(tags$b("Top regions: ")),
        p("Queensland rainforests, Western Australia coast, and Tasmania’s wilderness."),
        br(),
        h4(tags$b("Wildlife highlights:")),
        p("Glowworms in caves, Gouldian Finches in the north, manta rays along the reef, and orchids nationwide.")
      ))
    }
    
    hour_lbl <- format(strptime(paste0(plan$peak_hour, ":00"), "%H:%M"), "%I:%M %p")
    
    tagList(
      div(
        style = "background:#e8f5e9; border-left:4px solid #4a7c59;
             padding:16px; border-radius:8px; margin-bottom:14px;",
        p(paste0("Based on ", format(plan$total, big.mark=","), " matching sightings")),
        h4(tags$b("Best month to visit: ")),
        p(paste0( month.name[plan$peak_month])),
        h4(tags$b("Best time of day:")),
        p(paste0(" around ", hour_lbl))
      ),
      h5("Top 3 Recommended Regions"),
      tagList(lapply(seq_len(nrow(plan$top_regions)), function(i) {
        row <- plan$top_regions[i, ]
        div(
          style = "background:white; border-left:4px solid #4a7c59;
               padding:14px; border-radius:8px; margin-bottom:10px;",
          div(style = "display:flex; justify-content:space-between;",
              h5(row$obs_state, style = "margin:0;"),
              span(format(row$n, big.mark=","), " sightings",
                   style = "color:#4a7c59; font-weight:600;")
          )
        )
      }))
    )
  })
  
  output$trip_funfact <- renderUI({
    fact <- sample(funfacts, 1)  # random fact each time
    div(
      style = "background:#fffbea; border-left:4px solid #f39c12;
           padding:14px; border-radius:8px; margin-top:20px;",
      h5(tags$b("Wildlife Fun Fact")),
      tags$i(fact)
    )
  })
  
  
  trip_plan <- reactiveVal(NULL)
  
  observeEvent(input$generate_plan, {
    d <- all_organisms
    
    if (input$trip_filter == "Organism") {
      d <- d |> filter(organism == input$p_organism)
    } else if (input$trip_filter == "Place") {
      d <- d |> filter(obs_state == input$p_state)
    } else if (input$trip_filter == "Month") {
      d <- d |> filter(month == as.integer(input$p_month))
    } else if (input$trip_filter == "Time of Day") {
      hour_ranges <- list(
        "Morning (6-11)"    = 6:10,
        "Midday (11-15)"    = 11:14,
        "Afternoon (15-18)" = 15:17,
        "Evening (18-22)"   = 18:21,
        "Night (22-6)"      = c(22:23, 0:5)
      )
      hrs <- hour_ranges[[input$p_time]]
      d <- d |> filter(hour %in% hrs)
    }
    
    if (nrow(d) < 5) {
      trip_plan(NULL)
      return()
    }
    
    trip_plan(list(
      total       = nrow(d),
      top_regions = d |> filter(!is.na(obs_state)) |> count(obs_state) |> arrange(desc(n)) |> slice_head(n = 3),
      peak_month  = d |> count(month) |> slice_max(n, n = 1, with_ties = FALSE) |> pull(month),
      peak_hour   = d |> filter(!is.na(hour)) |> count(hour) |> slice_max(n, n = 1, with_ties = FALSE) |> pull(hour)
    ))
  })
  
  
  # update ONLY the markers when dropdown changes
  observe({
    data <- filtered_data()
    
    fill_col <- if (input$organism == "All") {
      unname(organism_colours[data$organism])
    } else {
      unname(organism_colours[input$organism])
    }
    
    proxy <- leafletProxy("map") |>
      clearMarkers() |>
      clearMarkerClusters()
    
    if (map_view() == "individual") {
      proxy |> addCircleMarkers(
        data        = data,
        lng         = ~obs_lon,
        lat         = ~obs_lat,
        radius      = 5,
        color       = fill_col,
        fillColor   = fill_col,
        fillOpacity = 0.3,
        weight      = 0.5,
        popup       = ~paste0(
          "<b>", organism, "</b><br>",
          "Date: ", date, "<br>",
          "State: ", obs_state
        )
      )
    } else {
      proxy |> addCircleMarkers(
        data           = data,
        lng            = ~obs_lon,
        lat            = ~obs_lat,
        radius         = 5,
        fillColor      = fill_col,
        fillOpacity    = 0.7,
        weight         = 0.5,
        color          = "white",
        clusterOptions = markerClusterOptions(),
        popup          = ~paste0(
          "<b>", organism, "</b><br>",
          "Date: ", date, "<br>",
          "State: ", obs_state
        )
      )
    }
    if (input$organism == "All") {
      leafletProxy("map") |>
        addLegend(
          position = "bottomright",
          colors = unname(organism_colours),    # the colour values
          labels = names(organism_colours),     # the organism names
          title = "Organism",
          layerId = "legend"                    # id so we can remove it later
        )
    } else {
      leafletProxy("map") |>
        removeControl("legend")                 # remove legend for single organism
    }
  })
  
}

shinyApp(ui = ui, server = server)
