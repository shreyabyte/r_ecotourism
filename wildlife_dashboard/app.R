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
  "Glowworm"       = "#FFD700",    # gold
  "Gouldian Finch" = "#FF8C00",    # darkorange
  "Manta Ray"      = "#0000FF",    # blue
  "Orchid"         = "#800080"     # purple
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

# what to bring tips based on peak month
packing_tips <- list(
  "1"  = "Pack warm layers and waterproof gear — January can bring summer storms.",
  "2"  = "Light clothing and sun protection — February is hot across most of Australia.",
  "3"  = "Comfortable walking shoes and a hat — March has warm pleasant days.",
  "4"  = "A light jacket for evenings — April marks the start of cooler nights.",
  "5"  = "Warm layers essential — May nights can be cold especially in the south.",
  "6"  = "Pack warm clothing — June is winter across most of Australia.",
  "7"  = "Thermal layers and a good torch for night sightings — July is the coldest month.",
  "8"  = "Warm days returning — August is great for early morning wildlife spotting.",
  "9"  = "Light layers and good walking shoes — September is peak spring season.",
  "10" = "Sunscreen and water essential — October heats up across Australia.",
  "11" = "Light breathable clothing — November is warming up fast.",
  "12" = "Torch for glowworms, insect repellent, and light clothing — December evenings are warm."
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
    tags$link(
      rel  = "stylesheet",
      href = paste0(
        "https://fonts.googleapis.com/css2?family=DM+Sans",
        ":wght@300;400;500;600;700&display=swap"
      )
    ),
    tags$style(HTML("
    
    /* ── Base ── */
body {
  background-color: #F0F4F8;
  font-family: 'DM Sans', sans-serif;
  color: #1E2D3D;
}

/* ── Title bar ── */
.title-bar {
  background: linear-gradient(135deg, #1E2D3D 0%, #2E4A6B 100%);
  color: #F0F4F8;
  padding: 22px 30px;
  margin-bottom: 0px;
  text-align: center;
  letter-spacing: 1px;
}
.title-bar h2 {
  font-size: 28px;
  font-weight: 700;
  margin-bottom: 4px;
  color: #F0F4F8;
}
.title-bar p {
  opacity: 0.7;
  font-size: 12px;
  margin: 0;
  letter-spacing: 2px;
  text-transform: uppercase;
  font-weight: 300;
}

/* ── Tabs ── */
.nav-tabs {
  background-color: #1E2D3D;
  border: none;
  padding: 0 20px;
}
.nav-tabs > li > a {
  color: #F0F4F8 !important;
  opacity: 0.5;
  border: none !important;
  border-radius: 0 !important;
  padding: 12px 20px;
  font-size: 12px;
  letter-spacing: 1.5px;
  text-transform: uppercase;
  font-weight: 500;
}
.nav-tabs > li.active > a {
  background-color: transparent !important;
  border-bottom: 3px solid #4A9EDB !important;
  opacity: 1 !important;
  color: #F0F4F8 !important;
}
.nav-tabs > li > a:hover {
  background-color: rgba(255,255,255,0.08) !important;
  opacity: 1 !important;
}
.tab-content {
  background-color: #F0F4F8;
  padding: 20px;
}

/* ── Sidebar ── */
.sidebar {
  background: #1E2D3D;
  padding: 20px;
  border-radius: 10px;
  color: #F0F4F8;
  margin-top: -34px;
}
.sidebar label {
  color: #A8BDD0 !important;
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  font-weight: 500;
}
.section-title {
  color: #4A9EDB;
  font-weight: 600;
  padding-bottom: 5px;
  margin-top: 0px;
  margin-bottom: 10px;
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: 2px;
}

/* ── Main panel ── */
.main-panel-bg {
  background-color: #F0F4F8;
  padding: 10px;
  border-radius: 8px;
}
.section-title-main {
  color: #1E2D3D;
  font-weight: 600;
  margin-top: 0;
  margin-bottom: 12px;
  font-size: 13px;
  text-transform: uppercase;
  letter-spacing: 1.5px;
}

/* ── Stat cards ── */
.stat-card {
  background: white;
  border-top: 3px solid #4A9EDB;
  padding: 14px 16px;
  border-radius: 10px;
  margin: 8px 4px;
  box-shadow: 0 2px 12px rgba(30,45,61,0.08);
}
.stat-number {
  font-size: 20px;
  font-weight: 700;
  color: #1E2D3D;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
}
.stat-label {
  font-size: 10px;
  color: #4A9EDB;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  margin-top: 2px;
  font-weight: 500;
}

/* ── Chart cards ── */
.chart-card {
  background: #F0F4F8;  
  padding: 18px;
  border-radius: 10px;
  box-shadow: 0 2px 12px rgba(30,45,61,0.08);
  margin-bottom: 16px;
}
.chart-title {
  color: #1E2D3D;
  font-size: 12px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  border-bottom: 2px solid #4A9EDB;
  padding-bottom: 8px;
  margin-bottom: 12px;
}

/* ── Section separator ── */
.section-separator {
  height: 2px;
  background: linear-gradient(90deg, #4A9EDB, transparent);
  margin: 10px 0 20px 0;
  border-radius: 2px;
}
.sidebar-separator {
  height: 1px;
  background-color: #2E4A6B;
  margin: 12px 0;
  border-radius: 1px;
}

/* ── Buttons ── */
.btn-success {
  background-color: #4A9EDB !important;
  border-color: #4A9EDB !important;
  color: white !important;
  font-weight: 500 !important;
  letter-spacing: 0.5px !important;
}
.btn-success:hover {
  background-color: #1E2D3D !important;
  border-color: #1E2D3D !important;
}
.btn-default {
  background-color: #2E4A6B !important;
  border-color: #2E4A6B !important;
  color: #F0F4F8 !important;
}

/* ── Fun fact boxes ── */
.fact-box {
  background: #EBF4FB;
  border-left: 4px solid #4A9EDB;
  padding: 14px;
  border-radius: 8px;
  margin-top: 16px;
  font-size: 13px;
  color: #1E2D3D;
}
.fact-box h5 {
  color: #4A9EDB;
  font-weight: 600;
  margin-bottom: 6px;
}

/* ── Trip cards ── */
.trip-card {
  background: white;
  border-left: 4px solid #4A9EDB;
  padding: 16px;
  border-radius: 10px;
  margin-bottom: 12px;
  box-shadow: 0 2px 12px rgba(30,45,61,0.08);
}

/* ── Action button ── */
.action-btn {
  width: 100%;
  margin-top: 15px;
  font-weight: 600;
  letter-spacing: 1px;
  background-color: #4A9EDB !important;
  border: none !important;
  padding: 10px !important;
  border-radius: 6px !important;
}

/* ── Scrollbar ── */
::-webkit-scrollbar { width: 6px; }
::-webkit-scrollbar-track { background: #F0F4F8; }
::-webkit-scrollbar-thumb { background: #4A9EDB; border-radius: 3px; }
    
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
                                           
                                           div(class = "sidebar-separator"),
                                           h4(class = "section-title", "Map View"),
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
                                      uiOutput("weather_fact")
                         ),
                         
                         mainPanel(width = 9,
                                   div(class = "main-panel-bg",
                                       h4(class = "section-title-main", "Seasonal Weather Patterns and Organism-Temperature Relationship"),
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
                                       uiOutput("trip_results"),
                                       
                                       
                                       uiOutput("trip_map_section")
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
                                     h4(class = "section-title-main", "Sightings Trends by Time and Region"),
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
  
  theme_set(
    theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor  = element_blank(),
        plot.background   = element_rect(fill = "#F0F4F8", colour = NA),
        panel.background  = element_rect(fill = "#F0F4F8", colour = NA),
        text              = element_text(family = "sans", colour = "#1E2D3D")
      )
  )
  
  # helper — returns colour for current explorer selection
  get_colour <- function(organism_input) {
    if (organism_input == "All") "#4A9EDB" else unname(organism_colours[organism_input])
  }
  
  # helper — returns colour for trends tab selection  
  get_t_colour <- function(organism_input) {
    if (organism_input == "All") "#4A9EDB" else unname(organism_colours[organism_input])
  }
  
  # helper — returns colour for weather tab selection
  get_w_colour <- function(organism_input) {
    if (organism_input == "All") "#4A9EDB" else unname(organism_colours[organism_input])
  }
  
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
    
    div(class = "fact-box",
        h5(tags$b("Wildlife Fun Fact!")),
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
    
    div(class = "fact-box",
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
    div(class = "fact-box",
        h5(tags$b("Trend Insight")),
        tags$i(sample(facts, 1))
    )
  })
  
  
  output$temp_chart <- renderPlot({
    col <- get_w_colour(input$w_organism)
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
                  fill = col , alpha = 0.15) +     # sky blue ribbon
      geom_line(aes(y = avg_temp, group = 1),
                colour = col , linewidth = 1.3) +  # sky blue line
      geom_point(aes(y = avg_temp),
                 colour = col , size = 3,
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
        values = c("Clear" = "#AED6F1", "Rainy" = "#1A5276"),
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
    bar_colour <- if (input$t_organism == "All") "#4A9EDB" else unname(organism_colours[input$t_organism])
    
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
    
col <- get_t_colour(input$t_organism)

col <- get_t_colour(input$t_organism)

trends_data() |>
  count(year) |>
  ggplot(aes(x = year, y = n)) +
  geom_line(colour = col, linewidth = 1) +
  geom_point(colour = col, size = 2.5) +
  geom_area(fill = col, alpha = 0.15) +
  labs(x = NULL, y = "Sightings") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

  })
  
  output$state_chart <- renderPlot({
    
    col <- get_t_colour(input$t_organism)
    
    trends_data() |>
      filter(!is.na(obs_state)) |>
      count(obs_state) |>
      arrange(desc(n)) |>
      ggplot(aes(x = reorder(obs_state, n), y = n, fill = n)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_gradient(
        low  = paste0(col, "55"),   # light version of organism colour
        high = col                  # full organism colour
      ) +
      labs(x = NULL, y = "Number of sightings") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
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
      return(tagList(
        
        # header card
        div(
          style = "background:#EBF4FB; border-left:4px solid #4A9EDB;
               padding:16px; border-radius:10px; margin-bottom:14px;",
          p(style = "color:#4A9EDB; font-size:11px; text-transform:uppercase;
                 letter-spacing:1.5px; margin-bottom:8px;",
            "General Australia Wildlife Guide"),
          fluidRow(
            column(6,
                   p(style = "font-size:11px; color:#888; text-transform:uppercase;
                     letter-spacing:1px; margin-bottom:2px;", "Best months"),
                   p(style = "font-size:16px; font-weight:700; color:#1E2D3D; margin:0;",
                     "Sep — Nov")
            ),
            column(6,
                   p(style = "font-size:11px; color:#888; text-transform:uppercase;
                     letter-spacing:1px; margin-bottom:2px;", "Best time of day"),
                   p(style = "font-size:16px; font-weight:700; color:#1E2D3D; margin:0;",
                     "07:00 AM — 10:00 AM")
            )
          )
        ),
        
        # what to pack default
        div(
          style = "background:white; border-left:4px solid #FFD700;
               padding:14px; border-radius:10px; margin-bottom:14px;
               box-shadow: 0 2px 8px rgba(30,45,61,0.06);",
          p(style = "font-size:11px; color:#888; text-transform:uppercase;
                 letter-spacing:1px; margin-bottom:6px;", "🎒 What to pack"),
          p(style = "font-size:13px; color:#1E2D3D; margin:0;",
            "Good walking shoes, sun protection, binoculars, and a camera. 
         Light layers work for most of Australia's wildlife regions.")
        ),
        
        # top regions default
        div(style = "margin-top:4px;",
            p(style = "font-size:11px; color:#888; text-transform:uppercase;
                 letter-spacing:1px; margin-bottom:10px;",
              "Top wildlife regions in Australia"),
            
            # region 1
            div(
              style = "background:white; border-left:4px solid #4A9EDB;
                 padding:14px; border-radius:10px; margin-bottom:10px;
                 box-shadow: 0 2px 8px rgba(30,45,61,0.06);",
              div(style = "display:flex; justify-content:space-between; align-items:center;",
                  div(
                    span(style = "background:#4A9EDB; color:white; border-radius:50%;
                          width:22px; height:22px; display:inline-flex;
                          align-items:center; justify-content:center;
                          font-size:11px; font-weight:700; margin-right:8px;", "1"),
                    span(style = "font-weight:600; color:#1E2D3D; font-size:14px;",
                         "Western Australia")
                  ),
                  span(style = "color:#4A9EDB; font-weight:600; font-size:13px;",
                       "Orchids & wildflowers")
              )
            ),
            
            # region 2
            div(
              style = "background:white; border-left:4px solid #4A9EDB;
                 padding:14px; border-radius:10px; margin-bottom:10px;
                 box-shadow: 0 2px 8px rgba(30,45,61,0.06);",
              div(style = "display:flex; justify-content:space-between; align-items:center;",
                  div(
                    span(style = "background:#4A9EDB; color:white; border-radius:50%;
                          width:22px; height:22px; display:inline-flex;
                          align-items:center; justify-content:center;
                          font-size:11px; font-weight:700; margin-right:8px;", "2"),
                    span(style = "font-weight:600; color:#1E2D3D; font-size:14px;",
                         "Northern Territory")
                  ),
                  span(style = "color:#4A9EDB; font-weight:600; font-size:13px;",
                       "Gouldian Finches")
              )
            ),
            
            # region 3
            div(
              style = "background:white; border-left:4px solid #4A9EDB;
                 padding:14px; border-radius:10px; margin-bottom:10px;
                 box-shadow: 0 2px 8px rgba(30,45,61,0.06);",
              div(style = "display:flex; justify-content:space-between; align-items:center;",
                  div(
                    span(style = "background:#4A9EDB; color:white; border-radius:50%;
                          width:22px; height:22px; display:inline-flex;
                          align-items:center; justify-content:center;
                          font-size:11px; font-weight:700; margin-right:8px;", "3"),
                    span(style = "font-weight:600; color:#1E2D3D; font-size:14px;",
                         "Tasmania")
                  ),
                  span(style = "color:#4A9EDB; font-weight:600; font-size:13px;",
                       "Glowworms")
              )
            )
        )
      ))
    }
    
    hour_lbl <- format(strptime(paste0(plan$peak_hour, ":00"), "%H:%M"), "%I:%M %p")
    
    tagList(
      
      # main info card
      div(
        style = "background:#EBF4FB; border-left:4px solid #4A9EDB;
             padding:16px; border-radius:10px; margin-bottom:14px;",
        p(style = "color:#4A9EDB; font-size:11px; text-transform:uppercase;
               letter-spacing:1.5px; margin-bottom:8px;",
          paste0("Based on ", format(plan$total, big.mark=","), " matching sightings")),
        
        fluidRow(
          column(6,
                 p(style = "font-size:11px; color:#888; text-transform:uppercase;
                   letter-spacing:1px; margin-bottom:2px;", "Best month"),
                 p(style = "font-size:18px; font-weight:700; color:#1E2D3D; margin:0;",
                   month.name[plan$peak_month])
          ),
          column(6,
                 p(style = "font-size:11px; color:#888; text-transform:uppercase;
                   letter-spacing:1px; margin-bottom:2px;", "Best time of day"),
                 p(style = "font-size:18px; font-weight:700; color:#1E2D3D; margin:0;",
                   format(strptime(paste0(plan$peak_hour, ":00"), "%H:%M"), "%I:%M %p"))
          )
        ),
        
        # trend indicator
        if (!is.na(plan$trend_pct)) {
          div(style = "margin-top:12px;",
              span(
                style = paste0("background:", 
                               if(plan$trend_dir == "up") "#d4edda" else "#f8d7da",
                               "; padding:4px 10px; border-radius:20px; font-size:12px;
                          font-weight:600; color:",
                               if(plan$trend_dir == "up") "#155724" else "#721c24"),
                paste0(if(plan$trend_dir == "up") "↑ " else "↓ ",
                       abs(plan$trend_pct), "% vs 3 years ago")
              )
          )
        },
        
        # best organism (only shows for Place or Month filters)
        if (!is.null(plan$best_organism)) {
          div(style = "margin-top:12px;",
              p(style = "font-size:11px; color:#888; text-transform:uppercase;
                   letter-spacing:1px; margin-bottom:2px;", "Most commonly spotted"),
              p(style = "font-size:15px; font-weight:600; color:#1E2D3D; margin:0;",
                plan$best_organism)
          )
        }
      ),
      
      # what to bring
      div(
        style = "background:white; border-left:4px solid #FFD700;
             padding:14px; border-radius:10px; margin-bottom:14px;
             box-shadow: 0 2px 8px rgba(30,45,61,0.06);",
        p(style = "font-size:11px; color:#888; text-transform:uppercase;
               letter-spacing:1px; margin-bottom:6px;", "🎒 What to pack"),
        p(style = "font-size:13px; color:#1E2D3D; margin:0;",
          packing_tips[[as.character(plan$peak_month)]])
      ),
      
      # top 3 regions
      div(style = "margin-top:4px;",
          p(style = "font-size:11px; color:#888; text-transform:uppercase;
               letter-spacing:1px; margin-bottom:10px;", "Top 3 recommended regions"),
          
          tagList(lapply(seq_len(nrow(plan$top_regions)), function(i) {
            row <- plan$top_regions[i, ]
            div(
              style = "background:white; border-left:4px solid #4A9EDB;
                 padding:14px; border-radius:10px; margin-bottom:10px;
                 box-shadow: 0 2px 8px rgba(30,45,61,0.06);",
              div(style = "display:flex; justify-content:space-between; align-items:center;",
                  div(
                    span(style = "background:#4A9EDB; color:white; border-radius:50%;
                          width:22px; height:22px; display:inline-flex;
                          align-items:center; justify-content:center;
                          font-size:11px; font-weight:700; margin-right:8px;",
                         i),
                    span(style = "font-weight:600; color:#1E2D3D; font-size:14px;",
                         row$obs_state)
                  ),
                  span(style = "color:#4A9EDB; font-weight:600; font-size:13px;",
                       format(row$n, big.mark=","), " sightings")
              )
            )
          }))
      )
    )
  })
  
  output$trip_map_section <- renderUI({
    plan <- trip_plan()
    
    div(
      style = "margin-top:16px;",
      p(style = "font-size:11px; color:#888; text-transform:uppercase;
               letter-spacing:1px; margin-bottom:10px;",
        if(is.null(plan)) "🗺️ Wildlife hotspots across Australia" 
        else "📍 Recommended regions on the map"),
      div(class = "chart-card",
          leafletOutput("trip_map", height = "300px")
      )
    )
  })
  
  output$trip_map <- renderLeaflet({
    plan <- trip_plan()
    
    # default map — show hotspots when no plan generated
    if (is.null(plan)) {
      
      # get one representative point per state
      state_centres <- all_organisms |>
        filter(!is.na(obs_state), !is.na(obs_lat), !is.na(obs_lon)) |>
        group_by(obs_state) |>
        summarise(
          lat = mean(obs_lat, na.rm = TRUE),
          lon = mean(obs_lon, na.rm = TRUE),
          n   = n(),
          top_organism = names(sort(table(organism), decreasing = TRUE))[1],
          .groups = "drop"
        )
      
      return(
        leaflet(state_centres) |>
          addProviderTiles("CartoDB.Positron") |>
          setView(lng = 134.0, lat = -28.0, zoom = 4) |>
          addCircleMarkers(
            lng         = ~lon,
            lat         = ~lat,
            radius      = ~pmin(sqrt(n/100), 20),  # size by sighting count
            color       = "white",
            fillColor   = "#4A9EDB",
            fillOpacity = 0.7,
            weight      = 2,
            popup       = ~paste0(
              "<b>", obs_state, "</b><br>",
              format(n, big.mark = ","), " total sightings<br>",
              "Most common: ", top_organism
            )
          )
      )
    }
    
    # personalised map — show top 3 regions
    region_coords <- all_organisms |>
      filter(obs_state %in% plan$top_regions$obs_state) |>
      group_by(obs_state) |>
      summarise(
        lat = mean(obs_lat, na.rm = TRUE),
        lon = mean(obs_lon, na.rm = TRUE),
        .groups = "drop"
      ) |>
      left_join(plan$top_regions, by = "obs_state") |>
      arrange(desc(n)) |>
      mutate(rank = row_number())
    
    rank_colours <- c("1" = "#4A9EDB", "2" = "#1E2D3D", "3" = "#AED6F1")
    
    leaflet(region_coords) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = 134.0, lat = -28.0, zoom = 4) |>
      addCircleMarkers(
        lng         = ~lon,
        lat         = ~lat,
        radius      = 18,
        color       = "white",
        fillColor   = ~rank_colours[as.character(rank)],
        fillOpacity = 0.9,
        weight      = 2,
        label       = ~paste0("#", rank, " ", obs_state),
        popup       = ~paste0(
          "<b>#", rank, " ", obs_state, "</b><br>",
          format(n, big.mark = ","), " sightings"
        )
      ) |>
      addLabelOnlyMarkers(
        lng          = ~lon,
        lat          = ~lat,
        label        = ~as.character(rank),
        labelOptions = labelOptions(
          noHide    = TRUE,
          direction = "center",
          textOnly  = TRUE,
          style     = list(
            "color"       = "white",
            "font-weight" = "bold",
            "font-size"   = "14px"
          )
        )
      )
  })
  
  output$trip_funfact <- renderUI({
    fact <- sample(funfacts, 1)  # random fact each time
    div(class = "fact-box",
        h5(tags$b("Wildlife Fun Fact")),
        tags$i(fact)
    )
  })
  
  
  trip_plan <- reactiveVal(NULL)
  
  observeEvent(input$generate_plan, {
    d <- all_organisms
    trend_base <- all_organisms
    
    if (input$trip_filter == "Organism") {
      trend_base <- trend_base |> filter(organism == input$p_organism)
    } else if (input$trip_filter == "Place") {
      trend_base <- trend_base |> filter(obs_state == input$p_state)
    }
    
    recent    <- trend_base |> filter(year >= 2022) |> nrow()
    older     <- trend_base |> filter(year >= 2019, year <= 2021) |> nrow()
    trend_pct <- if (older == 0) NA else round((recent - older) / older * 100)
    trend_dir <- if (is.na(trend_pct)) "unknown" else if (trend_pct > 0) "up" else "down"
    
    # best organism when filtering by place or month
    best_organism <- if (input$trip_filter %in% c("Place", "Month")) {
      d |>
        filter(!is.na(organism)) |>
        count(organism) |>
        slice_max(n, n = 1, with_ties = FALSE) |>
        pull(organism)
    } else {
      NULL
    }
    
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
      peak_hour   = d |> filter(!is.na(hour)) |> count(hour) |> slice_max(n, n = 1, with_ties = FALSE) |> pull(hour),
      trend_pct    = trend_pct,       
      trend_dir    = trend_dir,       
      best_organism = best_organism
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
