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


# UI and server so shiny app loads
ui <- fluidPage(
  theme = shinytheme("flatly"),  # clean light theme as base
  
  # custom CSS for earthy nature feel
  tags$head(
    tags$style(HTML("
      body { background-color: #f5f0e8; font-family: 'Georgia', serif; }
      .navbar { background-color: #4a7c59 !important; }
      .title-bar { 
        background-color: #4a7c59; 
        color: white; 
        padding: 15px 20px; 
        margin-bottom: 20px;
        border-radius: 8px;
      }
      .stat-card {
        background: white;
        border-left: 4px solid #4a7c59;
        padding: 15px;
        border-radius: 8px;
        margin: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .stat-number {
        font-size: 28px;
        font-weight: bold;
        color: #4a7c59;
      }
      .stat-label {
        font-size: 12px;
        color: #888;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      .sidebar { 
        background: white; 
        padding: 20px; 
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-top: -30px;
        margin-bottom: -28px;
      }
      .section-title {
        color: #4a7c59;
        font-weight: bold;
        border-bottom: 2px solid #4a7c59;
        padding-bottom: 5px;
        margin-bottom: 15px;
      }
    "))
  ),
  
  # title bar
  div(class = "title-bar",
      h2("🌿 Australian Wildlife Explorer"),
      p("Explore wildlife sightings across Australia (2014-2024)", 
        style = "margin:0; opacity:0.8;")
  ),
  
  # main layout
  sidebarLayout(
    sidebarPanel(
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
          h4(class = "section-title", "Best Time to Visit"),
          uiOutput("best_time"),
          
          hr(),
          h4(class = "section-title", "Sightings by State"),
          plotOutput("state_chart", height = "200px"),
      )
    ),
    
    mainPanel(
      # map
      leafletOutput("map", height = "450px"),
      
      br(),
      
      # two charts side by side
      fluidRow(
        column(6,
               div(style = "background:white; padding:15px; border-radius:8px;
                       box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                   h5(class = "section-title", "Sightings Over the Years"),
                   plotOutput("year_chart", height = "250px")
               )
        ),
        
        column(6,
               div(style = "background:white; padding:15px; border-radius:8px;
                 box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                   h5(class = "section-title", "Monthly Trend"),
                   plotOutput("month_chart", height = "250px")
               )
        )
      )
    )
  )
)

server <- function(input, output) {
  
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
    bar_colour <- if (input$organism == "All") "#2E8B57" else unname(organism_colours[input$organism])
    
    filtered_data() |>
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
    
    filtered_data() |>
      count(year) |>
      ggplot(aes(x = year, y = n)) +
      geom_line(colour = "#4a7c59", linewidth = 1) +   # green line
      geom_point(colour = "#4a7c59", size = 2.5) +     # dots on each year
      geom_area(fill = "#4a7c59", alpha = 0.15) +      # shaded area below
      labs(x = NULL, y = "Sightings") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })
  
  output$best_time <- renderUI({
    
    d <- filtered_data()
    
    # get peak month from data
    peak_month <- d |>
      count(month) |>
      slice_max(n, n = 1) |>
      pull(month)
    
    # get top state
    top_state <- d |>
      filter(!is.na(obs_state)) |>
      count(obs_state) |>
      slice_max(n, n = 1) |>
      pull(obs_state)
    
    # get peak hour
    peak_hour <- d |>
      filter(!is.na(hour)) |>
      count(hour) |>
      slice_max(n, n = 1, with_ties = FALSE) |>
      pull(hour)
    
    # format hour to AM/PM
    hour_label <- format(
      strptime(paste0(peak_hour, ":00"), "%H:%M"),
      "%I:%M %p"
    )
    
    # organism emoji
    emoji <- switch(input$organism,
                    "Glowworm"      = "✨",
                    "Gouldian Finch" = "🐦",
                    "Manta Ray"     = "🌊",
                    "Orchid"        = "🌸",
                    "🌿"
    )
    
    div(
      style = "background:#e8f5e9; border-left:4px solid #4a7c59;
             padding:12px; border-radius:8px;",
      p(strong(paste0(emoji, " ", 
                      ifelse(input$organism == "All", 
                             "All organisms", 
                             input$organism))),
        style = "color:#4a7c59; margin-bottom:8px; font-size:15px;"),
      p(paste0("🔢 Total sightings: ", nrow(d)),
        style = "margin:4px 0;"),
      p(paste0("📅 Peak month: ", month.name[peak_month]),
        style = "margin:4px 0;"),
      p(paste0("📍 Top region: ", top_state),
        style = "margin:4px 0;"),
      p(paste0("🕐 Best time: around ", hour_label),
        style = "margin:4px 0;"),
      hr(style = "margin:8px 0;"),
      p(em("Tip: Filter by year to see how patterns change over time!"),
        style = "font-size:11px; color:#666; margin:0;")
    )
  })
  
  output$state_chart <- renderPlot({
    
    filtered_data() |>
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
  
  # update ONLY the markers when dropdown changes
  observe({
    data <- filtered_data()
    
    leafletProxy("map") |>               # update existing map, don't redraw
      clearMarkers() |>                  # remove old markers first
      addCircleMarkers(
        data = data,
        lng = ~obs_lon,
        lat = ~obs_lat,
        radius = 5,
        color = organism_colours[input$organism],
        fillColor = if (input$organism == "All") { #picks color for selected organism
          unname(organism_colours[data$organism])  # each marker gets its own colour
        } else {
          unname(organism_colours[input$organism]) # all markers same colour
        },  
        fillOpacity = 0.3,
        weight=0.5,
        popup = ~paste0(                 # click marker for details
          "Species: ", sci_name,
          "<br>Date: ", date,
          "<br>State: ", obs_state
        )
      )
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