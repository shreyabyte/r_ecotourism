# ecotourism-analytics

An interactive R/Shiny analytics platform for exploring Australian wildlife sightings, tourism trends, and environmental patterns using the ecotourism dataset.

Developed as part of contributor test submissions for Google Summer of Code 2026.

## About the Project

The [`ecotourism`](https://github.com/numbats/ecotourism) package 
provides occurrence records for four Australian organisms:
glowworms, Gouldian finches, manta rays and orchids alongside 
daily weather data and domestic tourism statistics from 2014–2024.

---

## Project Components

### Interactive Tutorial
**Are Glowworm Sightings Driven by Tourism?**

A Quarto tutorial that develops three exercises practicing joins 
across the occurrence, weather and tourism datasets. The tutorial 
investigates whether glowworm sightings in Tasmania correlate with 
tourist activity and whether Holiday or Business tourists are 
the ones doing the recording.

- 🔗 [Live Tutorial](https://shreyabyte.github.io/r_ecotourism/tutorial_analysis/)
- 📁 [Source Code](https://github.com/shreyabyte/r_ecotourism/blob/master/tutorial_analysis/tutorial.qmd)

**Key features:**
- Interactive leaflet map of glowworm sightings
- Plotly charts for tourism vs sightings analysis
- DT scrollable data tables
- Callout solution blocks


### Wildlife Explorer Dashboard
**Australian Wildlife Explorer**

An interactive Shiny app for exploring wildlife sightings across 
Australia with four tabs:

- **Explorer** - interactive map with organism colours, 
  clustered/individual marker toggle, and 5 live stat cards
- **Weather Analysis** - temperature trends, rainfall patterns 
  and a temperature heatmap across organisms
- **Trip Planner** - personalised spotting guide based on 
  user preferences (organism, state, month, time of day)
- **Trends** - sightings over time, monthly patterns, 
  state breakdown and organism share over time

- 🔗 [Shiny App](https://shreyabyte.shinyapps.io/medium_task/)
- 📁 [Source Code](https://github.com/shreyabyte/r_ecotourism/blob/master/wildlife_dashboard/app.R)

**Key features:**
- Multi-tab dashboard layout
- Dynamic organism colour palette
- Personalised trip planning recommendations
- Stacked area chart for organism trends over time


### Wildlife Prediction System
**Predicting the Best Times to Spot Wildlife**

A Quarto document developing `predict_best_times()`: a 
confidence-aware scoring function that recommends the top five 
month × time-of-day combinations for spotting each organism.

- 🔗 [Live Document](https://shreyabyte.github.io/r_ecotourism/prediction_analysis/)
- 📁 [Source Code](https://github.com/shreyabyte/r_ecotourism/blob/master/prediction_analysis/prediction_analysis.qmd)

**Key features:**
- Automatic data cleaning (machine observations, hour = 0)
- Context-aware predictions for glowworms (cave tourism vs wild)
- Confidence scoring honest about data sparsity
- Month × hour heatmap visualisation across all four organisms

---

## Repository Structure
```
r_ecotourism/
├── wildlife_dashboard/
│   └── tutorial.qmd        # Tutorial questions + solutions
├── medium_task/
│   └── app.R               # Shiny app
├── prediction_analysis/
│   └── prediction_analysis.qmd       # Prediction function + analysis
└── README.md
```


## Setup
```r
# Install the ecotourism package
install.packages("pak")
pak::pak("vahdatjavad/ecotourism")

# Additional packages used across tasks
install.packages(c("tidyverse", "shiny", "leaflet", 
                   "plotly", "DT", "shinythemes", 
                   "shinyWidgets"))
```


*Data source: Atlas of Living Australia (2014–2024)*  
*Package: ecotourism (GSoC 2025)*