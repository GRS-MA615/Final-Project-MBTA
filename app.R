library(shiny)
library(googleway)
library(mapsapi)
library(dplyr)
library(randomcoloR)

ui <- navbarPage("DIRECTIONS", position = c("static-top"),
                 tabPanel("MAP", google_mapOutput(outputId = "mapMassachusetts"),
                 textInput(inputId = "origin", label = "Starting point"),
                 textInput(inputId = "destination", label = "Destination"),
                 actionButton(inputId = "getRoute", label = "Get Route")
                                                                   
)
)

server <- function(input, output, session) {
  
  api_key <- "AIzaSyBzsI78LFeLFjWKRWfFv1SAW_jO-p3o9vA"
  
  output$mapMassachusetts <- renderGoogle_map({
    google_map(key = api_key,
               search_box = TRUE,
               location = c(42.37856605851835, -71.10401430972485),
               scale_control = TRUE,
               height = 1000) %>%
      add_traffic()
  })
  
  observeEvent(input$getRoute,{
    
    print("getting route")
    o <- input$origin
    d <- input$destination
    
    doc <- mp_directions(origin = o,
                         destination = d,
                         alternatives = TRUE,
                         key = api_key,
                         quiet = TRUE,
                         transit_mode = c("bus", "subway"))
    df <- mp_get_routes(doc)
    i <- nrow(df)
    df$colour <- c(randomColor(count = i, luminosity = "dark"))

    google_map_update(map_id = "mapMassachusetts") %>%
      clear_traffic() %>%
      clear_polylines() %>%
      clear_markers() %>%
      add_traffic() %>%
      add_polylines(data = df,
                    polyline = "geometry",
                    stroke_colour = "colour",
                    stroke_weight = 5,
                    stroke_opacity = 0.9,
                    info_window = "duration_text",
                    load_interval = 100)
     
  })
  
  
}

shinyApp(ui, server)



