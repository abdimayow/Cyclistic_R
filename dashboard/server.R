library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)




function(input, output, session) {

  ## Interactive Map ###########################################

  #Copy
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -87.65562, lat = 41.90722, zoom = 11.5)
  })
  #41.93066638533173, -87.46838509054308
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
   # Copy
  stationsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(allstations[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(allstations,
           lat >= latRng[1] & lat <= latRng[2] &
             lng >= lngRng[1] & lng <= lngRng[2])
  })

  
  
  # Code2 Precalculate the breaks for the 'rides' column
  ridesBreaks <- hist(plot = FALSE, allstations$rides, breaks = 20)$breaks
  

  output$histRides <- renderPlot({
    # If no stations are in view, don't plot
    if (nrow(stationsInBounds()) == 0)
      return(NULL)
    
    hist(stationsInBounds()$rides,
         breaks = ridesBreaks,  # Use precomputed breaks for consistency
         main = "Total Rides (Visible Stations)",
         xlab = "Total Rides",
         xlim = range(allstations$rides),  # Use the range of all rides
         col = '#00DD00',  # Green fill
         border = 'white')  # White border for bins
  })
  
  
  #Code2
  output$scatterRides <- renderPlot({
    # If no stations are in view, don't plot
    if (nrow(stationsInBounds()) == 0)
      return(NULL)
    
    print(xyplot(casual ~ member, 
                 data = stationsInBounds(), 
                 xlim = range(allstations$member), 
                 ylim = range(allstations$casual),
                 main = "Scatter Plot of Member vs Casual Rides",
                 xlab = "Member Rides",
                 ylab = "Casual Rides"))
  })
  

  observe({
    colorBy <- input$color   # User's choice for color mapping (rides, member, or casual)
    sizeBy <- input$size     # User's choice for size mapping (rides, member, or casual)
    
    # Handle color mapping
    colorData <- allstations[[colorBy]]  # Dynamically select the column for color
    pal <- colorBin("viridis", colorData, 7, pretty = FALSE)  # Continuous color palette
    
    # Handle size mapping
    sizeData <- allstations[[sizeBy]]  # Dynamically select the column for size
    sizeThreshold <- 0.004  # Threshold (fraction of the max value) beyond which size is scaled down
    maxSize <- max(sizeData, na.rm = TRUE)  # Haif of Maximum value in the size data
    
    # Scale size for marker radius with a threshold
    radius <- ifelse(
      sizeData <= sizeThreshold * maxSize,  # Below threshold
      sizeData / maxSize * 2500,          # Normal scaling
      (sizeThreshold * 2500) + (sizeData - sizeThreshold * maxSize) / maxSize * 2500 * 0.5  # Scaled down for larger values
    )
    
    # Update the Leaflet map
    leafletProxy("map", data = allstations) %>%
      clearShapes() %>%
      addCircles(
        ~lng, ~lat, 
        radius = radius, 
        layerId = ~station_id,  # Use station_id as a unique identifier
        stroke = FALSE, 
        fillOpacity = 0.4, 
        fillColor = pal(colorData)  # Use color palette for circles
      ) %>%
      addLegend(
        "bottomleft", 
        pal = pal, 
        values = colorData, 
        title = colorBy, 
        layerId = "colorLegend"
      )
  })
  
  
  
  
  
  #Code
  # Show a popup at the given location
  showStationPopup <- function(station_id, lat, lng) {
    # Filter the selected station by station_id
    selectedStation <- allstations[allstations$station_id == station_id, ]
    
    # Create the popup content
    content <- as.character(tagList(
      tags$h4("Station:", selectedStation$station_name),
      tags$strong(HTML(sprintf("Station ID: %s", selectedStation$station_id))), tags$br(),
      sprintf("Member Rides: %s", formatC(selectedStation$member, format = "d", big.mark = ",")), tags$br(),
      sprintf("Casual Rides: %s", formatC(selectedStation$casual, format = "d", big.mark = ",")), tags$br(),
      sprintf("Total Rides: %s", formatC(selectedStation$rides, format = "d", big.mark = ","))
    ))
    
    # Add the popup to the map at the specified location
    leafletProxy("map") %>% 
      addPopups(lng, lat, content, layerId = station_id)
  }
 

  #Code
  # When map is clicked, show a popup with station info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click  # Get the map click event
    if (is.null(event))
      return()
    
    isolate({
      showStationPopup(event$id, event$lat, event$lng)  # Call the popup function
    })
  })
  
  

  ## Data Explorer ###########################################

  #Code
  observe({
    if (is.null(input$goto))
      return()
    
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      
      # Retrieve data from the input$goto object
      dist <- 0.5
      station_id <- input$goto$station_id
      lat <- input$goto$lat
      lng <- input$goto$lng
      
      # Call the popup function to display station information
      showStationPopup(station_id, lat, lng)
      
      # Adjust the map view to focus on the selected station
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  

  
  #Code
  output$stationTable <- DT::renderDataTable({
    # Filter the dataset based on inputs
    df <- allstations %>%
      filter(
        rides >= input$minRides,
        rides <= input$maxRides,
        is.null(input$station_names) | station_name %in% input$station_names,
        is.null(input$station_ids) | station_id %in% input$station_ids
      ) %>%
      mutate(Action = paste(
        '<a class="go-map" href="" data-lat="', lat, 
        '" data-long="', lng, 
        '" data-station_id="', station_id, 
        '"><i class="fa fa-crosshairs"></i></a>', 
        sep = ""
      ))
    
    # Set up server-side processing for large datasets
    action <- DT::dataTableAjax(session, df, outputId = "stationTable")
    
    # Render the table
    DT::datatable(
      df, 
      options = list(ajax = list(url = action)), 
      escape = FALSE
    )
  })
  
}
