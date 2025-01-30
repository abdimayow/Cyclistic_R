library(leaflet)

#Code
# Choices for drop-downs
vars <- c(
  "Member Rides" = "member",
  "Casual Rides" = "casual",
  "Total Rides" = "rides"
)


navbarPage(
  title = tags$div(
    tags$img(
      src = "logo.png",  # Path to your logo file
      class = "navbar-brand-logo"  # Add a class for CSS targeting
    ),
    "Cyclistic Bike Stations"
  ),
  id = "nav",
  
  # Move custom CSS and scripts to the header argument
  header = tags$head(
    includeCSS("styles.css"),   # Include your custom CSS
    includeScript("gomap.js"),   # Include any custom JavaScript
    
  ),
  
   
  tabPanel("Interactive map",
    div(class="outer",

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Station explorer"),

        selectInput("color", "Color", vars),
        selectInput("size", "Size", vars, selected = "rides"),
        conditionalPanel(
          "input.color == 'rides' || input.size == 'rides' || input.color == 'member' || input.size == 'member' || input.color == 'casual' || input.size == 'casual'",
          # You can add a numeric input or other condition-specific UI components if needed
          numericInput("threshold", "Ride threshold (filter top n rides)", 10)
        ),
        
        plotOutput("histRides", height = 200),
        plotOutput("scatterRides", height = 250)
        
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('Cyclistic BikeShare: Comparison between casual and mamber riders, Jan 2024–Dec 2024'), ' by Abdirahman Ali (2025).'
      )
    )
  ),

  tabPanel("Data explorer",
   
    fluidRow(
      column(1,
        numericInput("minRides", "Min ride", min=0, max=100, value=0)
      ),
      column(1,
        numericInput("maxRides", "Max ride", min=0, max=100, value=100)
      )
    ),
    hr(),
    tags$div(id = "data-tab",
    DT::dataTableOutput("stationTable")
    )
  ),
  tabPanel("Report",
           tags$div(
             id = "report-tab",  # Assign a unique ID for CSS targeting
             tags$iframe(
               src = "Analysis_Report.html",  # Path to the HTML report
               width = "100%",               # Full width of the container
               height = "100%",              # Full height of the container
               style = "border: none;"       # Remove iframe borders
               
             ),

           )
  ),

)
