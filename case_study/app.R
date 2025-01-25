library(shiny)
library(shinydashboard)

# Custom CSS to make the header fixed and change its color to light grey
fixed_header_css <- "
/* Fix header styling */
.main-header, .main-header .navbar, .skin-blue .main-header .logo {
  position: fixed;
  width: 100%;
  z-index: 1000;
  background-color: #FFFFE0 !important; /* Set header background color to light grey */
  color: #343a40 !important; /* Set text color */
}
.skin-blue .main-header .logo span {
  color: #eb3434 !important; /* Set title text color to maroon */
}
.content-wrapper, .main-footer {
  margin-top: 50px; /* Adjust margin based on header height */
}

/* Disable vertical scrollbar for the Shiny app */
.content-wrapper {
  overflow: hidden !important;
  height: 100vh; /* Ensure content does not extend beyond the viewport */
}
body {
  overflow: hidden !important; /* Disable scrollbars for the entire app */
}
.box.box-primary {
  border: 1px solid #eb3434 !important; /* Set the border color to #eb3434 */
}
.box.box-primary > .box-header {
  background-color: #eb3434 !important;
  color: #ffffff !important;
  padding: 1px; /* Adjust padding to minimize thickness */
}
"

# Define the UI
ui <- dashboardPage(
  title = "R Case Study Report",
  dashboardHeader(
    title = tags$div(
      tags$img(src = "logo.png", height = "40px", style = "margin-right: 10px;"),
      "Case Study Report",
      style = "color: #4682B4; text-align: center; width: 100%; font-size: 24px; font-weight: bold;"
    ),
    titleWidth = "100%"  # Extend the title across the header
  ),
  dashboardSidebar(disable = TRUE),  # Use an empty or disabled sidebar
  dashboardBody(
    tags$head(
      tags$style(HTML(fixed_header_css)), 
      tags$style(HTML('.box.box-primary > .box-header { 
                      background-color: #eb3434 !important; 
                      color: #ffffff !important; 
                      padding: 1px; /* Adjust padding to minimize thickness */
                    }'))
    ),
    fluidRow(
      box(
        title = "",
        status = "primary",
        solidHeader = TRUE,
        tags$iframe(
          src = "Analysis_Report.html",  # Relative to the www/ directory
          width = "100%",
          height = "800px",  # Adjust height as needed
          frameborder = "0"
        ),
        width = 12
      )
    )
  )
)

# Define an empty server function
server <- function(input, output) {
  # Since you do not have server logic, this can be left empty.
}

# Launch the Shiny app
shinyApp(ui = ui, server = server)