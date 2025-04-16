library(shiny)

ui <- fluidPage(
  titlePanel("🔥 Wildfire Model Interactive Tool"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("surfaceFuel", "Surface Fuel (tons/acre)", min = 0, max = 10, value = 1, step = 0.1),
      sliderInput("canopyCover", "Forest Canopy Cover (%)", min = 0, max = 100, value = 30),
      sliderInput("fuelMoisture", "Fuel Moisture (%)", min = 0, max = 50, value = 10),
      sliderInput("slope", "Slope Steepness (%)", min = 0, max = 100, value = 15),
      sliderInput("aspect", "Aspect (degrees)", min = 0, max = 360, value = 180),
      sliderInput("windSpeed", "Wind Speed (mph)", min = 0, max = 100, value = 10),
      sliderInput("windDir", "Wind Direction (degrees)", min = 0, max = 360, value = 90),
      actionButton("runSim", "Run Dummy Simulation")
    ),
    
    mainPanel(
      h4("Simulation Summary (This is similar to what we are going to do. but each variable is a simulation of what we intend to do.. More to come):"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$runSim, {
    output$summary <- renderPrint({
      cat("Dummy Simulation Run. \n")
      cat("-----------------------\n")
      cat("Surface Fuel: ", input$surfaceFuel, "tons/acre\n")
      cat("Canopy Cover: ", input$canopyCover, "%\n")
      cat("Fuel Moisture: ", input$fuelMoisture, "%\n")
      cat("Slope: ", input$slope, "%\n")
      cat("Aspect: ", input$aspect, "°\n")
      cat("Wind Speed: ", input$windSpeed, "mph\n")
      cat("Wind Direction: ", input$windDir, "°\n")
      cat("\n📝 More model logic will be added soon.")
    })
  })
}

shinyApp(ui = ui, server = server)
