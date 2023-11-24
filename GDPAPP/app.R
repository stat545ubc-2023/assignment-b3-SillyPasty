library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gapminder)

data <- gapminder

# Define UI for application
ui <- fluidPage(
  # Feature 1 image
  img(src = "gapminder.png", height = 100, width = 100),
  titlePanel("GDP Viewer"),
  sidebarLayout(
    sidebarPanel(
      # Feature 2 interactive filter
      selectInput("countryInput", "Country", choices = levels(data$country)),
      sliderInput("GDPInput", "Gdp", min = min(data$gdpPercap), max = max(data$gdpPercap), value = c(100, 5000), pre = "$"),
      sliderInput("yearInput", "Select GDP Year", step = 1, min = min(data$year), max = max(data$year), value = c(min(data$year), max(data$year))),
      # Feature 3 download
      downloadButton("downloadPlot", "Download Plot"),
      downloadButton("downloadCSV", "Download CSV")
    ),
    mainPanel(DT::dataTableOutput("results"),
              br(), br(),
              plotOutput("plot")),
  )
)

server <- function(input, output){
  data_filtered <- reactive({
    data %>%
      filter(country == input$countryInput,
             gdpPercap >= input$GDPInput[1],
             gdpPercap <= input$GDPInput[2],
             year >= input$yearInput[1],
             year <= input$yearInput[2]
      ) %>%
      select(country, year, gdpPercap)
  })

  output$plot <- renderPlot({
    ggplot(data_filtered(), aes(x = year, y = gdpPercap)) + geom_point() + ggtitle("GDP Per Capita Per Year")
  })

  output$results <- DT::renderDataTable({
    data_filtered()
  })

  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("gdp_plot_", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = {
        ggplot(data_filtered(), aes(x = year, y = gdpPercap)) + geom_point() + ggtitle("GDP Per Capita Per Year")
      }, device = "jpg", width = 8, height = 6, units = "in", dpi = 300)
    }
  )

  # Create a download handler
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("gdp_csv_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_filtered(), file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
