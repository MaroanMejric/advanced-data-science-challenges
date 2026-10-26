library(gapminder)
library(dplyr)
library(ggplot2)
library(shiny)

# User interface
ui <- fluidPage(

  titlePanel("Gapminder Data"),

  sidebarLayout(
    sidebarPanel(
      # Year input
      sliderInput("year",
                  "Year",
                  min = 1952,
                  max = 2007,
                  step = 5,
                  value = 1952),

      # Continent input
      selectInput("continent", "Continent",
                  choices = list("Africa", "Americas", "Asia", "Europe", "Oceania", "All"),
                  selected = "All"),

      # x input
      radioButtons("xaxis", "X",
                   choices = list("Life expectancy" = "lifeExp",
                                  "Population" = "pop",
                                  "GDP per capita" = "gdpPercap"),
                   selected = "gdpPercap"),

      # y input
      radioButtons("yaxis", "Y",
                   choices = list("Life expectancy" = "lifeExp",
                                  "Population" = "pop",
                                  "GDP per capita" = "gdpPercap"),
                   selected = "lifeExp")
    ),

    # scatter plot output
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

# Server logic
server <- function(input, output, session) {

  # reactive dataset: the countries of the selected year and continent
  dataset <- reactive({
    if (input$continent == "All") {
      filter(gapminder, year == input$year)
    } else {
      filter(gapminder, year == input$year, continent == input$continent)
    }
  })

  output$scatterPlot <- renderPlot({
    # draw the scatter plot
    ggplot(dataset(), aes(x = .data[[input$xaxis]], y = .data[[input$yaxis]],
                          size = pop, colour = country)) +
      geom_point(alpha = 0.5, show.legend = FALSE) +
      scale_colour_manual(values = country_colors) +
      scale_size(range = c(2, 20)) +
      scale_x_log10() +
      scale_y_log10() +
      labs(x = input$xaxis, y = input$yaxis) +
      theme_minimal()
  })
}

# Launch application
shinyApp(ui = ui, server = server)
