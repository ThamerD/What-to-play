#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)

games <- read_csv("../data/processed/games_1000.csv")


# Define UI for application that draws a histogram
ui <- page_fluid(

    theme = bs_theme(bootswatch = "darkly"),

    # Application title
    titlePanel("What to play?"),
    p("Can't decide what you want to play? Let us help you!"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("scatterPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatterPlot <- renderPlotly({
        # Create the scatter plot
        p <- ggplot(games, aes(x = peak_ccu, y = metacritic_score, 
                                    text = paste("Name: ", name, "<br>",
                                                 "Release Date: ", release_date, "<br>",
                                                 "Description: ", about_the_game))) +
            geom_point() +
            labs(x = "Number of players", y = "Average critic score") +
            theme_minimal()

        # Convert ggplot to plotly for interactive tooltips
        ggplotly(p, tooltip = "text")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
