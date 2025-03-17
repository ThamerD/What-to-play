library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)
library(tidyr)

games <- read_csv("../data/processed/games_1000.csv") |>
    separate(genres, into = c("genre", "other_genres"), sep = ",", extra = "drop", fill = "right", remove = FALSE) |>
    mutate(genre = gsub("\\[|\\]|'| ", "", genre)) |>
    filter(pct_pos_total != 0, metacritic_score != 0, genre != "[]")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bs_theme(version = 4, bootswatch = "flatly"),
    tags$style(HTML("
        .card {
            margin-bottom: 20px;
        }
        .container-fluid {
            padding: 60px;
        }
    ")),
    
    # Application title
    titlePanel("What to play? 🎮"),
    p("Can't decide what game you want to play? Here is dashboard of Steam's 1000 most popular games to help you!"),

    # Sidebar with date range input for filtering games by release date
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("dateRange",
                           "Select release date range:",
                           start = min(games$release_date, na.rm = TRUE),
                           end = max(games$release_date, na.rm = TRUE)),
            selectInput("genreInput",
                        "Select genres:",
                        choices = unique(games$genre),
                        selected = unique(games$genre),
                        multiple = TRUE),
            numericInput("minPlayerScore",
                         "Minimum player score:",
                         value = 0,
                         min = 0,
                         max = 100),
            numericInput("minCriticScore",
                         "Minimum critic score:",
                         value = 0,
                         min = 0,
                         max = 100)
        ),

        # Show plots of the generated distributions
        mainPanel(
            div(class = "card",
                div(class = "card-body",
                    h4(class = "text-center", "Scatter Plot of Player Score vs Critic Score"),
                    plotlyOutput("scatterPlot")
                )
            ),
            div(class = "card",
                div(class = "card-body",
                    h4(class = "text-center", "Count of Games per Genre"),
                    plotlyOutput("genreCountPlot")
                )
            )
        )
    )
)

# Define server logic required to draw histograms
server <- function(input, output) {

    output$scatterPlot <- renderPlotly({
        # Filter games by selected date range, genres, and scores
        filtered_games <- games |>
            filter(release_date >= input$dateRange[1] & release_date <= input$dateRange[2]) |>
            filter(genre %in% input$genreInput) |>
            filter(pct_pos_total >= input$minPlayerScore) |>
            filter(metacritic_score >= input$minCriticScore)

        # Add jitter to the data
        filtered_games <- filtered_games |>
            mutate(jittered_pct_pos_total = jitter(pct_pos_total),
                   jittered_metacritic_score = jitter(metacritic_score))

        # Create the scatter plot
        plot_ly(
            data = filtered_games,
            x = ~jittered_pct_pos_total,
            y = ~jittered_metacritic_score,
            type = 'scatter',
            mode = 'markers',
            color = ~genre,
            size = filtered_games$peak_ccu^(1/3),
            opacity = 0.8,
            text = ~paste(
                "Name: ", name, "<br>",
                "Release Date: ", release_date, "<br>",
                "Average player score: ", pct_pos_total, "<br>",
                "Average critic score: ", metacritic_score, "<br>",
                "Genres: ", genres, "<br>",
                "Peak player count: ", peak_ccu, "<br>"
            ),
            hoverinfo = 'text'
        ) |>
        layout(
            xaxis = list(title = "Average player score"),
            yaxis = list(title = "Average critic score"),
            legend = list(orientation = 'v', x = -0.35, y = 1)
        )
    })

    output$genreCountPlot <- renderPlotly({
        # Count games per genre
        genre_counts <- games |>
            filter(release_date >= input$dateRange[1] & release_date <= input$dateRange[2]) |>
            filter(genre %in% input$genreInput) |>
            filter(pct_pos_total >= input$minPlayerScore) |>
            filter(metacritic_score >= input$minCriticScore) |>
            count(genre) |>
            arrange(desc(n))

        # Create the bar plot
        plot_ly(
            data = genre_counts,
            x = ~n,
            y = ~reorder(genre, n),
            type = 'bar',
            orientation = 'h',
            marker = list(color = 'rgba(50, 171, 96, 0.6)',
                          line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))
        ) |>
        layout(
            title = "",
            xaxis = list(title = ""),
            yaxis = list(title = "")
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)