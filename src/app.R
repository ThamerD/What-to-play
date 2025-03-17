library(shiny)
library(bslib)
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
        .fixed-sidebar {
            position: fixed;
            top: 180px;
            width: 1500px;
        }
        .main-panel {
            margin-left: 20%;
        }
    ")),
    
    # Application title
    titlePanel("What to play? 🎮"),
    p("Can't decide what game you want to play? Here is dashboard of Steam's 1000 most popular games to help you! Created by Thamer Aldawood."),

    # Sidebar with date range input for filtering games by release date
    fluidRow(
        column(3,
               div(class = "fixed-sidebar",
                   sidebarPanel(
                       dateRangeInput("dateRange",
                                      "Select release date range:",
                                      start = min(games$release_date, na.rm = TRUE),
                                      end = max(games$release_date, na.rm = TRUE)),
                       selectInput("genreInput",
                                   "Select genres:",
                                   choices = unique(games$genre),
                                   # selected = unique(games$genre),
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
                                    max = 100),
                       selectInput("scoreType",
                                   "Select score type for line chart:",
                                   choices = c("Critic Score" = "metacritic_score", "Player Score" = "pct_pos_total"),
                                   selected = "metacritic_score")
                   )
               )
        ),
        column(9,
               div(class = "main-panel",
                   div(class = "card",
                       div(class = "card-body",
                           h4(class = "text-center", "Scatter Plot of Player Score vs Critic Score"),
                           p(class = "text-center", "*point size represents game popularity"),
                           plotlyOutput("scatterPlot")
                       )
                   ),
                   div(class = "card",
                       div(class = "card-body",
                           h4(class = "text-center", "Count of Games per Genre"),
                           plotlyOutput("genreCountPlot")
                       )
                   ),
                   div(class = "card",
                       div(class = "card-body",
                           h4(class = "text-center", "Game Popularity vs Score"),
                           plotlyOutput("lineChart")
                       )
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
            filter(if (length(input$genreInput) > 0) genre %in% input$genreInput else TRUE) |>
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
            filter(if (length(input$genreInput) > 0) genre %in% input$genreInput else TRUE) |>
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

    output$lineChart <- renderPlotly({
        # Filter games by selected date range, genres, and scores
        filtered_games <- games |>
            filter(release_date >= input$dateRange[1] & release_date <= input$dateRange[2]) |>
            filter(if (length(input$genreInput) > 0) genre %in% input$genreInput else TRUE) |>
            filter(pct_pos_total >= input$minPlayerScore) |>
            filter(metacritic_score >= input$minCriticScore)

        # Calculate average player count for each score
        avg_player_count <- filtered_games |>
            group_by(score = get(input$scoreType)) |>
            summarize(avg_peak_ccu = mean(peak_ccu, na.rm = TRUE)) |>
            arrange(score)

        # Create the line chart with average player count
        plot_ly(
            data = avg_player_count,
            x = ~score,
            y = ~avg_peak_ccu,
            type = 'scatter',
            mode = 'lines+markers',
            text = ~paste(
                "Score: ", score, "<br>",
                "Average Peak Player Count: ", avg_peak_ccu
            ),
            hoverinfo = 'text'
        ) |>
        layout(
            xaxis = list(title = ifelse(input$scoreType == "metacritic_score", "Critic Score", "Player Score")),
            yaxis = list(title = "Average Peak Player Count"),
            legend = list(orientation = 'v', x = -0.35, y = 1)
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)