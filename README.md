# What-to-play
A dashboard of Steam's 1000 most popular games.

## Motivation
Target audience: Indecisive gamers.

There are so many amazing games out there but so little time to play them. To help you decide what to spend your infinitely valuable time on, you can use this dashboard to compare Steam's 1000 most popular games according to various features such as average critic score, average player score, genre, and release date. Not only that, this dashboard will also help you look at some interesting relationships between a game's popularity and its public and critical reception. A simple way to start using this dashboard is to choose what game genres you're interested in, and use the release date filter to decide whether you only want to see newer games or if you don't mind older games, then take a look at the games that appear at the top right of the scatter plot as they will have the highers critic and player scores.

## Usage
- Ensure you have the following packages installed: ("shiny", "bslib", "dplyr", "readr", "plotly", "tidyr"). You can install them using the following R command: ``` install.packages(c("shiny", "bslib", "dplyr", "readr", "plotly", "tidyr")) ```
- Navigate to the src folder which contains the "app.R" file and use the following command in an R terminal: ``` shiny::runApp() ```

#### Data Source
The data was scraped from Steam's web API using the Steam-Games-Scraper tool (https://github.com/FronkonGames/Steam-Games-Scraper).
