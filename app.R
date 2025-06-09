# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(plotly)
library(ggplot2)

# Load data
load_movielens_data <- function() {
  movies <- read.csv("data/movies.csv", stringsAsFactors = FALSE)
  ratings <- read.csv("data/ratings.csv", stringsAsFactors = FALSE)
  tags <- read.csv("data/tags.csv", stringsAsFactors = FALSE)
  links <- read.csv("data/links.csv", stringsAsFactors = FALSE)
  
  list(
    movies = movies,
    ratings = ratings,
    tags = tags,
    links = links
  )
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "InfoViz Project"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary Statistics", tabName = "summary", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Summary Statistics Tab
      tabItem(tabName = "summary",
              h2("MovieLens Dataset Overview"),
              
              fluidRow(
                valueBoxOutput("total_movies"),
                valueBoxOutput("total_ratings"),
                valueBoxOutput("total_users")
              ),
              
              fluidRow(
                valueBoxOutput("total_tags"),
                valueBoxOutput("avg_rating"),
                valueBoxOutput("date_range")
              ),
              
              fluidRow(
                box(
                  title = "Dataset Structure", status = "primary", solidHeader = TRUE, width = 6,
                  DT::dataTableOutput("dataset_structure")
                ),
                
                box(
                  title = "Top 10 Most Rated Movies", status = "info", solidHeader = TRUE, width = 6,
                  DT::dataTableOutput("top_movies")
                )
              ),
              
              fluidRow(
                box(
                  title = "Rating Distribution", status = "success", solidHeader = TRUE, width = 6,
                  plotlyOutput("rating_distribution")
                ),
                
                box(
                  title = "Genre Distribution", status = "warning", solidHeader = TRUE, width = 6,
                  plotlyOutput("genre_distribution")
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Load data when app starts
  data <- reactive({
    load_movielens_data()
  })
  
  # Value boxes
  output$total_movies <- renderValueBox({
    valueBox(
      value = nrow(data()$movies),
      subtitle = "Total Movies",
      icon = icon("film"),
      color = "blue"
    )
  })
  
  output$total_ratings <- renderValueBox({
    valueBox(
      value = format(nrow(data()$ratings), big.mark = ","),
      subtitle = "Total Ratings",
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  output$total_users <- renderValueBox({
    valueBox(
      value = length(unique(data()$ratings$userId)),
      subtitle = "Total Users",
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$total_tags <- renderValueBox({
    valueBox(
      value = nrow(data()$tags),
      subtitle = "Total Tags",
      icon = icon("tags"),
      color = "purple"
    )
  })
  
  output$avg_rating <- renderValueBox({
    valueBox(
      value = round(mean(data()$ratings$rating), 2),
      subtitle = "Average Rating",
      icon = icon("star-half-alt"),
      color = "orange"
    )
  })
  
  output$date_range <- renderValueBox({
    timestamps <- data()$ratings$timestamp
    dates <- as.Date(as.POSIXct(timestamps, origin = "1970-01-01"))
    min_date <- format(min(dates), "%d/%m/%y")
    max_date <- format(max(dates), "%d/%m/%y")
    date_range <- paste(min_date, "-", max_date)
    
    valueBox(
      value = date_range,
      subtitle = "Rating Period",
      icon = icon("calendar"),
      color = "red"
    )
  })
  
  # Dataset structure table
  output$dataset_structure <- DT::renderDataTable({
    structure_data <- data.frame(
      Dataset = c("Movies", "Ratings", "Tags", "Links"),
      Rows = c(
        nrow(data()$movies),
        nrow(data()$ratings),
        nrow(data()$tags),
        nrow(data()$links)
      ),
      Columns = c(
        ncol(data()$movies),
        ncol(data()$ratings),
        ncol(data()$tags),
        ncol(data()$links)
      ),
      `Key Columns` = c(
        "movieId, title, genres",
        "userId, movieId, rating, timestamp",
        "userId, movieId, tag, timestamp",
        "movieId, imdbId, tmdbId"
      )
    )
    
    structure_data
  }, options = list(pageLength = 10, searching = FALSE, paging = FALSE))
  
  # Top movies table
  output$top_movies <- DT::renderDataTable({
    top_movies <- data()$ratings %>%
      group_by(movieId) %>%
      summarise(
        rating_count = n(),
        avg_rating = round(mean(rating), 2),
        .groups = 'drop'
      ) %>%
      arrange(desc(rating_count)) %>%
      head(10) %>%
      left_join(data()$movies, by = "movieId") %>%
      select(title, rating_count, avg_rating) %>%
      rename(
        Title = title,
        `Rating Count` = rating_count,
        `Avg Rating` = avg_rating
      )
    
    top_movies
  }, options = list(pageLength = 10, searching = FALSE, paging = FALSE))
  
  # Rating distribution plot
  output$rating_distribution <- renderPlotly({
    rating_counts <- data()$ratings %>%
      group_by(rating) %>%
      summarise(count = n(), .groups = 'drop')
    
    p <- ggplot(rating_counts, aes(x = factor(rating), y = count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(
        title = "Distribution of Ratings",
        x = "Rating",
        y = "Number of Ratings"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Genre distribution plot
  output$genre_distribution <- renderPlotly({
    # Extract and count genres
    genres_expanded <- data()$movies %>%
      mutate(genres_split = strsplit(genres, "\\|")) %>%
      tidyr::unnest(genres_split) %>%
      filter(genres_split != "(no genres listed)") %>%
      group_by(genres_split) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count)) %>%
      head(15)
    
    p <- ggplot(genres_expanded, aes(x = reorder(genres_split, count), y = count)) +
      geom_bar(stat = "identity", fill = "coral") +
      coord_flip() +
      labs(
        title = "Top 15 Movie Genres",
        x = "Genre",
        y = "Number of Movies"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)