# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(plotly)
library(ggplot2)
library(shinycssloaders)
library(tidyr)
library(lubridate)
library(visNetwork)
library(stringr)

# Load data and process it
load_movielens_data <- function() {
  # Read the raw data
  movies <- read.csv("data/movies.csv", stringsAsFactors = FALSE)
  ratings <- read.csv("data/ratings.csv", stringsAsFactors = FALSE)
  tags <- read.csv("data/tags.csv", stringsAsFactors = FALSE)
  links <- read.csv("data/links.csv", stringsAsFactors = FALSE)
  
  # Convert timestamp to proper date format
  ratings$date <- as.Date(as.POSIXct(ratings$timestamp, origin = "1970-01-01"))
  ratings$year <- year(ratings$date)
  
  # Extract release year from movie title (format: "Title (YEAR)")
  movies$releaseYear <- sapply(movies$title, function(title) {
    year_match <- regmatches(title, regexpr("\\((\\d{4})\\)\\s*$", title))
    if (length(year_match) > 0) {
      return(as.numeric(gsub("[()]", "", year_match)))
    } else {
      return(1995)  # Default year if no year found
    }
  }, USE.NAMES = FALSE)
  
  # Clean title by removing year
  movies$clean_title <- gsub("\\s*\\(\\d{4}\\)\\s*$", "", movies$title)
  # Fix title formatting - some titles have articles moved to the end
  movies$clean_title <- gsub("^(.+), (The|A|An)$", "\\2 \\1", movies$clean_title)
  
  # Merge ratings with movie data
  ratings_with_movies <- merge(ratings, movies, by = "movieId")
  
  # Create simplified movie time series - aggregate by year and movie
  movie_time_series <- ratings_with_movies %>%
    group_by(movieId, year) %>%
    summarise(
      avgRating = mean(rating, na.rm = TRUE),
      count = n(),
      title = first(clean_title),
      releaseYear = first(releaseYear),
      .groups = "drop"
    ) %>%
    filter(count >= 5) %>%  # Only keep years with at least 5 ratings
    arrange(movieId, year)
  
  return(list(
    movies = movies,
    ratings = ratings,
    tags = tags,
    links = links,
    ratings_with_movies = ratings_with_movies,
    movie_time_series = movie_time_series
  ))
}

# Function to create genre network data
create_genre_network <- function(movies, min_connections = 15) {
  tryCatch({
    # Separate genres into individual rows
    genre_data <- movies %>%
      separate_rows(genres, sep = "\\|") %>%
      filter(genres != "(no genres listed)", !is.na(genres))
    
    # Create genre pairs (edges) - only strong connections
    genre_pairs <- genre_data %>%
      select(movieId, genres) %>%
      inner_join(genre_data %>% select(movieId, genres), 
                 by = "movieId", relationship = "many-to-many") %>%
      filter(genres.x < genres.y) %>%
      count(genres.x, genres.y, name = "weight") %>%
      rename(from = genres.x, to = genres.y) %>%
      filter(weight >= min_connections) %>%
      arrange(desc(weight)) %>%
      slice_head(n = 50)  # Limit to top 50 connections
    
    # Define distinct colors for genres
    genre_colors <- c(
      "Action" = "#FF4444", "Adventure" = "#4488FF", "Animation" = "#AA44FF",
      "Children" = "#44FF44", "Comedy" = "#FFB347", "Crime" = "#8B4B8B",
      "Documentary" = "#20B2AA", "Drama" = "#2F4F4F", "Fantasy" = "#FF69B4",
      "Film-Noir" = "#1C1C1C", "Horror" = "#B22222", "Musical" = "#00CED1",
      "Mystery" = "#696969", "Romance" = "#FF6347", "Sci-Fi" = "#9370DB",
      "Thriller" = "#DC143C", "War" = "#8B4513", "Western" = "#FF8C00",
      "IMAX" = "#708090"
    )
    
    # Create ALL genre nodes
    all_nodes <- genre_data %>%
      count(genres, name = "movie_count") %>%
      mutate(
        id = genres,
        label = genres,
        title = paste0(
          "<div style='text-align: center; font-family: Arial; padding: 10px;'>",
          "<h3 style='margin: 5px; color: #2C3E50;'>", genres, "</h3>",
          "<p style='margin: 5px; font-size: 16px;'><strong>", scales::comma(movie_count), "</strong> movies</p>",
          "<p style='margin: 3px; font-size: 12px; color: #7F8C8D;'>",
          ifelse(genres %in% c(genre_pairs$from, genre_pairs$to), 
                 "Click to highlight connections", "Standalone genre"),
          "</p>",
          "</div>"
        ),
        value = movie_count,
        size = pmax(30, pmin(80, sqrt(movie_count) * 4)),
        color = ifelse(genres %in% names(genre_colors), 
                       genre_colors[genres], "#95A5A6"),
        borderWidth = 4,
        font.size = 16,
        font.color = "#FFFFFF",
        font.strokeWidth = 2,
        font.strokeColor = "#000000"
      ) %>%
      arrange(desc(movie_count)) %>%
      select(id, label, title, value, size, color, borderWidth, font.size, font.color, font.strokeWidth, font.strokeColor)
    
    # Only include edges that reference existing nodes
    edges <- genre_pairs %>%
      filter(from %in% all_nodes$id, to %in% all_nodes$id) %>%
      mutate(
        width = pmax(1, pmin(8, (weight - min(weight)) / (max(weight) - min(weight)) * 7 + 1)),
        title = paste0(
          "<div style='text-align: center; font-family: Arial; padding: 8px;'>",
          "<h4 style='margin: 5px; color: #2C3E50;'>", from, " + ", to, "</h4>",
          "<p style='margin: 3px; font-size: 16px;'><strong>", weight, "</strong> movies together</p>",
          "</div>"
        ),
        color_intensity = (weight - min(weight)) / (max(weight) - min(weight)),
        color = rgb(
          red = 0.1 + (1 - color_intensity) * 0.7,
          green = 0.4 + (1 - color_intensity) * 0.4,
          blue = 0.9,
          alpha = 0.6 + color_intensity * 0.4
        )
      ) %>%
      select(from, to, width, title, color)
    
    return(list(nodes = all_nodes, edges = edges))
    
  }, error = function(e) {
    print(paste("Error in create_genre_network:", e$message))
    return(list(
      nodes = data.frame(id = character(), label = character(), stringsAsFactors = FALSE),
      edges = data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
    ))
  })
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "InfoViz Project"),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Summary Statistics", tabName = "summary", icon = icon("chart-bar")),
    menuItem("Movie Comparison", tabName = "movies", icon = icon("film")),
    menuItem("Genre Network", tabName = "network", icon = icon("project-diagram")),
    menuItem("User Analysis", tabName = "users", icon = icon("user")),
    menuItem("Movie Explorer", tabName = "explorer", icon = icon("search"))
  )),
  
  dashboardBody(
    # Add custom CSS for modal styling
    tags$head(
      tags$style(HTML("
        .modal-dialog { margin-top: 50px; }
        .genre-grid { 
          display: grid; 
          grid-template-columns: repeat(auto-fit, minmax(120px, 1fr)); 
          gap: 8px; 
          padding: 15px; 
        }
        .genre-checkbox { margin-bottom: 8px !important; }
        .selected-genres-display { 
          background-color: #f8f9fa; 
          border: 1px solid #dee2e6; 
          padding: 8px; 
          border-radius: 4px; 
          min-height: 40px;
          max-height: 80px;
          overflow-y: auto;
        }
      "))
    ),
    
    tabItems(
      # Summary Statistics Tab
      tabItem(
        tabName = "summary",
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
            title = "Dataset Structure",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DT::dataTableOutput("dataset_structure")
          ),
          
          box(
            title = "Top 10 Most Rated Movies",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            DT::dataTableOutput("top_movies")
          )
        ),
        
        fluidRow(
          box(
            title = "Rating Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("rating_distribution")
          ),
          
          box(
            title = "Genre Distribution",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("genre_distribution")
          )
        )
      ),
      
      # Movie Explorer Tab
      tabItem(
        tabName = "explorer",
        fluidRow(
          column(3,
                 box(
                   title = "Movie Filters",
                   status = "primary",
                   solidHeader = TRUE,
                   width = NULL,
                   
                   sliderInput("rating_range", "Rating Range:",
                               min = 0.5, max = 5, value = c(0.5, 5), step = 0.1),
                   
                   sliderInput("min_ratings", "Minimum Number of Ratings:",
                               min = 1, max = 100, value = 10, step = 1),
                   
                   sliderInput("year_filter", "Release Year Range:",
                               min = 1920, max = 2018, value = c(1990, 2018), step = 1),
                   
                   # Genre selection with popup
                   div(
                     h5("Selected Genres:"),
                     div(
                       id = "selected_genres_display",
                       class = "selected-genres-display",
                       textOutput("selected_genres_text")
                     ),
                     br(),
                     div(style = "display: flex; gap: 10px;",
                         actionButton("btn_select_genres", "Select Genres", 
                                      icon = icon("tags"), class = "btn-primary btn-sm"),
                         actionButton("btn_reset", "Reset All", class = "btn-sm btn-warning")
                     ),
                     br()
                   ),
                   
                   # Color coding option
                   radioButtons("color_by", "Color Points By:",
                                choices = list("None" = "none", 
                                               "Decade" = "decade",
                                               "Popularity" = "popularity",
                                               "Quality Tier" = "quality",
                                               "Genre" = "genre"),
                                selected = "none")
                 )
          ),
          
          column(9,
                 box(
                   title = "Interactive Movie Explorer",
                   status = "primary",
                   solidHeader = TRUE,
                   width = NULL,
                   height = "600px",
                   withSpinner(plotlyOutput("movie_scatter", height = "550px"))
                 )
          )
        ),
        
        fluidRow(
          box(
            title = "Top Movies (Filtered Results)",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("filtered_movies_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Filter Summary",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("filter_summary")
          ),
          
          box(
            title = "How to Use This Visualization",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            tags$div(
              tags$h5("Understanding the Plot:"),
              tags$ul(
                tags$li(strong("X-axis:"), " Movie release year"),
                tags$li(strong("Y-axis:"), " Average user rating (1-5 stars)"),
                tags$li(strong("Point size:"), " Number of ratings (bigger = more popular)")
              ),
              
              tags$h5("Color Options:"),
              tags$ul(
                tags$li(strong("None:"), " All points blue"),
                tags$li(strong("Decade:"), " Color by time period (1980s, 1990s, etc.)"),
                tags$li(strong("Popularity:"), " Low (<50), Medium (50-99), High (100+) ratings"),
                tags$li(strong("Quality Tier:"), " Rating categories from Poor to Excellent"),
                tags$li(strong("Genre:"), " Color by main genre (first listed)")
              ),
              
              tags$h5("Interaction:"),
              tags$ul(
                tags$li(strong("Hover:"), " See movie details"),
                tags$li(strong("Zoom:"), " Mouse wheel to zoom in/out"),
                tags$li(strong("Pan:"), " Click and drag to move around")
              )
            )
          )
        )
      ),
      
      # User Analysis Tab
      tabItem(
        tabName = "users",
        fluidRow(
          column(3,
                 box(
                   title = "User Selection",
                   status = "primary",
                   solidHeader = TRUE,
                   width = NULL,
                   selectizeInput("selected_user", "Select User ID:",
                                  choices = NULL,
                                  options = list(placeholder = "Type to search...")),
                   br(),
                   h5("User Statistics:"),
                   verbatimTextOutput("user_stats_text")
                 )
          ),
          column(9,
                 fluidRow(
                   column(4, valueBoxOutput("user_total_ratings", width = NULL)),
                   column(4, valueBoxOutput("user_avg_rating", width = NULL)),
                   column(4, valueBoxOutput("user_active_years", width = NULL))
                 ),
                 box(
                   title = "Rating Distribution",
                   status = "primary",
                   solidHeader = TRUE,
                   width = NULL,
                   height = "350px",
                   plotlyOutput("user_rating_dist", height = "300px")
                 )
          )
        ),
        fluidRow(
          box(
            title = "Genre Preferences",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("user_genre_dist", height = "400px")
          ),
          box(
            title = "Rating Timeline",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("user_timeline", height = "400px")
          )
        )
      ),
      
      # Genre Network Tab
      tabItem(
        tabName = "network",
        fluidRow(
          column(3,
                 box(
                   title = "Network Controls", 
                   status = "primary", 
                   solidHeader = TRUE,
                   width = NULL,
                   selectInput("layout_type", "Layout Style:",
                               choices = list(
                                 "Force-directed" = "layout_with_fr",
                                 "Circular" = "layout_in_circle", 
                                 "Grid" = "layout_on_grid"
                               ),
                               selected = "layout_with_fr"),
                   
                   sliderInput("min_connections", "Minimum Co-occurrences:",
                               min = 10, max = 300, value = 30, step = 10),
                   
                   hr(),
                   
                   h5("Network Statistics:"),
                   verbatimTextOutput("network_stats", placeholder = TRUE)
                 ),
                 
                 box(
                   title = "How to Use This Visualization",
                   status = "info",
                   width = NULL,
                   tags$ul(
                     tags$li(strong("Node size:"), " Larger circles = more movies in that genre"),
                     tags$li(strong("Edge thickness:"), " Thicker lines = genres appear together more often"),
                     tags$li(strong("Hover:"), " Mouse over nodes/edges for detailed information"),
                     tags$li(strong("Click:"), " Click nodes to highlight their connections"),
                     tags$li(strong("Navigation:"), " Use mouse wheel to zoom, drag to pan")
                   )
                 )
          ),
          column(9,
                 box(
                   title = "Interactive Genre Network Graph", 
                   status = "primary", 
                   solidHeader = TRUE,
                   width = NULL,
                   height = "600px",
                   visNetworkOutput("genre_network", height = "500px")
                 )
          )
        ),
        fluidRow(
          column(6,
                 box(
                   title = "Legend - All Genres",
                   status = "info",
                   width = NULL,
                   DT::dataTableOutput("genre_legend")
                 )
          ),
          column(6,
                 box(
                   title = "Connection Strength Legend",
                   status = "info",
                   width = NULL,
                   div(
                     style = "padding: 10px;",
                     h5("Edge Gradient (Blue):"),
                     div(style = "margin: 15px 0;",
                         div(style = "width: 100%; height: 20px; background: linear-gradient(to right, rgba(26,107,230,0.6), rgba(13,54,115,1)); border: 1px solid #ccc; border-radius: 5px;")
                     ),
                     div(style = "display: flex; justify-content: space-between; font-size: 12px; margin-top: 5px;",
                         span("Weak connections"),
                         span("Strong connections")
                     )
                   )
                 )
          )
        )
      ),
      
      # Movie Comparison Tab
      tabItem(
        tabName = "movies", 
        fluidRow(
          box(
            width = 12,
            title = "Movie Rating Evolution Over Time",
            status = "primary",
            solidHeader = TRUE,
            
            fluidRow(
              column(6,
                     selectizeInput("selectedMovies", "Select Movies to Compare:",
                                    choices = NULL, multiple = TRUE,
                                    options = list(maxItems = 5, placeholder = "Search movies..."))),
              column(6,
                     sliderInput("yearRange", "Select Year Range:",
                                 min = 1995, max = 2018, value = c(2000, 2015), step = 1, sep = ""))
            ),
            
            fluidRow(
              column(6, checkboxInput("showTrend", "Show Trend Line", value = TRUE)),
              column(6, checkboxInput("showPoints", "Show Data Points", value = TRUE))
            )
          )
        ), 
        
        fluidRow(
          box(width = 12, withSpinner(plotlyOutput("movieSeriesPlot", height = "500px")))
        ), 
        
        fluidRow(
          box(width = 6, title = "Movie Statistics", status = "info",
              withSpinner(DT::dataTableOutput("movieStats"))),
          box(width = 6, title = "Rating Analysis", status = "info",
              withSpinner(verbatimTextOutput("ratingAnalysis")))
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Load data when app starts
  sample_data <- reactive({
    load_movielens_data()
  })
  
  # Genre selection state management
  values <- reactiveValues(
    selected_genres = c("Action", "Comedy", "Drama", "Thriller")
  )
  
  # Show genre selection modal
  observeEvent(input$btn_select_genres, {
    showModal(modalDialog(
      title = "Select Movie Genres",
      size = "l",
      div(
        class = "genre-grid",
        checkboxGroupInput("temp_genre_filter", "",
                           choices = c("Action", "Adventure", "Animation", "Children", 
                                       "Comedy", "Crime", "Documentary", "Drama",
                                       "Fantasy", "Horror", "Musical", "Mystery",
                                       "Romance", "Sci-Fi", "Thriller", "War", "Western"),
                           selected = values$selected_genres,
                           inline = FALSE)
      ),
      hr(),
      div(style = "text-align: center;",
          actionButton("btn_select_all_genres", "Select All", class = "btn-info btn-sm"),
          actionButton("btn_clear_all_genres", "Clear All", class = "btn-warning btn-sm")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_apply_genres", "Apply Selection", class = "btn-success")
      )
    ))
  })
  
  # Modal button handlers
  observeEvent(input$btn_select_all_genres, {
    updateCheckboxGroupInput(session, "temp_genre_filter",
                             selected = c("Action", "Adventure", "Animation", "Children", 
                                          "Comedy", "Crime", "Documentary", "Drama",
                                          "Fantasy", "Horror", "Musical", "Mystery",
                                          "Romance", "Sci-Fi", "Thriller", "War", "Western"))
  })
  
  observeEvent(input$btn_clear_all_genres, {
    updateCheckboxGroupInput(session, "temp_genre_filter", selected = character(0))
  })
  
  observeEvent(input$btn_apply_genres, {
    values$selected_genres <- input$temp_genre_filter
    removeModal()
  })
  
  # Display selected genres
  output$selected_genres_text <- renderText({
    if (length(values$selected_genres) == 0) {
      "All genres selected"
    } else if (length(values$selected_genres) <= 4) {
      paste(values$selected_genres, collapse = ", ")
    } else {
      paste(length(values$selected_genres), "genres selected")
    }
  })
  
  # Reset button
  observeEvent(input$btn_reset, {
    updateSliderInput(session, "rating_range", value = c(0.5, 5.0))
    updateSliderInput(session, "min_ratings", value = 10)
    updateSliderInput(session, "year_filter", value = c(1990, 2018))
    values$selected_genres <- c("Action", "Comedy", "Drama", "Thriller")
    updateRadioButtons(session, "color_by", selected = "none")
  })
  
  # Movie Explorer - Movies with stats
  movies_with_stats <- reactive({
    sample_data()$ratings %>%
      group_by(movieId) %>%
      summarise(
        avg_rating = mean(rating, na.rm = TRUE),
        num_ratings = n(),
        .groups = 'drop'
      ) %>%
      left_join(sample_data()$movies, by = "movieId") %>%
      filter(!is.na(avg_rating), !is.na(releaseYear)) %>%
      mutate(
        decade = paste0(floor(releaseYear/10)*10, "s"),
        # Popularity as ordered factor (High to Low)
        popularity = case_when(
          num_ratings >= 100 ~ "High",
          num_ratings >= 50 ~ "Medium", 
          TRUE ~ "Low"
        ),
        popularity = factor(popularity, levels = c("High", "Medium", "Low")),
        # Quality tier as ordered factor (Excellent to Poor)
        quality_tier = case_when(
          avg_rating >= 4.0 ~ "Excellent (4.0+)",
          avg_rating >= 3.5 ~ "Good (3.5-4.0)",
          avg_rating >= 3.0 ~ "Average (3.0-3.5)",
          avg_rating >= 2.5 ~ "Below Avg (2.5-3.0)",
          TRUE ~ "Poor (<2.5)"
        ),
        quality_tier = factor(quality_tier, levels = c(
          "Excellent (4.0+)", 
          "Good (3.5-4.0)", 
          "Average (3.0-3.5)", 
          "Below Avg (2.5-3.0)", 
          "Poor (<2.5)"
        )),
        # Dominant genre (first listed)
        dominant_genre = str_extract(genres, "^[^|]+")
      )
  })
  
  # Enhanced scatterplot with color coding
  output$movie_scatter <- renderPlotly({
    filtered_data <- movies_with_stats() %>%
      filter(
        avg_rating >= input$rating_range[1],
        avg_rating <= input$rating_range[2],
        num_ratings >= input$min_ratings,
        releaseYear >= input$year_filter[1],
        releaseYear <= input$year_filter[2]
      )
    
    if (length(values$selected_genres) > 0) {
      genre_pattern <- paste(values$selected_genres, collapse = "|")
      filtered_data <- filtered_data %>%
        filter(str_detect(genres, genre_pattern))
    }
    
    # Enhanced plot with conditional coloring
    p <- filtered_data %>%
      ggplot(aes(x = releaseYear, y = avg_rating, size = num_ratings,
                 text = paste("Title:", clean_title,
                              "\nYear:", releaseYear,
                              "\nRating:", round(avg_rating, 2),
                              "\nNum Ratings:", scales::comma(num_ratings),
                              "\nGenres:", genres)))
    
    # Conditional coloring with ordered legends
    if (input$color_by == "decade") {
      p <- p + geom_point(aes(color = decade), alpha = 0.7) +
        scale_color_viridis_d(name = "Decade")
    } else if (input$color_by == "popularity") {
      p <- p + geom_point(aes(color = popularity), alpha = 0.7) +
        scale_color_manual(
          values = c("High" = "#90EE90", "Medium" = "#98D8E8", "Low" = "#FFA07A"),
          name = "Popularity",
          drop = FALSE
        )
    } else if (input$color_by == "quality") {
      p <- p + geom_point(aes(color = quality_tier), alpha = 0.7) +
        scale_color_manual(
          values = c(
            "Excellent (4.0+)" = "#2E8B57",      # Dark green
            "Good (3.5-4.0)" = "#32CD32",        # Lime green  
            "Average (3.0-3.5)" = "#FFD700",     # Gold
            "Below Avg (2.5-3.0)" = "#FF8C00",   # Dark orange
            "Poor (<2.5)" = "#DC143C"            # Crimson
          ), 
          name = "Quality",
          drop = FALSE
        )
    } else if (input$color_by == "genre") {
      # Use a color palette that works well for many categories
      p <- p + geom_point(aes(color = dominant_genre), alpha = 0.7) +
        scale_color_viridis_d(name = "Main Genre", option = "turbo")
    } else {
      p <- p + geom_point(alpha = 0.6, color = "steelblue")
    }
    
    p <- p +
      scale_size_continuous(range = c(1, 10), name = "") +
      labs(title = "Interactive Movie Explorer - Release Year vs. Average Rating",
           x = "Release Year", y = "Average Rating") +
      theme_minimal() +
      ylim(0.5, 5)
    
    ggplotly(p, tooltip = "text")
  })
  
  # Filtered movies table
  output$filtered_movies_table <- DT::renderDataTable({
    filtered_data <- movies_with_stats() %>%
      filter(
        avg_rating >= input$rating_range[1],
        avg_rating <= input$rating_range[2],
        num_ratings >= input$min_ratings,
        releaseYear >= input$year_filter[1],
        releaseYear <= input$year_filter[2]
      )
    
    if (length(values$selected_genres) > 0) {
      genre_pattern <- paste(values$selected_genres, collapse = "|")
      filtered_data <- filtered_data %>%
        filter(str_detect(genres, genre_pattern))
    }
    
    display_data <- filtered_data %>%
      select(clean_title, releaseYear, avg_rating, num_ratings) %>%
      arrange(desc(avg_rating)) %>%
      head(15) %>%
      rename(
        Title = clean_title,
        Year = releaseYear,
        `Avg Rating` = avg_rating,
        `# Ratings` = num_ratings
      ) %>%
      mutate(`Avg Rating` = round(`Avg Rating`, 2))
    
    DT::datatable(display_data, 
                  options = list(pageLength = 10, dom = 't'),
                  rownames = FALSE)
  })
  
  # Advanced filter summary
  output$filter_summary <- renderText({
    filtered_data <- movies_with_stats() %>%
      filter(
        avg_rating >= input$rating_range[1],
        avg_rating <= input$rating_range[2],
        num_ratings >= input$min_ratings,
        releaseYear >= input$year_filter[1],
        releaseYear <= input$year_filter[2]
      )
    
    if (length(values$selected_genres) > 0) {
      genre_pattern <- paste(values$selected_genres, collapse = "|")
      filtered_data <- filtered_data %>%
        filter(str_detect(genres, genre_pattern))
    }
    
    total_movies <- nrow(movies_with_stats())
    filtered_movies <- nrow(filtered_data)
    avg_rating_filtered <- round(mean(filtered_data$avg_rating, na.rm = TRUE), 2)
    
    paste(
      "=== FILTER RESULTS ===",
      paste("Movies shown:", scales::comma(filtered_movies), "of", scales::comma(total_movies)),
      paste("Percentage:", round(filtered_movies/total_movies * 100, 1), "%"),
      paste("Average rating:", avg_rating_filtered),
      "",
      "=== ACTIVE FILTERS ===",
      paste("• Rating:", input$rating_range[1], "-", input$rating_range[2]),
      paste("• Min ratings:", input$min_ratings, "+"),
      paste("• Years:", input$year_filter[1], "-", input$year_filter[2]),
      paste("• Genres:", ifelse(length(values$selected_genres) > 0, 
                                paste(values$selected_genres, collapse = ", "), "All")),
      paste("• Color by:", input$color_by),
      sep = "\n"
    )
  })
  
  # Value boxes
  output$total_movies <- renderValueBox({
    valueBox(value = nrow(sample_data()$movies), subtitle = "Total Movies", icon = icon("film"), color = "blue")
  })
  
  output$total_ratings <- renderValueBox({
    valueBox(value = format(nrow(sample_data()$ratings), big.mark = ","), subtitle = "Total Ratings", icon = icon("star"), color = "yellow")
  })
  
  output$total_users <- renderValueBox({
    valueBox(value = length(unique(sample_data()$ratings$userId)), subtitle = "Total Users", icon = icon("users"), color = "green")
  })
  
  output$total_tags <- renderValueBox({
    valueBox(value = nrow(sample_data()$tags), subtitle = "Total Tags", icon = icon("tags"), color = "purple")
  })
  
  output$avg_rating <- renderValueBox({
    valueBox(value = round(mean(sample_data()$ratings$rating), 2), subtitle = "Average Rating", icon = icon("star-half-alt"), color = "orange")
  })
  
  output$date_range <- renderValueBox({
    timestamps <- sample_data()$ratings$timestamp
    dates <- as.Date(as.POSIXct(timestamps, origin = "1970-01-01"))
    min_date <- format(min(dates), "%d/%m/%y")
    max_date <- format(max(dates), "%d/%m/%y")
    date_range <- paste(min_date, "-", max_date)
    valueBox(value = date_range, subtitle = "Rating Period", icon = icon("calendar"), color = "red")
  })
  
  # Dataset structure table
  output$dataset_structure <- DT::renderDataTable({
    data_obj <- sample_data()
    structure_data <- data.frame(
      Dataset = c("Movies", "Ratings", "Tags", "Links"),
      Rows = c(
        nrow(data_obj$movies),
        nrow(data_obj$ratings),
        nrow(data_obj$tags),
        nrow(data_obj$links)
      ),
      Columns = c(
        ncol(data_obj$movies),
        ncol(data_obj$ratings),
        ncol(data_obj$tags),
        ncol(data_obj$links)
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
    data_obj <- sample_data()
    top_movies <- data_obj$ratings %>%
      group_by(movieId) %>%
      summarise(
        rating_count = n(),
        avg_rating = round(mean(rating), 2),
        .groups = 'drop'
      ) %>%
      arrange(desc(rating_count)) %>%
      head(10) %>%
      left_join(data_obj$movies, by = "movieId") %>%
      select(clean_title, rating_count, avg_rating) %>%
      rename(Title = clean_title, `Rating Count` = rating_count, `Avg Rating` = avg_rating)
    
    top_movies
  }, options = list(pageLength = 10, searching = FALSE, paging = FALSE))
  
  # Rating distribution plot
  output$rating_distribution <- renderPlotly({
    rating_counts <- sample_data()$ratings %>%
      group_by(rating) %>%
      summarise(count = n(), .groups = 'drop')
    
    p <- ggplot(rating_counts, aes(x = factor(rating), y = count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Distribution of Ratings", x = "Rating", y = "Number of Ratings") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Genre distribution plot
  output$genre_distribution <- renderPlotly({
    genres_expanded <- sample_data()$movies %>%
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
      labs(title = "Top 15 Movie Genres", x = "Genre", y = "Number of Movies") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Update movie choices for comparison
  observe({
    data_obj <- sample_data()
    
    movie_rating_counts <- data_obj$movie_time_series %>%
      group_by(movieId) %>%
      summarise(rating_count = n(), .groups = "drop") %>%
      filter(rating_count >= 3)
    
    movie_choices <- data_obj$movie_time_series %>%
      filter(movieId %in% movie_rating_counts$movieId) %>%
      select(movieId, title, releaseYear) %>%
      distinct() %>%
      arrange(title) %>%
      mutate(display_name = paste0(title, " (", releaseYear, ")"))
    
    updateSelectizeInput(session, "selectedMovies",
                         choices = setNames(movie_choices$movieId, movie_choices$display_name),
                         selected = head(movie_choices$movieId, 3))
  })
  
  # Update year slider range
  observe({
    data_obj <- sample_data()
    year_range <- range(data_obj$movie_time_series$year, na.rm = TRUE)
    
    updateSliderInput(session, "yearRange",
                      min = year_range[1],
                      max = year_range[2],
                      value = c(year_range[1] + 2, year_range[2] - 2))
  })
  
  # Movie Series Plot
  output$movieSeriesPlot <- renderPlotly({
    req(input$selectedMovies, input$yearRange)
    
    data_obj <- sample_data()
    
    movie_data <- data_obj$movie_time_series %>%
      filter(movieId %in% input$selectedMovies) %>%
      filter(year >= input$yearRange[1] & year <= input$yearRange[2])
    
    if(nrow(movie_data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 1, y = 1, label = "No data available for selected criteria", size = 6) +
        theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(movie_data, aes(x = year, y = avgRating, color = title, group = title)) +
      geom_line(linewidth = 1.2, alpha = 0.8) +
      scale_color_brewer(type = "qual", palette = "Set2") +
      labs(title = "Movie Rating Evolution Over Time",
           x = "Year", y = "Average Rating", color = "Movie") +
      scale_y_continuous(limits = c(1, 5)) +
      scale_x_continuous(breaks = seq(input$yearRange[1], input$yearRange[2], by = 1)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    if(input$showPoints) {
      p <- p + geom_point(aes(size = count), alpha = 0.7) +
        scale_size_continuous(range = c(2, 6), name = "# Ratings")
    }
    
    if(input$showTrend) {
      p <- p + geom_smooth(method = "lm", se = FALSE, alpha = 0.6, linewidth = 0.8, linetype = "dashed")
    }
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Movie Statistics Table
  output$movieStats <- DT::renderDataTable({
    req(input$selectedMovies, input$yearRange)
    
    data_obj <- sample_data()
    
    stats <- data_obj$movie_time_series %>%
      filter(movieId %in% input$selectedMovies) %>%
      filter(year >= input$yearRange[1] & year <= input$yearRange[2]) %>%
      group_by(movieId, title) %>%
      summarise(
        `Avg Rating` = round(mean(avgRating, na.rm = TRUE), 2),
        `Total Ratings` = sum(count, na.rm = TRUE),
        `Years of Data` = n(),
        `First Year` = min(year, na.rm = TRUE),
        `Last Year` = max(year, na.rm = TRUE),
        `Rating Std Dev` = round(sd(avgRating, na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    DT::datatable(stats, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  # Rating Analysis
  output$ratingAnalysis <- renderText({
    req(input$selectedMovies, input$yearRange)
    
    data_obj <- sample_data()
    
    data <- data_obj$movie_time_series %>%
      filter(movieId %in% input$selectedMovies) %>%
      filter(year >= input$yearRange[1] & year <= input$yearRange[2])
    
    if(nrow(data) == 0) return("No data available for selected criteria.")
    
    trends <- data %>%
      group_by(movieId, title) %>%
      summarise(
        avg_rating = mean(avgRating, na.rm = TRUE),
        rating_trend = tryCatch({
          if(n() >= 3) {
            lm(avgRating ~ year)$coefficients[2]
          } else {
            0
          }
        }, error = function(e) 0),
        year_span = max(year) - min(year) + 1,
        .groups = "drop"
      ) %>%
      mutate(
        trend_direction = case_when(
          rating_trend > 0.05 ~ "↗ IMPROVING",
          rating_trend < -0.05 ~ "↘ DECLINING", 
          TRUE ~ "→ STABLE"
        )
      )
    
    output_text <- paste0("Analysis for ", input$yearRange[1], "-", input$yearRange[2], ":\n\n")
    
    for(i in 1:nrow(trends)) {
      output_text <- paste0(output_text, 
                            "• ", trends$title[i], ":\n",
                            "  Average Rating: ", round(trends$avg_rating[i], 2), " stars\n",
                            "  Trend: ", trends$trend_direction[i], "\n",
                            "  Annual Change: ", round(trends$rating_trend[i], 3), " stars/year\n",
                            "  Data Span: ", trends$year_span[i], " years\n\n")
    }
    
    output_text
  })
  
  # Update user choices for User Analysis tab
  observe({
    user_choices <- sort(unique(sample_data()$ratings$userId))
    updateSelectizeInput(session, "selected_user",
                         choices = user_choices,
                         selected = user_choices[1],
                         server = TRUE)
  })
  
  # User Analysis - Reactive data
  user_data <- reactive({
    req(input$selected_user)
    sample_data()$ratings %>%
      filter(userId == input$selected_user)
  })
  
  # User value boxes
  output$user_total_ratings <- renderValueBox({
    valueBox(value = nrow(user_data()), subtitle = "Total Ratings", icon = icon("star"), color = "blue")
  })
  
  output$user_avg_rating <- renderValueBox({
    valueBox(value = round(mean(user_data()$rating), 2), subtitle = "Average Rating", icon = icon("chart-line"), color = "green")
  })
  
  output$user_active_years <- renderValueBox({
    active_years <- user_data() %>%
      summarise(years = n_distinct(year)) %>%
      pull(years)
    
    valueBox(value = active_years, subtitle = "Active Years", icon = icon("calendar"), color = "yellow")
  })
  
  # User statistics text
  output$user_stats_text <- renderText({
    user_stats <- user_data()
    if(nrow(user_stats) > 0) {
      paste(
        paste("User ID:", input$selected_user),
        paste("Total Ratings:", nrow(user_stats)),
        paste("Average Rating:", round(mean(user_stats$rating), 2)),
        paste("Rating Range:", min(user_stats$rating), "-", max(user_stats$rating)),
        paste("First Rating:", format(min(user_stats$date), "%d/%m/%Y")),
        paste("Last Rating:", format(max(user_stats$date), "%d/%m/%Y")),
        paste("Most Common Rating:", names(sort(table(user_stats$rating), decreasing = TRUE))[1]),
        sep = "\n"
      )
    } else {
      "No user selected"
    }
  })
  
  # User rating distribution
  output$user_rating_dist <- renderPlotly({
    rating_summary <- user_data() %>%
      count(rating) %>%
      complete(rating = 1:5, fill = list(n = 0))
    
    p <- rating_summary %>%
      ggplot(aes(x = factor(rating), y = n, fill = factor(rating))) +
      geom_col() +
      scale_fill_brewer(type = "seq", palette = "Blues") +
      labs(title = "Rating Distribution", x = "Rating", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_x_discrete(drop = FALSE)
    
    ggplotly(p)
  })
  
  # User genre distribution
  output$user_genre_dist <- renderPlotly({
    user_genres <- user_data() %>%
      left_join(sample_data()$movies, by = "movieId") %>%
      separate_rows(genres, sep = "\\|") %>%
      filter(genres != "(no genres listed)") %>%
      count(genres, sort = TRUE) %>%
      top_n(10, n)
    
    p <- ggplot(user_genres, aes(x = reorder(genres, n), y = n, fill = genres)) +
      geom_col() +
      coord_flip() +
      labs(title = "Top 10 Genres Rated", x = "Genre", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # User timeline
  output$user_timeline <- renderPlotly({
    timeline_data <- user_data() %>%
      mutate(month_year = floor_date(date, "month")) %>%
      count(month_year)
    
    p <- ggplot(timeline_data, aes(x = month_year, y = n)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 2) +
      labs(title = "Rating Activity Over Time", x = "Date", y = "Ratings per Month") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Reactive network data based on controls
  network_data_reactive <- reactive({
    create_genre_network(sample_data()$movies, input$min_connections)
  })
  
  # Genre Network
  output$genre_network <- renderVisNetwork({
    tryCatch({
      network_data <- network_data_reactive()
      
      if(nrow(network_data$nodes) == 0 || nrow(network_data$edges) == 0) {
        return(NULL)
      }
      
      vis <- visNetwork(network_data$nodes, network_data$edges) %>%
        visIgraphLayout(layout = input$layout_type, physics = FALSE) %>%
        visNodes(
          shape = "circle",
          shadow = list(enabled = TRUE, color = "rgba(0,0,0,0.3)", size = 8, x = 2, y = 2),
          borderWidth = 3,
          borderWidthSelected = 5,
          font = list(size = 16, color = "#FFFFFF", face = "Arial", strokeWidth = 2, strokeColor = "#000000")
        ) %>%
        visEdges(
          smooth = list(enabled = TRUE, type = "continuous", roundness = 0.2),
          physics = FALSE,
          selectionWidth = 3,
          hoverWidth = 2
        ) %>%
        visPhysics(enabled = FALSE) %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1, labelOnly = FALSE),
          clickToUse = FALSE
        ) %>%
        visInteraction(
          navigationButtons = TRUE,
          hover = TRUE,
          selectConnectedEdges = TRUE,
          tooltipDelay = 100,
          zoomView = TRUE,
          dragView = TRUE
        ) %>%
        visLayout(randomSeed = 123)
      
      vis
      
    }, error = function(e) {
      print(paste("Error in genre network:", e$message))
      return(NULL)
    })
  })
  
  # Network statistics
  output$network_stats <- renderText({
    network_data <- network_data_reactive()
    if(nrow(network_data$nodes) > 0 && nrow(network_data$edges) > 0) {
      paste(
        paste("Genres:", nrow(network_data$nodes)),
        paste("Connections:", nrow(network_data$edges)),
        paste("Avg connections per genre:", round(nrow(network_data$edges) * 2 / nrow(network_data$nodes), 1)),
        sep = "\n"
      )
    } else {
      "No data to display"
    }
  })
  
  # Genre legend table
  output$genre_legend <- DT::renderDataTable({
    network_data <- network_data_reactive()
    if(nrow(network_data$nodes) > 0) {
      legend_data <- network_data$nodes %>%
        select(Genre = label, `Number of Movies` = value, Color = color) %>%
        arrange(desc(`Number of Movies`)) %>%
        mutate(
          Color = paste0('<div style="width:20px;height:20px;background-color:', Color, 
                         ';border:1px solid #ccc;border-radius:50%;display:inline-block;"></div>')
        )
      
      DT::datatable(legend_data, 
                    escape = FALSE, 
                    options = list(
                      pageLength = 20,
                      dom = 't',
                      ordering = TRUE,
                      columnDefs = list(
                        list(className = 'dt-center', targets = c(1, 2))
                      )
                    ),
                    rownames = FALSE) %>%
        DT::formatCurrency(columns = "Number of Movies", currency = "", digits = 0)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)