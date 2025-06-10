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
  # Use a safer approach that handles all cases
  movies$releaseYear <- sapply(movies$title, function(title) {
    # Look for 4-digit year in parentheses at the end
    year_match <- regmatches(title, regexpr("\\((\\d{4})\\)\\s*$", title))
    if (length(year_match) > 0) {
      return(as.numeric(gsub("[()]", "", year_match)))
    } else {
      return(1995)  # Default year if no year found
    }
  }, USE.NAMES = FALSE)
  
  # Clean title by removing year - fix the duplicate year issue
  movies$clean_title <- gsub("\\s*\\(\\d{4}\\)\\s*$", "", movies$title)
  
  # Fix title formatting - some titles have articles moved to the end
  # Convert "Godfather, The" back to "The Godfather"
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
        # Smooth width calculation based on weight
        width = pmax(1, pmin(8, (weight - min(weight)) / (max(weight) - min(weight)) * 7 + 1)),
        title = paste0(
          "<div style='text-align: center; font-family: Arial; padding: 8px;'>",
          "<h4 style='margin: 5px; color: #2C3E50;'>", from, " + ", to, "</h4>",
          "<p style='margin: 3px; font-size: 16px;'><strong>", weight, "</strong> movies together</p>",
          "</div>"
        ),
        # Blue gradient from light to dark based on weight
        color_intensity = (weight - min(weight)) / (max(weight) - min(weight)),
        color = rgb(
          red = 0.1 + (1 - color_intensity) * 0.7,    # Light blue to dark
          green = 0.4 + (1 - color_intensity) * 0.4,  # Adjust green component
          blue = 0.9,                                  # Keep blue strong
          alpha = 0.6 + color_intensity * 0.4         # Transparency gradient
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
    menuItem(
      "Summary Statistics",
      tabName = "summary",
      icon = icon("chart-bar")
    ),
    menuItem(
      "Movie Comparison", 
      tabName = "movies",
      icon = icon("film")
    ),
    menuItem(
      "Genre Network", 
      tabName = "network",
      icon = icon("project-diagram")
    ),
    menuItem(
      "User Analysis", 
      tabName = "users",
      icon = icon("user")
    )
  )),
  
  dashboardBody(tabItems(
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
                 column(4,
                        valueBoxOutput("user_total_ratings", width = NULL)
                 ),
                 column(4,
                        valueBoxOutput("user_avg_rating", width = NULL)
                 ),
                 column(4,
                        valueBoxOutput("user_active_years", width = NULL)
                 )
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
               
               # MOVED: How to Use box right below controls
               box(
                 title = "How to Use This Visualization",
                 status = "info",
                 width = NULL,
                 tags$ul(
                   tags$li(strong("Node size:"), " Larger circles = more movies in that genre"),
                   tags$li(strong("Edge thickness:"), " Thicker lines = genres appear together more often"),
                   tags$li(strong("Edge colors:"), " See connection strength legend"),
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
                   ),
                   hr(),
                   p(style = "font-size: 13px; color: #666;", 
                     "• Darker blue = more movies together", br(),
                     "• Thicker lines = stronger connections", br(),
                     "• Both color and thickness increase with strength")
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
          
          # Row for controls
          fluidRow(
            column(
              6,
              selectizeInput(
                "selectedMovies",
                "Select Movies to Compare:",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  maxItems = 5, 
                  placeholder = "Search movies...",
                  highlight = FALSE,
                  render = I('{
                    option: function(item, escape) {
                      return "<div>" + escape(item.label) + "</div>";
                    }
                  }')
                )
              )
            ),
            column(
              6,
              sliderInput(
                "yearRange",
                "Select Year Range:",
                min = 1995,
                max = 2018,
                value = c(2000, 2015),
                step = 1,
                sep = ""
              )
            )
          ),
          
          # Row for additional options
          fluidRow(
            column(6, checkboxInput("showTrend", "Show Trend Line", value = TRUE)),
            column(6, checkboxInput("showPoints", "Show Data Points", value = TRUE))
          )
        )
      ), 
      
      fluidRow(
        box(
          width = 12, 
          withSpinner(plotlyOutput("movieSeriesPlot", height = "500px"))
        )
      ), 
      
      fluidRow(
        box(
          width = 6,
          title = "Movie Statistics",
          status = "info",
          withSpinner(DT::dataTableOutput("movieStats"))
        ),
        box(
          width = 6,
          title = "Rating Analysis",
          status = "info",
          withSpinner(verbatimTextOutput("ratingAnalysis"))
        )
      )
    )
  ))
)

# Define Server
server <- function(input, output, session) {
  # Load data when app starts
  sample_data <- reactive({
    load_movielens_data()
  })
  
  # Update year slider range based on actual data
  observe({
    data_obj <- sample_data()
    year_range <- range(data_obj$movie_time_series$year, na.rm = TRUE)
    
    updateSliderInput(session, "yearRange",
                      min = year_range[1],
                      max = year_range[2],
                      value = c(year_range[1] + 2, year_range[2] - 2))
  })
  
  # Value boxes
  output$total_movies <- renderValueBox({
    valueBox(
      value = nrow(sample_data()$movies),
      subtitle = "Total Movies",
      icon = icon("film"),
      color = "blue"
    )
  })
  
  output$total_ratings <- renderValueBox({
    valueBox(
      value = format(nrow(sample_data()$ratings), big.mark = ","),
      subtitle = "Total Ratings",
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  output$total_users <- renderValueBox({
    valueBox(
      value = length(unique(sample_data()$ratings$userId)),
      subtitle = "Total Users",
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$total_tags <- renderValueBox({
    valueBox(
      value = nrow(sample_data()$tags),
      subtitle = "Total Tags",
      icon = icon("tags"),
      color = "purple"
    )
  })
  
  output$avg_rating <- renderValueBox({
    valueBox(
      value = round(mean(sample_data()$ratings$rating), 2),
      subtitle = "Average Rating",
      icon = icon("star-half-alt"),
      color = "orange"
    )
  })
  
  output$date_range <- renderValueBox({
    timestamps <- sample_data()$ratings$timestamp
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
  }, options = list(
    pageLength = 10,
    searching = FALSE,
    paging = FALSE
  ))
  
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
      rename(Title = clean_title,
             `Rating Count` = rating_count,
             `Avg Rating` = avg_rating)
    
    top_movies
  }, options = list(
    pageLength = 10,
    searching = FALSE,
    paging = FALSE
  ))
  
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
    # Extract and count genres
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
  
  # Update movie choices for comparison - fix duplicate year issue
  observe({
    data_obj <- sample_data()
    
    # Get movies with sufficient ratings for meaningful analysis
    movie_rating_counts <- data_obj$movie_time_series %>%
      group_by(movieId) %>%
      summarise(rating_count = n(), .groups = "drop") %>%
      filter(rating_count >= 3)  # At least 3 years of data
    
    # Create clean movie choices without duplicate years
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
  
  # Update user choices for User Analysis tab
  observe({
    user_choices <- sort(unique(sample_data()$ratings$userId))
    updateSelectizeInput(session, "selected_user",
                         choices = user_choices,
                         selected = user_choices[1],
                         server = TRUE)
  })
  
  # Movie Series Plot - FIXED VERSION WITHOUT GEOM_SMOOTH
  output$movieSeriesPlot <- renderPlotly({
    req(input$selectedMovies, input$yearRange)
    
    data_obj <- sample_data()
    
    # Filter data by selected movies and year range
    movie_data <- data_obj$movie_time_series %>%
      filter(movieId %in% input$selectedMovies) %>%
      filter(year >= input$yearRange[1] & year <= input$yearRange[2])
    
    if(nrow(movie_data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 1, y = 1, label = "No data available for selected criteria", size = 6) +
        theme_void()
      return(ggplotly(p))
    }
    
    # Create the base plot - ALWAYS show the connecting lines
    p <- ggplot(movie_data, aes(x = year, y = avgRating, color = title, group = title)) +
      geom_line(linewidth = 1.2, alpha = 0.8) +  # Always show connecting lines
      scale_color_brewer(type = "qual", palette = "Set2") +
      labs(title = "Movie Rating Evolution Over Time",
           x = "Year", 
           y = "Average Rating", 
           color = "Movie") +
      scale_y_continuous(limits = c(1, 5)) +
      scale_x_continuous(breaks = seq(input$yearRange[1], input$yearRange[2], by = 1)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Add points if requested
    if(input$showPoints) {
      p <- p + geom_point(aes(size = count), alpha = 0.7) +
        scale_size_continuous(range = c(2, 6), name = "# Ratings")
    }
    
    # Add TREND LINES (smoothed) if requested - this is the actual trend line
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
    
    # Calculate trends for each movie
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
        ),
        trend_strength = abs(rating_trend)
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
  
  # User Analysis - Reactive data
  user_data <- reactive({
    req(input$selected_user)
    sample_data()$ratings %>%
      filter(userId == input$selected_user)
  })
  
  # User value boxes
  output$user_total_ratings <- renderValueBox({
    valueBox(
      value = nrow(user_data()),
      subtitle = "Total Ratings",
      icon = icon("star"),
      color = "blue"
    )
  })
  
  output$user_avg_rating <- renderValueBox({
    valueBox(
      value = round(mean(user_data()$rating), 2),
      subtitle = "Average Rating",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$user_active_years <- renderValueBox({
    active_years <- user_data() %>%
      summarise(years = n_distinct(year)) %>%
      pull(years)
    
    valueBox(
      value = active_years,
      subtitle = "Active Years",
      icon = icon("calendar"),
      color = "yellow"
    )
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
    p <- user_data() %>%
      ggplot(aes(x = factor(rating), fill = factor(rating))) +
      geom_bar() +
      scale_fill_brewer(type = "seq", palette = "Blues") +
      labs(title = "Rating Distribution", x = "Rating", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
    
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
      
      # Ensure we have valid network data
      if(nrow(network_data$nodes) == 0 || nrow(network_data$edges) == 0) {
        return(NULL)
      }
      
      # Create the network with improved styling
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