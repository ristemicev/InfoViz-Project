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
            column(6, checkboxInput("showTrend", "Show Trend Line", value = FALSE)),
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
}

# Run the application
shinyApp(ui = ui, server = server)