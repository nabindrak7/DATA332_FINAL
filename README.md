# Advanced Soccer Analytics Dashboard
# A Comprehensive Analysis of European Football Performance Metrics

![ronaldo-bicycle-kick-overview](https://github.com/user-attachments/assets/1a52d810-7f91-467b-9a71-a212242e56ab)


## Authors - Naby Karki and Aryaman Pradhan



## Project Overview
This project looks at European football, utilizing a unique analysis of historical data (from 2008 to 2016) from over 400 clubs and 11 leagues. The project uses datasets that consist of team-level performance, official tactical attributes, and player statistics for more than 10,000 players. The goal is to look for patterns or insights that relate to match outcomes or how teams play. In addition to demonstrating some analysis, the project presents a potential computational feature and hopefully provides deepening visualizations to engage users, particularly those in the sport that may not be formally trained in analytics. The dashboard, built in R Shiny, balanced thorough analysis and user-centered design, while being mindful of technical limitations from data size to visualization complexity.  
[Shiny Application]( https://nabikarki21.shinyapps.io/Data_332_Final/). 

## 📁 What Does This Database Include?

### Datasets Used

The dashboard integrates **seven core datasets**, each cleaned, processed, and merged for comprehensive analysis:

---

### 🏟️ Match Data (`Match.csv`)
- Contains ~25,000 historical matches.
- **Key fields:** `home_team_goal`, `away_team_goal`, `date`, `league_id`, `team_api_id`, etc.
- **Derived fields:** 
  - `total_goals` – sum of goals by both teams
  - `match_result` – "Home Win", "Away Win", or "Draw"
  - `goal_difference` – goal margin

---

### 🏙️ Team Data (`Team.csv`)
- Details on 400+ football clubs.
- **Fields include:** `team_long_name`, `team_short_name`, `team_api_id`

---

### 🧠 Team Attributes (`Team_Attributes.csv`)
- Contains tactical and strategic metrics for teams.
- **Key features include:**
  - `build_up_play_speed`
  - `defence_pressure`
  - `chance_creation_passing`
- Enables clustering of teams by playstyle using k-means.

---

### 👤 Player Data (`Player.csv`)
- Profile information for ~11,000 professional footballers.
- **Fields include:** `player_name`, `birthday`, `height`, `weight`

---

### 🎯 Player Attributes (`Player_Attributes.csv`)
- Skill-based performance ratings recorded over time.
- **Key attributes:**
  - `overall_rating`, `finishing`, `dribbling`, `short_passing`, `strength`, etc.
- Supports trend analysis and predictive modeling.

---

### 🌍 League Data (`League.csv`)
- Metadata on 11 European football leagues.
- **Fields include:** `league_id`, `league_name`, `country_id`
- Used to categorize and compare matches across leagues.

---

### 🗺️ Country Data (`Country.csv`)
- **Fields include:** `id`, `name`
- Used to enrich league and team data with country names.

---

## Data Source

![image](https://github.com/user-attachments/assets/8e187e68-817d-424b-b597-1f6e560184d4)


- **Loading The Data**: Code snippet to display how the datas were imported using a link.
```
# --- Load Data ---
base_url <- "https://raw.githubusercontent.com/nabindrak7/DATA332_FINAL/244cbaf8fb75158aa1c106100dd69733a5233ca7/332Final"

country <- read_csv(paste0(base_url, "/Country.csv")) %>% clean_names()
league <- read_csv(paste0(base_url, "/League.csv")) %>% clean_names()
player <- read_csv(paste0(base_url, "/Player.csv")) %>% clean_names()
team <- read_csv(paste0(base_url, "/Team.csv")) %>% clean_names()
team_attr <- read_csv(paste0(base_url, "/Team_Attributes.csv")) %>% clean_names()

temp_dir <- tempdir()
download.file(paste0(base_url, "/Match.zip"), file.path(temp_dir, "match.zip"), mode = "wb")
unzip(file.path(temp_dir, "match.zip"), exdir = temp_dir)
match <- read_csv(file.path(temp_dir, "Match.csv")) %>% clean_names()

download.file(paste0(base_url, "/Player_Attributes.zip"), file.path(temp_dir, "player_attr.zip"), mode = "wb")
unzip(file.path(temp_dir, "player_attr.zip"), exdir = temp_dir)
player_attr <- read_csv(file.path(temp_dir, "Player_Attributes.csv")) %>% clean_names()

```
- **Data Preprocessing**: 
- ```
  match <- match %>% 
  mutate(date = ymd(date)) %>% 
  drop_na(home_team_api_id, away_team_api_id) %>%
  mutate(total_goals = home_team_goal + away_team_goal,
         goal_difference = home_team_goal - away_team_goal,
         match_result = case_when(
           goal_difference > 0 ~ "Home Win",
           goal_difference < 0 ~ "Away Win",
           TRUE ~ "Draw"
         ))
  player_attr <- player_attr %>% 
  mutate(date = ymd(date)) %>% 
  drop_na(overall_rating) %>%
  mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm = TRUE))))

  team_attr <- team_attr %>% 
  mutate(date = ymd(date)) %>% 
  drop_na(build_up_play_speed) %>%
  mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm = TRUE))))
  ```

## What The interactive Shiny application shows :
-Dashboard Overview: Key metrics and quick insights
-Team Analysis: Goal trends and performance distributions
-Player Analysis: Skill comparisons and rating trends
-Tactical Analysis: Team style clustering
-Match Prediction: Outcome probability models
-Player Rating Prediction: Future performance projections

## Project Files
- `Match.csv`: Raw dataset containing ~25,000 historical matches
- `Team.csv`: Details on 400+ football clubs
- `Team_Attributes.csv`: Contains tactical and strategic metrics for teams
- `Player.csv`:Profile information for ~11,000 professional footballers
- `Player_Attributes.csv` : Skill-based performance ratings recorded over time
- `League.csv` : Used to categorize and compare matches across leagues
- `Country.csv` : Used to enrich league and team data with country names
- - `Data332.R` : The R studio file

## Requirements

Packages required to run and build this R code and shinyAPP
```
library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(janitor)
library(lubridate)
library(factoextra)
library(caret)
library(rpart)
library(rpart.plot)
library(nnet)
library(plotly)
library(shinythemes)
library(DT)
library(ggthemes)
library(viridis)
```
## # Merging different datasets
```  
  league_info <- league %>% select(league_id = id, league_name = name, league_country_id = country_id)
country_info <- country %>% rename(league_country_id = id, country_name = name)

match <- match %>% 
  left_join(league_info, by = "league_id") %>%
  left_join(country_info, by = "league_country_id") %>%
  left_join(team, by = c("home_team_api_id" = "team_api_id")) %>%
  rename(home_team_name = team_long_name) %>%
  left_join(team, by = c("away_team_api_id" = "team_api_id")) %>%
  rename(away_team_name = team_long_name)

```

## Creating Player Metrics

```
player_performance <- player_attr %>%
  group_by(player_api_id) %>%
  summarise(
    avg_rating = mean(overall_rating, na.rm = TRUE),
    avg_dribbling = mean(dribbling, na.rm = TRUE),
    avg_shooting = mean(finishing, na.rm = TRUE),
    avg_passing = mean(short_passing, na.rm = TRUE),
    avg_defending = mean(marking, na.rm = TRUE),
    avg_physical = mean(strength, na.rm = TRUE),
    last_updated = max(date)
  ) %>%
  left_join(player, by = "player_api_id")

  ```

##  User Interface structure for the Shiny app using fluidPage(), along with the dashboard theme
```
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      /* General Styling */
      body {
        font-family: 'Arial', sans-serif;
        line-height: 1.6;
      }
      .navbar {
        font-size: 16px;
        font-weight: 500;
      }
      .tab-content {
        padding: 20px;
        background-color: white;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
      .info-card {
        background-color: #f8f9fa;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 25px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .section-header {
        color: #2c3e50;
        border-bottom: 2px solid #3498db;
        padding-bottom: 10px;
        margin-top: 20px;
      }
      .highlight-box {
        background-color: #e8f4fc;
        border-left: 4px solid #3498db;
        padding: 15px;
        border-radius: 4px;
        margin: 15px 0;
      }
      .feature-list {
        margin-left: 20px;
      }
      .feature-item {
        margin-bottom: 8px;
      }
    "))
  ),
  
```

## Flight Delay Predictor
```
# Seasonal‐average table (2023 & 2024) by Month/Day/Hour
seasonal_tbl <- raw %>%
  rename(
    FlightDate      = `Date (MM/DD/YYYY)`,
    DepDelayMinutes = `Departure delay (Minutes)`
  ) %>%
  mutate(
    FlightDate = mdy(FlightDate),
    Year       = year(FlightDate),
    Month      = month(FlightDate),
    Day        = day(FlightDate),
    Hour       = hour(as_hms(`Scheduled departure time`))
  ) %>%
  filter(Year %in% c(2023, 2024)) %>%
  group_by(Month, Day, Hour) %>%
  summarize(AvgDelay = mean(DepDelayMinutes, na.rm = TRUE), .groups = "drop")

predict_delay <- function(date2025, hour2025) {
  d  <- ymd(date2025)
  m  <- month(d); dy <- day(d); hr <- hour2025
  out <- seasonal_tbl %>% filter(Month == m, Day == dy, Hour == hr) %>% pull(AvgDelay)
  if (length(out) == 0) out <- mean(seasonal_tbl$AvgDelay, na.rm = TRUE)
  round(out, 1)
}
```
## Building Shiny App

### UI
```
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      /* General Styling */
      body {
        font-family: 'Arial', sans-serif;
        line-height: 1.6;
      }
      .navbar {
        font-size: 16px;
        font-weight: 500;
      }
      .tab-content {
        padding: 20px;
        background-color: white;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
      .info-card {
        background-color: #f8f9fa;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 25px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .section-header {
        color: #2c3e50;
        border-bottom: 2px solid #3498db;
        padding-bottom: 10px;
        margin-top: 20px;
      }
      .highlight-box {
        background-color: #e8f4fc;
        border-left: 4px solid #3498db;
        padding: 15px;
        border-radius: 4px;
        margin: 15px 0;
      }
      .feature-list {
        margin-left: 20px;
      }
      .feature-item {
        margin-bottom: 8px;
      }
    "))
  ),
  
  titlePanel(div(
    h1("Advanced Soccer Analytics Dashboard", style = "color: #2c3e50;"),
    h4("A Comprehensive Analysis of European Football Performance Metrics", style = "color: #7f8c8d;")
  )),
  
  navbarPage(
    "",
    id = "nav",
    
    # Project Documentation Tab
    tabPanel(
      "Project Documentation",
      icon = icon("file-alt"),
      div(class = "info-card",
          h2(class = "section-header", "Soccer Project- DATA332"),
          p("This section provides comprehensive documentation of the football analytics research project."),
          
          tabsetPanel(
            tabPanel(
              "Project Requirements",
              div(class = "highlight-box",
                  h3("Project Requirements"),
                  p("The project was designed to meet the following analytical requirements:"),
                  
                  h4("1. Data Collection & Processing"),
                  tags$ul(
                    tags$li("Integrate multiple football datasets into a unified structure"),
                    tags$li("Clean and normalize data from different sources"),
                    tags$li("Handle missing values and inconsistent records"),
                    tags$li("Create derived metrics for analysis")
                  ),
                  
                  h4("2. Performance Analysis"),
                  tags$ul(
                    tags$li("Develop team performance metrics (goals, wins, home/away splits)"),
                    tags$li("Create player skill evaluation frameworks"),
                    tags$li("Analyze trends over multiple seasons"),
                    tags$li("Compare leagues and countries")
                  ),
                  
                  h4("3. Tactical Insights"),
                  tags$ul(
                    tags$li("Cluster teams by playing style characteristics"),
                    tags$li("Identify tactical patterns in different leagues"),
                    tags$li("Correlate team attributes with match outcomes")
                  ),
                  
                  h4("4. Predictive Modeling"),
                  tags$ul(
                    tags$li("Develop match outcome prediction models"),
                    tags$li("Incorporate multiple predictive factors"),
                    tags$li("Provide probabilistic outputs"),
                    tags$li("Validate model accuracy")
                  ),
                  
                  h4("5. Visualization & Reporting"),
                  tags$ul(
                    tags$li("Create interactive visualizations of key metrics"),
                    tags$li("Design intuitive dashboard navigation"),
                    tags$li("Support exploratory data analysis"),
                    tags$li("Generate exportable reports")
                  )
              )
            ),
            
            tabPanel(
              "Project Scope",
              div(class = "highlight-box",
                  h3("Scope Definition"),
                  p("The project scope encompasses the following boundaries and deliverables:"),
                  
                  h4("Included in Scope"),
                  tags$ul(
                    tags$li("Analysis of 11 European leagues from 2008-2016"),
                    tags$li("Team performance metrics for 400+ clubs"),
                    tags$li("Player attributes for 10,000+ footballers"),
                    tags$li("Tactical analysis using official team attributes"),
                    tags$li("Interactive visualizations for key metrics"),
                    tags$li("Match prediction model with probabilistic outputs")
                  ),
                  
                  h4("Excluded from Scope"),
                  tags$ul(
                    tags$li("Real-time data processing (historical analysis only)"),
                    tags$li("Individual player tracking data (positional analysis)"),
                    tags$li("Financial/transfer market analysis"),
                    tags$li("Youth/academy player development"),
                    tags$li("In-depth injury analysis")
                  ),
                  
                  h4("Data Limitations"),
                  tags$ul(
                    tags$li("Attribute data available at discrete time points (not continuous)"),
                    tags$li("Limited tactical data for smaller leagues"),
                    tags$li("Variability in data completeness across seasons"),
                    tags$li("Subjective components in player ratings")
                  ),
                  
                  h4("Technical Constraints"),
                  tags$ul(
                    tags$li("Shiny application performance with large datasets"),
                    tags$li("Visual clarity with multiple overlapping metrics"),
                    tags$li("Model accuracy given available features"),
                    tags$li("Cross-browser compatibility for visualizations")
                  )
              )
            ),
            
            tabPanel(
              "Backlog & Future Ideas",
              div(class = "highlight-box",
                  h3("Planned Enhancements & Concepts"),
                  p("The following ideas are planned for future implementation:"),
                  
                  h4("Analytical Enhancements"),
                  tags$ul(
                    tags$li("Player similarity analysis and comparison engine"),
                    tags$li("Team formation analysis and effectiveness metrics"),
                    tags$li("Expected Goals (xG) model integration"),
                    tags$li("Transfer market value estimation model"),
                    tags$li("Player development trajectory projections")
                  ),
                  
                  h4("Data Expansion"),
                  tags$ul(
                    tags$li("Incorporate match event data (passes, shots, tackles)"),
                    tags$li("Add more recent seasons (2017-present)"),
                    tags$li("Include additional leagues (South America, Asia)"),
                    tags$li("Integrate weather data for match conditions analysis")
                  ),
                  
                  h4("Technical Improvements"),
                  tags$ul(
                    tags$li("Automated data refresh pipeline"),
                    tags$li("User accounts and saved analyses"),
                    tags$li("Advanced filtering and segmentation"),
                    tags$li("Exportable report generation"),
                    tags$li("Mobile-optimized interface")
                  ),
                  
                  h4("Visualization Upgrades"),
                  tags$ul(
                    tags$li("Interactive pitch visualizations"),
                    tags$li("Animated timeline of team/player performance"),
                    tags$li("Custom radar charts for player profiles"),
                    tags$li("Network graphs for team passing patterns")
                  )
              )
            )
          )
      )
    ),
    
    # 1. INTRODUCTION TAB
    tabPanel(
      "Introduction",
      icon = icon("info-circle"),
      div(class = "info-card",
          h2(class = "section-header", "Football Analytics Research Project"),
          
          h3("Project Overview"),
          p("This interactive dashboard provides comprehensive analysis of European football performance from 2008-2016. The tool enables exploration of:"),
          tags$ul(class = "feature-list",
                  tags$li(class = "feature-item", "Team performance across multiple leagues"),
                  tags$li(class = "feature-item", "Player skill development and comparisons"),
                  tags$li(class = "feature-item", "Tactical style clustering"),
                  tags$li(class = "feature-item", "Match outcome predictions")
          ),
          
          div(class = "highlight-box",
              h4("Data Sources"),
              p("The analysis integrates:"),
              tags$ul(
                tags$li("11,000+ matches from top European leagues"),
                tags$li("Team attributes including playing styles"),
                tags$li("Detailed player skill ratings"),
                tags$li("League and country metadata")
              )
          ),
          
          h3("How to Use This Dashboard"),
          p("Navigate through the tabs to explore different aspects:"),
          tags$ul(class = "feature-list",
                  tags$li(class = "feature-item", tags$b("Dashboard Overview:"), " Key metrics and quick insights"),
                  tags$li(class = "feature-item", tags$b("Team Analysis:"), " Goal trends and performance distributions"),
                  tags$li(class = "feature-item", tags$b("Player Analysis:"), " Skill comparisons and rating trends"),
                  tags$li(class = "feature-item", tags$b("Tactical Analysis:"), " Team style clustering"),
                  tags$li(class = "feature-item", tags$b("Match Prediction:"), " Outcome probability models"),
                  tags$li(class = "feature-item", tags$b("Player Rating Prediction:"), " Future performance projections"),
                  tags$li(class = "feature-item", tags$b("Data Explorer:"), " Raw data investigation")
          ),
          
          h3("Methodology Highlights"),
          p("The analytical approaches include:"),
          tags$div(style = "column-count: 2;",
                   tags$ul(style = "margin-top: 0;",
                           tags$li("Performance metric calculations"),
                           tags$li("Interactive data visualization"),
                           tags$li("k-means clustering"),
                           tags$li("Win probability modeling"),
                           tags$li("Trend analysis"),
                           tags$li("Comparative statistics"),
                           tags$li("Linear regression"),
                           tags$li("Feature engineering")
                   )
          )
      )
    ),
    
    # 2. DASHBOARD OVERVIEW TAB
    tabPanel(
      "Dashboard Overview",
      icon = icon("tachometer-alt"),
      div(class = "info-card",
          h2(class = "section-header", "Key Performance Indicators"),
          p("This dashboard provides quick insights into European football performance metrics from 2008-2016. The visualizations below highlight:"),
          tags$ul(
            tags$li("Comparative statistics across 11 European leagues"),
            tags$li("Top performing players by various skill metrics"),
            tags$li("Recent high-scoring matches with 5+ goals")
          ),
          p("Hover over charts for detailed values and use the interactive controls to explore the data.")
      ),
      
      fluidRow(
        column(6,
               div(class = "highlight-box",
                   h3("League Performance Comparison"),
                   p("Analyze key league metrics including:"),
                   tags$ul(
                     tags$li("Average goals per match (blue bars)"),
                     tags$li("Home win percentage (green line)"),
                     tags$li("High-scoring match frequency")
                   ),
                   plotlyOutput("overviewLeaguePlot", height = "400px"),
                   p("Tip: Click on legend items to show/hide data series")
               )
        ),
        column(6,
               div(class = "highlight-box",
                   h3("Top Rated Players"),
                   p("Explore players with highest average ratings across:"),
                   tags$ul(
                     tags$li("Overall performance"),
                     tags$li("Technical skills"),
                     tags$li("Physical attributes")
                   ),
                   plotlyOutput("overviewPlayerPlot", height = "400px"),
                   p("Note: Based on player attributes data from all available seasons")
               )
        )
      ),
      
      fluidRow(
        column(12,
               div(class = "highlight-box",
                   h3("Recent High-Scoring Matches"),
                   p("Browse matches with the highest total goals (5+ goals) from recent seasons. Features:"),
                   tags$ul(
                     tags$li("Sortable columns - click headers to sort"),
                     tags$li("Color-coded match results"),
                     tags$li("Detailed match information")
                   ),
                   dataTableOutput("highScoringMatchesTable"),
                   p("Data spans all available leagues from 2008-2016")
               )
        )
      )
    ),
    
    # 3. TEAM ANALYSIS TAB
    tabPanel(
      "Team Analysis",
      icon = icon("users"),
      div(class = "info-card",
          h2(class = "section-header", "Team Performance Analysis"),
          p("This section examines team performance through two complementary approaches:"),
          tags$ul(
            tags$li(tags$b("Goal Trends:"), " Compare teams' offensive and defensive performance"),
            tags$li(tags$b("Goal Distribution:"), " Analyze scoring patterns across leagues")
          ),
          p("Use the filters to focus on specific leagues and seasons.")
      ),
      
      tabsetPanel(
        tabPanel(
          "Goal Trends",
          div(class = "highlight-box",
              h3("Team Goal Performance"),
              p("This visualization shows:"),
              tags$ul(
                tags$li("Goals scored (blue bars) vs conceded (red bars)"),
                tags$li("League average (dashed line) for reference"),
                tags$li("Performance trend line (green)")
              ),
              p("Adjust the filters to analyze different leagues and seasons.")
          ),
          sidebarLayout(
            sidebarPanel(
              h4("Analysis Controls"),
              selectInput("selected_year_goals", "Select Year:", 
                          choices = sort(unique(year(match$date)))),
              selectInput("selected_league_goals", "Select League:", 
                          choices = unique(match$league_name)),
              sliderInput("top_teams_slider", "Number of Teams to Display:",
                          min = 3, max = 20, value = 10),
              checkboxInput("show_avg_line", "Show League Average", TRUE),
              checkboxInput("show_trend_line", "Show Trend Line", TRUE)
            ),
            mainPanel(
              plotlyOutput("goalsPlot", height = "500px"),
              div(class = "info-card",
                  h4("Interpretation Guide"),
                  p("Teams with:"),
                  tags$ul(
                    tags$li("Tall blue bars = Strong offensive performance"),
                    tags$li("Short red bars = Solid defensive record"),
                    tags$li("Both = Dominant teams likely at top of table")
                  )
              )
            )
          )
        ),
        
        tabPanel(
          "Goal Distribution",
          div(class = "highlight-box",
              h3("League Scoring Patterns"),
              p("Compare goal distributions across leagues using:"),
              tags$ul(
                tags$li("Box plots: Show median, quartiles, and outliers"),
                tags$li("Violin plots: Display density distribution")
              )
          ),
          sidebarLayout(
            sidebarPanel(
              h4("Visualization Settings"),
              selectInput("selected_year_dist", "Select Year:", 
                          choices = sort(unique(year(match$date)))),
              radioButtons("dist_type", "Distribution Type:",
                           choices = c("Box Plot" = "box", "Violin Plot" = "violin")),
              checkboxInput("show_points", "Show Individual Matches", FALSE),
              selectInput("color_palette", "Color Scheme:",
                          choices = c("Viridis", "Plasma", "Magma", "Inferno"))
            ),
            mainPanel(
              plotlyOutput("leagueBoxplot", height = "500px"),
              div(class = "info-card",
                  h4("Key Insights"),
                  uiOutput("distributionInsights")
              )
            )
          )
        )
      )
    ),
    
    # 4. PLAYER ANALYSIS TAB
    tabPanel(
      "Player Analysis",
      icon = icon("user"),
      div(class = "info-card",
          h2(class = "section-header", "Player Performance Evaluation"),
          p("This section provides tools to analyze player skills and development:"),
          tags$ul(
            tags$li(tags$b("Skill Comparison:"), " Rank players by specific attributes"),
            tags$li(tags$b("Rating Trends:"), " Track performance changes over time")
          ),
          p("Filter by position and time period to focus your analysis.")
      ),
      
      tabsetPanel(
        tabPanel(
          "Skill Comparison",
          div(class = "highlight-box",
              h3("Player Skill Rankings"),
              p("Compare players across different skill metrics. The visualization shows:"),
              tags$ul(
                tags$li("Top performers in selected skill category"),
                tags$li("Option to group by player position"),
                tags$li("Interactive tooltips for detailed information")
              )
          ),
          sidebarLayout(
            sidebarPanel(
              h4("Comparison Parameters"),
              selectInput("selected_year_player", "Select Year:", 
                          choices = sort(unique(year(player_attr$date)))),
              selectInput("skill_metric", "Select Skill Metric:",
                          choices = c("Dribbling" = "dribbling",
                                      "Shooting" = "finishing",
                                      "Passing" = "short_passing",
                                      "Defending" = "marking",
                                      "Physical" = "strength")),
              sliderInput("top_players_slider", "Number of Players:",
                          min = 5, max = 25, value = 15),
              checkboxInput("group_by_position", "Group by Position", TRUE),
              p("Tip: Hover over bars for player details")
            ),
            mainPanel(
              plotlyOutput("skillComparisonPlot", height = "500px"),
              div(class = "info-card",
                  h4("Interpretation Guide"),
                  uiOutput("skillComparisonInsights")
              )
            )
          )
        ),
        
        tabPanel(
          "Rating Trends",
          div(class = "highlight-box",
              h3("Performance Over Time"),
              p("Track how player ratings evolve across different metrics:"),
              tags$ul(
                tags$li("Analyze by position category"),
                tags$li("Compare different skill attributes"),
                tags$li("Examine specific time periods")
              )
          ),
          sidebarLayout(
            sidebarPanel(
              h4("Trend Analysis Settings"),
              selectInput("selected_position", "Position:",
                          choices = c("Goalkeeper", "Defender", "Midfielder", "Forward", "All")),
              selectInput("trend_metric", "Metric to Analyze:",
                          choices = c("Overall Rating" = "overall_rating",
                                      "Potential" = "potential",
                                      "Crossing" = "crossing",
                                      "Long Shots" = "long_shots")),
              sliderInput("trend_years", "Year Range:",
                          min = min(year(player_attr$date)),
                          max = max(year(player_attr$date)),
                          value = c(2010, 2015),
                          step = 1)
            ),
            mainPanel(
              plotlyOutput("ratingTrend", height = "450px"),
              div(class = "info-card",
                  h4("Trend Interpretation"),
                  uiOutput("trendAnalysis")
              )
            )
          )
        )
      )
    ),
    
    # 5. TACTICAL ANALYSIS TAB
    tabPanel(
      "Tactical Analysis",
      icon = icon("project-diagram"),
      div(class = "info-card",
          h2(class = "section-header", "Team Style Clustering"),
          p("This analysis identifies tactical patterns using k-means clustering of team attributes:"),
          tags$ul(
            tags$li("Group teams by similar playing styles"),
            tags$li("Compare clusters in 2D visualization"),
            tags$li("Analyze cluster characteristics")
          ),
          p("Select variables that define tactical approaches (e.g., build-up speed, defensive pressure).")
      ),
      
      sidebarLayout(
        sidebarPanel(
          h4("Clustering Parameters"),
          selectInput("selected_league_cluster", "Select League:", 
                      choices = unique(match$league_name)),
          uiOutput("team_selector"),
          selectInput("cluster_vars", "Tactical Variables:",
                      choices = c("Build-Up Speed" = "build_up_play_speed",
                                  "Build-Up Passing" = "build_up_play_passing",
                                  "Chance Creation Passing" = "chance_creation_passing",
                                  "Defensive Pressure" = "defence_pressure"),
                      multiple = TRUE,
                      selected = c("build_up_play_speed", "build_up_play_passing")),
          sliderInput("num_clusters", "Number of Clusters:",
                      min = 2, max = 6, value = 3),
          actionButton("run_cluster", "Run Analysis", class = "btn-primary"),
          p("Note: Analysis may take a few moments to complete")
        ),
        mainPanel(
          fluidRow(
            column(12,
                   div(class = "highlight-box",
                       h4("2D Cluster Visualization"),
                       plotlyOutput("clusterPlot2D", height = "450px")
                   )
            )
          ),
          div(class = "info-card",
              h4("Cluster Characteristics"),
              p("Summary statistics for each cluster group:"),
              dataTableOutput("clusterSummaryTable")
          ),
          div(class = "info-card",
              h4("Methodology"),
              p("This analysis uses k-means clustering to group teams based on their tactical attributes:"),
              tags$ul(
                tags$li("Variables are standardized before clustering"),
                tags$li("Algorithm partitions teams into k clusters"),
                tags$li("Each team assigned to cluster with nearest centroid")
              )
          )
        )
      )
    ),
    
    # 6. ADVANCED PREDICTION TAB (Decision Tree only)
    tabPanel(
      "Advanced Prediction",
      icon = icon("robot"),
      div(class = "info-card",
          h2(class = "section-header", "Advanced Match Prediction Model"),
          p("This section uses machine learning to predict match outcomes with higher accuracy."),
          tags$ul(
            tags$li(tags$b("Decision Tree Model:"), " Visualizes the prediction logic")
          ),
          
          tabPanel(
            "Decision Tree",
            div(class = "highlight-box",
                h3("Decision Tree Prediction Model"),
                p("This model shows the decision-making process for predicting match outcomes:"),
                tags$ul(
                  tags$li("Visual representation of prediction rules"),
                  tags$li("Shows most important factors"),
                  tags$li("Easy to interpret")
                )
            ),
            sidebarLayout(
              sidebarPanel(
                h4("Model Parameters"),
                selectInput("tree_league", "Select League:", 
                            choices = unique(match$league_name)),
                sliderInput("tree_depth", "Tree Depth:",
                            min = 1, max = 5, value = 3),
                checkboxInput("tree_defensive", "Focus on Defensive Performance", FALSE),
                actionButton("run_tree", "Run Model", class = "btn-primary"),
                div(class = "highlight-box",
                    h4("Interpretation Guide"),
                    p("The tree shows:"),
                    tags$ul(
                      tags$li("Decision nodes (questions)"),
                      tags$li("Leaf nodes (predictions)"),
                      tags$li("Percentage = prediction confidence")
                    )
                )
              ),
              mainPanel(
                plotOutput("treePlot", height = "600px"),
                div(class = "info-card",
                    h4("Model Performance"),
                    verbatimTextOutput("treeMetrics")
                )
              )
            )
          )
      ),
      
      # 7. MATCH PREDICTION TAB
      tabPanel(
        "Match Prediction",
        icon = icon("chart-line"),
        div(class = "info-card",
            h2(class = "section-header", "Match Outcome Prediction"),
            p("This model predicts match outcomes based on:"),
            tags$ul(
              tags$li("Team recent form (last 5 matches)"),
              tags$li("Head-to-head historical record"),
              tags$li("Home advantage factor")
            ),
            p("Select teams to see predicted probabilities and historical matches.")
        ),
        
        sidebarLayout(
          sidebarPanel(
            h4("Match Selection"),
            selectInput("pred_league", "League:", 
                        choices = unique(match$league_name)),
            uiOutput("team1_select"),
            uiOutput("team2_select"),
            actionButton("predict_btn", "Predict Outcome", class = "btn-primary"),
            div(class = "highlight-box",
                h4("Model Details"),
                p("Win probability calculated using:"),
                tags$ul(
                  tags$li("50% weight on recent form"),
                  tags$li("30% weight on head-to-head record"),
                  tags$li("20% weight on home advantage")
                )
            )
          ),
          mainPanel(
            div(class = "highlight-box",
                h3("Predicted Outcome Probabilities"),
                plotlyOutput("win_prob_plot")
            ),
            div(class = "info-card",
                h4("Prediction Details"),
                verbatimTextOutput("prediction_details")
            ),
            div(class = "highlight-box",
                h4("Recent Head-to-Head Matches"),
                p("Last 5 meetings between selected teams:"),
                dataTableOutput("h2h_matches")
            )
          )
        )
      )
    )
  )
)
  
```
### Server
```
server <- function(input, output, session) {
    
    # Match Prediction Model
    output$team1_select <- renderUI({
      req(input$pred_league)
      teams <- match %>% 
        filter(league_name == input$pred_league) %>% 
        distinct(home_team_name) %>% 
        pull(home_team_name)
      selectInput("team1", "Select Team 1:", choices = teams)
    })
    
    output$team2_select <- renderUI({
      req(input$pred_league)
      teams <- match %>% 
        filter(league_name == input$pred_league) %>% 
        distinct(home_team_name) %>% 
        pull(home_team_name)
      selectInput("team2", "Select Team 2:", choices = teams)
    })
    
    # Tactical Analysis outputs
    output$team_selector <- renderUI({
      req(input$selected_league_cluster)
      
      teams <- match %>%
        filter(league_name == input$selected_league_cluster) %>%
        distinct(home_team_name) %>%
        pull(home_team_name)
      
      selectizeInput("selected_teams_cluster", "Select Teams to Analyze:",
                     choices = teams, selected = teams[1:min(10, length(teams))],
                     multiple = TRUE, options = list(maxItems = 15))
    })
    
    cluster_data <- eventReactive(input$run_cluster, {
      req(input$selected_teams_cluster, input$cluster_vars)
      
      selected_team_ids <- team %>%
        filter(team_long_name %in% input$selected_teams_cluster) %>%
        pull(team_api_id)
      
      style_data <- team_attr %>%
        filter(team_api_id %in% selected_team_ids) %>%
        group_by(team_api_id) %>%
        summarise(across(all_of(input$cluster_vars), mean, na.rm = TRUE)) %>%
        left_join(team, by = "team_api_id") %>%
        filter(team_long_name %in% input$selected_teams_cluster)
      
      # Standardize the data
      scaled_data <- scale(style_data[, input$cluster_vars])
      rownames(scaled_data) <- style_data$team_long_name
      
      # Perform clustering
      kmeans_result <- kmeans(scaled_data, centers = input$num_clusters)
      
      list(
        data = style_data,
        scaled_data = scaled_data,
        clusters = kmeans_result$cluster,
        centers = kmeans_result$centers
      )
    })
    
    output$clusterPlot2D <- renderPlotly({
      req(cluster_data())
      
      plot_data <- data.frame(
        team = rownames(cluster_data()$scaled_data),
        x = cluster_data()$scaled_data[, 1],
        y = if (ncol(cluster_data()$scaled_data) >= 2) {
          cluster_data()$scaled_data[, 2]
        } else {
          rep(0, nrow(cluster_data()$scaled_data))
        },
        cluster = as.factor(cluster_data()$clusters)
      )
      
      centers <- data.frame(
        x = cluster_data()$centers[, 1],
        y = if (ncol(cluster_data()$centers) >= 2) {
          cluster_data()$centers[, 2]
        } else {
          rep(0, nrow(cluster_data()$centers))
        },
        cluster = as.factor(1:nrow(cluster_data()$centers))
      )
      
      p <- plot_ly() %>%
        add_markers(data = plot_data, x = ~x, y = ~y, color = ~cluster,
                    text = ~paste("Team:", team, "<br>Cluster:", cluster),
                    hoverinfo = 'text', size = 10) %>%
        add_markers(data = centers, x = ~x, y = ~y, color = ~cluster,
                    symbol = ~cluster, symbols = 'x', size = 15,
                    text = ~paste("Cluster", cluster, "Center"),
                    hoverinfo = 'text', showlegend = FALSE)
      
      if (length(input$cluster_vars) >= 2) {
        p <- p %>% layout(
          title = "Team Style Clustering (2D Projection)",
          xaxis = list(title = input$cluster_vars[1]),
          yaxis = list(title = input$cluster_vars[2])
        )
      } else {
        p <- p %>% layout(
          title = "Team Style Clustering (1D Projection)",
          xaxis = list(title = input$cluster_vars[1]),
          yaxis = list(title = "", showticklabels = FALSE)
        )
      }
      
      p
    })
    
    output$clusterSummaryTable <- renderDataTable({
      req(cluster_data())
      
      summary_data <- data.frame(
        Cluster = 1:input$num_clusters,
        Size = as.vector(table(cluster_data()$clusters)),
        cluster_data()$centers
      )
      
      datatable(summary_data, options = list(pageLength = input$num_clusters)) %>%
        formatRound(columns = 3:ncol(summary_data), digits = 2)
    })
    
# Reactive data for prediction models
    prediction_data <- reactive({
      req(input$tree_league)
      
      match %>%
        filter(league_name == input$tree_league) %>%
        mutate(
          outcome = factor(match_result, levels = c("Home Win", "Draw", "Away Win")),
          home_team_last5 = ave(home_team_goal, home_team_api_id, FUN = function(x) {
            if(length(x) >= 5) mean(tail(x, 5)) else mean(x)}),
          away_team_last5 = ave(away_team_goal, away_team_api_id, FUN = function(x) {
            if(length(x) >= 5) mean(tail(x, 5)) else mean(x)}),
          home_team_streak = ave(home_team_goal - away_team_goal, home_team_api_id, 
                                 FUN = function(x) {
                                   if(length(x) >= 3) sum(sign(tail(x, 3))) else sum(sign(x))}),
          away_team_streak = ave(away_team_goal - home_team_goal, away_team_api_id, 
                                 FUN = function(x) {
                                   if(length(x) >= 3) sum(sign(tail(x, 3))) else sum(sign(x))})
        ) %>%
        select(outcome, home_team_goal, away_team_goal, 
               home_team_last5, away_team_last5,
               home_team_streak, away_team_streak) %>%
        na.omit()
    })
    
    # Decision Tree Model
    tree_model <- eventReactive(input$run_tree, {
      req(prediction_data())
      
      set.seed(123)
      train_index <- createDataPartition(prediction_data()$outcome, p = 0.8, list = FALSE)
      train_data <- prediction_data()[train_index, ]
      test_data <- prediction_data()[-train_index, ]
      
      if(input$tree_defensive) {
        formula <- outcome ~ away_team_last5 + home_team_streak
      } else {
        formula <- outcome ~ home_team_last5 + away_team_last5 + home_team_streak + away_team_streak
      }
      
      model <- rpart(formula, 
                     data = train_data,
                     method = "class",
                     control = rpart.control(maxdepth = input$tree_depth))
      
      predictions <- predict(model, test_data, type = "class")
      cm <- confusionMatrix(predictions, test_data$outcome)
      
      list(model = model, cm = cm)
    })
    
    output$treePlot <- renderPlot({
      req(tree_model())
      
      rpart.plot(tree_model()$model, 
                 box.palette = list("#3498db", "#2ecc71", "#e74c3c"),
                 shadow.col = "gray", 
                 nn = TRUE)
    })
  
  output$treePlot <- renderPlot({
    req(tree_model())
    
    rpart.plot(tree_model()$model, 
               box.palette = list("#3498db", "#2ecc71", "#e74c3c"),
               shadow.col = "gray", 
               nn = TRUE)
  })
  
  output$treeMetrics <- renderPrint({
    req(tree_model())
    cat("Decision Tree Performance:\n")
    cat("-------------------------\n")
    print(tree_model()$cm$overall)
    cat("\nClass-wise Performance:\n")
    print(tree_model()$cm$byClass)
  })
  
  # Random Forest Model
  rf_model <- eventReactive(input$run_rf, {
    req(prediction_data())
    
    set.seed(123)
    train_index <- createDataPartition(prediction_data()$outcome, p = 0.8, list = FALSE)
    train_data <- prediction_data()[train_index, ]
    test_data <- prediction_data()[-train_index, ]
    
    ctrl <- trainControl(method = "cv", number = 5)
    
    model <- train(outcome ~ home_team_last5 + away_team_last5 + 
                     home_team_streak + away_team_streak,
                   data = train_data,
                   method = "rf",
                   ntree = input$rf_ntree,
                   tuneGrid = data.frame(mtry = input$rf_mtry),
                   trControl = ctrl)
    
    predictions <- predict(model, test_data)
    cm <- confusionMatrix(predictions, test_data$outcome)
    
    list(model = model, cm = cm)
  })
  
  output$rfImportance <- renderPlot({
    req(rf_model())
    
    varImp <- varImp(rf_model()$model)
    ggplot(varImp, top = 5) +
      theme_minimal() +
      labs(title = "Variable Importance",
           x = "Importance Score",
           y = "") +
      scale_fill_viridis_d()
  })
  
  output$rfMetrics <- renderPrint({
    req(rf_model())
    cat("Random Forest Performance:\n")
    cat("-------------------------\n")
    print(rf_model()$cm$overall)
    cat("\nClass-wise Performance:\n")
    print(rf_model()$cm$byClass)
  })
  
  output$rfPredictions <- renderDataTable({
    req(rf_model(), prediction_data())
    
    set.seed(123)
    sample_data <- prediction_data()[sample(nrow(prediction_data()), 10), ]
    predictions <- predict(rf_model()$model, sample_data, type = "prob")
    
    result <- cbind(
      sample_data %>% select(home_team_last5, away_team_last5),
      Actual = sample_data$outcome,
      Predicted = predict(rf_model()$model, sample_data),
      round(predictions, 3)
    )
    
    datatable(result, options = list(pageLength = 5)) %>%
      formatStyle(c('Home Win', 'Draw', 'Away Win'),
                  backgroundColor = styleInterval(seq(0.1, 0.9, by = 0.1),
                                                  rev(viridis(10))))
  })
  
  # Model Comparison
  model_comparison <- reactive({
    req(prediction_data())
    
    set.seed(123)
    train_index <- createDataPartition(prediction_data()$outcome, p = 0.8, list = FALSE)
    train_data <- prediction_data()[train_index, ]
    test_data <- prediction_data()[-train_index, ]
    
    ctrl <- trainControl(method = "cv", number = 5, savePredictions = TRUE)
    
    models <- c("rpart", "rf", "glmnet", "knn", "naive_bayes")
    
    withProgress(message = 'Training models', value = 0, {
      model_list <- lapply(models, function(x) {
        incProgress(1/length(models), detail = paste("Training", x))
        train(outcome ~ ., data = train_data, method = x, trControl = ctrl)
      })
    })
    
    names(model_list) <- models
    
    # Extract performance metrics
    results <- resamples(model_list)
    
    list(models = model_list, results = results)
  })
  
  output$modelComparisonPlot <- renderPlotly({
    req(model_comparison())
    
    metrics <- model_comparison()$results$values %>%
      as.data.frame() %>%
      select(ends_with("~Accuracy")) %>%
      pivot_longer(everything(), names_to = "Model", values_to = "Accuracy") %>%
      mutate(Model = gsub("~Accuracy", "", Model))
    
    plot_ly(metrics, x = ~Model, y = ~Accuracy, type = "box",
            color = ~Model, colors = viridis_pal()(5)) %>%
      layout(title = "Model Accuracy Comparison",
             yaxis = list(title = "Accuracy"))
  })
  
  output$modelSummary <- renderPrint({
    req(model_comparison())
    
    cat("Model Performance Summary:\n")
    cat("-------------------------\n")
    summary(model_comparison()$results)
  })
  
  output$modelRecommendation <- renderUI({
    req(model_comparison())
    
    acc <- model_comparison()$results$values %>%
      as.data.frame() %>%
      select(ends_with("~Accuracy")) %>%
      summarise_all(mean)
    
    best_model <- names(which.max(acc))
    
    tagList(
      h4("Recommendation:"),
      p(paste("Based on cross-validation results, the", 
              tags$b(toupper(best_model)), 
              "model performs best with an average accuracy of",
              round(100 * acc[[paste0(best_model, "~Accuracy")]], 1), "%.")),
      p("Considerations:"),
      tags$ul(
        tags$li("Random Forest (rf) typically offers the best balance of accuracy and interpretability"),
        tags$li("Decision trees (rpart) are more interpretable but less accurate"),
        tags$li("Linear models (glmnet) are fast but may underperform for complex patterns")
      )
    )
  })
  
  # Win probability model
  win_prob_model <- function(team1, team2, league) {
    # Get recent form (last 5 matches)
    team1_form <- match %>%
      filter((home_team_name == team1 | away_team_name == team1) & 
               league_name == league) %>%
      arrange(desc(date)) %>%
      head(5) %>%
      mutate(points = case_when(
        (home_team_name == team1 & match_result == "Home Win") | 
          (away_team_name == team1 & match_result == "Away Win") ~ 3,
        match_result == "Draw" ~ 1,
        TRUE ~ 0
      )) %>%
      summarise(form = mean(points, na.rm = TRUE)) %>%
      pull(form)
    
    team2_form <- match %>%
      filter((home_team_name == team2 | away_team_name == team2) & 
               league_name == league) %>%
      arrange(desc(date)) %>%
      head(5) %>%
      mutate(points = case_when(
        (home_team_name == team2 & match_result == "Home Win") | 
          (away_team_name == team2 & match_result == "Away Win") ~ 3,
        match_result == "Draw" ~ 1,
        TRUE ~ 0
      )) %>%
      summarise(form = mean(points, na.rm = TRUE)) %>%
      pull(form)
    
    # Get head-to-head record
    h2h <- match %>%
      filter(league_name == league &
               ((home_team_name == team1 & away_team_name == team2) |
                  (home_team_name == team2 & away_team_name == team1))) %>%
      arrange(desc(date)) %>%
      head(10)
    
    if (nrow(h2h) > 0) {
      h2h_summary <- h2h %>%
        mutate(team1_win = ifelse((home_team_name == team1 & match_result == "Home Win") | 
                                    (away_team_name == team1 & match_result == "Away Win"), 1, 0),
               team2_win = ifelse((home_team_name == team2 & match_result == "Home Win") | 
                                    (away_team_name == team2 & match_result == "Away Win"), 1, 0),
               draw = ifelse(match_result == "Draw", 1, 0)) %>%
        summarise(team1_wins = sum(team1_win),
                  team2_wins = sum(team2_win),
                  draws = sum(draw))
      
      h2h_weight <- 0.3  # Weight given to head-to-head record
    } else {
      h2h_weight <- 0
    }
    
    # Calculate probabilities (simplified model)
    form_weight <- 0.5
    home_advantage <- 0.2  # Additional weight for home team
    
    # Base probabilities based on form
    team1_prob <- (team1_form / 3) * 100 * form_weight
    team2_prob <- (team2_form / 3) * 100 * form_weight
    
    # Adjust for head-to-head if available
    if (nrow(h2h) > 0) {
      total_matches <- h2h_summary$team1_wins + h2h_summary$team2_wins + h2h_summary$draws
      team1_prob <- team1_prob + (h2h_summary$team1_wins / total_matches * 100 * h2h_weight)
      team2_prob <- team2_prob + (h2h_summary$team2_wins / total_matches * 100 * h2h_weight)
    }
    
    # Add home advantage (assuming team1 is home team)
    team1_prob <- team1_prob + (home_advantage * 100)
    
    # Normalize to 100%
    total <- team1_prob + team2_prob
    team1_prob <- team1_prob / total * 100
    team2_prob <- team2_prob / total * 100
    
    # Draw probability (simplified)
    draw_prob <- 25  # Base draw probability
    if (nrow(h2h) > 0) {
      draw_prob <- draw_prob + (h2h_summary$draws / total_matches * 100 * h2h_weight)
    }
    
    # Adjust win probabilities for draw probability
    adj_factor <- (100 - draw_prob) / 100
    team1_prob <- team1_prob * adj_factor
    team2_prob <- team2_prob * adj_factor
    
    return(list(team1 = team1_prob, team2 = team2_prob, draw = draw_prob))
  }
  
  # Reactive for prediction results
  prediction_results <- eventReactive(input$predict_btn, {
    req(input$team1, input$team2, input$pred_league)
    
    # Get win probabilities
    probs <- win_prob_model(input$team1, input$team2, input$pred_league)
    
    # Get head-to-head matches
    h2h_matches <- match %>%
      filter(league_name == input$pred_league &
               ((home_team_name == input$team1 & away_team_name == input$team2) |
                  (home_team_name == input$team2 & away_team_name == input$team1))) %>%
      arrange(desc(date)) %>%
      head(5) %>%
      select(date, home_team_name, away_team_name, home_team_goal, away_team_goal, match_result)
    
    list(probs = probs, h2h_matches = h2h_matches)
  })
  
  # Output for win probability plot
  output$win_prob_plot <- renderPlotly({
    results <- prediction_results()
    
    data <- data.frame(
      Outcome = c(paste(input$team1, "Win"), paste(input$team2, "Win"), "Draw"),
      Probability = c(results$probs$team1, results$probs$team2, results$probs$draw)
    )
    
    plot_ly(data, x = ~Outcome, y = ~Probability, type = 'bar',
            marker = list(color = c('#3498db', '#e74c3c', '#2ecc71'))) %>%
      layout(title = paste("Predicted Outcome Probabilities for", input$team1, "vs", input$team2),
             xaxis = list(title = ""),
             yaxis = list(title = "Probability (%)", range = c(0, 100)))
  })
  
  # Output for prediction details
  output$prediction_details <- renderPrint({
    results <- prediction_results()
    cat("Prediction Model Details:\n")
    cat("------------------------\n")
    cat(input$team1, "Win Probability:", round(results$probs$team1, 1), "%\n")
    cat(input$team2, "Win Probability:", round(results$probs$team2, 1), "%\n")
    cat("Draw Probability:", round(results$probs$draw, 1), "%\n\n")
    cat("Model considers:\n")
    cat("- Recent form (last 5 matches)\n")
    cat("- Head-to-head record (last 10 matches)\n")
    cat("- Home advantage (for team listed first)\n")
  })
  
  # Output for head-to-head matches
  output$h2h_matches <- renderDataTable({
    results <- prediction_results()
    datatable(results$h2h_matches, options = list(pageLength = 5)) %>%
      formatStyle('match_result',
                  backgroundColor = styleEqual(
                    c("Home Win", "Away Win", "Draw"),
                    c('#3498db', '#e74c3c', '#2ecc71')
                  ))
  })
  
  # Output for team metrics
  output$team_metrics <- renderDataTable({
    results <- placement_results()
    
    metrics <- data.frame(
      Metric = c("Current Position", "Total Points", "Win Rate", "Draw Rate", "Loss Rate",
                 "Avg Goals For", "Avg Goals Against"),
      Value = c(paste0(results$current_position, "/", results$total_teams),
                round(results$metrics$total_points, 1),
                paste0(round(results$metrics$win_rate * 100, 1), "%"),
                paste0(round(results$metrics$draw_rate * 100, 1), "%"),
                paste0(round(results$metrics$loss_rate * 100, 1), "%"),
                round(results$metrics$avg_goals_for, 2),
                round(results$metrics$avg_goals_against, 2))
    )
    
    datatable(metrics, options = list(dom = 't'), rownames = FALSE)
  })
  #Skill Comparison
  output$skillComparisonPlot <- renderPlotly({
    req(input$selected_year_player, input$skill_metric)
    
    skill_data <- player_attr %>%
      filter(year(date) == input$selected_year_player) %>%
      group_by(player_api_id) %>%
      summarise(avg_skill = mean(.data[[input$skill_metric]], na.rm = TRUE)) %>%
      left_join(player, by = "player_api_id") %>%
      arrange(desc(avg_skill)) %>%
      head(input$top_players_slider)
    
    p <- plot_ly(skill_data,
                 x = ~reorder(player_name, avg_skill),
                 y = ~avg_skill,
                 type = 'bar',
                 marker = list(color = '#3498db'),
                 text = ~paste("Player:", player_name, "<br>Skill:", round(avg_skill, 1)),
                 hoverinfo = 'text') %>%
      layout(title = paste("Top", input$top_players_slider, "Players by", 
                           tools::toTitleCase(gsub("_", " ", input$skill_metric)), "-", input$selected_year_player),
             xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Skill Rating"),
             margin = list(b = 150))
    
    return(p)
  })
  
  
  
  # Output for placement rationale
  output$placement_rationale <- renderPrint({
    results <- placement_results()
    cat("Prediction Rationale:\n")
    cat("---------------------\n")
    cat("Team:", input$placement_team, "\n")
    cat("Current Position (", input$placement_season, "): ", 
        results$current_position, " of ", results$total_teams, "\n", sep = "")
    cat("Win Rate:", round(results$metrics$win_rate * 100, 1), "%\n")
    cat("Draw Rate:", round(results$metrics$draw_rate * 100, 1), "%\n")
    cat("Loss Rate:", round(results$metrics$loss_rate * 100, 1), "%\n\n")
    cat("Predicted Placement for Next Season:", results$prediction, "\n\n")
    cat("Model considers:\n")
    cat("- Current season performance metrics\n")
    cat("- League position trends\n")
    cat("- Historical performance patterns\n")
  })  
  
  # Reactive data for overview tab
  overview_league_data <- reactive({
    match %>%
      group_by(league_name) %>%
      summarise(
        avg_goals = mean(total_goals, na.rm = TRUE),
        home_win_perc = mean(match_result == "Home Win", na.rm = TRUE),
        high_scoring = mean(total_goals >= 3, na.rm = TRUE),
        n_matches = n()
      ) %>%
      arrange(desc(avg_goals))
  })
  
  overview_player_data <- reactive({
    player_performance %>%
      arrange(desc(avg_rating)) %>%
      head(15) %>%
      select(player_name, avg_rating, avg_dribbling, avg_shooting, avg_passing)
  })
  
  high_scoring_matches <- reactive({
    match %>%
      filter(total_goals >= 5) %>%
      arrange(desc(total_goals), desc(date)) %>%
      head(20) %>%
      select(date, league_name, home_team_name, away_team_name, home_team_goal, away_team_goal, total_goals)
  })
  
  # Overview tab outputs
  output$overviewLeaguePlot <- renderPlotly({
    data <- overview_league_data()
    
    plot_ly(data, x = ~reorder(league_name, avg_goals), y = ~avg_goals, type = 'bar',
            name = 'Avg Goals', marker = list(color = '#3498db')) %>%
      add_trace(y = ~home_win_perc, name = 'Home Win %', marker = list(color = '#2ecc71')) %>%
      layout(title = "League Performance Metrics",
             xaxis = list(title = ""),
             yaxis = list(title = "Metric Value"),
             barmode = 'group',
             hovermode = 'compare',
             legend = list(orientation = 'h', x = 0, y = 1.1))
  })
  
  output$overviewPlayerPlot <- renderPlotly({
    data <- overview_player_data()
    
    plot_ly(data, x = ~player_name, y = ~avg_rating, type = 'bar',
            name = 'Avg Rating', marker = list(color = '#3498db')) %>%
      add_trace(y = ~avg_dribbling, name = 'Dribbling', marker = list(color = '#e74c3c')) %>%
      add_trace(y = ~avg_shooting, name = 'Shooting', marker = list(color = '#f39c12')) %>%
      add_trace(y = ~avg_passing, name = 'Passing', marker = list(color = '#27ae60')) %>%
      layout(title = "Top Players by Performance Metrics",
             xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Rating"),
             barmode = 'group',
             hovermode = 'compare')
  })
  
  output$highScoringMatchesTable <- renderDataTable({
    datatable(high_scoring_matches(),
              options = list(pageLength = 5, autoWidth = TRUE),
              rownames = FALSE) %>%
      formatStyle('total_goals',
                  background = styleColorBar(high_scoring_matches()$total_goals, '#f39c12'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  
  # Dynamic Team Selection
  observe({
    req(input$gs_league)
    teams <- match %>% 
      filter(league_name == input$gs_league) %>% 
      distinct(home_team_name) %>% 
      pull(home_team_name) %>% 
      as.character()
    updateSelectInput(session, "gs_team", choices = teams)
  })
  
  # Player Rating Model
  player_rating_model <- eventReactive(input$gs_predict, {
    req(input$gs_league, input$gs_team, input$gs_season)
    
    tryCatch({
      # Prepare training data
      rating_train_data <- player_attr %>%
        filter(year(date) >= 2008 & year(date) <= 2015) %>%
        mutate(season = year(date)) %>%
        left_join(player, by = "player_api_id") %>%
        group_by(season, team_api_id) %>%
        summarise(
          avg_rating = mean(overall_rating, na.rm = TRUE),
          avg_potential = mean(potential, na.rm = TRUE),
          avg_passing = mean(short_passing, na.rm = TRUE),
          avg_shooting = mean(finishing, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        left_join(team, by = "team_api_id") %>%
        select(season, team_long_name, avg_rating, avg_potential, avg_passing, avg_shooting) %>%
        rename(team_name = team_long_name) %>%
        arrange(team_name, season) %>%
        group_by(team_name) %>%
        mutate(next_season_rating = lead(avg_rating)) %>%
        ungroup() %>%
        drop_na()
      
      # Prepare position-specific training data
      pos_train_data <- player_attr %>%
        filter(year(date) >= 2008 & year(date) <= 2015) %>%
        mutate(season = year(date)) %>%
        left_join(player, by = "player_api_id") %>%
        left_join(team, by = "team_api_id") %>%
        group_by(season, team_long_name, preferred_position) %>%
        summarise(
          avg_rating = mean(overall_rating, na.rm = TRUE),
          avg_potential = mean(potential, na.rm = TRUE),
          avg_passing = mean(short_passing, na.rm = TRUE),
          avg_shooting = mean(finishing, na.rm = TRUE),
          player_count = n(),
          .groups = "drop"
        ) %>%
        arrange(team_long_name, preferred_position, season) %>%
        group_by(team_long_name, preferred_position) %>%
        mutate(next_season_rating = lead(avg_rating)) %>%
        ungroup() %>%
        drop_na() %>%
        filter(player_count >= 3) # Only include positions with at least 3 players
      
      if (nrow(rating_train_data) < 100) {
        stop("Insufficient training data available")
      }
      
      # Train team-level model
      team_model <- train(
        next_season_rating ~ avg_rating + avg_potential + avg_passing + avg_shooting,
        data = rating_train_data,
        method = "lm",
        trControl = trainControl(method = "cv", number = 5)
      )
      
      # Train position-level model
      pos_model <- train(
        next_season_rating ~ avg_rating + avg_potential + avg_passing + avg_shooting + preferred_position,
        data = pos_train_data,
        method = "lm",
        trControl = trainControl(method = "cv", number = 5)
      )
      
      # Prepare prediction data for team
      team_pred_data <- player_attr %>%
        filter(year(date) == (as.numeric(input$gs_season) - 1)) %>%
        left_join(player, by = "player_api_id") %>%
        left_join(team, by = "team_api_id") %>%
        filter(team_long_name == input$gs_team) %>%
        summarise(
          avg_rating = mean(overall_rating, na.rm = TRUE),
          avg_potential = mean(potential, na.rm = TRUE),
          avg_passing = mean(short_passing, na.rm = TRUE),
          avg_shooting = mean(finishing, na.rm = TRUE)
        )
      
      if (nrow(team_pred_data) == 0) {
        stop("No data available for the selected team/season combination")
      }
      
      # Prepare prediction data for positions
      pos_pred_data <- player_attr %>%
        filter(year(date) == (as.numeric(input$gs_season) - 1)) %>%
        left_join(player, by = "player_api_id") %>%
        left_join(team, by = "team_api_id") %>%
        filter(team_long_name == input$gs_team) %>%
        group_by(preferred_position) %>%
        summarise(
          avg_rating = mean(overall_rating, na.rm = TRUE),
          avg_potential = mean(potential, na.rm = TRUE),
          avg_passing = mean(short_passing, na.rm = TRUE),
          avg_shooting = mean(finishing, na.rm = TRUE),
          player_count = n(),
          .groups = "drop"
        ) %>%
        filter(player_count >= 2) # Only predict for positions with at least 2 players
      
      # Make predictions
      team_pred <- predict(team_model, newdata = team_pred_data)
      pos_pred <- if (nrow(pos_pred_data) > 0) {
        predict(pos_model, newdata = pos_pred_data)
      } else {
        NULL
      }
      
      list(
        predicted_rating = team_pred,
        position_predictions = if (!is.null(pos_pred)) {
          tibble(
            position = pos_pred_data$preferred_position,
            predicted_rating = pos_pred,
            player_count = pos_pred_data$player_count
          )
        } else {
          NULL
        },
        model_summary = summary(team_model),
        input_data = team_pred_data,
        error = NULL
      )
    }, error = function(e) {
      list(
        predicted_rating = NA,
        position_predictions = NULL,
        model_summary = NULL,
        input_data = NULL,
        error = e$message
      )
    })
  })
  
  # Output - Predicted Player Rating Bar Chart
  output$gs_prediction_plot <- renderPlotly({
    results <- player_rating_model()
    
    if (!is.null(results$error)) {
      return(plotly_empty() %>% 
               layout(title = paste("Error:", results$error),
                      annotations = list(text = results$error, showarrow = FALSE)))
    }
    
    plot_data <- data.frame(
      Category = c("Predicted Avg Rating"),
      Value = results$predicted_rating
    )
    
    plot_ly(plot_data, x = ~Category, y = ~Value, type = 'bar',
            marker = list(color = '#3498db')) %>%
      layout(
        title = paste("Predicted Player Rating for", input$gs_team, input$gs_season),
        yaxis = list(title = "Average Rating", range = c(0, 100)),
        showlegend = FALSE
      )
  })
  # --- SERVER: Output - Historical Player Rating Trend Line ---
  output$gs_trend_plot <- renderPlotly({
    req(input$gs_team)
    
    rating_history <- player_attr %>%
      mutate(season = year(date)) %>%
      left_join(player, by = "player_api_id") %>%
      left_join(team, by = "team_api_id") %>%
      filter(team_long_name == input$gs_team) %>%
      group_by(season) %>%
      summarise(avg_rating = mean(overall_rating, na.rm = TRUE), .groups = "drop")
    
    if (nrow(rating_history) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No historical data available",
                      annotations = list(text = "No data for selected team", showarrow = FALSE)))
    }
    
    plot_ly(rating_history, x = ~season, y = ~avg_rating,
            type = 'scatter', mode = 'lines+markers',
            line = list(color = '#3498db', width = 3),
            marker = list(color = '#2c3e50', size = 8),
            text = ~paste("Season:", season, "<br>Rating:", round(avg_rating, 1)),
            hoverinfo = 'text') %>%
      layout(
        title = paste(input$gs_team, "Player Rating Trend"),
        xaxis = list(title = "Season", dtick = 1),
        yaxis = list(title = "Average Rating", range = c(0, 100)),
        hovermode = 'compare'
      )
  })
  
  # --- SERVER: Output - Predicted Ratings by Position ---
  output$gs_position_plot <- renderPlotly({
    results <- player_rating_model()
    
    if (is.null(results$position_predictions)) {
      return(plotly_empty() %>% 
               layout(title = "No position data available",
                      annotations = list(text = "Insufficient players per position", showarrow = FALSE)))
    }
    
    plot_data <- results$position_predictions %>%
      arrange(desc(predicted_rating))
    
    plot_ly(plot_data, x = ~position, y = ~predicted_rating, type = 'bar',
            marker = list(color = '#3498db'),
            text = ~paste("Players:", player_count),
            hoverinfo = 'y+text') %>%
      layout(
        title = paste("Predicted Ratings by Position for", input$gs_team, input$gs_season),
        xaxis = list(title = "Position", categoryorder = "array", categoryarray = ~position),
        yaxis = list(title = "Average Rating", range = c(0, 100)),
        showlegend = FALSE
      )
  })
  
  # --- SERVER: Output - Position Distribution ---
  output$gs_position_dist <- renderPlotly({
    req(input$gs_team, input$gs_season)
    
    position_data <- player_attr %>%
      filter(year(date) == (as.numeric(input$gs_season) - 1)) %>%
      left_join(player, by = "player_api_id") %>%
      left_join(team, by = "team_api_id") %>%
      filter(team_long_name == input$gs_team) %>%
      count(preferred_position) %>%
      filter(n > 0)
    
    if (nrow(position_data) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No position data available",
                      annotations = list(text = "No player position data", showarrow = FALSE)))
    }
    
    plot_ly(position_data, labels = ~preferred_position, values = ~n, type = 'pie',
            textinfo = 'label+percent',
            insidetextorientation = 'radial',
            marker = list(colors = RColorBrewer::brewer.pal(nrow(position_data), "Set3"))) %>%
      layout(
        title = paste("Player Position Distribution for", input$gs_team),
        showlegend = FALSE
      )
  })
  
  # Team Analysis outputs
  output$goalsPlot <- renderPlotly({
    req(input$selected_year_goals, input$selected_league_goals)
    
    filtered_data <- match %>%
      filter(year(date) == input$selected_year_goals, 
             league_name == input$selected_league_goals) %>%
      group_by(home_team_name) %>%
      summarise(
        total_goals_scored = sum(home_team_goal),
        total_goals_conceded = sum(away_team_goal),
        avg_goals_per_match = mean(total_goals),
        n_matches = n()
      ) %>%
      arrange(desc(total_goals_scored)) %>%
      head(input$top_teams_slider)
    
    league_avg <- match %>%
      filter(year(date) == input$selected_year_goals, 
             league_name == input$selected_league_goals) %>%
      summarise(avg_goals = mean(total_goals)) %>%
      pull(avg_goals)
    
    p <- plot_ly(filtered_data, x = ~reorder(home_team_name, total_goals_scored), 
                 y = ~total_goals_scored, type = 'bar',
                 name = 'Goals Scored', marker = list(color = '#3498db')) %>%
      add_trace(y = ~total_goals_conceded, name = 'Goals Conceded', 
                marker = list(color = '#e74c3c')) %>%
      layout(title = paste("Team Goal Performance -", input$selected_league_goals, input$selected_year_goals),
             xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Goals"),
             barmode = 'group',
             hovermode = 'compare')
    
    if (input$show_avg_line) {
      p <- p %>% add_lines(y = ~league_avg, name = 'League Avg', 
                           line = list(color = '#2c3e50', dash = 'dash'))
    }
    
    if (input$show_trend_line) {
      p <- p %>% add_lines(y = ~fitted(loess(total_goals_scored ~ as.numeric(reorder(home_team_name, total_goals_scored)))),
                           name = 'Trend', line = list(color = '#27ae60'))
    }
    
    p
  })
  
  output$leagueBoxplot <- renderPlotly({
    req(input$selected_year_dist)
    
    filtered <- match %>% 
      filter(year(date) == input$selected_year_dist) %>%
      mutate(league_name = factor(league_name))
    
    if (input$dist_type == "box") {
      p <- plot_ly(filtered, x = ~league_name, y = ~total_goals, 
                   type = "box", color = ~league_name,
                   colors = viridis_pal(option = input$color_palette)(length(unique(filtered$league_name))))
    } else {
      p <- plot_ly(filtered, x = ~league_name, y = ~total_goals, 
                   type = "violin", color = ~league_name,
                   colors = viridis_pal(option = input$color_palette)(length(unique(filtered$league_name))))
    }
    
    if (input$show_points) {
      p <- p %>% add_markers(y = ~total_goals, marker = list(size = 3, opacity = 0.5),
                             showlegend = FALSE)
    }
    
    p %>% layout(title = paste("Goal Distribution by League -", input$selected_year_dist),
                 xaxis = list(title = "League"),
                 yaxis = list(title = "Goals per Match"),
                 showlegend = FALSE)
  })
  
  output$distributionInsights <- renderUI({
    req(input$selected_year_dist)
    
    stats <- match %>%
      filter(year(date) == input$selected_year_dist) %>%
      group_by(league_name) %>%
      summarise(
        avg_goals = mean(total_goals),
        median_goals = median(total_goals),
        goal_sd = sd(total_goals),
        high_scoring = mean(total_goals >= 3) * 100
      )
    
    highest_avg <- stats %>% arrange(desc(avg_goals)) %>% head(1)
    lowest_avg <- stats %>% arrange(avg_goals) %>% head(1)
    most_variable <- stats %>% arrange(desc(goal_sd)) %>% head(1)
    
    tagList(
      h5("Key Insights:"),
      tags$ul(
        tags$li(
          paste("Highest scoring league:", highest_avg$league_name,
                "with", round(highest_avg$avg_goals, 2), "goals per match on average")
        ),
        tags$li(
          paste("Lowest scoring league:", lowest_avg$league_name,
                "with", round(lowest_avg$avg_goals, 2), "goals per match on average")
        ),
        tags$li(
          paste("Most variable league:", most_variable$league_name,
                "with standard deviation of", round(most_variable$goal_sd, 2), "goals")
        ),
        tags$li(
          paste(round(mean(stats$high_scoring), 1),
                "% of matches across all leagues had 3+ goals")
        )
      )
    )
  })
  
  
  # Player Analysis outputs
  output$playerSkillPlot <- renderPlotly({
    req(input$selected_year_player, input$skill_metric)
    
    skill_data <- player_attr %>%
      filter(year(date) == input$selected_year_player) %>%
      group_by(player_api_id) %>%
      summarise(avg_skill = mean(.data[[input$skill_metric]], na.rm = TRUE)) %>%
      left_join(player, by = "player_api_id") %>%
      arrange(desc(avg_skill)) %>%
      head(input$top_players_slider)
    
    if (input$group_by_position) {
      p <- plot_ly(skill_data, x = ~reorder(player_name, avg_skill), y = ~avg_skill,
                   type = 'bar', color = ~factor(ifelse(is.na(weight), "Unknown", 
                                                        ifelse(weight < 70, "Light", 
                                                               ifelse(weight < 80, "Medium", "Heavy")))),
                   colors = c('#3498db', '#2ecc71', '#e74c3c')) %>%
        layout(showlegend = TRUE)
    } else {
      p <- plot_ly(skill_data, x = ~reorder(player_name, avg_skill), y = ~avg_skill,
                   type = 'bar', marker = list(color = '#3498db'))
    }
    
    p %>% layout(title = paste("Top Players by", tools::toTitleCase(input$skill_metric), "-", input$selected_year_player),
                 xaxis = list(title = "", tickangle = -45),
                 yaxis = list(title = paste(tools::toTitleCase(input$skill_metric), "Rating")),
                 hovermode = 'compare')
  })
  
  output$playerComparisonTable <- renderDataTable({
    req(input$selected_year_player, input$skill_metric)
    
    skill_data <- player_attr %>%
      filter(year(date) == input$selected_year_player) %>%
      group_by(player_api_id) %>%
      summarise(avg_skill = mean(.data[[input$skill_metric]], na.rm = TRUE)) %>%
      left_join(player, by = "player_api_id") %>%
      arrange(desc(avg_skill)) %>%
      head(20) %>%
      select(Name = player_name, Rating = avg_skill, Height, Weight, Birthday) %>%
      mutate(Rating = round(Rating, 1))
    
    datatable(skill_data, options = list(pageLength = 5)) %>%
      formatStyle('Rating',
                  background = styleColorBar(skill_data$Rating, '#3498db'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  
  output$ratingTrend <- renderPlotly({
    req(input$selected_position, input$trend_metric)
    
    position_filters <- list(
      "Goalkeeper" = c("gk_diving", "gk_handling", "gk_kicking", "gk_reflexes"),
      "Defender" = c("marking", "standing_tackle", "sliding_tackle"),
      "Midfielder" = c("short_passing", "ball_control", "vision"),
      "Forward" = c("finishing", "volleys", "shot_power"),
      "All" = c("overall_rating", "potential")
    )
    
    filtered_data <- player_attr %>%
      filter(year(date) >= input$trend_years[1],
             year(date) <= input$trend_years[2])
    
    if (input$selected_position != "All") {
      if (input$selected_position == "Goalkeeper") {
        filtered_data <- filtered_data %>% filter(!is.na(gk_diving))
      } else if (input$selected_position == "Defender") {
        filtered_data <- filtered_data %>% filter(!is.na(marking))
      } else if (input$selected_position == "Midfielder") {
        filtered_data <- filtered_data %>% filter(!is.na(short_passing))
      } else if (input$selected_position == "Forward") {
        filtered_data <- filtered_data %>% filter(!is.na(finishing))
      }
    }
    
    
    
    trend_data <- filtered_data %>%
      mutate(year = year(date)) %>%
      group_by(year) %>%
      summarise(avg_metric = mean(.data[[input$trend_metric]], na.rm = TRUE))
    
    plot_ly(trend_data, x = ~year, y = ~avg_metric, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#3498db', width = 3),
            marker = list(color = '#2c3e50', size = 8)) %>%
      layout(
        title = paste("Trend in", tools::toTitleCase(gsub("_", " ", input$trend_metric)),
                      "for", input$selected_position, "Players"),
        xaxis = list(title = "Year", dtick = 1),
        yaxis = list(title = paste("Average", tools::toTitleCase(gsub("_", " ", input$trend_metric)))),
        hovermode = 'compare'
      )
  })
  
  
  output$trendAnalysis <- renderUI({
    req(input$selected_position, input$trend_metric)
    
    trend_data <- player_attr %>%
      filter(year(date) >= input$trend_years[1],
             year(date) <= input$trend_years[2]) %>%
      mutate(year = year(date)) %>%
      group_by(year) %>%
      summarise(avg_metric = mean(.data[[input$trend_metric]], na.rm = TRUE))
    
    change <- (last(trend_data$avg_metric) - first(trend_data$avg_metric)) / 
      first(trend_data$avg_metric) * 100
    
    tagList(
      h5("Trend Analysis:"),
      p(paste("From", input$trend_years[1], "to", input$trend_years[2], "the average",
              tolower(gsub("_", " ", input$trend_metric)), "for", tolower(input$selected_position),
              "players", ifelse(change >= 0, "increased by", "decreased by"),
              paste0(abs(round(change, 1)), "%."))),
      p("The trend line shows the year-over-year changes in player performance metrics.")
    )
  })
}
```
## Running the App
```
shinyApp(ui=ui , server=server )
```
## Click the link below to visit the interactive shiny app 

 https://nabikarki21.shinyapps.io/Data_332_Final/



  
