# Load library
library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(countrycode)
library(rnaturalearth)
library(sf)
library(networkD3)
library(htmlwidgets)

# Get working directory
getwd()

# Read file
spotify_kp <- read.csv("spotify_kpop.csv", fileEncoding = "UTF-8")
kpop_survey <- read.csv("cleaned_kpop_data.csv", fileEncoding = "UTF-8")

# Visualisation 1
capital <- function(artist) {
  case_when(
    grepl("iu", artist, ignore.case = TRUE) ~ "IU",
    grepl("bts", artist, ignore.case = TRUE) ~ "BTS",
    grepl("jung kook", artist, ignore.case = TRUE) ~ "BTS",
    grepl("suga", artist, ignore.case = TRUE) ~ "BTS",
    grepl("j-hope", artist, ignore.case = TRUE) ~ "BTS",
    grepl("lisa", artist, ignore.case = TRUE) ~ "Lisa",
    grepl("twice", artist, ignore.case = TRUE) ~ "TWICE",
    grepl("blackpink", artist, ignore.case = TRUE) ~ "BLACKPINK",
    grepl("red velvet", artist, ignore.case = TRUE) ~ "Red Velvet",
    grepl("itzy", artist, ignore.case = TRUE) ~ "ITZY",
    grepl("ive", artist, ignore.case = TRUE) ~ "IVE",
    grepl("\\(g\\)i-dle", artist, ignore.case = TRUE) ~ "(G)I-DLE",
    grepl("rosé", artist, ignore.case = TRUE) ~ "Rosé",
    grepl("newjeans", artist, ignore.case = TRUE) ~ "NewJeans",
    TRUE ~ artist
  )
}

spotify_kp_artist <- spotify_kp %>%
  separate_rows(artist, sep = ", ") %>%
  mutate(unk_art = case_when(
    grepl("jung kook", artist, ignore.case = TRUE) ~ "bts",
    grepl("suga", artist, ignore.case = TRUE) ~ "bts",
    grepl("j-hope", artist, ignore.case = TRUE) ~ "bts",
    grepl("lisa", artist, ignore.case = TRUE) ~ "blackpink",
    TRUE ~ artist
  )) %>%
  mutate(unk_art = str_remove(unk_art, "coldplay"),
         unk_art = str_remove(unk_art, "halsey"),
         unk_art = str_remove(unk_art, "charlie puth"),
         unk_art = str_remove(unk_art, "dua lipa"),
         unk_art = str_remove(unk_art, "selena gomez"),
         unk_art = str_remove(unk_art, "steve aoki")) %>%
  filter(unk_art != "") %>%
  mutate(unk_art = capital(unk_art))

top10_data <- spotify_kp_artist %>%
  group_by(unk_art) %>%
  summarise(streaming_amt = sum(streams, na.rm = TRUE)) %>%
  arrange(desc(streaming_amt)) %>%
  slice_head(n = 10)

top10_spotify_kp <- spotify_kp_artist %>%
  filter(unk_art %in% top10_data$unk_art)


# Visualisation 3
# Function for converting age to age group
assign_age_group <- function(age) {
  case_when(
    age >= 10 & age <= 15 ~ "10-15",
    age >= 16 & age <= 20 ~ "16-20",
    age >= 21 & age <= 25 ~ "21-25",
    age >= 26 & age <= 30 ~ "26-30",
    age > 30 ~ "30+"
  )
}

assign_music_hr <- function(hr) {
  case_when(
    hr < 1 ~ "< 1 hour",
    hr >= 1 & hr <= 2 ~ "1-2 hours",
    hr > 2 & hr <= 3 ~ "2-3 hours",
    hr > 3 & hr <= 4 ~ "3-4 hours",
    hr > 4 & hr <= 5 ~ "4-5 hours",
    hr > 5 ~ "> 5 hours"
  )
}

# Assign age to age group
kpop_survey$age_group <- sapply(kpop_survey$age, assign_age_group)

# Assign music listening hour to groups
kpop_survey$music_group <- sapply(kpop_survey$daily_music_hr, assign_music_hr)

kpop_df <- kpop_survey %>%
  select("age_group", "obsessed_yn", "music_group", "pursuit") %>%
  mutate(pursuit = ifelse(pursuit == "others (combination of the the four and visit korea)", 
                          "combinations of the four", 
                          pursuit))


# R shiny
ui <- fluidPage(
  title = "Influence of South Korea Music Industry Globally", 
  tags$head(
    tags$style(HTML("
      body {
        padding: 0;
        margin: 0;
        background-color: #ffffff;
        font-family: 'Georgia', serif;
      }
      
      .container-fluid {
        padding: 0;
        margin: 0;
      }
      
      .header-container {
        display: flex;
        min-height: 100vh;
        width: 100%; 
        margin: 0;
        padding: 0;
      }
      
      .sidebar-container {
        width: 250px;
        background-color: #032539;
        padding: 60px 0;
        color: #f8f9fa;
      }
      
      .navbar-vertical {
        display: flex;
        flex-direction: column;
        padding: 0;
        list-style-type: none;
        margin: 0;
      }
      
      .navbar-vertical .nav-item {
        padding: 15px 30px;
      }
      
      .navbar-vertical .nav-item a {
        color: #EBEBE9;
        text-decoration: none;
        display: flex;
        align-items: center;
        font-size: 14px;
        font-weight: bold;
      }
      
      .navbar-vertical .nav-item:hover {
        background-color: #f8f9fa;
      }
      
      .navbar-vertical .nav-item:hover a {
          color: #032539;
      }
      
      .navbar-vertical .nav-item.active {
        border-left: 8px solid #FA991C;
        background-color: #f8f9fa;
      }
      
      .navbar-vertical .nav-item.active a {
        color: #032539;
      }
      
      .content-container {
        flex-grow: 1;
        padding: 40px;
        background-color: #f8f9fa;
      }
      .img-container {
        position: relative;
        width: 100%;
        height: auto;
      }
      .img-container img {
        width: 100%;
        height: auto;
        opacity: 0.8;
      }

      .title {
        background-color: #DFDCD5;
        text-align: center;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        color: #032539;
        font-size: 64px;
        border-radius: 10px;
        font-weight: bold;
        padding: 20px;
      }
      .btn-home {
        background-color: #FA991C;
        color: #032539;
        border: none;
        border-radius: 30px;
        padding: 15px 30px;
        position: absolute;
        top: 75%;
        left: 50%;
        transform: translateX(-50%);
      }
      .btn-home:hover, .bg-btn:hover, .stream-btn:hover, .map-btn:hover{
        background-color: #d88c1a;
      }
      .bg-btn {
        background-color: #FA991C;
        color: #032539;
        border: none;
        border-radius: 30px;
        padding: 15px 30px;
        position: absolute;
        bottom: 30px;
        right: 0;
        transform: translateX(-50%);
      }
      .stream-btn {
        background-color: #FA991C;
        color: #032539;
        border: none;
        border-radius: 30px;
        padding: 15px 30px;
        position: absolute;
        bottom: 10px;
        right: -130px;
        transform: translateX(-50%);
      }
      .map-btn {
        background-color: #FA991C;
        color: #032539;
        border: none;
        border-radius: 30px;
        padding: 15px 30px;
        position: absolute;
        bottom: 10px;
        right: -230px;
        transform: translateX(-50%);
      }

      .background-text {
        position: absolute;
        width: 450px;
        right: 130px;
        font-size: 16px;
        text-align: justify;
      }
      .bg-img img {
        position: absolute;
        width: 400px;
        height: auto;
        right: 650px;
      }
      .graph-title {
        text-align: center;
        color: #f8f9fa;
        font-weight: bold;
        padding: 15px;
        font-size: 36px;
        background-color: #032539;
        border-radius: 20px;
      }
      .stream-graph {
        position: absolute;
        top: 130px;
        width: 75%;
      }
      .sidebar {
        width: 200px;
        height: auto;
        overflow-y: auto;
      }
      .bar-slider {
        position: absolute;
        top: 20%;
        left: 50px;
        width: 250px;
        background: white;
        padding: 10px 10px 200px 10px;
        border-radius: 5px;
        font-family: 'Georgia', serif;
      }
      .stack-bar-graph {
        position: absolute;
        top: 20%;
        right: 10px;
        width: 770px;
        font-family: 'Georgia', serif;
      }
      .stream-text-container {
        position: absolute;
        top: 540px;
        width: 77%;
        padding: 10px 20px 10px 20px;
        border-radius: 10px;
        background-color: #DFDCD5;
        text-align: justify;
      }
      .stream_map {
        position: relative;
        top: 10%;
      }
      .leaflet .legend {
        font-family: 'Georgia', serif;
      }
      .map-controls {
        position: absolute;
        top: 350px;
        left: 30px;
        width: 200px;
        background: white;
        opacity: 0.8;
        padding: 10px;
        border-radius: 5px;
        z-index: 1000;
        font-family: 'Georgia', serif;
      }
      .map-text-container {
        position: absolute;
        top: 640px;
        margin-bottom: 20px;
        width: 77%;
        background-color: #DFDCD5;
        border-radius: 10px;
        padding: 10px 20px 10px 20px;
        text-align: justify;
      }
      .map_graph {
        position: relative;
        top: 10px;
      }
      .sankey-text {
        position: absolute;
        right: 50px;
        top: 160px;
        width: 750px;
        background-color: #DFDCD5;
        border-radius: 10px;
        padding: 0px 10px;
        text-align: center;
      }
      .culture-container {
        position: absolute;
        top: 20%;
        width: 75%;
      }
      .culture-sidebar {
        position: absolute;
        top: 17px;
        left: 50px;
        width: 300px;
        background: white;
        border-radius: 5px;
      }
      .culture-text-container {
        position: absolute;
        top: 80%;
        width: 77%;
        background-color: #DFDCD5;
        border-radius: 10px;
        padding: 10px 20px 10px 20px;
        text-align: justify;
      }
      .text-1 {
        position: absolute;
        right: 685px;
      }
      .text-2 {
        position: absolute;
        right: 445px;
      }
      .text-3 {
        position: absolute;
        right: 230px;
      }
      .text-4 {
        position: absolute;
        right: 0px;
      }
    
    "))
  ),

  
  tags$div(
    class = "header-container",
    
    # Sidebar
    tags$div(
      class = "sidebar-container",
      tags$ul( # side bar
        class = "navbar-vertical",
        tags$li(class = "nav-item", 
                tags$a(href = "#home", 
                       onclick = "setTab('home')", # Set click event
                       tags$span(icon("home"), style = "margin-right: 7px;"),
                       "Home")),
        tags$li(class = "nav-item", 
                tags$a(href = "#background", 
                       onclick = "setTab('background')",  # Set click event
                       tags$span(icon("circle-info"), style = "margin-right: 7px;"),
                       "Industry Background")),
        tags$li(class = "nav-item", 
                tags$a(href = "#streaming", 
                       onclick = "setTab('streaming')",  # Set click event
                       tags$span(icon("music"), style = "margin-right: 7px;"),
                       "Streaming Trends")),
        tags$li(class = "nav-item", 
                tags$a(href = "#global", 
                       onclick = "setTab('global')",  # Set click event
                       tags$span(icon("globe"), style = "margin-right: 7px;"),
                       "Global Impact")),
        tags$li(class = "nav-item", 
                tags$a(href = "#cultural", 
                       onclick = "setTab('cultural')", # Set click event
                       tags$span(icon("film"), style = "margin-right: 7px;"),
                       "Cultural Impact"))
      )
    ),
    
    # Content area
    tags$div(
      class = "content-container",
      div(id = "content",
          # Home content
          conditionalPanel(
            condition = "input.active_tab == 'home'",
            div(class = "img-container",
                img(src = "https://i.pinimg.com/564x/d6/1f/07/d61f07632af8030125eab919b25db0ed.jpg"),
                div(class = "title", "Influence of South Korea Music Industry Globally"),
                tags$button("Learn More", id = "btn", class = "btn-home")
            )
          ),
          # Background content
          conditionalPanel(
            condition = "input.active_tab == 'background'",
            div(class = "bg-img",
                img(src = "https://i.pinimg.com/736x/eb/b8/25/ebb8256414723ffa0c5ba31d15a0d875.jpg")),
            div(class = "background-text",
              HTML("
                <h2 style = 'background-color: #032539; color: #f8f9fa; border-radius: 10px; text-align: center; padding: 10px; font-weight: bold;'>Background</h2>
                <p>The South Korean music industry has gained a global following in recent decades, 
                crossing Korean borders to achieve significant success in the international market. 
                The rise of K-pop, exemplified by groups like BTS and BLACKPINK who have achieved remarkable streaming 
                numbers exceeding 12 billion and 5 billion respectively, demonstrates the industry's global reach and 
                commercial impact. Furthermore, K-pop engagement through social media has a positive impact on the satisfaction 
                of cultural destinations' content, influencing the desire to visit Korea.</p>
                <p>
                This cultural phenomenon, known as the 'Korean Wave' or 'Hallyu', extends beyond music to influence various aspects 
                of global popular culture, from fashion and beauty to tourism and entertainment. The industry's success has created a 
                unique synergy between musical entertainment and cultural tourism, with fans investing significant time in engaging with Korean culture, as shown by demographic studies across different age groups.
                </p><p>
                The global streaming data indicates strong market penetration across diverse geographic regions, with particularly high engagement in Southeast Asia, 
                North America, and parts of Europe. This widespread distribution indicates that the South Korean music industry has effectively crossed cultural and 
                linguistic barriers to establish a significant international presence.</p>
                <p style='font-size: 12px; text-decoration: underline;'>
                    Image source: <a href='https://au.pinterest.com/pin/898397825662214568/' target='_blank'>Link</a>
                </p>
                
              ")
            ),
            # Assign button
            tags$button("Explore More >", id = "bgbtn", class = "bg-btn"),
          ),
          
          # Streaming Platform content
          conditionalPanel(
            condition = "input.active_tab == 'streaming'",
            div(class = "graph-title", "The Explosive Rise of K-pop (2013 - 2023)"),
            div(class = "stream-graph",
                # Apply slide bar
                sidebarLayout(
                  sidebarPanel(class = "bar-slider",
                    sliderInput("stream_year_range", "Year Range:",
                                min = 2013, max = 2023,
                                value = c(2013, 2023),
                                step = 1,
                                sep = ""),
                    radioButtons("xaxis_choice", "Aspects:",
                                 choices = list("Year" = "year", "Artist" = "artist"),
                                 selected = "year")
                  ),
                  # Output plot
                  mainPanel(class = "stack-bar-graph",
                    plotOutput("stacked_bar_plot")
                  )
                )
            ),
            div(class = "stream-text-container",
                p(HTML("
                  In 2013, PSY's viral sensation \"Gangnam Style\" marked K-pop's first global breakthrough, reaching first place in 
                  chart rankings despite modest streaming numbers (0.1M), demonstrating the genre's international potential before its digital transformation.
                ")),
                
                p(HTML("<br>The journey unfolds in three distinct phases:")),
                
                p(HTML("
                  <strong>Early Foundations (2015-2017)</strong>: K-pop grew from 0.4M streams to 239.7M streams, with chart rankings at #61 showing early global potential.
                ")),
                p(HTML("
                  <strong>Digital Breakthrough (2018-2020)</strong>: Streaming exploded from 1B to 4.9B streams, accelerated by pandemic-era digital consumption.
                ")),
                p(HTML("
                  <strong>Peak Performance (2021-2022)</strong>: K-pop has reached new heights, with streams rising from 6B to 6.3B, establishing itself as a global entertainment powerhouse.
                ")),
                p(HTML('Data source: <a href="https://www.kaggle.com/datasets/jfreyberg/spotify-chart-data" target="_blank" style="font-size: 12px; text-decoration: underline;">Spotify Streaming Data</a>'))
                
            ),
            # Assign button
            tags$button("But where did all these streams come from >", id = "streambtn", class = "stream-btn"),
          ),
          
          # Global Influence content
          conditionalPanel(
            condition = "input.active_tab == 'global'",
            div(class = "graph-title", "From Seoul to the World: Mapping K-pop's Revolution"),
            div(class = "stream_map",
                div(class = "map_graph", leafletOutput("map", height = "500px")),
                absolutePanel(
                  class = "map-controls",
                  sliderInput("year_range", "Year Range: ",
                              min = 2013, max = 2023,
                              value = c(2013, 2023),
                              sep = "")
                )
            ),
            div(class = "map-text-container",
                p("K-pop's streaming numbers have reached new heights, transforming a world where geographical boundaries fade into streams of 
                  shared cultural experiences. This remarkable global journey showcases K-pop as a dominant cultural force. Southeast Asia leads 
                  the wave with over 2 billion streams, followed by North America with an impressive 500 million to 1 billion. Meanwhile, Europe 
                  and South America are emerging as growing markets. This illustrates K-pop's evolution from a local phenomenon to a global sensation."),
                p(HTML('Data source: <a href="https://www.kaggle.com/datasets/jfreyberg/spotify-chart-data" target="_blank" style="font-size: 12px; text-decoration: underline;">Spotify Streaming Data</a>'))
            ),
            tags$button("What happens when millions of fans worldwide don't just listen to K-pop >", id = "mapbtn", class = "map-btn"),
          ),
          
          # Cultural Influence content
          conditionalPanel(
            condition = "input.active_tab == 'cultural'",
            div(class = "graph-title", "Beyond the Music: Mapping K-pop's Cultural Effect"),
            div(class = "sankey-text",
                HTML("
                  <p class = text-1>Age</p>
                  <p class = text-2>Engagement</p>
                  <p class = text-3>Music Hours</p>
                  <p class = text-4>Cultural Impact</p>
                ")),
            div(class = "culture-container",
                # Checkbox tooltips
                sidebarLayout(
                  sidebarPanel(class = "culture-sidebar",
                     checkboxGroupInput("age_group",
                                        label = "Age Group: ",
                                        choices = unique(kpop_df$age_group),
                                        selected = unique(kpop_df$age_group))
                  ),
                  mainPanel(class = "culture-graph",
                     sankeyNetworkOutput("sankey")
                  )
                )
            ),
            div(class = "culture-text-container",
                p("The diagram reveals patterns of cultural engagement that range from casual interest to deep immersion. 
                  It begins with age demographics, highlighting that the 16-25 group leads in overall engagement. 
                  These young audiences are not just passive listeners; many dedicate 4-5 hours or more to their K-pop experience.  
                  What’s particularly compelling is how this musical interest expands into broader cultural participation. 
                  The diagram illustrates various pathways of cultural adoption, including learning Korean fashion and makeup techniques, 
                  pursuing dance, and engaging in deeper cultural studies. Most intriguingly, many fans participate in a combination of 
                  these four activities, demonstrating how K-pop acts as a gateway to comprehensive cultural exploration. "),
                p(HTML('Data source: <a href="https://figshare.com/articles/dataset/KPOP_DATA_xlsx/12093648?file=22236195" target="_blank" style="font-size: 12px; text-decoration: underline;">KPOP Survey Data</a>'))
                
            )
          )
      )
    )
  ),
  
  # JavaScript for handling active tab
  tags$script(HTML("
    
    Shiny.addCustomMessageHandler('setTab', function(tabValue) {
      document.querySelectorAll('.nav-item').forEach(item => {
        item.classList.remove('active');
      });
      document.querySelector('.nav-item a[href=\"#' + tabValue + '\"]').parentElement.classList.add('active');
      Shiny.setInputValue('active_tab', tabValue);
    });
    
    function setTab(tabValue) {
      document.querySelectorAll('.nav-item').forEach(item => {
        item.classList.remove('active');
      });
      event.currentTarget.parentElement.classList.add('active');
      Shiny.setInputValue('active_tab', tabValue);
    }
    
    document.getElementById('btn').onclick = function() {
      setTab('background');
    };
    
    document.getElementById('bgbtn').onclick = function() {
      setTab('streaming');
    };
    
    document.getElementById('streambtn').onclick = function() {
      setTab('global');
    };
    
    document.getElementById('mapbtn').onclick = function() {
      setTab('cultural');
    };
    
    
  "))
)

# Server function
server <- function(input, output, session) {
  
  # Set default to home page
  session$sendCustomMessage("setTab", "home")
  
  # Reactive artist filtered data
  art_filtered_data <- reactive({
    req(input$stream_year_range)
    
    # Ensure year data is available
    if (input$stream_year_range[1] == input$stream_year_range[2]) {
      year_select <- input$stream_year_range[1]
      return(
        top10_spotify_kp %>%
          mutate(year = year(date)) %>%
          filter(year == year_select)
      )
    } else {
      return (
        top10_spotify_kp %>%
          mutate(year = year(date)) %>%
          filter(year >= input$stream_year_range[1] & year <= input$stream_year_range[2])
      )
    }
  })
  
  # Reactive year filtered data
  year_filtered_data <- reactive({
    req(input$stream_year_range)
    
    if (input$stream_year_range[1] == input$stream_year_range[2]) {
      year_select <- input$stream_year_range[1]
      return(
        spotify_kp %>%
          mutate(year = year(date)) %>%
          filter(year == year_select)
      )
    } else {
      return (
        spotify_kp %>%
          mutate(year = year(date)) %>%
          filter(year >= input$stream_year_range[1] & year <= input$stream_year_range[2])
      )
    }
  })
  
  # Reactive data for stacked bar plot
  spotify_summary <- reactive({
    if (input$xaxis_choice == "artist") {
      art_filtered_data() %>%
        group_by(unk_art) %>%
        summarise(
          chart_position = min(position),
          streaming = sum(streams),
          .groups = "drop"
        ) %>%
        mutate(x_axis = unk_art)
    } else {
      year_filtered_data() %>%
        group_by(year) %>%
        summarise(
          chart_position = min(position),
          streaming = sum(streams),
          .groups = "drop"
        ) %>%
        mutate(x_axis = as.character(year))
    }
  })
  
  # Stacked Bar 
  output$stacked_bar_plot <- renderPlot({
    
    bar_data <- spotify_summary()
    chart_scale <- max(bar_data$streaming) / 100
    
    # Generate the stacked bar plot
    ggplot(bar_data, aes(x = x_axis)) +
      geom_bar(aes(y = chart_position * chart_scale, fill = "Music Chart Ranking"), stat = "identity", position = "dodge") + # Bar chart
      geom_bar(aes(y = streaming, fill = "Music Streaming Amount"), stat = "identity", position = "dodge") +
      geom_line(aes(x = x_axis, y = streaming, group = 1, color = "Music Streaming Amount"), size = 1) + # Line chart
      geom_line(aes(x = x_axis, y = chart_position * chart_scale, group = 1, color = "Music Chart Ranking"), size = 1) +
      geom_text(aes(x = x_axis, y = streaming, label = paste0(round(streaming/1e6, 1), "M")), 
                color = "#FA991C", nudge_y = 0.05 * max(bar_data$streaming), size = 4) +
      geom_text(aes(x = x_axis, y = chart_position * chart_scale, label = chart_position),
                color = "#032539", nudge_y = 0.03 * max(bar_data$streaming), size = 3) +
      labs(
        x = ifelse(input$xaxis_choice == "artist", "Artist", "Year"),
        y = "Music Streaming Amount",
        fill = "Stacked bar",
        color = "Line chart"
      ) +
      scale_fill_manual(values = c("Music Streaming Amount" = "#FA991C", "Music Chart Ranking" = "#032539")) +
      scale_color_brewer(palette = "Set1") +
      scale_y_continuous(labels = function(x) paste0(round(x/1e6), "M"),
                         sec.axis = sec_axis(
                           ~ . / chart_scale,
                           name = "Music Chart Ranking"
                         )) +
      theme_minimal() +
      theme( # Customise the graph style
        axis.title = element_text(size = 14, face = "bold", family = "Georgia"),
        axis.text = element_text(size = 12, family = "Georgia"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "#f8f9fa", color = NA),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold", family = "Georgia"),
        legend.text = element_text(size = 10, family = "Georgia"), 
      )
  })
  
  # Map Streaming
  map_filtered <- reactive({
    filtered_streams <- spotify_kp %>%
      filter(entry_country != "global") %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
      group_by(entry_country) %>%
      summarise(total_stream = sum(streams, na.rm = TRUE))
    
    # Convert entry country to country code
    filtered_streams$country_name <- countrycode(filtered_streams$entry_country, 
                                            origin = "iso2c", 
                                            destination = "country.name.en")
    
    filtered_streams$country_name <- str_replace(filtered_streams$country_name, "Dominican Republic", "Dominican Rep.")
    filtered_streams$country_name <- str_replace(filtered_streams$country_name, "United States", "United States of America")
    filtered_streams$country_name <- str_replace(filtered_streams$country_name, "Hong Kong SAR China", "Hong Kong")
    
    # Load world data
    world_data <- ne_countries(scale = "medium", returnclass = "sf")
      
    update_world_data <- world_data %>%
      left_join(filtered_streams, by = c("name" = "country_name"))
    update_world_data$total_stream[is.na(update_world_data$total_stream)] <- 0
    
    update_world_data
  })
  
  # Color legend for map
  pal <- reactive({
    colorBin(palette = "YlOrRd", 
             domain = map_filtered()$total_stream[map_filtered()$total_stream > 1e6],
             bins = c(1e6, 100e6, 500e6, 1000e6, 1500e6, 2000e6, 2500e6))
  })
  
  # Initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addProviderTiles("CartoDB.Positron")
  })
  
  # Observe changes in map_filtered and update the map
  observe({
    req(map_filtered(), pal()) # Change here
    
    leafletProxy("map", data = map_filtered()) %>%
      clearShapes() %>%  # Clear any previous polygons
      addPolygons(
        fillColor = ~ ifelse(total_stream < 1e6, "lightgray", pal()(total_stream)),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = ~ paste0(name, ": ", paste0(round(total_stream / 1e6), " M"), " streams")
      ) %>%
      clearControls() %>%  # Clear any previous legends
      addLegend(
        pal = pal(),
        values = ~ total_stream[total_stream > 1e6],
        opacity = 0.7,
        title = "Streaming Amount (Millions)",
        position = "bottomright",
        labFormat = function(type, cuts) {
          labels <- c("1M - 100M", "100M - 500M", "500M - 1000M", "1000M - 1500M", "1500M - 2000M", "2000M - 2500M")
          labels
        }
      )
  })
  
  # Visualisation 3
  kpop_filtered <- reactive({
    req(input$age_group)
    kpop_df %>%
      filter(age_group %in% input$age_group)
  })
  
  # Sankey diagram
  output$sankey <- renderSankeyNetwork({
    
    kpop_data <- kpop_filtered()
    
    # Create nodes
    nodes <- data.frame(name = unique(c(as.character(kpop_data$age_group),
                                        as.character(kpop_data$obsessed_yn),
                                        as.character(kpop_data$music_group),
                                        as.character(kpop_data$pursuit))))
    
    # Create first link with age_group
    link_age_engage <- kpop_data %>%
      count(age_group, obsessed_yn) %>%
      rename(source = age_group, target = obsessed_yn, value = n) %>%
      mutate(IDsource = source)
    
    # Create second link 
    link_engage_music <- kpop_data %>%
      group_by(age_group, obsessed_yn, music_group) %>%
      summarise(value = n(), .groups = 'drop') %>%
      rename(source = obsessed_yn, target = music_group) %>%
      mutate(IDsource = age_group)
    
    # Create third link
    link_music_pursuit <- kpop_data %>%
      group_by(age_group, music_group, pursuit) %>%
      summarise(value = n(), .groups = 'drop') %>%
      rename(source = music_group, target = pursuit) %>%
      mutate(IDsource = age_group)
    
    links <- bind_rows(link_age_engage, link_engage_music, link_music_pursuit)
    
    links$source <- match(links$source, nodes$name) - 1
    links$target <- match(links$target, nodes$name) - 1
    
    age_groups <- unique(kpop_data$age_group)
    my_color <- sprintf(
      'd3.scaleOrdinal().domain([%s]).range([%s])',
      paste0('"', age_groups, '"', collapse = ','),
      paste0('"', RColorBrewer::brewer.pal(length(age_groups), "Pastel1"), '"', collapse = ',')
    )
  
    # Setip sankey diagram
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source", 
                  Target = "target", Value = "value", NodeID = "name",
                  height = "700px", width = "100%", LinkGroup = "IDsource", colourScale = my_color,
                  nodePadding = 20, fontSize = 12, nodeWidth = 20, iterations = 0)
  
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)

